#!/usr/bin/env luajit
-- this can be run standalone, but it is also included from run.lua to do the script portion
local ffi = require 'ffi'
local class = require 'ext.class'
local path = require 'ext.path'
local table = require 'ext.table'
local string = require 'ext.string'
local assert = require 'ext.assert'
local tolua = require 'ext.tolua'

local disasmcol = 32

local function align(n, s)
	s = tostring(s)
	return s..(' '):rep(n - #s)
end

-- cmdline is defined at the end for when you run this standalone
local function runScript(game, cmdline)
	local rom = game.rom

	local function fixname(s)
		return (s:sub(1,1):gsub('%A', '_')
			..s:sub(2):gsub('[^%a%d]', '_'))
	end

	-- TODO at some point convert the traces into functions
	-- and then convert the gotos within functions into if's or while's

	local labelsForAddr = {}
	local function addLabel(addr, name)
		labelsForAddr[addr] = labelsForAddr[addr] or table()
		labelsForAddr[addr]:insert(fixname(name))
	end

	-- keep track of labels from builtin calls
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		local whatPointsToScriptAdAddr = game.whatsPointingToAddr[cmdobj.addr]
		if whatPointsToScriptAdAddr then
			for _,what in ipairs(whatPointsToScriptAdAddr) do
				if what.builtin then
					addLabel(cmdobj.addr, what.builtin)
				end
			end
		end
	end

	-- keep track of labels from map npc and touch-trigger addresses
	for i=0,game.countof(game.maps)-1 do
		local mapInfo = game.getMap(i)
		if mapInfo.startEventScriptAddr then
			addLabel(mapInfo.startEventScriptAddr, 'map'..i..'_start')
		end
		for j,t in ipairs(mapInfo.touchTriggers) do
			local addr = t:getScriptAddr()
			if addr then
				addLabel(addr, 'map'..i..'_touch'..(j-1))
			end
		end
		for j,n in ipairs(mapInfo.npcs) do
			local addr = n:getScriptAddr()
			if addr then
				addLabel(addr, 'map'..i..'_npc'..(j-1))
			end
		end
	end

	local scriptBaseAddr = ffi.offsetof(game.Game, 'eventScript')	-- 0xa0000

	-- now collect addrs and find if they are calls or not
	-- (if they are calls then we will define them as functions)
	local addrsIsFunc = table.map(labelsForAddr, function(v,k) return true, k end):setmetatable(nil)

	-- map from dest-address to source instructions that goto here
	-- similar to "whatsPointingToAddr" ? I gotta combine all these like structures...
	local addrsIsGoto = {}

	for _,cmdobj in ipairs(game.eventScriptCmds) do

		-- call
		if game.EventCmds.Call:isa(cmdobj)
		or game.EventCmds.CallRepeat:isa(cmdobj)
		or game.EventCmds.StartTimer:isa(cmdobj)	-- it is a function call right?  that gets returned from, right?
		then
			addrsIsFunc[cmdobj:getDestAddr()] = true

		-- call (right?)
		elseif game.EventCmds.CallSwitchNPCFlags:isa(cmdobj) then
			for _,option in ipairs(cmdobj.options) do
				addrsIsFunc[scriptBaseAddr + option.addrOfs] = true
			end

		elseif game.EventCmds.JumpBasedOnBattleFlag:isa(cmdobj)
		or game.EventCmds.Jump5050:isa(cmdobj)
		-- Branch is goto, fwd = skip code, backwards = loop
		--  50/50 = 'if math.random() < .5 then...' cond on loop or if
		-- TODO for branch, if the target is *only* this goto
		-- and it's backwards
		-- and there's no other labels within the address range
		-- then turn it into a loop
		or game.Cmds.Cond:isa(cmdobj)
		-- are these even used?
		or game.WorldCmds.IfKeyThenGoto:isa(cmdobj)
		or game.WorldCmds.IfFacingThenGoto:isa(cmdobj)
		then
			local destAddr = cmdobj:getDestAddr()
			addrsIsGoto[destAddr] = addrsIsGoto[destAddr] or table()
			addrsIsGoto[destAddr]:insert(cmdobj)

		elseif game.ObjectCmds.Branch:isa(cmdobj) then
			local destAddr = cmdobj:getDestAddr()
			-- branch will return nil when it's an infinite-loop
			if destAddr then
				addrsIsGoto[destAddr] = addrsIsGoto[destAddr] or table()
				addrsIsGoto[destAddr]:insert(cmdobj)
			end
		end
	end

-- override goto's with calls + returns
-- but that's no good for when the goto is the form of an if-block ...
--[[
	local old = game.Cmds.Cmd.getGotoOfsStr
	game.Cmds.Cmd.getGotoOfsStr = function(addrOfs, op)
		if op then return old(addrOfs, op) end
		return game.addrLabel(addrOfs)..'() return'
	end
	-- but you also gotta replace in alls subclasses...
	for i=0,255 do
		game.EventCmds[i].getGotoOfsStr = game.Cmds.Cmd.getGotoOfsStr
		game.WorldCmds[i].getGotoOfsStr = game.Cmds.Cmd.getGotoOfsStr
		game.ObjectCmds[i].getGotoOfsStr = game.Cmds.Cmd.getGotoOfsStr
		game.VehicleCmds[i].getGotoOfsStr = game.Cmds.Cmd.getGotoOfsStr
	end
--]]

	-- make calls gen to modern func call
	game.EventCmds.Call.desc = "<?=getGotoOfsStr(destAddrOfs, '')?>()"
	game.EventCmds.CallRepeat.desc = "for i=1,<?=count?> do <?=getGotoOfsStr(destAddrOfs, '')?>() end"

	-- when call/goto getting addr, check builtin
	local function addrLabel(addr)
		local builtin = labelsForAddr[addr]
		if builtin then return builtin[1] end
		--return ('$%06x'):format(addr)
		return ('_%06x'):format(addr)
	end
	game.addrLabel = addrLabel


	--[[
	TODO while iterating
	if we get a Cond cmd
	... and it points ahead of us
	... and it points before any 'return' or branch or branch-target
	... and absolutely nobody else points to it or anything between blocks
	... then replace it all with an if-block

	prescription ...
	- start at addrIsFunc's
	- cycle until 'return'
	- insert them into the func's stmts
	- goto's can turn into if's or while's
	- any dangling code?
	- also how to avoid 'return's within if-blocks, when the function keeps going?
		- maybe by hunting for if's beforehand?

in all cases, function-blocks or in-blocks, we can collect commands into block statements if...
... there's no other gotos or calls into the block's statements, i.e. only between known labels
... but getting block-end of a function requires tracing to the 'return'
... while getting block-end of 'if' is just based on where the cond target 'goto' is.

	--]]

	local function tab(indent)
		return ('    '):rep(indent)
	end

	-- return comment with address, bytes, and disasm code
	-- TODO how about a few columsn on the left instead of every-other-line ?
	-- and if the byte dump is too big then give this a few newlines
	local function Cmd_getAsmLine(self)
		-- print addr
		return
			('%06x '):format(self.addr)

			-- print shorthand cmdset
			..({
				EventCmds = 'EV',
				WorldCmds = 'WO',
				ObjectCmds = 'OB',
				VehicleCmds = 'VE',
			})[self.cmdset]

			-- print out bytes
			-- TODO for cmds too big, put their data on multiple lines?
			..ffi.string(rom + self.addr, self.sizeInBytes)
				:gsub('.', function(b)
					return (' %02x'):format(b:byte())
				end)
	end

	local function Cmd_toCode(self)
		return tab(self.indent + 1)..tostring(self)
	end

	-- special-case for multiline indentation for some like charSwitch...
	local function EventCmds_CallSwitchNPCFlags_toCode(self)
		local indent = tab(self.indent + 1)
		local s = indent..'charSwitch{\n'
		for _,option in ipairs(self.options) do
			s = s ..
				indent..'    {'
				..option.characterIndex..', '
				..game.addrLabel(scriptBaseAddr + option.addrOfs)
				..'},\n'
		end
		s = s .. indent..'}\n'
		return s
	end

	-- replace the two-line end} with outer scope indent
	local function ObjectCmds_Branch_toCode(self)
		return tab(self.indent+1)
			..tostring(self):gsub(
				'\n',
				'\n'..tab(self.indent)
			)
	end

	local function Cmd_toCodeLine(self)
		local s = ''
		-- [[ labels:
		local whatPointsToScriptAdAddr = game.whatsPointingToAddr[self.addr]
		if whatPointsToScriptAdAddr then
			-- if we're in-function and the last wasn't return then don't define a new func -- only a label
			-- and if we still got a call here then we have a problem

			-- print header
			s = s .. '\n'

			local label
			local builtin = labelsForAddr[self.addr]
			if builtin then
				for i=2,#builtin do
					local b = builtin[i]
					s = s .. '::' .. b .. '::\n'
				end
				label = builtin[1]
			else
				label = addrLabel(self.addr)
			end
			-- TODO multiple addrsIsFunc / labelsForAddr
			-- have one just call the other, or equate to the other
			local thisInFunc = addrsIsFunc[self.addr]
			if thisInFunc then
				s = s .. (' '):rep(disasmcol) .. label .. '=||do\n'
			else
				s = s .. '::' .. label .. '::\n'
			end
		end
		--]]
		--[[ every other line
		local ls = table()
		ls:insert((self:getAsmLine()))
		-- TODO here insert labels if this is the target of a GOTO...
		ls:insert(self:toCode())
		s = s .. ls:concat'\n'
		--]]
		-- [[ lhs and rhs
		s = s .. align(disasmcol, self:getAsmLine() or '')..self:toCode()
		--]]
		return s
	end


	local BusyLoopBlock  = class()
	BusyLoopBlock.toCodeLine = Cmd_toCodeLine
	function BusyLoopBlock:getAsmLine()
		return ('%06x loop'):format(self.addr)
	end
	function BusyLoopBlock:toCode()
		local s = table()
		local indent = tab(self.indent+1)
		s:insert(indent..'while true do')
		for _,stmt in ipairs(self.stmts) do
			s:insert(stmt:toCodeLine())	-- toCodeLine means also insert asm if asked to
		end
		s:insert((' '):rep(disasmcol)..indent..'end')
		return s:concat'\n'
	end


	local Lambda = class()
	Lambda.toCodeLine = Cmd_toCodeLine
	function Lambda:getAsmLine()
		return ('%06x lambda'):format(self.addr)
	end
	function Lambda:toCode()
		-- same as ObjectScript:toCode, print stmts block:
		local s = table()
		local indent = tab(self.indent+1)
		s:insert(indent..'local '..self.label..'=|objIndex|do')
		if self.stmts then
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine())	-- toCodeLine means also insert asm if asked to
			end
		end
		s:insert((' '):rep(disasmcol)..indent..'end')
		return s:concat'\n'
	end


	local CallAndReturn = class()
	function CallAndReturn:init(args)
		self.indent = assert.index(args, 'indent')
		self.func = assert.index(args, 'func')
		self.addr = args.addr	-- optional since the lambda calls dont have an addr
	end
	CallAndReturn.toCodeLine = Cmd_toCodeLine
	function CallAndReturn:getAsmLine()
		if self.addr then
			return ('%06x'):format(self.addr)
		end
	end
	function CallAndReturn:toCode()
		return tab(self.indent+1)..'return '..self.func.label..'(objIndex)'
	end


	-- EventCmds.ObjScript now has nested stmts
	local function EventCmds_ObjectScript_toCode(self)
		local indent = tab(self.indent+1)
		-- if it's just one call to a lambda then don't make our own lambda
		if self.stmts and #self.stmts == 1
		and CallAndReturn:isa(self.stmts[1])
		then
			return indent..'objScript{objIndex='..self.cmd
				..(self.blocking and ', block=true' or '')
				..', cb='..self.stmts[1].func.label
				..'}'
		end

		local s = table{Cmd_toCode(self)}
		-- TODO if we have just one call-and-return then just insert that as our callback instead of a lambda of our stmts...
		if self.stmts then
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine())	-- toCodeLine means also insert asm if asked to
			end
		end
		if self.stmts then
			s:insert((' '):rep(disasmcol)..indent..'end}')
		-- if we don't have self.stmts defined then expect the end} to come from the 'return' I guess?  I might have removed that one too soon...
		end
		return s:concat'\n'
	end


	for i=0,0xff do
		local function check(cl)
			if game.EventCmds.ObjectScript:isa(cl) then
				cl.toCode = EventCmds_ObjectScript_toCode
			elseif game.ObjectCmds.Branch:isa(cl) then
				cl.toCode = ObjectCmds_Branch_toCode
			elseif game.EventCmds.CallSwitchNPCFlags:isa(cl) then
				cl.toCode = EventCmds_CallSwitchNPCFlags_toCode
			else
				cl.toCode = Cmd_toCode
			end

			cl.toCodeLine = Cmd_toCodeLine
			cl.getAsmLine = Cmd_getAsmLine
		end
		check(game.EventCmds[i])
		check(game.ObjectCmds[i])
		check(game.WorldCmds[i])
		check(game.VehicleCmds[i])
	end


	if not cmdline.skipOpts then
	-- BEGIN HIGH LEVEL CODE CONVERSION

		-- first thing, collect objScript blocks.
		-- notice this will break the other reverse-refs. meh.
		do
			i = 1
			while i <= #game.eventScriptCmds do
				local cmdobj = game.eventScriptCmds[i]
				if game.EventCmds.ObjectScript:isa(cmdobj) then
					-- then trace until ObjectCmds.EndScript
					-- and insert into cmdobj.stmts
					assert.eq(cmdobj.stmts, nil)
					cmdobj.stmts = table()
					while i+1 <= #game.eventScriptCmds do
						local nextcmd = game.eventScriptCmds:remove(i+1)
						if not game.ObjectCmd:isa(nextcmd) then
							error(("mid-obj-script found non-ObjectCmd 0x%02x addr 0x%06x"):format(nextcmd.cmd, nextcmd.addr))
						end
						nextcmd.parent = cmdobj
						cmdobj.stmts:insert(nextcmd)
						if game.ObjectCmds.EndScript:isa(nextcmd) then break end

						-- infinite loop can terminate obj-script regions
						-- all FC FF i.e. inf-loops end obj-script regions. all 2 of them.
						-- do all FC overall end obj-scripts with inf-loops?
						-- seems the non-"FC FF" FC's usu have a FF i.e. 'end-obj-script' after them...
						-- but not all...
						-- technically you could have a branch-fwd preceding the branch-bwd and that could skip the loop part ...
						-- but empirically, i wonder if all FC's have immediately following either a loop or a return.
						if game.ObjectCmds.BranchBack:isa(nextcmd) then
							-- sometimes there's one last objscript 'return'
							if game.ObjectCmds.EndScript:isa(game.eventScriptCmds[i+1]) then
								local lastcmd = game.eventScriptCmds:remove(i+1)
								lastcmd.parent = cmdobj
								cmdobj.stmts:insert(lastcmd)
							end
							-- now the next stmt should no longer be an obj cmd
							break
						end
					end
				end
				i = i + 1
			end
		end

--[[
		-- OK now sometimes objScript branches will jump outside the objScript
		-- in those cases, you have to extract the target block contents into a preceding lambda,
		-- so that the both blocks can use it
		-- I think I can use game.whatsPointingToAddr[destAddr]'s to determine when to do this
		do
			local j=1
			while j<=#game.eventScriptCmds do
				local o = game.eventScriptCmds[j]
				if game.EventCmds.ObjectScript:isa(o) then
					for i=#o.stmts,1,-1 do
						local target = o.stmts[i]

						local branchesTo
						if game.whatsPointingToAddr[target.addr] then
							for _,src in ipairs(game.whatsPointingToAddr[target.addr]) do
								local branchSrc = src.branchSrc
								if branchSrc
								then
	assert.eq(branchSrc:getDestAddr(), target.addr)
									if branchSrc.parent ~= target.parent then
	-- happens 118 times
	--print(('!!! found inter-obj-script-block jump from _%06x to _%06x'):format(branchSrc.addr, target.addr))
	print(('obj jump from _%06x to _%06x'):format(branchSrc.addr, target.addr))
										branchesTo = branchesTo or table()
										branchesTo:insert(branchSrc)
									end
								end
							end
						end

						if branchesTo then
	--print(#branchesTo..' point to '..('%06x'):format(target.addr))
							--[=[ see where it goes...
							-- if another block jumps into this block *here*
							-- see if it is always (a) the first stmt in the obj-script block
							-- or (b) a goto
							if i == 1 then
								-- first stmt, we can extract all stmts as a lambda
							elseif game.ObjectCmds.BranchBack:isa(target)
							or game.ObjectCmds.BranchFwd:isa(target)
							then
								-- branch, goto, just use the dest addr
							else
								-- hmm ok this does happen sometimes
								-- so sometimes I'll have to extract midway of an objscript lambda...
	-- happens 15 times
	--print"... and it doesn't point to either the 1st stmt or another branch..."
							end
							--]=]
							-- [=[
							-- extract into preceding lambda starting at this stmt.
							-- TODO
							local lambda = Lambda()
							lambda.indent = o.indent
							lambda.stmts = o.stmts:sub(i)
	assert.eq(lambda.stmts[1], target)
	assert.eq(lambda.stmts[1].addr, target.addr)
							lambda.addr = lambda.stmts[1].addr
							lambda.label = ('_%06x'):format(lambda.addr)
							lambda.whoCallsThis = table()

							o.stmts = o.stmts:sub(1, i-1)
							local ocall = CallAndReturn{
								indent = o.indent+1,
								func = lambda,
								addr = lambda.addr,
							}
							o.stmts:insert(ocall)
							lambda.whoCallsThis:insert(ocall)
							-- o.parent.stmts is gloabl-scope...
							game.eventScriptCmds:insert(j, lambda)
							j=j+1
							--]=]

							for _,branch in ipairs(branchesTo) do

								-- we also rlly want to replace 'branch' in its owner with a 'CallAndReturn'
								local branchParent = branch.parent
								assert(branchParent)
								local k = branchParent.stmts:find(branch)
								local bpcall = CallAndReturn{
									indent = o.indent+1,
									func = lambda,
									addr = branch.addr,
								}
								branchParent.stmts[k] = bpcall
								lambda.whoCallsThis:insert(bpcall)

								-- no more label
								for k=#game.whatsPointingToAddr[lambda.addr],1,-1 do
									if game.whatsPointingToAddr[lambda.addr][k].branchSrc == branch then
										game.whatsPointingToAddr[lambda.addr]:remove(k)
									end
								end
							end
							if #game.whatsPointingToAddr[lambda.addr] == 0 then
								game.whatsPointingToAddr[lambda.addr] = nil
							end

							-- what happens if we have more than one target into an obj-script?
							-- how to extract the lambdas..
							--break
						end
					end
				end
				j=j+1
			end
		end

		for j,o in ipairs(game.eventScriptCmds) do
			-- now within objScript or lambdas
			-- after all blocks are simplified
			-- if there's any return at the end then toss it
			if game.EventCmds.ObjectScript:isa(o)
			or Lambda:isa(o)
			then
				local last = o.stmts:last()
				if game.ObjectCmds.EndScript:isa(last)
				and not game.whatsPointingToAddr[last.addr]
				then
					o.stmts:remove()
				end
			end
		end

	-- [=[
		for j=#game.eventScriptCmds,1,-1 do
			local l = game.eventScriptCmds[j]
			-- if there's any lambdas that are nothing but a CallAndReturn that points to another lambda
			if Lambda:isa(l) and #l.stmts == 1 then
				local c = l.stmts[1]
				if CallAndReturn:isa(c) then
					-- then replace this lambda with the other
	--print(('collapsing tailcall at _%06x'):format(c.addr))
					for _,w in ipairs(l.whoCallsThis) do
						-- have them call through
						w.func = c.func
					end
					-- and remove the unneeded lambda
					game.eventScriptCmds:remove(j)
				end
			end
		end
	--]=]

	-- [=[
		-- now convert any branch-back within same obj-script block into a while-loop
		for _,o in ipairs(game.eventScriptCmds) do
			if game.EventCmds.ObjectScript:isa(o) then
				for i,bb in ipairs(o.stmts) do
					assert(bb.addr)
					if game.ObjectCmds.BranchBack:isa(bb)
					and bb.offset ~= 0xff
					then
	assert.eq(bb.parent, o)
						local j, target = o.stmts:find(nil, function(o2) return o2.addr == bb:getDestAddr() end)
						if not target then
	print('!!! WARNING !!! branch-back at '
		..('_%06x'):format(bb.addr)
		..' points to unknown dest '
		..(bb:getDestAddr() and ('_%06x'):format(bb:getDestAddr()) or 'nil')
	)
						else
	assert.eq(target.parent, o)
	assert.lt(j, i)
							-- now we can cut this block out and put it in a while-loop
							local busyloop = BusyLoopBlock()
							busyloop.indent = o.stmts[1].indent
							busyloop.stmts = o.stmts:sub(j,i-1)
							busyloop.addr = busyloop.stmts[1].addr
							busyloop.parent = o
							for _,s in ipairs(busyloop.stmts) do
								s.indent = s.indent + 1
							end

							-- skip o.stmts[i] since it is the goto
							local oldstmts = o.stmts
							o.stmts = table()
							for q=1,j-1 do
								o.stmts:insert(oldstmts[q])
							end
							o.stmts:insert(busyloop)
							for q=i+1,#oldstmts do
								o.stmts:insert(oldstmts[q])
							end
						end
					end
				end
			end
		end
	--]=]
--]]

	-- END HIGH LEVEL CODE CONVERSION
	end	-- cmdline.skipOpts

	local inFunc
	print'BEGIN EVENT SCRIPT'
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		local whatPointsToScriptAdAddr = game.whatsPointingToAddr[cmdobj.addr]
		if whatPointsToScriptAdAddr and addrsIsFunc[cmdobj.addr] then
			inFunc = true
		end

		local lastWasReturn = game.Cmds.Return:isa(cmdobj)
			or game.Cmds.EndScript:isa(cmdobj)

		-- only print 'end' if the last command printed was a 'return'
		if inFunc and lastWasReturn then
			if cmdline.skipOpts then
				print(cmdobj:toCodeLine())
			else
				print((' '):rep(disasmcol-4), 'end -- return')
			end
			inFunc = false	-- ... or not?
		else
			print(cmdobj:toCodeLine())
		end
	end
	print()
	print'END EVENT SCRIPT'
	print()

	do
		local function addrtostr(x) return addrLabel(x) end
		local function check(a,b)
			if a.endAddr > b.addr then
				print('!!! collision between '..a:printInterval()..' and '..b:printInterval())
			elseif a.endAddr < b.addr then
				print('-- empty region from '..addrtostr(a.endAddr)..' and '..addrtostr(b.addr))
			end
		end
		local sortedTraces = table.values(game.decompileTraces)
		sortedTraces:sort(function(a,b) return a.addr < b.addr end)
		-- TODO ends as well
		for i=1,#sortedTraces-1 do
			check(sortedTraces[i], sortedTraces[i+1])
		end
		print()
	end
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local cmdline = require 'ext.cmdline'(...)
	-- hmm if luajit does get ... upon require then ff6 will get passed a bad file
	-- maybe xpcall and bailout on fail?
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	runScript(game, cmdline)
end

return runScript
