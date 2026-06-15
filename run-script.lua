#!/usr/bin/env luajit
-- this can be run standalone, but it is also included from run.lua to do the script portion
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
local assert = require 'ext.assert'
local tolua = require 'ext.tolua'


local function align(n, s)
	s = tostring(s)
	return s..(' '):rep(n - #s)
end

local function runScript(game, showAddrs)
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
		local whatPointsToScriptAdAddr = game.eventScriptAddrs[cmdobj.addr]
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
	-- similar to "eventScriptAddrs" ? I gotta combine all these like structures...
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

-- [=[

	local function tab(indent)
		return ('\t'):rep(indent)
	end

	-- return comment with address, bytes, and disasm code
	local function Cmd_getAsmLine(self)
		-- print addr
		return '-- '
			..('_%06x'):format(self.addr)
			--addrLabel(self.addr),
			..'\t'

			-- print shorthand cmdset
			..({
				EventCmds = 'EV',
				WorldCmds = 'WO',
				ObjectCmds = 'OB',
				VehicleCmds = 'VE',
			})[self.cmdset]

			-- print out bytes
			-- TODO for cmds too big, put their data on multiple lines?
			..
				--align(24,
					ffi.string(rom + self.addr, self.sizeInBytes)
						:gsub('.', function(b)
							return (' %02x'):format(b:byte())
						end)
				--)
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
				indent..'\t{'
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

	-- EventCmds.ObjScript now has nested stmts
	local function EventCmds_ObjectScript_toCode(self)
		local s = table{Cmd_toCode(self)}
		if self.stmts then
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine())	-- toCodeLine means also insert asm if asked to
			end
		end
		s:insert(tab(self.indent+1)..'end}')
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

			cl.toCodeLine = function(self)
				return Cmd_getAsmLine(self)..'\n'
					..self:toCode()
			end
		end
		check(game.EventCmds[i])
		check(game.ObjectCmds[i])
		check(game.WorldCmds[i])
		check(game.VehicleCmds[i])
	end


	-- in fact, first-first-first thing, collect objScript blocks.
	-- notice this will break the other reverse-refs. meh.
	do
		i = 1
		while i <= #game.eventScriptCmds do
			local cmdobj = game.eventScriptCmds[i]

			-- [==[
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
							cmdobj.stmts:insert(lastcmd)
						end
						-- now the next stmt should no longer be an obj cmd
						break
					end
				end
			end
			--]==]
			--[==[ see if all obj-cmds branch-back always is either by FF, or has a obj-return next, or has a non-objcmd next
			-- i.e. all are terminating
			if game.ObjectCmds.BranchBack:isa(cmdobj) then
				if game.ObjectCmds.EndScript:isa(game.eventScriptCmds[i+1]) then
					-- return and end obj script
					if game.ObjectCmd:isa(game.eventScriptCmds[i+2]) then
						error(("obj-script found branch-back then 'return' that doesnt terminate obj-script block addr 0x%06x"):format(cmdobj.addr))
					end
					-- good
				elseif not game.ObjectCmd:isa(game.eventScriptCmds[i+1]) then
					-- obj-script block inf-loop
					-- good
				else
					error(("obj-script found branch-back that doesnt terminate obj-script block addr 0x%06x"):format(cmdobj.addr))
				end
			end
			--]==]

			i = i + 1
		end
	end
--]=]

	local inFunc
	print'BEGIN EVENT SCRIPT'
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		local whatPointsToScriptAdAddr = game.eventScriptAddrs[cmdobj.addr]
		if whatPointsToScriptAdAddr then

			-- if we're in-function and the last wasn't return then don't define a new func -- only a label
			-- and if we still got a call here then we have a problem

			-- print header
			print()

			local label
			local builtin = labelsForAddr[cmdobj.addr]
			if builtin then
				for i=2,#builtin do
					local b = builtin[i]
					io.write('::', b, '::\n')
				end
				label = builtin[1]
			else
				label = addrLabel(cmdobj.addr)
			end
			-- TODO multiple addrsIsFunc / labelsForAddr
			-- have one just call the other, or equate to the other
			local thisInFunc = addrsIsFunc[cmdobj.addr]
			if thisInFunc then
				io.write(label, '=||do\n')
			else
				io.write('::', label, '::\n')
			end
			inFunc = inFunc or thisInFunc

			--[[
			for _,what in ipairs(whatPointsToScriptAdAddr) do
				if what.builtin then
					local builtinLabel = fixname(what.builtin)
					if builtinLabel ~= label then
						io.write('::', builtinLabel, '::\n')
					end
				end
			end
			--]]
		end

		local lastWasReturn = game.Cmds.Return:isa(cmdobj)
			or game.Cmds.EndScript:isa(cmdobj)

		-- only print 'end' if the last command printed was a 'return'
		if inFunc and lastWasReturn then
			print'end -- return'
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
	-- hmm if luajit does get ... upon require then ff6 will get passed a bad file
	-- maybe xpcall and bailout on fail?
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	local showAddrs = not not select(2, ...)
	runScript(game, showAddrs)
end

return runScript
