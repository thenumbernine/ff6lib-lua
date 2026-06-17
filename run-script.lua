#!/usr/bin/env luajit
--[[
this can be run standalone, but it is also included from run.lua to do the script portion
args:
	skipOpts = just output script as-is
	hideAddrs = hide the addresses and bytes of the disasm
--]]
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
	assert.type(n, 'number')
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
		local whatPointsToScriptAtAddr = game.whatsPointingToAddr[cmdobj.addr]
		if whatPointsToScriptAtAddr then
			for _,what in ipairs(whatPointsToScriptAtAddr) do
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
		if not self.addr then return end
		assert.index(self, 'cmdset')
		assert.index(self, 'sizeInBytes')
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
		return tab(self.indent + 1)
			..tostring(self):gsub(
				'\n',
				'\n'..tab(self.indent+1)
			)
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

	local function Cmd_toCodeLine(self)
		local s = ''
		-- [[ labels:
		local whatPointsToScriptAtAddr = game.whatsPointingToAddr[self.addr]
		if

		-- if we're in-function and the last wasn't return then don't define a new func -- only a label
		-- and if we still got a call here then we have a problem
		cmdline.skipOpts and

		whatPointsToScriptAtAddr then

			-- print header
			-- TODO baking this into the function-gathering of the AST ... but that means two separate pathways for skipOpts vs not skipOpts
			s = s .. '\n'

			local thisInFunc = addrsIsFunc[self.addr]
			if thisInFunc then
				if not cmdline.hideAddrs then
					s = s .. (' '):rep(disasmcol)
				end
				if labelsForAddr[self.addr] then
					if #labelsForAddr[self.addr] == 1 then
						s = s .. labelsForAddr[self.addr][1]..'=||do\n'
					else
						for i,label in ipairs(labelsForAddr[self.addr]) do
							s = s .. label .. (i == 1 and '=' or ':=')
						end
						s = s .. '||do\n'
					end
				else
					s = s .. (addrLabel(self.addr)..'=||do\n')
				end
			else
				if labelsForAddr[self.addr] then
					for _,label in ipairs(labelsForAddr[self.addr]) do
						s = s .. '::'..label..'::\n'
					end
				else
					s = s .. '::' .. addrLabel(self.addr) .. '::\n'
				end
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
		if not cmdline.hideAddrs then
			local asmline = self:getAsmLine()
			s = s .. align(disasmcol, asmline or '')
		end
		s = s .. self:toCode()
		--]]
		return s
	end


	local Patch  = class()
	Patch.toCodeLine = Cmd_toCodeLine
	function Patch:getAsmLine()
		if not self.addr then return end
		return ('%06x patch'):format(self.addr)
	end
	function Patch:toCode()
		return '\n'
			..tab(self.indent)..self.code:gsub(
				'\n',
				'\n'..tab(self.indent)
			)
	end


	local Loop  = class()
	Loop.toCodeLine = Cmd_toCodeLine
	function Loop:getAsmLine()
		if not self.addr then return end
		return ('%06x loop'):format(self.addr)
	end
	function Loop:toCode()
		local s = table()
		local indent = tab(self.indent+1)
		if self.is5050 then
			s:insert(indent..'repeat')
		else
			s:insert(indent..'while true do')
		end
		for _,stmt in ipairs(self.stmts) do
			s:insert(stmt:toCodeLine())	-- toCodeLine means also insert asm if asked to
		end
		if self.is5050 then
			s:insert(
				(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
				..indent..'until math.random() < .5')
		else
			s:insert(
				(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
				..indent..'end'
			)
		end
		return s:concat'\n'
	end


	-- TODO object-script lambda vs other lambdas?
	local Lambda = class()
	function Lambda:init()
		self.whoCallsThis = table()
	end
	Lambda.toCodeLine = Cmd_toCodeLine
	function Lambda:getAsmLine()
		if not self.addr then return end
		return ('%06x lambda'):format(self.addr)
	end
	function Lambda:toCode()
		-- same as ObjectScript:toCode, print stmts block:
		local s = table()
		local indent = tab(self.indent+1)

		local def
		if self.labels and #self.labels > 1 then
			def = ''
			for i,label in ipairs(self.labels) do
				def = def .. label .. (i == 1 and '=' or ':=')
			end
		else
			def = self.label .. '='
		end
		def = def .. '|'
		if self.args then
			def = def .. table.concat(self.args, ', ')
		end
		def = def .. '|do'
		if self.isLocal then
			def = 'local '..def
		end
		s:insert(indent..def)

		if self.stmts then
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine())	-- toCodeLine means also insert asm if asked to
			end
		end
		s:insert(
			(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
			..indent..'end'
		)
		return s:concat'\n'
	end


	local TailCall = class()
	function TailCall:init(args)
		self.indent = assert.index(args, 'indent')
		self.func = assert.index(args, 'func')
		self.addr = args.addr	-- optional since the lambda calls dont have an addr
	end
	TailCall.toCodeLine = Cmd_toCodeLine
	function TailCall:getAsmLine()
		if not self.addr then return end
		return ('%06x tailcall'):format(self.addr)
	end
	function TailCall:toCode()
		return tab(self.indent+1)..'return '..self.func.label..'(objIndex)'
	end

	local If = class()
	function If:init(args)
		--self.cond = assert.index(args, 'cond')
		--self.stmts = assert.index(args, 'stmts')
	end
	function If:toCode()
		local indent = tab(self.indent+1)
		local s = indent..'if '..self.cond..' then\n'
		for _,stmt in ipairs(self.stmts) do
			s = s .. stmt:toCodeLine()..'\n'
		end
		s = s .. (cmdline.hideAddrs and '' or (' '):rep(disasmcol))
			..indent..'end'
		return s
	end
	If.toCodeLine = Cmd_toCodeLine
	function If:getAsmLine()
		if not self.addr then return end
		return ('%06x if'):format(self.addr)
	end





	-- EventCmds.ObjScript now has nested stmts
	local function EventCmds_ObjectScript_toCode(self)
		local indent = tab(self.indent+1)
		-- if it's just one call to a lambda then don't make our own lambda
		if self.stmts and #self.stmts == 1
		and TailCall:isa(self.stmts[1])
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
			s:insert(
				(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
				..indent..'end}'
			)
		-- if we don't have self.stmts defined then expect the end} to come from the 'return' I guess?  I might have removed that one too soon...
		end
		return s:concat'\n'
	end


	for i=0,0xff do
		local function check(cl)
			if game.EventCmds.ObjectScript:isa(cl) then
				cl.toCode = EventCmds_ObjectScript_toCode
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


	-- BEGIN HIGH LEVEL CODE CONVERSION
	if not cmdline.skipOpts then
		game.ObjectCmds.EndScript.desc = '    return'

		-- first first thing, patch any code directly
		-- from = inclusive, to = exclusive, repl = code
		local function patch(from, to, code)
			local i1 = game.eventScriptCmds:find(nil, function(o) return o.addr == from end)
			assert(i1)
			local i2 = game.eventScriptCmds:find(nil, function(o) return o.addr == to end)
			assert(i2)
			local patch = Patch()
			patch.addr = from
			patch.code = code
			patch.indent = game.eventScriptCmds[i1].indent+1
			game.eventScriptCmds = table():append(
				game.eventScriptCmds:sub(1, i1-1),
				{patch},
				(game.eventScriptCmds:sub(i2))
			)
		end

		patch(0x0aefc8, 0x0af003, [[
local _0aefca=|objIndex, dest|do
	while true do
		if dest <= 0x0aefca then
			objMove(objIndex, 0, 1)
			objMove(objIndex, 3, 5)
		end
		if dest <= 0x0aefcc then
			objMove(objIndex, 3, 1)
			objMove(objIndex, 0, 5)
		end
		if dest <= 0x0aefce then
			objMove(objIndex, 0, 3)
			objMove(objIndex, 0, 3)
		end
		if dest <= 0x0aefd0 then
			objMove(objIndex, 0, 5)
			objMove(objIndex, 3, 1)
		end
		if dest <= 0x0aefd2 then
			objMove(objIndex, 3, 2)
			sleep(10/15)
			objMoveDiag(objIndex, 0)
			objMove(objIndex, 1, 3)
		end
		if dest <= 0x0aefd7 then
			objMove(objIndex, 0, 2)
			objMove(objIndex, 3, 1)
			objMoveDiag(objIndex, 3)
			objMove(objIndex, 3, 2)
		end
		if dest <= 0x0aefdb then
			objMoveDiag(objIndex, 3)
			objMove(objIndex, 3, 2)
			objMoveDiag(objIndex, 3)
			objSetPos(objIndex, 59, 34)
			objMove(objIndex, 0, 2)
		end
		dest = 0
	end
end
objScript{objIndex=17, cb=|objIndex|_0aefca(objIndex, 0x0aefca)}
objScript{objIndex=18, cb=|objIndex|_0aefca(objIndex, 0x0aefcc)}
objScript{objIndex=19, cb=|objIndex|_0aefca(objIndex, 0x0aefce)}
objScript{objIndex=20, cb=|objIndex|_0aefca(objIndex, 0x0aefd0)}
objScript{objIndex=21, cb=|objIndex|_0aefca(objIndex, 0x0aefd2)}
objScript{objIndex=22, cb=|objIndex|_0aefca(objIndex, 0x0aefd7)}
objScript{objIndex=23, cb=|objIndex|_0aefca(objIndex, 0x0aefdb)}
]])

		--[[ this is the same ugly code block on repeat:
		local search = {0x10, 0x04, 0x81, 0xfc, 0x01, 0xff, 0x11, 0x03, 0xfc, 0x05, 0xff, 0x12, 0x03, 0xfc, 0x05, 0xff, 0x13, 0x03, 0xfc, 0x05, 0xff}
		for i=0,game.romsize-#search do
			local dif
			for j=0,#search-1 do
				if j ~= 2	-- this is 0x8x for some x
				and rom[i+j] ~= search[1+j]
				then
					dif = true
					break
				end
			end
			if not dif then
				print('found at '..('_%06x'):format(i)..' with '..('%02x'):format(rom[i+2]))
			end
		end
		--]]

		for _,info in ipairs{
			{0x0a14a4, 1},
			{0x0a1947, 2},
			{0x0a1ac7, 2},
			{0x0a1d1c, 1},
			{0x0a1edf, 3},
			{0x0a241a, 3},
			{0x0a2586, 3},
			{0x0a268b, 3},
		} do
			local startAddr, xmove = table.unpack(info)
			local endAddr = startAddr + 21
			local label= ('_%06x'):format(startAddr+2)
			patch(startAddr, endAddr,
[[local ]]..label..[[=|objScript|do
	while true do
		objMove(objIndex, ]]..xmove..[[, 1)
	end
end
objScript{objIndex=16, cb=]]..label..[[}
objScript{objIndex=17, cb=]]..label..[[}
objScript{objIndex=18, cb=]]..label..[[}
objScript{objIndex=19, cb=]]..label..[[}
]])
		end

		-- instead of  make two while loops inside each other, i'm just patching it
		patch(0x0ae1ca, 0x0ae1de, [[
objScript{objIndex=29, cb=|objIndex|do
    objSetSpeed(objIndex, 4)
	while true do
		sleep(3/15)
		objSetPos(objIndex, 61, 4)
		objMoveDiag(objIndex, 8)
		objMoveDiag(objIndex, 2)
		objSetPos(objIndex, 0, 0)
		if math.random() < .5 then
			sleep(3/15)
		end
	end
end}
]])


-- [[ hmm this is losing .parent pointers...
		-- it's very popular for obj-gotos to goto obj-gotos...
		-- try to consolidate them ...
		do
			-- TODO this  more often
			local cmdForAddr = {}
			for _,cmdobj in ipairs(game.eventScriptCmds) do
				cmdForAddr[cmdobj.addr] = cmdobj
			end
			for _,o in ipairs(game.eventScriptCmds) do
				if game.ObjectCmds.BranchBack:isa(o)
				and o.offset ~= 0xff
				then
					local chain = table()

					local dest = o
					while true do
						local nextDest = assert.index(cmdForAddr, dest:getDestAddr())

						if not (
							game.ObjectCmds.BranchBack:isa(nextDest)
							and nextDest.offset ~= 0xff
						) then break end

						if game.whatsPointingToAddr[nextDest.addr] then
							for j=#game.whatsPointingToAddr[nextDest.addr],1,-1 do
								if game.whatsPointingToAddr[nextDest.addr][j].cmdobj == dest then
									game.whatsPointingToAddr[nextDest.addr]:remove(j)
								end
							end
							if #game.whatsPointingToAddr[nextDest.addr] == 0 then
								game.whatsPointingToAddr[nextDest.addr] = nil
							end
						end

						chain:insert(dest)
						dest = nextDest
					end

					if dest ~= o
					and game.ObjectCmds.BranchBack:isa(dest)
					and dest.offset ~= 0xff
					then
-- caught 41 pokemon here
--print('!!! goto-to-goto at '..('%06x'):format(dest.addr))
						for _,c in ipairs(chain) do
							-- make sure it's a branch-back...
							if dest.addr < c.addr then
								c.offset = c.addr - dest.addr
								--setmetatable(c, game.ObjectCmds.BranchBack)
							elseif dest.addr > c.addr then
								c.offset = dest.addr - c.addr
								--setmetatable(c, game.ObjectCmds.BranchFwd)
							else
								error'here'
							end
							assert.eq(c:getDestAddr(), dest.addr)
							-- and refresh whatsPointingToAddr[]
							-- TODO why is this saying that some gotos no longer have whatsPointingHere entries?
							game.whatsPointingToAddr[dest.addr] = game.whatsPointingToAddr[dest.addr] or table()
							game.whatsPointingToAddr[dest.addr]:insert{
								cmdobj = c,
								branchFromAddr = c.addr,
								cmdset = c.cmdset,
							}
						end
					end
				end
			end
		end
--]]

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
								local branchSrc = src.cmdobj
								if branchSrc
								then
assert.eq(branchSrc:getDestAddr(), target.addr)
									if branchSrc.parent ~= target.parent then
-- happens 118 times
--print(('inter-obj-script-block jump from _%06x to _%06x'):format(branchSrc.addr, target.addr))
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
							lambda.isLocal = true
							lambda.args = {'objIndex'}
							lambda.indent = o.indent
							lambda.stmts = o.stmts:sub(i)
							for _,n in ipairs(lambda.stmts) do
								n.parent = lambda
							end
assert.eq(lambda.stmts[1], target)
							lambda.addr = lambda.stmts[1].addr
							lambda.label = ('_%06x'):format(lambda.addr)

							o.stmts = o.stmts:sub(1, i-1)
							local ocall = TailCall{
								indent = o.indent+1,
								func = lambda,
								addr = lambda.addr,
							}
							ocall.parent = o
							o.stmts:insert(ocall)
							lambda.whoCallsThis:insert(ocall)
							-- o.parent.stmts is gloabl-scope...
							game.eventScriptCmds:insert(j, lambda)
							j=j+1
							--]=]

							for _,branch in ipairs(branchesTo) do
								-- we also rlly want to replace 'branch' in its owner with a 'TailCall'
								local branchParent = branch.parent
								assert(branchParent)
								local k = branchParent.stmts:find(branch)
if not k then
	-- why did I assume branch-back would have a parent always?
	print(('!!! %06x lost its parent pointer'):format(branch.addr))
else
									local bpcall = TailCall{
										indent = o.indent+1,
										func = lambda,
										addr = branch.addr,
									}
									branchParent.stmts[k] = bpcall
									lambda.whoCallsThis:insert(bpcall)

									-- no more label
									for k=#game.whatsPointingToAddr[lambda.addr],1,-1 do
										if game.whatsPointingToAddr[lambda.addr][k].cmdobj == branch then
											game.whatsPointingToAddr[lambda.addr]:remove(k)
										end
									end
end
							end
							if #game.whatsPointingToAddr[lambda.addr] == 0 then
								game.whatsPointingToAddr[lambda.addr] = nil
								addrsIsGoto[lambda.addr] = nil
								addrsIsFunc[lambda.addr] = nil
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
			-- if there's any lambdas that are nothing but a TailCall that points to another lambda
			if Lambda:isa(l) and #l.stmts == 1 then
				local c = l.stmts[1]
				if TailCall:isa(c) then
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
			if game.EventCmds.ObjectScript:isa(o)
			or Lambda:isa(o)
			then

				-- convert branch-5050-fwd's into if-blocks
				local i = 1
				while i <= #o.stmts do
					local bb = o.stmts[i]
					assert(bb.addr)
					if (
						game.ObjectCmds.BranchFwd50:isa(bb)
						-- doesn't exist
						--or game.ObjectCmds.BranchFwd:isa(bb)
					) then
assert.eq(bb.parent, o)
						local j, target = o.stmts:find(nil, function(o2) return o2.addr == bb:getDestAddr() end)
						if not target then
print('!!! WARNING !!! branch-fwd at '
	..('_%06x'):format(bb.addr)
	..' points to unknown dest '
	..(bb:getDestAddr() and ('_%06x'):format(bb:getDestAddr()) or 'nil')
)
						else
assert.eq(target.parent, o)
assert.gt(j, i)

assert(game.whatsPointingToAddr[target.addr])
							for k=#game.whatsPointingToAddr[target.addr],1,-1 do
								if game.whatsPointingToAddr[target.addr][k].cmdobj == bb then
									game.whatsPointingToAddr[target.addr]:remove(k)
								end
							end
							if #game.whatsPointingToAddr[target.addr] == 0 then
								game.whatsPointingToAddr[target.addr] = nil
								addrsIsGoto[target.addr] = nil
								addrsIsFunc[target.addr] = nil
							end

							-- now we can cut this block out and put it in a while-loop
							local if_ = If()
							if_.cond = 'math.random() < .5'
							if_.indent = bb.indent
							if_.stmts = o.stmts:sub(i+1,j-1)
							if_.addr = if_.stmts[1].addr
							if_.parent = o
							for _,c in ipairs(if_.stmts) do
								c.parent = if_
								c.indent = if_.indent + 1
							end

							o.stmts = table():append(
								o.stmts:sub(1,i-1),
								{if_},
								o.stmts:sub(j)
							)
						end
					else
						i = i + 1
					end
				end



				-- convert branch-back and branch-5050-back into while-loops
				local i = 1
				while i <= #o.stmts do
					local bb = o.stmts[i]
					assert(bb.addr)
					if (
						game.ObjectCmds.BranchBack:isa(bb)
						or game.ObjectCmds.BranchBack50:isa(bb)
					) and bb.offset ~= 0xff
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

assert(game.whatsPointingToAddr[target.addr])
							for k=#game.whatsPointingToAddr[target.addr],1,-1 do
								if game.whatsPointingToAddr[target.addr][k].cmdobj == bb then
									game.whatsPointingToAddr[target.addr]:remove(k)
								end
							end
							if #game.whatsPointingToAddr[target.addr] == 0 then
								game.whatsPointingToAddr[target.addr] = nil
								addrsIsGoto[target.addr] = nil
								addrsIsFunc[target.addr] = nil
							end

							-- now we can cut this block out and put it in a while-loop
							local loop = Loop()
							loop.is5050 = game.ObjectCmds.BranchBack50:isa(bb)
							loop.indent = bb.indent
							loop.stmts = o.stmts:sub(j,i-1)
							loop.addr = loop.stmts[1].addr
							loop.parent = o
							for _,c in ipairs(loop.stmts) do
								c.parent = loop
								c.indent = c.indent + 1
							end

							-- skip o.stmts[i] since it is the goto
							o.stmts = table():append(
								o.stmts:sub(1,j-1),
								{loop},
								o.stmts:sub(i+1)
							)
						end
						i = j
					else
						i = i + 1
					end
				end
			end
		end
	--]=]


		-- [=[
		-- now group our global-scope start/end points into functions
		-- this is so-so because I don't know when the functions end/begin
		-- and just stopping at 'return' isn't guaranteed because a 'return' with a jump over it could be an "if x then return" block.
		-- and likewise any 'if x then goto y end' with y ending in 'return' is synonymous with an 'if x then return y() end'
		do
			local i=1
			while i <= #game.eventScriptCmds do
				local cmdobj = game.eventScriptCmds[i]
				-- i think true for all global-scope function-begin commands ...? or could it not be?
				if cmdobj.addr then
--io.stderr:write(('gathering function at %06x\n'):format(cmdobj.addr))
					local whatPointsToScriptAtAddr = game.whatsPointingToAddr[cmdobj.addr]
					if whatPointsToScriptAtAddr then
						local thisInFunc = addrsIsFunc[cmdobj.addr]
						if thisInFunc then
							-- ... then trace to the next return/endscript
							local nextobj
							local j=i
							while j<=#game.eventScriptCmds do
								nextobj = game.eventScriptCmds[j]
								if game.Cmds.Return:isa(nextobj)
								or game.Cmds.EndScript:isa(nextobj)
								then
									break
								end
								j=j+1
							end
--io.stderr:write(('... until %06x\n'):format(nextobj.addr))


							-- ok j is the last stmt of the function
							-- make sure the caller is outside the function
for _,src in ipairs(whatPointsToScriptAtAddr) do
	if src.cmdobj then
		if game.eventScriptCmds[i].addr <= src.cmdobj.addr
		and src.cmdobj.addr <= game.eventScriptCmds[j].addr
		then
			print('!!! WARNING !!! function at '..('%06x'):format(cmdobj.addr)..' called from '..('%06x'):format(src.cmdobj.addr)..' within the function')
		end
	end
end
							-- now copy the stmts into our lambda-block at global-scope
							local lambda = Lambda()
							lambda.indent = 0
							lambda.stmts = game.eventScriptCmds:sub(i,j)
							for _,n in ipairs(lambda.stmts) do
								n.parent = lambda
								-- TODO don't use indent, use recursion in the serialization
								n.indent = lambda.indent + 1
							end
assert.eq(lambda.stmts[1], cmdobj)
							lambda.addr = lambda.stmts[1].addr
-- TODO 'labels' not 'label'
							lambda.labels = labelsForAddr[cmdobj.addr]
							lambda.label = labelsForAddr[cmdobj.addr]
								and labelsForAddr[cmdobj.addr][1]
								or ('_%06x'):format(lambda.addr)


							-- [[ remove trailing returns that nobody points to...
							local last = lambda.stmts:last()
							if (
								game.Cmds.Return:isa(last)
								-- this is like a hard exit() right?
								--or game.Cmds.EndScript:isa(last)
							) and not game.whatsPointingToAddr[last.addr]
							then
								lambda.stmts:remove()
							end
							--]]


							game.eventScriptCmds = table():append(
								game.eventScriptCmds:sub(1,i-1),
								{lambda},
								game.eventScriptCmds:sub(j+1)
							)
						end
					end
				end
				i=i+1
			end
			i=i+1
		end
		--]=]

		-- [=[ now inline our charSwitch's
		do
			local cmdForAddr = {}
			local function buildCmdForAddr(stmts)
				for _,cmdobj in ipairs(stmts) do
					if cmdobj.stmts then
						buildCmdForAddr(cmdobj.stmts)
					end
					cmdForAddr[cmdobj.addr] = cmdobj
				end
			end
			buildCmdForAddr(game.eventScriptCmds)

			local function checkBlock(stmts)
				local i=1
				while i <= #stmts do
					local o = stmts[i]
					if o.stmts then
						checkBlock(o.stmts)
					end
					if game.EventCmds.CallSwitchNPCFlags:isa(o) then
						for optionIndex,option in ipairs(o.options) do
							local destAddr = option.addrOfs + scriptBaseAddr
							assert.index(game.whatsPointingToAddr, destAddr)

--[[ there's a few that are >1 call and so won't be inlined
print('!!! char switch at '
	..('%06x'):format(o.addr)
	..' option #'..optionIndex
	..' points to address with '
	..#game.whatsPointingToAddr[destAddr]
	..' other targets: '
	..game.whatsPointingToAddr[destAddr]:mapi(function(src)
		local s = '{'
		if not src.cmdobj then
			s=s..'cmdobj=nil'
		else
			s=s..'addr='..('%06x'):format(src.cmdobj.addr)
			s=s..', cmdobj==o='..tostring(src.cmdobj==o)
		end
		s=s..'}'
		return s
	end):concat', '
)
--]]
							-- see if there is only one jump into the target
							if #game.whatsPointingToAddr[destAddr] == 1
							and game.whatsPointingToAddr[destAddr][1].cmdobj == o
							then
							-- then we can inline this function
-- 180 pokemon caught here
print('!!! found func at '
	..('%06x'):format(destAddr)
	..' that is only an option of charSwitch at '
	..('%06x'):format(o.addr)
	..' that we can inline')

								-- doesn't always exist, especially if the lambda became inlined...
								local destcmd = assert.index(cmdForAddr, destAddr)
								if not Lambda:isa(destcmd) then
-- TODO TODO TODO hmm
-- There's one function that calls midway through another ...
-- time to split the function and insert a tailcall at the end of the first into the second
print("   !!! and it's not a Lambda !!!")
								else
									-- otherwise,
									-- as long as they have no label ...
									if destcmd.labels then
-- never happens, so we can safely inline
print("   !!! and it has external labels !!!")
									else
										assert.eq(option.stmts, nil)
										-- 1) copy destcmd.stmts into option.stmts
										-- 2) remove the lambda from its parent
										-- 3) filter out whatsPointingToAddr to the lambda (and cmdForAddr?)
									end
								end
								--assert.is(destcmd, Lambda)
							end
						end
					end
					i=i+1
				end
			end
			checkBlock(game.eventScriptCmds)
		end
		--]=]


		-- and same thing for the 50/50 loops?
		-- if it is jump-fwd then change to an if
		-- if it is jump-back then change to a repeat-until loop


		local Goto = class()
		function Goto:init(args)
			self.dest = assert.index(args, 'dest')
		end
		function Goto:toCode()
			return 'goto '..self.dest
		end
		Goto.toCodeLine = Cmd_toCodeLine
		function Goto:getAsmLine() return 'goto' end

		local Break = class()
		function Break:toCode()
			return 'break'
		end
		Break.toCodeLine = Cmd_toCodeLine
		function Break:getAsmLine() return 'break' end

--[=[ now generalize all our common 'if's so I can inline their stmts later
		-- I could do this earlier... hmm...
		-- just have to replace all the whatsPointingToAddr's .cmdobj's ...
		-- TODO this makes me want to preface this whole file with a conversion into lua.parser.'s AST nodes...
		local convertIfs
		do
			local i=1
			while i <= #game.eventScriptCmds do
				local o = game.eventScriptCmds[i]
				local function replace(...)
					game.eventScriptCmds:remove(i)
					for j=1,select('#', ...) do
						local r = select(j, ...)
						r.addr = o.addr
						r.indent = o.indent
						r.parent = o.parent
						game.eventScriptCmds:insert(i, r)
						i = i + 1
					end
				end
				if game.EventCmds.JumpBasedOnBattleFlag:isa(o)
				or game.EventCmds.Jump5050:isa(o)
				or game.Cmds.Cond:isa(o)
				or (game.ObjectCmds.Branch:isa(o) and o.random)
				or game.WorldCmds.IfKeyThenGoto:isa(o)
				or game.WorldCmds.IfFacingThenGoto:isa(o)
				then
					replace(
						If{
							cond = o:getCond(),
							stmts = {
								Goto{dest=addrLabel(o:getDestAddr())},
							},
						}
					)
				elseif game.EventCmds.EndRepeatSwitch:isa(o) then
					replace(
						If{
							cond = o:getCond(),
							stmts = {Break()},
						},
						-- this is a for-loop-end stmt
						-- I should instead be nesting for-loop stmts
						game.EventCmds.EndRepeat()
					)
				end
				i=i+1
			end
		end
		--]=]


--[==[
		-- next trick, scan global-scope for goto-destinations
		-- see who jumps into them
		-- make sure the previous instruction is a 'return'/'endscript'
		for i,o in ipairs(game.eventScriptCmds) do
			-- if something points here...
			local whatPointsToScriptAtAddr = game.whatsPointingToAddr[o.addr]
			if whatPointsToScriptAtAddr
			and #whatPointsToScriptAtAddr == 1
			and not addrsIsFunc[o.addr]
			then
				-- make sure previous command isn't a 'return'
				if (
					i == 1
					or game.Cmds.Return:isa(game.eventScriptCmds[i-1])
					or game.Cmds.EndScript:isa(game.eventScriptCmds[i-1])
				)
				then
-- happens 2776 times without filtering out function-addresses
-- hapepns 973 times with filtering out function-addresses
--print("jump dest with preceding return - inline potential - at "..('_%06x'):format(o.addr))
					-- TODO inline
				else
-- happens 824 times without filtering out function-addresses
-- happens 787 times with filtering out function-addresses
--print("jump dest with no preceding return - func continuation - at "..('_%06x'):format(o.addr))
					-- TOOD make a lambda
				end
			end
		end
--]==]


		-- now rearrange to match the generated ff6t3d script
		-- eventually it's gonna be a matter of chopping it into scene/map pieces ...
		-- from-to segments of how to arrange everything:
		local destOrder = {
--			{0x0aca64,
		}


	-- END HIGH LEVEL CODE CONVERSION
	end	-- cmdline.skipOpts

	local inFunc
	print'BEGIN EVENT SCRIPT'
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		local whatPointsToScriptAtAddr = game.whatsPointingToAddr[cmdobj.addr]
		if whatPointsToScriptAtAddr and addrsIsFunc[cmdobj.addr] then
			inFunc = true
		end

		local lastWasReturn = game.Cmds.Return:isa(cmdobj)
			or game.Cmds.EndScript:isa(cmdobj)

		-- only print 'end' if the last command printed was a 'return'
		if inFunc and lastWasReturn then
			if cmdline.skipOpts then
				print(cmdobj:toCodeLine())
			else
				if not cmdline.hideAddrs then
					io.write((' '):rep(disasmcol-4))
				end
				print('end -- return')
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
