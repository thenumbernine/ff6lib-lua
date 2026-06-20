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
	local commonReturnAddr = 0x0a5eb3

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
		elseif game.EventCmds.CallSwitchNPCFlags:isa(cmdobj)
		or game.EventCmds.CallForDialogResult:isa(cmdobj)
		then
			for _,option in ipairs(cmdobj.options) do
				addrsIsFunc[scriptBaseAddr + option.addrOfs] = true
			end

		elseif game.EventCmds.CondBattleFlag:isa(cmdobj)
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
				--`addrsIsGoto` is only used for converting gotos to funcs,
				-- and ObjectCmds gotos are already converted by the time I start on non-object-cmds, so ...
				--addrsIsGoto[destAddr] = addrsIsGoto[destAddr] or table()
				--addrsIsGoto[destAddr]:insert(cmdobj)
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

	local function Cmd_toCode(self, indent)
		return tostring(self)
	end

	-- special-case for multiline indentation for some like charSwitch...
	local function EventCmds_CallSwitchNPCFlags_toCode(self, indent)
		indent = indent or 0
		local tab = ('\t'):rep(indent)
		local s = 'charSwitch{\n'
		for _,option in ipairs(self.options) do
			if not cmdline.hideAddrs then
				s = s .. (' '):rep(disasmcol)
			end
			s = s ..
				tab..'\t{'
				..option.characterIndex..', '
			if option.addrOfs then
				assert(not option.stmts)
				s = s .. game.addrLabel(scriptBaseAddr + option.addrOfs)
			elseif option.stmts then
				assert(not option.addrOfs)
				s = s .. '||do\n'
				for _,stmt in ipairs(option.stmts) do
					s = s .. stmt:toCodeLine(indent+2) .. '\n'
				end
				if not cmdline.hideAddrs then
					s = s .. (' '):rep(disasmcol)
				end
				s = s .. tab .. '\tend'
			end
			s = s ..'},\n'
		end
		if not cmdline.hideAddrs then
			s = s .. (' '):rep(disasmcol)
		end
		s = s .. tab..'}'
		return s
	end

	local function EventCmds_CallForDialogResult_toCode(self, indent)
		indent = indent or 0
		local tab = ('\t'):rep(indent)
		local anyHaveStmts = table.findi(self.options, nil, function(option) return option.stmts end)
		if anyHaveStmts then
		else
			return tostring(self)
		end

		indent = indent or 0
		local tab = ('\t'):rep(indent)
		local s = 'callForDialogResult(\n'
		for i,option in ipairs(self.options) do
			if not cmdline.hideAddrs then
				s = s .. (' '):rep(disasmcol)
			end
			s = s ..tab..'\t'
			if option.addrOfs then
				assert(not option.stmts)
				if scriptBaseAddr + option.addrOfs == commonReturnAddr then	-- call return <-> do nothing
					s = s .. '||do end'
				else
					s = s .. game.addrLabel(scriptBaseAddr + option.addrOfs)
				end
			elseif option.stmts then
				assert(not option.addrOfs)
				s = s .. '||do\n'
				for _,stmt in ipairs(option.stmts) do
					s = s .. stmt:toCodeLine(indent+2) .. '\n'
				end
				if not cmdline.hideAddrs then
					s = s .. (' '):rep(disasmcol)
				end
				s = s .. tab .. '\tend'
			end
			if i < #self.options then
				s = s .. ','
			end
			s = s .. '\n'
		end
		if not cmdline.hideAddrs then
			s = s .. (' '):rep(disasmcol)
		end
		s = s .. tab..')'
		return s
	end

	local function EventCmds_StartTimer_toCode(self, indent)
		if not self.stmts then return tostring(self) end
		assert(not self.destAddrOfs)

		indent = indent or 0
		local tab = ('\t'):rep(indent)

		indent = indent or 0
		local tab = ('\t'):rep(indent)
		local s = 'startTimer{duration='
			..self.duration..', flags='
			..self.flags..', cb='
		s = s .. '||do\n'
		for _,stmt in ipairs(self.stmts) do
			s = s .. stmt:toCodeLine(indent+1) .. '\n'
		end
		if not cmdline.hideAddrs then
			s = s .. (' '):rep(disasmcol)
		end
		s = s .. tab .. 'end}'
		return s
	end



	local Lambda
	local function Cmd_toCodeLine(self, indent)
		indent = indent or 0
		local s = ''

		-- [[ labels:
		local whatPointsToScriptAtAddr = game.whatsPointingToAddr[self.addr]
		if whatPointsToScriptAtAddr then

			-- print header
			-- TODO baking this into the function-gathering of the AST ... but that means two separate pathways for skipOpts vs not skipOpts

			local thisIsFunc = addrsIsFunc[self.addr]
			if thisIsFunc

			-- if we're in-function and the last wasn't return then don't define a new func -- only a label
			-- and if we still got a call here then we have a problem
			-- TODO if `not skipOpts` then we dont need the function labels that are spit out by Lambda ...
			-- ideally, convert all these mid-function-functions into tailcall+separate function and there'll be no need for this condition:
			and cmdline.skipOpts

			then

				s = s .. '\n'
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
				if not (
					Lambda:isa(self)
					or (Lambda:isa(self.parent) and self.parent.stmts[1] == self)
				) then

					if labelsForAddr[self.addr] then
						for _,label in ipairs(labelsForAddr[self.addr]) do
							s = s .. '::'..label..'::\n'
						end
					else
						s = s .. '::' .. addrLabel(self.addr) .. '::\n'
					end
				end
			end
		end
		--]]
		--[[ every other line
		local ls = table()
		ls:insert((self:getAsmLine()))
		-- TODO here insert labels if this is the target of a GOTO...
		ls:insert(self:toCode(indent))
		s = s .. ls:concat'\n'
		--]]
		-- [[ lhs and rhs
		if not cmdline.hideAddrs then
			local asmline = self:getAsmLine()
			if #asmline <= disasmcol then
				s = s .. align(disasmcol, asmline or '')
			else
				s = s .. asmline..'\n'
					..(' '):rep(disasmcol)
			end
		end
		s = s .. ('\t'):rep(indent)..self:toCode(indent)
		--]]
		return s
	end


	local Patch  = class()
	Patch.toCode = Cmd_toCode
	Patch.toCodeLine = Cmd_toCodeLine
	function Patch:getAsmLine()
		if not self.addr then return end
		return ('%06x patch'):format(self.addr)
	end


	local Repeat  = class()
	Repeat.toCodeLine = Cmd_toCodeLine
	function Repeat:getAsmLine()
		if not self.addr then return end
		return ('%06x repeat'):format(self.addr)
	end
	Repeat.stopCond = 'false'	-- forever by default
	function Repeat:toCode(indent)
		indent = indent or 0
		local s = table()
		s:insert('repeat')
		for _,stmt in ipairs(self.stmts) do
			s:insert(stmt:toCodeLine(indent+1))	-- toCodeLine means also insert asm if asked to
		end
		local tab = ('\t'):rep(indent)
		s:insert(
			(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
			..tab..'until '..self.stopCond
		)
		return s:concat'\n'
	end


	local While  = class()
	While.toCodeLine = Cmd_toCodeLine
	function While:getAsmLine()
		if not self.addr then return end
		return ('%06x while'):format(self.addr)
	end
	While.goCond = 'true'	-- forever by default
	function While:toCode(indent)
		indent = indent or 0
		local s = table()
		s:insert('while '..self.goCond..' do')
		for _,stmt in ipairs(self.stmts) do
			s:insert(stmt:toCodeLine(indent+1))	-- toCodeLine means also insert asm if asked to
		end
		local tab = ('\t'):rep(indent)
		s:insert(
			(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
			..tab..'end'
		)
		return s:concat'\n'
	end


	-- TODO object-script lambda vs other lambdas?
	Lambda = class()
	function Lambda:init()
		self.whoCallsThis = table()
	end
	Lambda.toCodeLine = Cmd_toCodeLine
	function Lambda:getAsmLine()
		if not self.addr then return end
		return ('%06x lambda'):format(self.addr)
	end
	function Lambda:toCode(indent)
		indent = indent or 0
		-- same as ObjectScript:toCode, print stmts block:
		local s = table()

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
		s:insert(def)

		if self.stmts then
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine(indent+1))	-- toCodeLine means also insert asm if asked to
			end
		end
		local tab = ('\t'):rep(indent)
		s:insert(
			(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
			..tab..'end'
		)
		return s:concat'\n'
	end


	local TailCall = class()
	function TailCall:init(args)
		self.func = assert.index(args, 'func')
		self.addr = args.addr	-- optional since the lambda calls dont have an addr
	end
	TailCall.toCodeLine = Cmd_toCodeLine
	function TailCall:getAsmLine()
		if not self.addr then return end
		return ('%06x tailcall'):format(self.addr)
	end
	function TailCall:toCode(indent)
		return 'return '..self.func.label..'(objIndex)'
	end

	local If = class()
	function If:init(args)
		--self.cond = assert.index(args, 'cond')
		--self.stmts = assert.index(args, 'stmts')
	end
	function If:toCode(indent)
		indent = indent or 0
		local s = 'if '..self.cond..' then\n'
		for _,stmt in ipairs(self.stmts) do
			s = s .. stmt:toCodeLine(indent+1)..'\n'
		end
		local tab = ('\t'):rep(indent)
		s = s .. (cmdline.hideAddrs and '' or (' '):rep(disasmcol))
			..tab..'end'
		return s
	end
	If.toCodeLine = Cmd_toCodeLine
	function If:getAsmLine()
		if not self.addr then return end
		return ('%06x if'):format(self.addr)
	end



	-- EventCmds.ObjScript now has nested stmts
	local function EventCmds_ObjectScript_toCode(self, indent)
		indent = indent or 0
		local tab = ('\t'):rep(indent)
		-- if it's just one call to a lambda then don't make our own lambda
		if self.stmts and #self.stmts == 1
		and TailCall:isa(self.stmts[1])
		then
			return tab..'objScript{objIndex='..self.cmd
				..(self.blocking and ', block=true' or '')
				..', cb='..self.stmts[1].func.label
				..'}'
		end

		local s = table{Cmd_toCode(self, indent)}
		-- TODO if we have just one call-and-return then just insert that as our callback instead of a lambda of our stmts...
		if self.stmts then
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine(indent+1))
			end
		end
		if self.stmts then
			s:insert(
				(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
				..tab..'end}'
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
			elseif game.EventCmds.CallForDialogResult:isa(cl) then
				cl.toCode = EventCmds_CallForDialogResult_toCode
			elseif game.EventCmds.StartTimer:isa(cl) then
				cl.toCode = EventCmds_StartTimer_toCode
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
		game.ObjectCmds.EndScript.desc = 'return'

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
							lambda.stmts = o.stmts:sub(i)
							for _,n in ipairs(lambda.stmts) do
								n.parent = lambda
							end
assert.eq(lambda.stmts[1], target)
							lambda.addr = lambda.stmts[1].addr
							lambda.label = ('_%06x'):format(lambda.addr)

							o.stmts = o.stmts:sub(1, i-1)
							local ocall = TailCall{
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
							if_.stmts = o.stmts:sub(i+1,j-1)
							if_.addr = if_.stmts[1].addr
							if_.parent = o
							for _,c in ipairs(if_.stmts) do
								c.parent = if_
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
							local loop = Repeat()
							if game.ObjectCmds.BranchBack50:isa(bb) then
								loop.stopCond = 'math.random() < .5'
							end
							loop.stmts = o.stmts:sub(j,i-1)
							loop.addr = loop.stmts[1].addr
							loop.parent = o
							for _,c in ipairs(loop.stmts) do
								c.parent = loop
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
						-- [[ only funcs?
						local thisIsFunc = addrsIsFunc[cmdobj.addr]
						if thisIsFunc then
						--]]
						--[[ gotos as well? (TODO convert gotos into while/if before doing this)
						do
						--]]
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
							lambda.stmts = game.eventScriptCmds:sub(i,j)
							for _,n in ipairs(lambda.stmts) do
								n.parent = lambda
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


		--[[
		now after functions are built and before we inline anything,
		sometimes char-switch or call-based-on-dialog-result will jump mid-func
		so swap those out with tail-calls and new lambdas
		(notice that if they are mid-some other block then this will break something, hopefully not silently)
		--]]
		do
			-- build at global
			local cmdForAddr = {}
			for _,cmdobj in ipairs(game.eventScriptCmds) do
				cmdForAddr[cmdobj.addr] = cmdobj
			end

			-- these will point to cmds inside the functions so we have to search the function statement address range
			local addrSet = {}
			-- [[ only funcs?
			for addr in pairs(addrsIsFunc) do addrSet[addr] = true end
			--]]
			--[[ gotos as well?
			for addr in pairs(addrsIsFunc) do addrSet[addr] = true end
			for addr in pairs(addrsIsGoto) do addrSet[addr] = true end
			--]]
			local addrs = table.keys(addrSet):sort()

			for i=#addrs,1,-1 do
				local addr = addrs[i]
				if cmdForAddr[addr] then
--print((' %06x is addrIsFunc and in eventScriptCmds'):format(addr))
				else
-- 108 of these:
--print(('!!! WARNING %06x is addrIsFunc but not in eventScriptCmds!!!'):format(addr))
					local start
					for j=i-1,1,-1 do
						if cmdForAddr[addrs[j]] then
							start = j
							break
						end
					end
					assert(start)
					local lambda = cmdForAddr[addrs[start]]
					assert(Lambda:isa(lambda))
					local eventScriptIndex = game.eventScriptCmds:find(lambda)
					assert(eventScriptIndex)

					local j = lambda.stmts:find(nil, function(o)
						return o.addr == addr
					end)
					if not j then
print(('!!! WARNING %06x is in addrSet but not in eventScriptCmds!!!'):format(addr))
print(("  ... and found the previous function at %06x but !!! WARNING !!! couldn't find it among the function's stmts"):format(lambda.addr))
					else
-- [[
						assert.gt(j, 1)	-- if it's the first stmt then there shouldn't be separate addresses...

						-- now split off lambda into a new lambda
						local newLambda = Lambda()
						newLambda.stmts = lambda.stmts:sub(j)
						for _,n in ipairs(newLambda.stmts) do
							n.parent = newLambda
						end
						newLambda.addr = newLambda.stmts[1].addr
						newLambda.labels = labelsForAddr[addr]
						newLambda.label = labelsForAddr[addr]
							and labelsForAddr[addr][1]
							or ('_%06x'):format(addr)
						game.eventScriptCmds:insert(eventScriptIndex+1, newLambda)

						lambda.stmts = lambda.stmts:sub(1, j-1)
						local tailCall = TailCall{
							func = newLambda,
							addr = lambda.stmts:last().addr,
						}
						newLambda.whoCallsThis:insert(tailCall)
						tailCall.parent = lambda
						lambda.stmts:insert(tailCall)
--]]
					end
				end
			end
		end


		local Nop = class()

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

			local checkBlock
			--[[
			cmdobj = object that holds the cmd, to point into
			dst = what to assign .stmts to
			--]]
			local function doInline(cmdobj, dst, destAddr)
				assert.index(game.whatsPointingToAddr, destAddr)

--[[ there's a few that are >1 call and so won't be inlined
print('!!! cmd at '
	..('%06x'):format(cmdobj.addr)
	..' is potential for lambda-inline, points to address '
	..('%06x'):format(destAddr)
	..' with '
	..#game.whatsPointingToAddr[destAddr]
	..' targets: '
	..game.whatsPointingToAddr[destAddr]:mapi(function(src)
		local s = '{'
		if not src.cmdobj then
			s=s..'cmdobj=nil'
		else
local tmp = getmetatable(src.cmdobj)
setmetatable(src.cmdobj, nil)
local cmdobjrawname = tostring(src.cmdobj)
setmetatable(src.cmdobj, tmp)
			s=s..'cmdobj='..cmdobjrawname
			s=s..', classname='..require 'ext.tolua'(src.cmdobj.classname)
			s=s..', addr='..('%06x'):format(src.cmdobj.addr)
			s=s..', cmdobj==cmdobj='..tostring(src.cmdobj==cmdobj)
		end
		s=s..'}'
		return s
	end):concat', '
)
--]]
				-- see if there is only one jump into the target
				if #game.whatsPointingToAddr[destAddr] ~= 1 then return end
				if game.whatsPointingToAddr[destAddr][1].cmdobj ~= cmdobj then return end

				-- then we can inline this function
-- 180 pokemon caught here
--print('!!! found destAddr at '..('%06x'):format(destAddr)..' that is an only target of cmd at '..('%06x'):format(cmdobj.addr)..' that we can inline')

				-- doesn't always exist, especially if the lambda became inlined...
				local lambda = assert.index(cmdForAddr, destAddr)
--assert.eq(lambda.addr, destAddr)
				if not Lambda:isa(lambda) then
-- TODO TODO TODO hmm
-- There's one function that calls midway through another ...
-- time to split the function and insert a tailcall at the end of the first into the second
print('!!! found destAddr at '..('%06x'):format(destAddr)..' that is an only target of cmd at '..('%06x'):format(cmdobj.addr).." that we can inline ... BUT it's not a Lambda!")
					return
				end

				-- otherwise,
				-- as long as they have no label ...
				if lambda.labels then
-- never happens, so we can safely inline
print("   !!! and it has external labels !!!")
					return
				end

--[[ this happens every time, probably because the lambda's label
for _,stmt in ipairs(lambda.stmts) do
	if stmt.addr
	and game.whatsPointingToAddr[stmt.addr]
	then
print('	!!! and the lambda contains goto labels !!!')
		break
	end
end
--]]
				-- 1) copy lambda.stmts into option.stmts
				dst.stmts = lambda.stmts
				for _,stmt in ipairs(dst.stmts) do
					stmt.parent = cmdobj
				end
				-- whichever one it is ...
				dst.addrOfs = nil
				dst.destAddrOfs = nil
				-- 2) remove the lambda from its parent
				if lambda.parent then
					local j = lambda.parent.stmts:find(lambda)
					lambda.parent.stmts[j] = Nop()	-- don't invalidate iterators
					lambda.parent.stmts[j].parent = lambda
				else
					local j = game.eventScriptCmds:find(lambda)
					game.eventScriptCmds[j] = Nop()	-- don't invalidate iterators
				end
				-- 3) filter out whatsPointingToAddr to the lambda (and cmdForAddr?)
				game.whatsPointingToAddr[destAddr] = nil

				-- and check those stmts too
				checkBlock(dst.stmts)
				--assert.is(lambda, Lambda)
			end

			function checkBlock(stmts)
				local i=1
				while i <= #stmts do
					local cmdobj = stmts[i]
					if cmdobj.stmts then
						checkBlock(cmdobj.stmts)
					end
					if game.EventCmds.CallSwitchNPCFlags:isa(cmdobj)
					or game.EventCmds.CallForDialogResult:isa(cmdobj)
					then
						for optionIndex,option in ipairs(cmdobj.options) do
							if option.stmts then
								assert(not option.addrOfs)
							else
								local destAddr = scriptBaseAddr + option.addrOfs
								doInline(cmdobj, option, destAddr)
							end
						end
					elseif game.EventCmds.StartTimer:isa(cmdobj) then
						if cmdobj.stmts then
							assert(not cmdobj.destAddrOfs)
						else
							doInline(cmdobj, cmdobj, cmdobj:getDestAddr())
						end
					end
					i=i+1
				end
			end
			checkBlock(game.eventScriptCmds)


			-- only now remove
			local function removeNops(stmts)
				for j=#stmts,1,-1 do
					local o = stmts[j]
					if o.stmts then
						removeNops(o.stmts)
					end
					if Nop:isa(o) then stmts:remove(j) end
				end
			end
			removeNops(game.eventScriptCmds)
		end
		--]=]


		-- and same thing for the 50/50 loops?
		-- if it is jump-fwd then change to an if
		-- if it is jump-back then change to a repeat-until loop


		local Goto = class()
		function Goto:init(args)
			self.dest = assert.index(args, 'dest')
		end
		function Goto:toCode(indent)
			return 'goto '..self.dest
		end
		Goto.toCodeLine = Cmd_toCodeLine
		function Goto:getAsmLine() return 'goto' end

		local Break = class()
		function Break:toCode(indent)
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
						r.parent = o.parent
						game.eventScriptCmds:insert(i, r)
						i = i + 1
					end
				end
				if game.EventCmds.CondBattleFlag:isa(o)
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

		-- I could bunch this up with 'Repeat' ...
		-- but for detection I want them separate classes ...
		-- I should just make a common superclass ...
		-- I should just use a proper langfix ast...
		local ForLoop = class()
		ForLoop.toCodeLine = Cmd_toCodeLine
		function ForLoop:getAsmLine()
			if not self.addr then return end
			return ('%06x for-loop'):format(self.addr)
		end
		function ForLoop:toCode(indent)
			indent = indent or 0
			local s = table()
			s:insert('for i=1,'..self.count..' do')
			for _,stmt in ipairs(self.stmts) do
				s:insert(stmt:toCodeLine(indent+1))	-- toCodeLine means also insert asm if asked to
			end
			local tab = ('\t'):rep(indent)
			s:insert(
				(cmdline.hideAddrs and '' or (' '):rep(disasmcol))
				..tab..'end'
			)
			return s:concat'\n'
		end

		-- by here I should be able to build for-loop blocks, right?
		do
			local function buildForLoops(stmtsObj, stmtsKey, parent)
				local stmts = assert.index(stmtsObj, stmtsKey)
				local i=1
				local startIndexes = table()
				while i <= #stmts do
					local cmd = stmts[i]
					if game.EventCmds.BeginRepeat:isa(cmd) then
						assert(not cmd.stmts)
						startIndexes:insert(i)
						if #startIndexes >= 2 then
							assert.lt(startIndexes[#startIndexes-1], startIndexes:last()) 
						end
					elseif game.EventCmds.EndRepeat:isa(cmd)
					or game.EventCmds.EndRepeatSwitch:isa(cmd)
					then
						assert(not cmd.stmts)
						if #startIndexes == 0 then
							error(("found EndRepeat/Switch without BeginRepeat at %06x"):format(cmd.addr))
						end

						-- trace backwards from end--for
						local startIndex = startIndexes:remove()
						local startCmd = stmts[startIndex]

						local loop = ForLoop()
						loop.count = startCmd.count

						-- since we are throwing away the ending, make sure nothin points there
						assert(not game.whatsPointingToAddr[stmts[i].addr])

						if game.EventCmds.EndRepeatSwitch:isa(cmd) then
							loop.stmts = stmts:sub(startIndex+1,i)
							cmd.skipEndFor = true	-- hack, just reuse EndRepeatSwitch at the end
							-- replace the "if mapFlagGet(436) then break end ... end--for"
							-- with just "if mapFlagGet(436) then break end"
						else
							loop.stmts = stmts:sub(startIndex+1,i-1)
						end


						loop.addr = loop.stmts[1].addr
						loop.parent = parent
						for _,c in ipairs(loop.stmts) do
							c.parent = loop
						end

						local newStmts = table():append(
							stmts:sub(1,startIndex-1),
							{loop},
							stmts:sub(i+1)
						)
						stmtsObj[stmtsKey] = newStmts
						stmts = newStmts

						--buildForLoops(loop, 'stmts', loop)

						assert.eq(stmts[startIndex], loop)
						i = startIndex
					else
						if cmd.stmts then
							buildForLoops(cmd, 'stmts', cmd)
						end
					end
					i=i+1
				end
				if #startIndexes > 0 then
					error('!!! found for-loops without end: '
						..startIndexes:mapi(function(i)
							return ('%06x'):format(stmts[i].addr)
						end)
						:concat', '
					)
				end
			end
			buildForLoops(game, 'eventScriptCmds')
		end

		-- [[ same but with if-statements?
		do
			local function buildIfStmts(stmtsObj, stmtsKey)
				local stmts = assert.index(stmtsObj, stmtsKey)
				local i = #stmts
				while i >= 1 do
					local cmd = stmts[i]
					if (
						game.Cmds.Cond:isa(cmd)
						or game.EventCmds.CondBattleFlag:isa(cmd)
					)
					and stmtsObj ~= game	-- don't do global scope, or it'll mess up on the not-yet-lambda'd global stmts...
					then
						local destAddr = cmd:getDestAddr()
						if destAddr ~= commonReturnAddr then
--print(('!!! cond %06x -> %06x'):format(cmd.addr, destAddr))
							-- then look for the goto in our stmts ...
							local j = stmts:find(nil, function(c) return c.addr == destAddr end)
							if not j then
								-- this is a if-goto that goes outside this block...
-- this is probably pretty common
-- it's gonna have to turn into lambdas and tailcalls
--print(('!!! jump to outside of block at %06x -> %06x'):format(cmd.addr, destAddr))
							elseif j < i then
								-- prev: ... if X then goto prev => repeat ... until not X

								if game.whatsPointingToAddr[destAddr] then
									for j=#game.whatsPointingToAddr[destAddr],1,-1 do
										if game.whatsPointingToAddr[destAddr][j].cmdobj == cmd then
											game.whatsPointingToAddr[destAddr]:remove(j)
										end
									end
									if #game.whatsPointingToAddr[destAddr] == 0 then
										game.whatsPointingToAddr[destAddr] = nil
									end
								end

-- only 13
-- replacing if-goto with while-loop 0a75ce -> 0a75b0
-- replacing if-goto with while-loop 0af98f -> 0af78e
-- replacing if-goto with while-loop 0afa6d -> 0af962 ... not hit
-- replacing if-goto with while-loop 0b08db -> 0b08c2
-- replacing if-goto with while-loop 0b1ff3 -> 0b1fef
-- replacing if-goto with while-loop 0b1ffd -> 0b1ff3
-- replacing if-goto with while-loop 0b4cd7 -> 0b4cd3
-- replacing if-goto with while-loop 0b52d9 -> 0b4f73
-- replacing if-goto with while-loop 0b52ed -> 0b522a
-- replacing if-goto with while-loop 0b68f1 -> 0b6828
-- replacing if-goto with while-loop 0b8183 -> 0b812c
-- replacing if-goto with while-loop 0b819b -> 0b812c
-- replacing if-goto with while-loop 0c5223 -> 0b69ff

print(('!!! replacing if-goto with while-loop %06x -> %06x'):format(cmd.addr, destAddr))
-- current impl only catches 10 of them
-- replacing if-goto with while-loop 0a75ce -> 0a75b0
-- replacing if-goto with while-loop 0af98f -> 0af78e
-- replacing if-goto with while-loop 0b08db -> 0b08c2
-- replacing if-goto with while-loop 0b1ff3 -> 0b1fef
-- replacing if-goto with while-loop 0b1ffd -> 0b1ff3
-- replacing if-goto with while-loop 0b4cd7 -> 0b4cd3
-- replacing if-goto with while-loop 0b52d9 -> 0b4f73
-- replacing if-goto with while-loop 0b68f1 -> 0b6828
-- replacing if-goto with while-loop 0b8183 -> 0b812c
-- replacing if-goto with while-loop 0c5223 -> 0b69ff

								local loop = While()
								loop.goCond = cmd:getCond()
								-- TODO should it be the if's addr or the goto's addr?  both are needed...
								loop.addr = destAddr
								loop.parent = cmd.parent
								loop.stmts = stmts:sub(j, i-1)
								for _,c in ipairs(loop.stmts) do
									c.parent = loop
								end

								local newStmts = table():append(
									stmts:sub(1, j-1),
									{loop},
									stmts:sub(i+1)	-- skip cmd since it goes into the loop now
								)
								stmtsObj[stmtsKey] = newStmts
								stmts = newStmts
								i = j+1

							-- if we handle these in reverse-order then we can catch more...
							elseif i < j then
-- first-to-last catches 792
-- last-to-first catches 833
--print(('!!! replacing if-goto with if-then %06x -> %06x'):format(cmd.addr, destAddr))
								-- if X then goto next; ... next: => if not X then ... end

								local if_ = If()
								local cond = cmd:getCond()
								local rest = cond:match'^not (.*)$'
								if rest then
									if_.cond = rest
								else
									if_.cond = 'not '..cmd:getCond()
								end
								if_.parent = cmd.parent
								if_.addr = cmd.addr
								if_.stmts = stmts:sub(i+1, j-1)
								for _,c in ipairs(if_.stmts) do
									c.parent = if_
								end

								local newStmts = table():append(
									stmts:sub(1, i-1),	-- skip cmd since it goes into the loop now
									{if_},
									stmts:sub(j)
								)
								stmtsObj[stmtsKey] = newStmts
								stmts = newStmts

								if game.whatsPointingToAddr[destAddr] then
									for j=#game.whatsPointingToAddr[destAddr],1,-1 do
										if game.whatsPointingToAddr[destAddr][j].cmdobj == cmd then
											game.whatsPointingToAddr[destAddr]:remove(j)
										end
									end
									if #game.whatsPointingToAddr[destAddr] == 0 then
										game.whatsPointingToAddr[destAddr] = nil
									end
								end

								-- also while we're here, if the 1st stmt is another 'if'
								--  then we can turn it into an 'and'
								if #if_.stmts == 1
								and If:isa(if_.stmts[1])
								then
									if_.cond = '('..if_.cond..') and ('..if_.stmts[1].cond..')'
									if_.stmts = if_.stmts[1].stmts
									for _,c in ipairs(if_.stmts) do
										c.parent = if_
									end
								end
							elseif j == i then
								-- same: if X then goto same => while X do end
								-- nope, this doesn't exist.
								error("does this exist?")
							end
						end
					elseif cmd.stmts then
						buildIfStmts(cmd, 'stmts')
					end
					i=i-1
				end
			end
			buildIfStmts(game, 'eventScriptCmds')
		end
		--]]

-- [[
		-- now rearrange to match the generated ff6t3d script
		-- eventually it's gonna be a matter of chopping it into scene/map pieces ...
		-- from-to segments of how to arrange everything:
		-- this will be just the addrs of the lambdas
		local destOrder = {
			-- generic
			0x0aca64,	-- SetCharFlagForDir
			0x0aca8d,	-- MoveToFaceUpAtNPC
			0x0ac766,	-- UpdateAndShowParty
			0x0ac6ac,	-- SetPartyForCharFlag
			0x0acb95,	-- PartyHideAllExceptLeaderAndGiveUserControl
			0x0b2e2b,	-- PartySetSolid
			0x0b2e34,	-- PartySetNotSolid
			0x0ce499,	-- RestoreParty
			0x0ce566,	-- GameOver
			0x0a5ea9,	-- PostBattle
			{0x0c9aeb, 0x0c9b18}, 	-- SavePoint
			0x0c36a6,	-- MagiciteGhost, map104_npc4, map221_npc18

			-- intro
			0x0a0003,	-- GameStart
			0x0a5e33,	-- NewGame
			{0x0a5e23, 0x0a5e33},	-- [incl,excl) NewGame to-be-inlined
			0x0a5e8e,	-- NewGame to-be-inlined
			0x0c985b,	-- Prologue

			-- narshe
			0x0c9a4f,	-- StartNarsheIntro
			0x0cb1e7,	-- NarsheOpenSecretCave

			-- map18 = cliff before narshe

			-- map19 = intro narshe
			0x0c9b1d,	-- map19_touch0
			0x0c9b71,	-- map19_touch1
			0x0c9bb3,	-- map19_touch2, map19_touch3
			0x0c9c08,	-- map19_touch4
			0x0c9c94,	-- map19_touch5

			-- map20 = narshe
			0x0c33b8,	-- map20_npc26
			0x0c70ab,	-- map20_touch9
			{0x0c7120, 0x0c72ba},	-- map20_touch9 to-be-inlined
			0x0c7083,	-- map20_touch8
			0x0c7097,	-- map20_touch10
			0x0ca279,	-- map20_touch3
			0x0cb054,	-- map20_touch0
			0x0cb06a,	-- map20_touch2
			0x0cb07b,	-- map20_touch1 ... inside of map20_touch2
			0x0cb133,	-- map20_touch4
			{0xcb154, 0xcb1e7},	-- map20_touch4 to-be-inlined
			{0xcb148, 0xcb154},	-- map20_touch4 to-be-inlined
			0x0cb205,	-- map20_touch5
			{0x0cb35c, 0x0cb370},	-- map20_touch5 to-be-inlined
			0x0cb21d,	-- map20_touch7
			{0x0cb370, 0x0cb3f9},	-- map20_touch6 to-be-inlined NOTICE this continues into the next function which is _0cb37f
			0x0cb230,	-- map20_touch6 ... inside of map20_touch7
			0x0cb37f,	-- map20 script	<- if I start making functions out of goto targets then this doesn't get found anymore...
			0x0cd0e7,	-- map20_start
			0x0cd1ef,	-- map20_npc0, map20_npc6
			0x0cd1f3,	-- map20_npc1
			0x0cd1f7,	-- map20_npc2
			0x0cd1fb,	-- map20_npc3
			0x0cd1ff,	-- map20_npc4
			0x0cd203,	-- map20_npc5
			0x0cd207,	-- map20_npc18
			{0x0cd211, 0x0cd215},	-- map20_npc18 to-be-inlined
			0x0cd215,	-- map20_npc19
			{0x0cd21f, 0x0cd223},	-- map20_npc19 to-be-inlined
			0x0cd223,	-- map20_npc20
			{0x0cd22d, 0x0cd231},	-- map20_npc20 to-be-inlined
			0x0cd231,	-- map20_npc21
			{0x0cd23b, 0x0cd23f},	-- map20_npc21 to-be-inlined
			0x0cd23f,	-- map20_npc22
			{0x0cd249, 0x0cd24d},	-- map20_npc22 to-be-inlined
			0x0cd331,	-- map20_touch11
			0x0cd34a,	-- map20_touch13
			0x0cd35c,	-- map20_touch12, map20_touch14 ... inside of map20_touch13
			0x0cd424,	-- map20_touch15
			0x0cd456,	-- map20_touch16, map20_touch17, map20_touch18

			-- map23 = narshe cliff
			0x0cd4a8,	-- map23_touch0
			0x0cd4dd,	-- map23_touch1
			0x0cd4f1,	-- map23_touch3
			0x0cd4fe,	-- map23_touch2 ... inside of map23_touch3
			0x0cd523,	-- map23_touch4, map23_touch5, map23_touch6
			0x0cd594,	-- map23_npc11
			0x0cd5df,	-- map23_npc12

			-- map39 = intro north narshe
			0x0c9d0d,	-- map39_touch3
			0x0c9d97,	-- map39_touch1
			0x0c9da7,	-- map39_touch2
			0x0c9db2,	-- map39_touch0

			-- map41
			0x0c9e23,	-- map41_touch0
			0x0c9ef2,	-- map41_start
			0x0c9f2a,	-- map41_touch1
			0x0c9f37,	-- map41_touch2

			-- map244
			{0x0c88bf, 0x0c8a3f}, 	-- VectorRoof_0c88bf etc
			0x0c86eb,	-- map244_npc20
			0x0c86ff,	-- map244_npc21
			0x0c8713,	-- map244_npc22
			0x0c8727,	-- map244_npc23
			0x0c87a6,	-- map244_npc24
			0x0c92b1,	-- map244_npc25
		}
--]]

		local newCmds = table()
		for i,dest in ipairs(destOrder) do
			if type(dest) == 'number' then
				local j = game.eventScriptCmds:find(nil, function(o) return o.addr == dest end)
				if not j then
					print(('!!! when reorganizing, failed to find %06x'):format(dest))
				else
					local o = game.eventScriptCmds:remove(j)
					newCmds:insert(o)
				end
			elseif type(dest) == 'table' then
				local destStart, destEnd = table.unpack(dest)
				assert.type(destStart, 'number')
				assert.type(destEnd, 'number')
				local insertLoc = #newCmds+1
				local found
				for j=#game.eventScriptCmds,1,-1 do
					local o = game.eventScriptCmds[j]
					if destStart <= o.addr and o.addr < destEnd then
						found = true
						game.eventScriptCmds:remove(j)
						newCmds:insert(insertLoc, o)
					end
				end
				if not found then
					print(('!!! when reorganizing, failed to find range {%06x, %06x}'):format(destStart, destEnd))
				end
			else
				error'here'
			end
		end
		newCmds:append(game.eventScriptCmds)
		game.eventScriptCmds = newCmds

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
				print(cmdobj:toCodeLine(1))
			else
				if not cmdline.hideAddrs then
					io.write((' '):rep(disasmcol-4))
				end
				print('end -- return')
			end
			inFunc = false	-- ... or not?
		else
			print(cmdobj:toCodeLine(0))
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
