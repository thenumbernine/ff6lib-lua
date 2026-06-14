#!/usr/bin/env luajit
-- this can be run standalone, but it is also included from run.lua to do the script portion
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
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
	local addrsIsGoto = {}
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		if game.EventCmds.Call:isa(cmdobj)
		or game.EventCmds.CallRepeat:isa(cmdobj)
		then
			addrsIsFunc[scriptBaseAddr + cmdobj.destAddrOfs] = true
		elseif game.EventCmds.JumpBasedOnBattleFlag:isa(cmdobj)
		or game.EventCmds.Jump5050:isa(cmdobj)
		or game.Cmds.Cond:isa(cmdobj)
		or game.WorldCmds.IfKeyThenGoto:isa(cmdobj)
		or game.WorldCmds.IfFacingThenGoto:isa(cmdobj)
		--or game.EventCmds.StartTimer:isa(cmdobj)
		then
			-- wait ...
			--addrsIsGoto[cmdobj.addr] = true
		elseif game.EventCmds.CallSwitchNPCFlags:isa(cmdobj) then
			for _,option in ipairs(cmdobj.options) do
				addrsIsFunc[scriptBaseAddr + option.addrOfs] = true
			end

		-- goto, but I don't have its getBranchAddrs, because that causes problems in disasm tracing atm
		elseif game.ObjectCmds.Branch:isa(cmdobj) then
			-- wait ...
			--addrsIsGoto[cmdobj:getDestAddr()] = true
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


	local lastWasReturn
	local inFunc
	print'BEGIN EVENT SCRIPT'
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		local whatPointsToScriptAdAddr = game.eventScriptAddrs[cmdobj.addr]
		if whatPointsToScriptAdAddr then
			-- TODO only if the last command printed was a 'return'
			if inFunc and lastWasReturn then
				print'end'
				inFunc = false	-- ... or not?
			end
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

-- [[ show addresses and bytes
		if showAddrs then
			-- print addr
			io.write('-- ',
				('_%06x'):format(cmdobj.addr),
				--addrLabel(cmdobj.addr),
				'\t')

			-- print shorthand cmdset
			io.write(({
				EventCmds = 'EV',
				WorldCmds = 'WO',
				ObjectCmds = 'OB',
				VehicleCmds = 'VE',
			})[cmdobj.cmdset])

			-- print out bytes
			-- TODO for cmds too big, put their data on multiple lines?
			io.write((
				--align(24,
					ffi.string(rom + cmdobj.addr, cmdobj.sizeInBytes)
						:gsub('.', function(b)
							return (' %02x'):format(b:byte())
						end)
				--)
			))
			print()
	end
--]]

		io.write(('\t'):rep(cmdobj.indent + 1))
		print(cmdobj)

		lastWasReturn = game.Cmds.Return:isa(cmdobj)
			or game.Cmds.EndScript:isa(cmdobj)
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
