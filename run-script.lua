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

local function runScript(game)
	local rom = game.rom

	print'BEGIN EVENT SCRIPT'
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		local whatPointsToScriptAdAddr = game.eventScriptAddrs[cmdobj.addr]
		if whatPointsToScriptAdAddr then
			-- print header
			print()
			print(
				('$%06x: '):format(cmdobj.addr)
				..whatPointsToScriptAdAddr:mapi(function(x)
					return (tolua(x, {
						indent = false,
						serializeForType = {
							number = function(state, x, tab, luapath, keyRef)
								return ('$%06x'):format(x)
							end,
						},
					}))
				end):concat'; '
			)
		end

		-- print addr
		io.write(('$%06x'):format(cmdobj.addr), '\t')

		-- print shorthand cmdset
		io.write(({
			EventCmds = 'EV ',
			WorldCmds = 'WO ',
			ObjectCmds = 'OB ',
			VehicleCmds = 'VE ',
		})[cmdobj.cmdset])

		-- print out bytes
		-- TODO for cmds too big, put their data on multiple lines?
		io.write(align(
			24,
			ffi.string(rom + cmdobj.addr, cmdobj.sizeInBytes)
				:gsub('.', function(b)
					return ('%02x '):format(b:byte())
				end)
		))

		if game.ObjectCmd:isa(cmdobj)
		and not game.ObjectCmds.EndScript:isa(cmdobj)
		then
			io.write'    '
		end

		print(cmdobj)
	end
	print()
	print'END EVENT SCRIPT'
	print()

	do
		local function addrtostr(x) return ('$%06x'):format(x) end
		local function check(a,b)
			if a.endAddr > b.addr then
				print('!!! collision between '..a:printInterval()..' and '..b:printInterval())
			elseif a.endAddr < b.addr then
				print('! empty region from '..addrtostr(a.endAddr)..' and '..addrtostr(b.addr))
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
	runScript(game)
end

return runScript
