#!/usr/bin/env luajit

local ffi = require 'ffi'
local path = require 'ext.path'
local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local string = require 'ext.string'

local function runBattleScript(game, cmdline)
	cmdline = cmdline or {}
	local rom = game.rom
	local Game = game.Game

	local monsterScriptsAddr = ffi.offsetof(Game, 'monsterScripts')	-- 0x0f8700

	local monstersForAddr = {}
	local scriptAddrs = {}
	for i=0,game.numMonsters-1 do
		local addr = monsterScriptsAddr + game.monsterScriptOfs[i]
		scriptAddrs[addr] = true
		monstersForAddr[addr] = monstersForAddr[addr] or table()
		monstersForAddr[addr]:insert(i)
	end
	scriptAddrs = table.keys(scriptAddrs):sort()

	for i=1,#scriptAddrs do
		local addr = scriptAddrs[i]
		local nextAddr = scriptAddrs[i+1]
			or (monsterScriptsAddr + ffi.sizeof(game.monsterScripts))

		print()
		for _,monsterIndex in ipairs(monstersForAddr[addr]) do
			local name = string.trim(tostring(game.monsterNames[monsterIndex]))
			if name ~= '' then name = ' '..name end
			print('monster #'..monsterIndex..name)
		end

		print('script = '
			..('0x%06x'):format(addr)
			..' - '..('0x%06x'):format(nextAddr)
		)
		local startptr = rom + addr
		local len = nextAddr - addr
		local data = ffi.string(startptr, len)
		local p = ffi.cast('uint8_t*', startptr)
		local pend = p + len
		while p < pend do
			local cmd = p[0]
			io.write(('%06x\t'):format(p - rom))
			if cmd < 0xf0 then
				print('doAttack('..cmd..')\t-- '..game.getSpellName(cmd))
				p=p+1
			elseif cmd == 0xf0 then
				local is = table{p[1], p[2], p[3]}
				print('pickAttack('..is:concat', '..')'
					..'\t-- options: '..is:mapi(function(i) return tostring(game.getSpellName(i)) end):concat', '
				)
				p=p+4
			elseif cmd == 0xf1 then
				local target
				if p[1] == 54 then
					target = 'self'	 -- ? who is 'self'?
				else
					target = 'target #'..p[1]
				end
				print('setTarget('..target..')')
				p=p+2
			elseif cmd == 0xf2 then
				local x1 = p[1]
				local exitEffect = bit.band(0x7f, x1)
				local scrollBG = 0 ~= bit.band(0x80, x1)
				local x23 = ffi.cast('uint16_t*', p+2)[0]
				local battleIndex = bit.band(0x7fff, x23)
				local restoreMonsters = 0 ~= bit.band(0x8000, x23)
				print('change battle', exitEffect, scrollBG, battleIndex, restoreMonsters)
				p=p+4
			elseif cmd == 0xf3 then
				print('dialog', ffi.cast('uint16_t*', p+1)[0])
				p=p+3
			elseif cmd == 0xf4 then
				print('cmd', p[1], p[2], p[3])
				p=p+4
			elseif cmd == 0xf5 then
				local anim = p[1]
				local op = ({
					[0] = "show, set hp to max",
					[1] = "hide, normal",
					[2] = "show, at current hp",
					[3] = "hide, allow targeting",
					[4] = "hide, battle won't end",
					[5] = "hide, debug mode"
				})[p[2]] or 'op???'..p[2]
				local enemyIndex = p[3]
				print(('%q'):format(op), 'anim='..anim,
					enemyIndex == 0 and 'this monster'
					or enemyIndex == 0xff and 'all monsters'
					or (enemyIndex-1))
				p=p+4
			elseif cmd == 0xf6 then
				local useVsThrow = p[1]
				local item12 = p[2]
				local item3 = p[3]
				if useVsThrow == 0 then
					print('use', x, item12, item3)
				elseif useVsThrow == 1 then
					print('throw', x, item12, item3)
				else
					print('use/throw???'..useVsThrow, item12, item3)
				end
				p=p+4
			elseif cmd == 0xf7 then
				local x = p[1]
				print('battle event', x)
				p=p+1
			elseif cmd == 0xf8 then
				local var = p[1]
				local op = bit.band(3, bit.rshift(p[2], 6))
				local value = bit.band(0x3f, p[2])
				if op == 0 then
					print('var['..var..'] = '..value)
				elseif op == 1 then
					print('var['..var..'] ???= '..value)
				elseif op == 2 then
					print('var['..var..'] += '..value)
				elseif op == 3 then
					print('var['..var..'] -= '..value)
				end
				p=p+3
			elseif cmd == 0xf9 then
				if p[1] == 0 then
					print('xor flag', p[2], p[3])
				elseif p[1] == 1 then
					print('set flag', p[2], p[3])
				elseif p[1] == 2 then
					print('clear flag', p[2], p[3])
				else
					print('change flag???'..p[1], p[2], p[3])
				end
				p=p+4
			elseif cmd == 0xfa then
				local x1 = p[1]
				if x1 == 0x09 then
					print('play sound', p[2], p[3])
				else
					-- is fa09 special?
					local x2 = p[2]
					local x3 = p[3]
					print('anim', x1, x2, x3)
				end
				p=p+4
			elseif cmd == 0xfb then
				local cmd2 = p[1]
				if cmd2 < 0x0e then
					print('misc', cmd2, p[2])
				else
					print('misc???', cmd2, p[2])
				end
				p=p+3
			elseif cmd == 0xfc then
				local condIndex = p[1]
				if condIndex == 1 then
					-- p[2] means command-2 which means menu-item-2 which is magic
					print('if command', p[2], p[3])
				elseif condIndex == 2 then
					print('if attack', p[2], p[3])
				elseif condIndex == 3 then
					print('if item', p[2], p[3])
				elseif condIndex == 4 then
					assert.eq(p[3], 0)
					print('if element', p[2])
				elseif condIndex == 5 then
					assert.eq(p[2], 0)
					assert.eq(p[3], 0)
					print('if wasTargeted() then')	-- what's this mean? if an action targeted this monster?
				elseif condIndex == 6 then
					print('if target', p[2], 'hp', p[3] * 128)
				elseif condIndex == 7 then
					print('if target', p[2], 'mp', p[3])
				elseif condIndex == 8 then
					print('if target', p[2], 'has status', p[3])
				elseif condIndex == 9 then
					print('if target', p[2], "doesn't have status", p[3])
				elseif condIndex == 11 then
					assert.eq(p[3], 0)
					print('if monster timer', p[2])
				elseif condIndex == 12 then
					print('if var['..p[2]..'] < '..p[3])
				elseif condIndex == 13 then
					print('if var['..p[2]..'] > '..p[3])
				elseif condIndex == 14 then
					print('if target', p[2], 'level <', p[3])
				elseif condIndex == 15 then
					local target
					-- for all target conds or just this one?
					if p[2] == 68 then
						target = 'randomTarget()'
					elseif p[2] < 16 then
						-- target 0-15 is character 0-15 is object 0-15
						target = 'getObj('..p[2]..')'
					else
						target = 'getTarget('..p[2]..')'
					end
					print('if '..target..'.level > '..p[3]..' then')
				elseif condIndex == 16 then
					assert.eq(p[2], 0)
					assert.eq(p[3], 0)
					print('if one type of monster')
				elseif condIndex == 17 then
					assert.eq(p[3], 0)
					local monsterName = p[2] == 0 and 'this monster'
						or p[2] == 0xff and 'all monsters'
						or 'monster '..(p[2]-1)
					print('if', monsterName, 'is alive')
				elseif condIndex == 18 then
					assert.eq(p[3], 0)
					local monsterName = p[2] == 0 and 'this monster'
						or p[2] == 0xff and 'all monsters'
						or 'monster '..(p[2]-1)
					print('if', monsterName, 'is dead')
				elseif condIndex == 19 then
					local team, op
					if p[2] == 0 then
						team = 'party'
						op = '>='
					elseif p[2] == 1 then
						team = 'enemies'
						op = '<='
					else
						team = '???'..p[2]
						op = '???'
					end
					print('if #', team, 'alive',  op, p[3])
				elseif condIndex == 20 then
					print('if battleFlag['..p[3]..']')
				elseif condIndex == 21 then
					print('if not battleFlag['..p[3]..']')
				elseif condIndex == 22 then
					assert.eq(p[3], 0)
					print('if battle timer', p[2])
				elseif condIndex == 23 then
					assert.eq(p[3], 0)
					print('if target', p[2], 'is valid')
				elseif condIndex == 24 then
					assert.eq(p[2], 0)
					assert.eq(p[3], 0)
					print('if gau present')
				elseif condIndex == 25 then
					assert.eq(p[3], 0)
					local monsterName = p[2] == 0 and 'this monster'
						or p[2] == 0xff and 'all monsters'
						or 'monster '..(p[2]-1)
					print('if', monsterName, 'slot')	-- what does 'this monster slot' or 'all monster slot' mean?
				elseif condIndex == 26 then
					print('if element', p[3])
				elseif condIndex == 27 then
					print('if battleFlag['..ffi.cast('uint16_t*', p+1)[0]..']')
				else
					print('if???', p[1], p[2], p[3])
				end
				p=p+4
			elseif cmd == 0xfd then
				print('passTurn()')
				p=p+1
			elseif cmd == 0xfe then
				print('end--if')
				p=p+1
			elseif cmd == 0xff then
				print('do return end')
				p=p+1
			else
				print('unknown', p[0])
				p=p+1
			end
		end
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
	runBattleScript(game, cmdline)
end

return runBattleScript
