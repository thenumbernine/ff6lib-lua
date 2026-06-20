#!/usr/bin/env luajit

local ffi = require 'ffi'
local path = require 'ext.path'
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
		print('\t'..
			string.hex(data)
			:gsub('..', ' %0'))
		print()

		local p = ffi.cast('uint8_t*', startptr)
		local pend = p + len
		while p < pend do
			local cmd = p[0]
			io.write(('%06x\t'):format(p - rom))
			if cmd < 0xf0 then
				print('', 'attack', cmd)
				p=p+1
			elseif cmd == 0xf0 then
				local x1 = p[1]
				local x2 = p[2]
				local x3 = p[3]
				print('', 'attack choose one:', x1, x2, x3)
				p=p+4
			elseif cmd == 0xf1 then
				print('', 'change target', p[1])
				p=p+2
			elseif cmd == 0xf2 then
				local x1 = p[1]
				local exitEffect = bit.band(0x7f, x1)
				local scrollBG = 0 ~= bit.band(0x80, x1)
				local x23 = ffi.cast('uint16_t*', p+2)[0]
				local battleIndex = bit.band(0x7fff, x23)
				local restoreMonsters = 0 ~= bit.band(0x8000, x23)
				print('', 'change battle', exitEffect, scrollBG, battleIndex, restoreMonsters)
				p=p+4
			elseif cmd == 0xf3 then
				print('', 'dialog', ffi.cast('uint16_t*', p+1)[0])
				p=p+3
			elseif cmd == 0xf4 then
				print('', 'cmd', p[1], p[2], p[3])
				p=p+4
			elseif cmd == 0xf5 then
				local anim = p[1]
				local showcmd = p[2]
				local enemyIndex = p[3]
				print('', 'show/hide', anim, showcmd, enemyIndex)
				p=p+4
			elseif cmd == 0xf6 then
				local useVsThrow = p[1]
				local item12 = p[2]
				local item3 = p[3]
				if useVsThrow == 0 then
					print('', 'use', x, item12, item3)
				elseif useVsThrow == 1 then
					print('', 'throw', x, item12, item3)
				else
					print('', 'use/throw?', useVsThrow, item12, item3)
				end
				p=p+4
			elseif cmd == 0xf7 then
				local x = p[1]
				print('', 'battle event', x)
				p=p+1
			elseif cmd == 0xf8 then
				local var = p[1]
				local op = bit.band(3, bit.rshift(p[2], 6))
				local value = bit.band(0x3f, p[2])
				if op == 0 then
					print('', 'var['..var..'] = '..value)
				elseif op == 1 then
					print('', 'var['..var..'] ???= '..value)
				elseif op == 2 then
					print('', 'var['..var..'] += '..value)
				elseif op == 3 then
					print('', 'var['..var..'] -= '..value)
				end
				p=p+3
			elseif cmd == 0xf9 then
				if p[1] == 0 then
					print('', 'xor flag', p[2], p[3])
				elseif p[1] == 1 then
					print('', 'set flag', p[2], p[3])
				elseif p[1] == 2 then
					print('', 'clear flag', p[2], p[3])
				else
					print('', 'change flag?', p[1], p[2], p[3])
				end
				p=p+4
			elseif cmd == 0xfa then
				local x1 = p[1]
				if x1 == 0x09 then
					print('', 'play sound', p[2], p[3])
				else
					-- is fa09 special?
					local x2 = p[2]
					local x3 = p[3]
					print('', 'anim', x1, x2, x3)
				end
				p=p+4
			elseif cmd == 0xfb then
				local cmd2 = p[1]
				if cmd2 < 0x0e then
					print('', 'misc', cmd2, p[2])
				else
					print('', 'misc?', cmd2, p[2])
				end
				p=p+3
			elseif cmd == 0xfc then
				print('', 'cond', p[1], p[2], p[3])
				p=p+4
			elseif cmd == 0xfd then
				print('', 'wait one turn')
				p=p+1
			elseif cmd == 0xfe then
				print('', 'end-if')
				p=p+1
			elseif cmd == 0xff then
				print('', 'end-script')
				p=p+1
			else
				print('', 'unknown', p[0])
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
