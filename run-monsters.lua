#!/usr/bin/env luajit
local ffi = require 'ffi'
local path = require 'ext.path'
local range = require 'ext.range'

local function run(game, cmdline)
	local countof = game.countof
	local Game = game.Game

	for i=0,game.numMonsters-1 do
		print('monster #'..i)
		print('Name="'..game.monsterNames[i]..'"')
		print('AttackName="'..game.monsterAttackNames[i]..'"')
		print(game.monsters[i])
		print('spells = '..game.monsterSpells[i])
		print(game.monsterItems[i])
		print('sketches = '..game.monsterSketches[i])
		if i < countof(game.monsterRages) then print('rages = '..game.monsterRages[i]) end

		local scriptOfs = game.monsterScriptOfs[i]
		local scriptNextOfs = range(0,game.numMonsters)
			:mapi(function(j) return game.monsterScriptOfs[j] end)
			:filteri(function(ofs) return ofs > scriptOfs end)
			[1] or ffi.sizeof(game.monsterScriptData)

		local baseAddr = ffi.offsetof(Game, 'monsterScriptData')	-- 0x0f8700
		print('script = '..('0x%04x'):format(baseAddr + scriptOfs)..' - '..('0x%04x'):format(baseAddr + scriptNextOfs))
	--	print('\t'..ffi.string(game.monsterScriptData + scriptOfs, scriptNextOfs - scriptOfs):hex():gsub('..', ' %0'))
		print()
	end
end

if select('#', ...) > 0 then
	local cmdline = require 'ext.cmdline'(...)
	print'--[['
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	print'--]]'
	run(game, cmdline)
end

return run
