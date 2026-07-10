#!/usr/bin/env luajit
local ffi = require 'ffi'
local path = require 'ext.path'
local string = require 'ext.string'
local assert = require 'ext.assert'
local op = require 'ext.op'

local function runItems(game, cmdline)
	print'items = {'
	for i=0,game.numItems-1 do
		print('\t['..i..'] = {')
		print('\t\tname="'..game.itemNames[i]..'",')
		local desc = string.trim(game.gamezstr(game.itemDescBase + game.itemDescOffsets[i]))
		if desc ~= '' then
			print('\t\tdesc="'..desc..'",')
		end
		local item = game.items[i]
		for fieldname, ctype, field in item:fielditer() do
			local value = item[fieldname]
			if fieldname == 'unused_0_7'
			or fieldname == 'unused_5_2'
			or fieldname == 'unused_5_6'
			or fieldname == 'unused_b_1'
			or fieldname == 'unused_c_7'
			or fieldname == 'unused_d_2'
			or fieldname == 'unused_d_5'
			or fieldname == 'unused_13_2'
			then
				assert.eq(value, 0)
			elseif fieldname == 'itemType' then
				print('\t\titemType = "'..({
					[0] = 'Tool',
					[1] = 'Weapon',
					[2] = 'Body',
					[3] = 'Shield',
					[4] = 'Headgear',
					[5] = 'Accessory',
					[6] = 'Item',
				})[value]..'",')
			elseif fieldname == 'spellLearn' then
				if value.rate ~= 0 then
					print('\t\tspellLearn = '..value..',')
				end
			elseif fieldname == 'spellCast' then
				if item.castOnAttack ~= 0
				or item.castOnItemUse ~= 0
				then
					print('\t\tspellCast = "'..game.getSpellName(value)..'",')
				end
			-- separate use between weapon vs armor:
			elseif fieldname == 'element_weaponDamage_equipHalfDamage' then
				if value ~= 0 then
					if item.itemType == 1 then
						print('\t\tdamageElement= '..value..',')
					else
						print('\t\telementHalfDamage = '..value..',')
					end
				end
			elseif fieldname == 'battlePower_defense' then
				if value ~= 0 then
					if item.itemType == 1 then
						print('\t\tphysPower = '..value..',')
					else
						print('\t\tdefense = '..value..',')
					end
				end
			elseif fieldname == 'hitChance_magicDefense' then
				if value ~= 0 then
					if item.itemType == 1 then
						print('\t\thitChance= '..value..',')
					else
						print('\t\tmagicDefense = '..value..',')
					end
				end
			elseif fieldname == 'itemUseAbility' then
				if value > 0 then
					print('\t\titemUseAbility = "'..(game.itemUseAbilityNames[value+1] or tostring(value))..'",')
				end
			elseif fieldname == 'itemSpecialAbility' then
				if value > 0 then
					print('\t\titemSpecialAbility = "'..game.itemSpecialAbilityNames[value+1]..'",')
				end
			else
				if type(ctype) == 'string' and ctype:match':1$' then
					value = value ~= 0
				end

				if fieldname == 'magicBlock' then
					if value > 5 then
						value = 5 - value
					end
					value = value * 10
				end

				if op.safeindex(ctype, 'isBitflags')
				and ffi.cast(ctype.baseType..'*', value.s)[0] == 0
				then
				else
					-- skip false's and 0's
					if value ~= false and value ~= 0 then
						print('\t\t'..fieldname..' = '..tostring(value)..',')
					end
				end
			end
		end
		local colinfo = game.itemColosseumInfos[i]
		print('\t\tcolosseum = {monster = '..colinfo.monster
			..', itemWon = '..colinfo.itemWon
			..(colinfo.hideName ~= 0 and ', hideName=true' or '')
			..'},')
		print('\t},')
	end
	print'}'
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local cmdline = require 'ext.cmdline'(...)
	-- hmm if luajit does get ... upon require then ff6 will get passed a bad file
	-- maybe xpcall and bailout on fail?
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	runItems(game, cmdline)
end

return runItems
