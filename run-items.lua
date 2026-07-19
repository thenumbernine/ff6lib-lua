#!/usr/bin/env luajit
local ffi = require 'ffi'
local path = require 'ext.path'
local string = require 'ext.string'
local table = require 'ext.table'
local assert = require 'ext.assert'
local op = require 'ext.op'

-- 0-based in game, 1-based table, so +1 the index
local itemTypes = {
	'Tool',
	'Weapon',
	'Body',
	'Shield',
	'Headgear',
	'Accessory',
	'Item',
}

local defenseTypes = table{'Body', 'Shield', 'Headgear', 'Accessory'}

local equippableTypes = table{'Weapon', 'Body', 'Shield', 'Headgear', 'Accessory'}


-- notice game.itemTypeNames[] is based on the first character in the name ... (right?)

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

		local itemType = itemTypes[1+item.type]
		local isEquippable = not not equippableTypes:findi(itemType)
		local isDefenseType = defenseTypes:findi(itemType)

		-- when to rename fields.  custom per type.
		local renameField = {
			evade = 'physEvade',
			magicBlock = 'magicEvade',
			changeJumpToXJump = 'jumpContinuously',
		}

		if itemType == 'Weapon' then
			renameField.battlePower_defense = 'physPower'
		elseif itemType == 'Item' then
			renameField.battlePower_defense = 'pointsGiven'
		else
			renameField.battlePower_defense = 'physDefense'
		end

		if itemType == 'Weapon' then
			renameField.element_weaponDamage_equipHalfDamage = 'elementsDamage'
		else
			renameField.element_weaponDamage_equipHalfDamage = 'elementsHalf'
		end

		if itemType == 'Item' then
			-- offset 0x13 is different for itemtype-item
			renameField.swordTech = 'damageUndead'
			renameField.sameDamageFromBackRow = 'removeStatus'	-- baked in anyways, don't need to rename the field cuz it's not printed
			renameField.twoHands = 'causeDamage'
			-- why is this "1/16th of max?" seems more like it is max.
			renameField.runic = 'oneSixteenthOfMax'
		end


		for fieldname, ctype, field in item:fielditer() do
			local value = item[fieldname]
			if fieldname == 'unused_0_7'
			or fieldname == 'unused_5_2'
			or fieldname == 'unused_5_6'
			or fieldname == 'unused_b_1'
			or fieldname == 'unused_c_7'
			or fieldname == 'unused_d_5'
			or fieldname == 'unused_13_2'
			then
				assert.eq(value, 0)
			elseif fieldname == 'type' then
				print('\t\ttype = "'..itemTypes[1+value]..'",')
			elseif fieldname == 'equip' then
				local x = value.s[0]
				local equipFlags = bit.band(0x3f, x)
				if equipFlags ~= 0 then
					print('\t\tequipFlags = 0x'..bit.tohex(equipFlags, 2)..',')
				end
				if 0 ~= bit.band(0x40, x) then
					print('\t\timpItem = true,')
				end
				if 0 ~= bit.band(0x80, x) then
					print('\t\tmeritAward = true,')
				end
			elseif fieldname == 'spellLearn' then
				if value.rate ~= 0 then
					print('\t\tspellLearnRate = '..value.rate..',')
					print('\t\tspellLearned = "'..game.getSpellName(value.spell.i)..'",')
				end
			elseif fieldname == 'casts' then
				if item.castsOnAttack ~= 0
				or item.castsOnItemUse ~= 0
				then
					print('\t\tcasts = "'..game.getSpellName(value)..'",')
				end
			elseif fieldname == 'hitChance_magicDefense' then
				if value ~= 0 then
					if itemType == 'Weapon' then
						print('\t\thitChance= '..value..',')
					elseif itemType == 'Item' then
						-- handle this later
					else
						print('\t\tmagicDefense = '..value..',')
					end
				end
			elseif itemType == 'Item' and (
				fieldname == 'elementsAbsorb'
				or fieldname == 'elementsImmune'
				or fieldname == 'elementsWeak'
				or fieldname == 'sameDamageFromBackRow'
			) then
				-- handle item effects given/removed later
			elseif fieldname == 'itemUseAbility' then
				if value > 0 then
					print('\t\titemUseAbility = "'..(game.itemUseAbilityNames[value+1] or tostring(value))..'",')
				end
			elseif fieldname == 'itemSpecialAbility' then
				if value > 0 then
					print('\t\titemSpecialAbility = "'..game.itemSpecialAbilityNames[value+1]..'",')
				end
			elseif fieldname == 'immuneToEffect1'
			or fieldname == 'immuneToEffect2'
			or fieldname == 'givesEffect3'
			or fieldname == 'givesEffect2'
			then
				-- later
				-- convert to table-of-string-names
				-- immuneToEffect1 is only for equippable i.e.
			else
				if type(ctype) == 'string' and ctype:match':1$' then
					value = value ~= 0
				end

				-- signed-8-bit fields:
				if fieldname == 'vigor'
				or fieldname == 'speed'
				or fieldname == 'stamina'
				or fieldname == 'magicPower'
				then
					value = 0 ~= bit.band(value, 8)
						and -bit.band(value, 7)
						or value
				elseif fieldname == 'magicBlock'
				or fieldname == 'evade'
				then
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
						print('\t\t'..(renameField[fieldname] or fieldname)..' = '..tostring(value)..',')
					end
				end
			end
		end

		-- last re-add these converted:
		do
			local function getEffectNames(value)
				local effectNames = table()
				for flagname, flagType in value:fielditer() do
					if value[flagname] ~= 0 then
						effectNames:insert(flagname)
					end
				end
				return effectNames
			end

			if itemType == 'Item' then
				local attributes = table()
				for j=1,4 do
					attributes:append(
						getEffectNames(
							ffi.cast(
								ffi.typeof('$*', game['Effect'..j]),
								ffi.cast('uint8_t*', item) + 0x15 + (j-1)
							)[0]
						)
					)
				end
				if #attributes > 0 then
					local dstname = 0 ~= item.sameDamageFromBackRow
						and 'attributesRemoved'
						or 'attributes'
					print('\t\t'..dstname..' = {'..attributes:mapi(function(e) return ('%q'):format(e) end):concat', '..'},')
				end
			end

			-- only for isEquippable?
			local attributesImmune = table.append(
				getEffectNames(item.immuneToEffect1),
				getEffectNames(item.immuneToEffect2)
			)
			if #attributesImmune > 0 then
assert(isEquippable)
				print('\t\tattributesImmune = {'..attributesImmune:mapi(function(e) return ('%q'):format(e) end):concat', '..'},')
			end

			-- only for isEquippable?
			local attributesGiven = table.append(
				getEffectNames(item.givesEffect2),
				getEffectNames(item.givesEffect3)
			)
			if #attributesGiven > 0 then
assert(isEquippable)
assert.ne(itemType, 'Item')	-- Item already has attributes
				print('\t\tattributes = {'..attributesGiven:mapi(function(e) return ('%q'):format(e) end):concat', '..'},')
			end
		end

		local colinfo = game.itemColosseumInfos[i]
		print('\t\tcolosseumMonster = '..colinfo.monster..',')
		print('\t\t colosseumWinItem = '..colinfo.itemWon..',')
		if colinfo.hideName ~= 0 then
			print('\t\tcolosseumHideName = true,')
		end
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
