#!/usr/bin/env luajit
local ffi = require 'ffi'
local path = require 'ext.path'
local assert = require 'ext.assert'
local table = require 'ext.table'
local op = require 'ext.op'

local function runSpells(game, cmdline)
	print'spells = {'
	local countof = game.countof
	for i=0,countof(game.spells)-1 do
		print('\t['..i..'] = {')
		print('\t\tname="'..game.getSpellName(i)..'",')
		local desc = game.getSpellDesc(i)
		if desc then
			print('\t\tdesc="'..desc..'",')
		end
		local spell = game.spells[i]
		for fieldname, ctype, field in spell:fielditer() do
			local value = spell[fieldname]
			if fieldname == 'unused_7_2' then
				-- skip unused fields
				assert.eq(value, 0)
			elseif fieldname == 'specialAbility' and value == 255 then
				-- skip empty specialAbility's
			elseif fieldname == 'removeEffects'
			or fieldname == 'toggleEffects'
			then
				-- bake these into the field name
			elseif fieldname == 'effect1'
			or fieldname == 'effect2'
			or fieldname == 'effect3'
			or fieldname == 'effect4'
			then
				-- skip for now, do all at once at the end
			else
				if type(ctype) == 'string' and ctype:match':1$' then
					value = value ~= 0
				end

				-- skip empty bitflags
				-- TODO struct's tostringOmitEmpty should do this ....
				if op.safeindex(ctype, 'isBitflags')
				and ffi.cast(ctype.baseType..'*', value.s)[0] == 0
				then
				else
					-- skip false's
					if value ~= false and value ~= 0 then
						print('\t\t'..fieldname..' = '..tostring(value)..',')
					end
				end
			end
		end

		-- handle attributes
		local destField =
			spell.removeEffects ~= 0 and 'attributesRemoved'
			or spell.toggleEffects ~= 0 and 'attributesToggled'
			or 'attributes'
		local effectNames = table()
		for _,fieldname  in ipairs{'effect1', 'effect2', 'effect3', 'effect4'} do
			local value = spell[fieldname]
			for flagname, flagType in value:fielditer() do
				if value[flagname] ~= 0 then
					-- specal for mortal
					if flagname == 'Mortal' then
						if destField == 'attributes' then
							print('\t\ttakesLife = true,')
						elseif destField == 'attributesRemoved' then
							print('\t\tgivesLife = true,')
						else
							error'here'
						end
					elseif flagname == 'RemovedFromBattle' then
						-- TODO use the RemovedFromBattle attribute?
						-- but really, dead units aren't removed from battle, they are just counted-towards-loss
						if destField == 'attributes' then
							print('\t\tremovesFromBattle = true,')
						elseif destField == 'attributesRemoved' then
							print('\t\treturnsToBattle = true,')
						else
							error'here'
						end
					else
						-- TODO depending on death flag, either call it 'takesLife' or 'givesLife'
						effectNames:insert(flagname)
					end
				end
			end
		end
		if #effectNames > 0 then
			print('\t\t'..destField..' = {'..effectNames:concat', '..'},')
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
	runSpells(game, cmdline)
end

return runSpells
