#!/usr/bin/env luajit
local ffi = require 'ffi'
local path = require 'ext.path'
local assert = require 'ext.assert'
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
			elseif fieldname == 'specialEffect' and value == 255 then
				-- skip empty specialEffect's
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
					if value ~= false then
						print('\t\t'..fieldname..' = '..tostring(value)..',')
					end
				end
			end
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
