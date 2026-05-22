local ffi = require 'ffi'
local assert = require 'ext.assert'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local SRAMWindow = ArrayWindow:subclass()

SRAMWindow.characterIndex = 0

SRAMWindow.name = 'sram'

function SRAMWindow:getCount()
	if not self.app.sram then return end
	return 3
end

function SRAMWindow:showIndexUI()
	local app = self.app
	local game = app.game
	if not app.sram or not game then return end
	local save = app.sram.saves.s + self.index

	ig.igSeparator()
	if ig.igCollapsingHeader'save fields:' then
		for fieldname, ctype, field in save:fielditer() do
			-- struct
			if fieldname == 'characters'
			-- checkbox arrays
			or fieldname == 'esperFlags'
			or fieldname == 'swdtechFlags'
			or fieldname == 'blitzFlags'
			or fieldname == 'loreFlags'
			or fieldname == 'rageFlags'
			or fieldname == 'danceFlags'
			or fieldname == 'mapFlags'
			or fieldname == 'npcFlags'
			-- handled separately
			or fieldname == 'treasureFlags'
			or fieldname == 'battleFormationFlags'
			then
				-- handle these below as checkbox grids
			else
				-- default
				self:editField(save, fieldname, ctype, field)
			end
		end

		ig.igSameLine()
		if ig.igButton'checksum...' then
			local x = 0
			local ptr = ffi.cast('uint8_t*', save)
			for i=0,ffi.sizeof(game.SaveSlot)-3 do
				x = x + ptr[i]
			end
			save.checksum = bit.band(0xffff, x)
		end
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'character fields:' then
		ig.luatableInputInt('characterIndex', self, 'characterIndex')
		local character = save.characters.s + bit.band(15, self.characterIndex)
		for fieldname, ctype, field in character:fielditer() do
			self:editField(character, fieldname, ctype, field)
		end

		-- 12 x 54 spells saved...
		if self.characterIndex < 12 then
			local numLearnSpells = 54
			local charSpellLearns = save.spellsLearned + self.characterIndex * numLearnSpells
			for i=0,numLearnSpells-1 do
				ig.igPushID_Int(i)
				ig.luatableInputInt('', charSpellLearns, i)
				ig.igPopID()
				ig.igSameLine()
				app.spellWindow:popupButton(i)
			end
		end
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'flags:' then

		local function showFlags(args)
			local flagField = args.flagField
			local getname = args.getname
			local onchange = args.onchange
			local link = args.link
assert.type(flagField, 'string')
			ig.igPushID_Str(flagField)

			local colSize = link and 8 or 16
			ig.igSeparator()
			local flagobj = save[flagField]	-- should be a uint8_t[] array
			local count = bit.lshift(game.countof(flagobj), 3)
			for i=0,count-1 do
				ig.igPushID_Int(i)

				local byteofs = bit.rshift(i, 3)
				local bitofs = bit.band(i, 7)
				local mask = bit.lshift(1, bitofs)
				self.__tmp = 0 ~= bit.band(mask, flagobj[byteofs])

				local checkboxName = getname and getname(i) or flagField..' '..i
				if ig.luatableTooltipCheckbox(checkboxName, self, '__tmp') then
					flagobj[byteofs] = bit.bxor(flagobj[byteofs], mask)
					if onchange then onchange() end
				end

				if link then
					ig.igSameLine()
					if ig.igButton'>' then
						link(i)
					end
					ig.hoverTooltip(checkboxName)
				end

				if i < count-1 and i % colSize < colSize-1 then
					ig.igSameLine()
				end

				ig.igPopID()
			end

			ig.igPopID()
		end

		ig.igText'espers:'
		showFlags{
			flagField = 'esperFlags',
			getname = function(i) return game.getSpellName(i + 54) end,
			link = function(i) return app.spellWindow:open(i + 54) end,
		}

		ig.igText'swdtechs:'
		showFlags{
			flagField = 'swdtechFlags',
			getname = function(i) return tostring(game.swordTechNames[i]) end,
		}

		ig.igText'blitzes:'
		showFlags{
			flagField = 'blitzFlags',
			getname = function(i) return game.getSpellName(i + 93) end,
			link = function(i) return app.spellWindow:open(i + 93) end,
		}

		ig.igText'lores:'
		showFlags{
			flagField = 'loreFlags',
			getname = function(i) return game.getSpellName(i + 139) end,
			link = function(i) return app.spellWindow:open(i + 139) end,
		}

		ig.igText'dances:'
		showFlags{
			flagField = 'danceFlags',
			getname = function(i) return tostring(game.mogDanceNames[i]) end,
		}

		ig.igText'map flags:'
		showFlags{
			flagField = 'mapFlags',
		}

		-- TODO reverse-reference to NPCs that use this flag
		ig.igText'npc flags:'
		showFlags{
			flagField = 'npcFlags',
		}

		-- TODO reverse-reference to treasures that use this flag
		ig.igText'treasure flags:'
		showFlags{
			flagField = 'treasureFlags',
		}

		-- TODO reverse-references
		ig.igText'battle formation flags:'
		showFlags{
			flagField = 'battleFormationFlags',
			getname = function(i)
				return game.formations[i]:getDesc()
			end,
			link = function(i)
				app.battleFormationWindow:open(i)
			end,
			onchange = function()
				self:refreshMonstersEnabled()
			end,
		}

		ig.igText'rages:'
		showFlags{
			flagField = 'rageFlags',
			getname = function(i) return tostring(game.monsterNames[i]) end,
			link = function(i) return app.monsterWindow:open(i) end,
		}
	end
end

function SRAMWindow:setIndex(...)
	SRAMWindow.super.setIndex(self, ...)

	self:refreshMonstersEnabled()
end

function SRAMWindow:refreshMonstersEnabled()
	local app = self.app
	local game = app.game
	if not app.sram or not game then return end
	if self.index < 0 or self.index >= self:getCount() then return end
	local save = app.sram.saves.s + self.index

	self.monstersEnabled = {}
	local numFormationFlags = bit.lshift(game.countof(save.battleFormationFlags), 3)
	for i=0,numFormationFlags-1 do
		local byteofs = bit.rshift(i, 3)
		local bitofs = bit.band(i, 7)
		local mask = bit.lshift(1, bitofs)
		local formationEnabled = 0 ~= bit.band(mask, save.battleFormationFlags[byteofs])
		formationsEnabled[i] = formationEnabled
		if formationEnabled then
			local formation = game.formations + i
			-- do I care about chooseNextFour as well?
			for j=1,6 do
				if formation:getMonsterActive(j) then
					local monsterIndex = formation:getMonsterIndex(j)
					self.monstersEnabled[monsterIndex] = true
				end
			end
		end
	end
end

return SRAMWindow
