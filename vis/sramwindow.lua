local ffi = require 'ffi'
local assert = require 'ext.assert'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local SRAMWindow = ArrayWindow:subclass()

SRAMWindow.characterIndex = 0

SRAMWindow.name = 'sram'

function SRAMWindow:getIndex(i)
	if not self.app.game or not self.app.sram then return end
	if i <= 0 or i > self:getCount() then return end
	return self.app.sram.saves.s + i
end

function SRAMWindow:getCount()
	if not self.app.sram then return end
	return 3
end

function SRAMWindow:showIndexUI()
	local app = self.app
	local game = app.game
	local save = self:getCurIndex()
	if not save then return end

	ig.igSeparator()
	if ig.igCollapsingHeader'save fields:' then
		for fieldname, ctype, field in save:fielditer() do
			-- struct handled below
			if fieldname == 'characters'
			or fieldname == 'spellsLearned'
			or fieldname == 'itemTypes'
			or fieldname == 'itemCounts'
			-- checkbox arrays handled below
			or fieldname == 'esperFlags'
			or fieldname == 'swdtechFlags'
			or fieldname == 'blitzFlags'
			or fieldname == 'loreFlags'
			or fieldname == 'rageFlags'
			or fieldname == 'danceFlags'
			or fieldname == 'mapFlags'
			or fieldname == 'npcFlags'
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

	-- TODO mapPos here and using int2
	if ig.igButton'goto mapPos...' then
		app.mapWindow:open(save.map)
		app.tileWindow:setXY(save.mapPos.x, save.mapPos.y)
		app:centerView(save.mapPos.x, save.mapPos.y)
	end
	if ig.igButton'goto lastTownPos...' then
		app.mapWindow:open(save.map)
		app.tileWindow:setXY(save.lastTownPos.x, save.lastTownPos.y)
		app:centerView(save.lastTownPos.x, save.lastTownPos.y)
	end
	if ig.igButton'goto airshipPos...' then
		app.mapWindow:open(0)	-- TODO how to tell if the airship is in WoB or WoR?
		app.tileWindow:setXY(save.airshipPos.x, save.airshipPos.y)
		app:centerView(save.airshipPos.x, save.airshipPos.y)
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
	if ig.igCollapsingHeader'items:' then
		local colSize = 4
		local count = 256
		for i=0,count-1 do
			ig.igPushID_Int(i)

			ig.igSetNextItemWidth(64)

			self.tmpInt = self.tmpInt or ffi.new'int[1]'
			self.tmpInt[0] = save.itemTypes[i].i
			if ig.igCombo('', self.tmpInt, app.itemNames) then
				save.itemTypes[i].i = self.tmpInt[0]
			end

			ig.igSameLine()
			if ig.igButton'>' then
				app.itemWindow:open(save.itemTypes[i].i)
			end
			ig.igSameLine()
			ig.igSetNextItemWidth(32)
			ig.luatableTooltipInputFloatAsText('item '..i..' count', save.itemCounts, i)

			if i < count-1 and i % colSize < colSize-1 then
				ig.igSameLine()
			end

			ig.igPopID()
		end
	end

	local function showFlags(args)
		local flagField = args.flagField
		local getname = args.getname
		local onchange = args.onchange
		local link = args.link
		local title = args.title

		ig.igSeparator()
		if not ig.igCollapsingHeader(title) then return end

assert.type(flagField, 'string')
		ig.igPushID_Str(flagField)

		local colSize = link and 8 or 16
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

	showFlags{
		title = 'espers:',
		flagField = 'esperFlags',
		getname = function(i) return game.getSpellName(i + 54) end,
		link = function(i) return app.spellWindow:open(i + 54) end,
	}

	showFlags{
		title = 'swdtechs:',
		flagField = 'swdtechFlags',
		getname = function(i) return tostring(game.swordTechNames[i]) end,
	}

	showFlags{
		title = 'blitzes:',
		flagField = 'blitzFlags',
		getname = function(i) return game.getSpellName(i + 93) end,
		link = function(i) return app.spellWindow:open(i + 93) end,
	}

	showFlags{
		title = 'lores:',
		flagField = 'loreFlags',
		getname = function(i) return game.getSpellName(i + 139) end,
		link = function(i) return app.spellWindow:open(i + 139) end,
	}

	showFlags{
		title = 'dances:',
		flagField = 'danceFlags',
		getname = function(i) return tostring(game.mogDanceNames[i]) end,
	}

	showFlags{
		title = 'map flags:',
		flagField = 'mapFlags',
	}

	-- TODO reverse-reference to NPCs that use this flag
	showFlags{
		title = 'npc flags:',
		flagField = 'npcFlags',
	}

	-- TODO reverse-reference to treasures that use this flag
	showFlags{
		title = 'treasure flags:',
		flagField = 'treasureFlags',
	}

	showFlags{
		title = 'battle formation flags:',
		flagField = 'battleFormationFlags',
		getname = function(i)
			return '#'..i..': {'..game.formations[i]:getDesc()..'}'
		end,
		link = function(i)
			app.battleFormationWindow:open(i)
		end,
		onchange = function()
			self:refreshMonstersEnabled()
		end,
	}

	showFlags{
		title = 'rages:',
		flagField = 'rageFlags',
		getname = function(i) return tostring(game.monsterNames[i]) end,
		link = function(i) return app.monsterWindow:open(i) end,
	}
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
		local enabled = 0 ~= bit.band(mask, save.battleFormationFlags[byteofs])
		if enabled then
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
