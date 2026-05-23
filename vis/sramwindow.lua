local ffi = require 'ffi'
local assert = require 'ext.assert'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local SRAMWindow = ArrayWindow:subclass()

SRAMWindow.characterIndex = 0

SRAMWindow.name = 'sram'
SRAMWindow.count = 3	-- fixed, ... right?

function SRAMWindow:init(...)
	SRAMWindow.super.init(self, ...)

	for i=0,self.count-1 do
		self['calcChecksumOnSave'..i] = true -- by default
	end
end

function SRAMWindow:getIndex(i)
	if not self.app.game or not self.app.sram then return end
	if i <= 0 or i > self:getCount() then return end
	return self.app.sram.saves.s + i
end

function SRAMWindow:getCount()
	-- do I need this test?
	if not self.app.sram then return end
	return self.count
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

		if ig.igButton'recalc checksum...' then
			self:recalcChecksum(self.index)
		end
		ig.luatableCheckbox('auto recalc checksum on save', self, 'calcChecksumOnSave'..self.index)
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
		local canlink = args.canlink
		local link = args.link
		local title = args.title
		local hilite = args.hilite

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

			local thishilite
			if hilite then
				thishilite = hilite(i)
			end
			if thishilite ~= nil then
				ig.igPushStyleColor_U32(ig.ImGuiCol_FrameBg, thishilite)
			end

			local checkboxName = getname and getname(i) or flagField..' '..i
			if ig.luatableTooltipCheckbox(checkboxName, self, '__tmp') then
				flagobj[byteofs] = bit.bxor(flagobj[byteofs], mask)
				if onchange then onchange() end
			end

			if link then
				--[[ this messees up spacing
				-- TODO make this all a table
				if not canlink or canlink(i) then
				--]] do
					ig.igSameLine()
					if ig.igButton'>' then
						link(i)
					end
					ig.hoverTooltip(checkboxName)
				end
			end

			if i < count-1 and i % colSize < colSize-1 then
				ig.igSameLine()
			end

			if thishilite ~= nil then
				ig.igPopStyleColor(1)
			end

			ig.igPopID()
		end

		ig.igPopID()
	end

	showFlags{
		title = 'espers:',
		flagField = 'esperFlags',
		getname = function(i)
			return game.getSpellName(i + 54)
		end,
		link = function(i)
			return app.spellWindow:open(i + 54)
		end,
	}

	showFlags{
		title = 'swdtechs:',
		flagField = 'swdtechFlags',
		getname = function(i)
			return tostring(game.swordTechNames[i])
		end,
	}

	showFlags{
		title = 'blitzes:',
		flagField = 'blitzFlags',
		getname = function(i)
			return game.getSpellName(i + 93)
		end,
		link = function(i)
			return app.spellWindow:open(i + 93)
		end,
	}

	showFlags{
		title = 'lores:',
		flagField = 'loreFlags',
		getname = function(i)
			return game.getSpellName(i + 139)
		end,
		link = function(i)
			return app.spellWindow:open(i + 139)
		end,
	}

	showFlags{
		title = 'dances:',
		flagField = 'danceFlags',
		getname = function(i)
			return tostring(game.mogDanceNames[i])
		end,
	}

	showFlags{
		title = 'map flags:',
		flagField = 'mapFlags',
	}

	-- TODO reverse-reference to NPCs that use this flag
	showFlags{
		title = 'npc flags:',
		flagField = 'npcFlags',
		getname = function(i)
			local info = self.npcForFlag[i]
			if not info then return end
			return '#'..i..': map '..info.mapIndex..', npc '..info.npcIndex
		end,
		canlink = function(i)
			return self.npcForFlag[i]
		end,
		link = function(i)
			local info = self.npcForFlag[i]
			if not info then return end
			app.mapWindow:open(info.mapIndex)
			app.npcWindow:open(info.npcIndex)
			local n = info.npc
			self.app.tileWindow:setXY(n.x, n.y)
			self.app:centerView(n.x, n.y)
		end,
	}

	-- TODO reverse-reference to treasures that use this flag
	showFlags{
		title = 'treasure flags:',
		flagField = 'treasureFlags',
		getname = function(i)
			local info = self.treasureForFlag[i]
			if not info then return end
			return '#'..i..': map '..info.mapIndex..', treasure '..info.treasureIndex
		end,
		canlink = function(i)
			return self.treasureForFlag[i]
		end,
		link = function(i)
			local info = self.treasureForFlag[i]
			if not info then return end
			app.mapWindow:open(info.mapIndex)
			app.treasureWindow:open(info.treasureIndex)
			local t = info.treasure
			self.app.tileWindow:setXY(t.pos.x, t.pos.y)
			self.app:centerView(t.pos.x, t.pos.y)
		end,
	}

	showFlags{
		title = 'battle formation flags:',
		flagField = 'battleFormationFlags',
		getname = function(i)
			return '#'..i..': {'..game.formations[i]:getDesc()..'}'
		end,
		-- canlink ... all battleFormationFlags are valid, right?
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
		getname = function(i)
			return tostring(game.monsterNames[i])
		end,
		-- canlink ... all rageFlags are valid for monsters, right?
		link = function(i)
			return app.monsterWindow:open(i)
		end,
		hilite = function(i)
			if i < 256 then
				return self.monstersEnabled[i] and 0x5f00ff00 or 0x5f0000ff
			end
		end,
	}
end

function SRAMWindow:recalcChecksum(i)
	local save = self:getIndex(i)
	if not save then return end
	local game = self.app.game
	local x = 0
	local ptr = ffi.cast('uint8_t*', save)
	for i=0,ffi.sizeof(game.SaveSlot)-3 do
		x = x + ptr[i]
	end
	save.checksum = bit.band(0xffff, x)
end

function SRAMWindow:setIndex(...)
	local app = self.app
	local game = app.game
	if not app.sram or not game then return end

	SRAMWindow.super.setIndex(self, ...)
	self:refreshMonstersEnabled()

	-- refresh links from treasure flags to treasure chests
	-- I guess this needs to be done whenever the treasure links change (TODO do this in the treasureWindow?)
	self.treasureForFlag = {}
	self.npcForFlag = {}
	for i=0,game.countof(game.maps)-1 do
		local mapInfo = game.getMap(i)		-- this wont bloat mem too much right?

		for j,treasure in ipairs(mapInfo.treasures) do
			-- I tihnk there is no zero flag ...
			-- warn upon doubling up? or first/last come first/last serve?
			self.treasureForFlag[treasure.flag] = {
				mapIndex = i,
				treasureIndex = j-1,
				treasure = treasure,
			}	-- map is 0-based, treasure is 0-based
		end

		for j,npc in ipairs(mapInfo.npcs) do
			self.npcForFlag[npc.flag] = {
				mapIndex = i,
				npcIndex = j-1,
				npc = npc,
			}
		end
	end
end

function SRAMWindow:refreshMonstersEnabled()
	local app = self.app
	local game = app.game
	if not app.sram or not game then return end
	if self.index < 0 or self.index >= self:getCount() then return end
	local save = app.sram.saves.s + self.index

	self.monstersEnabled = {}	-- 0-based
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
