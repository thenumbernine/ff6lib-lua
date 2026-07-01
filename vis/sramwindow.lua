local ffi = require 'ffi'
local range = require 'ext.range'
local assert = require 'ext.assert'
local ig = require 'imgui'
local makePalette = require 'ff6.graphics'.makePalette
local ArrayWindow = require 'ff6.vis.arraywindow'
local readbit = require 'ff6.vis.util'.readbit
local writebit = require 'ff6.vis.util'.writebit


-- this goes in game somewhere
local numLearnSpells = 54
local numCharsCanLearnSpells = 12


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
	if i < 0 or i >= self:getCount() then return end
	return self.app.sram.saves.s + i
end

function SRAMWindow:getCount()
	-- do I need this test?
	if not self.app.sram then return end
	return self.count
end

local uv0 = ig.ImVec2(0,0)
local uv1 = ig.ImVec2(1,1)
function SRAMWindow:showIndexUI()
	local app = self.app
	local game = app.game
	local save = self:getCurIndex()
	if not save then return end

	ig.igSeparator()
	if ig.igCollapsingHeader'magic progress:' then
		for charIndex=0,numCharsCanLearnSpells-1 do
			local char = save.characters.s[charIndex]

			local tex = self.charTexs and self.charTexs[1+charIndex]
			if tex then
				ig.igImage(tex.id, tex.imsize)
				if ig.igIsItemHovered(ig.ImGuiHoveredFlags_None) then
					ig.igBeginTooltip()
					ig.igText(tostring(char.name))
					ig.igEndTooltip()
				end
			else
				ig.igText(tostring(char.name))
			end

			-- current esper's magic
			local charSpellLearns = save.spellsLearned + charIndex * numLearnSpells


			local allSofar, allTotal = 0, 0
			local unlockedSofar, unlockedTotal = 0, 0
			for spellIndex=0,numLearnSpells-1 do
				if self.spellsEnabled[spellIndex] then
					unlockedSofar = unlockedSofar + math.min(100, charSpellLearns[spellIndex])	-- 0xff <-> 100%
					unlockedTotal = unlockedTotal + 100
				end
				allSofar = allSofar + math.min(100, charSpellLearns[spellIndex])	-- 0xff <-> 100%
				allTotal = allTotal + 100
			end

			-- progress for all spells:
			local allSpellsProgress = allTotal > 0 and allSofar / allTotal
			ig.igSameLine()
			if allSpellsProgress then
				ig.igText(('all: %.1f'):format(100 * allSpellsProgress)..'/100')
			else
				ig.igText'all: -'
			end

			-- progress for unlocked spells from all acquired espers:
			local unlockedSpellsProgress = unlockedTotal > 0 and unlockedSofar / unlockedTotal
			ig.igSameLine()
			if unlockedSpellsProgress then
				ig.igText(('unlocked: %.1f'):format(100 * unlockedSpellsProgress)..'/100')
			else
				ig.igText'unlocked:-'
			end

			local function getEsperProgress(esperIndex)
				if esperIndex < 0 or esperIndex >= game.numEspers then return end
				local sofar = 0
				local total = 0
				for i=0,4 do
					local spellIndex = game.espers[esperIndex].spellLearn.s[i].spell.i
					if spellIndex >= 0 and spellIndex < numLearnSpells then
						sofar = sofar + math.min(100, charSpellLearns[spellIndex])	-- 0xff <-> 100%
						total = total + 100
					end
				end
				if total == 0 then return end
				return sofar / total
			end

			-- progress for current esper
			local thisEsperProgress = getEsperProgress(char.esper.i)
			ig.igSameLine()
			if thisEsperProgress then
				ig.igText(('esper: %.1f'):format(100 * thisEsperProgress)..'/100')
			else
				ig.igText'esper: -'
			end

			-- finished with the current esper?
			if thisEsperProgress == 1
			and (unlockedSpellsProgress or 0) < 1
			then
				local recommendEsper
				for esperIndex=game.numEspers-1,0,-1 do
					if readbit(save.esperFlags, esperIndex) then
						local esperProgress = getEsperProgress(esperIndex)
						if esperProgress and esperProgress < 1 then
							recommendEsper = esperIndex
							break
						end
					end
				end
				if recommendEsper then
					ig.igSameLine()
					ig.igText('...next '..game.getEsperName(recommendEsper))
				end
			end
		end
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'save fields:' then
		for fieldname, ctype, field in save:fielditer() do
			-- struct handled below
			if fieldname == 'characters'
			or fieldname == 'roster'
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

	for _,field in ipairs{
		'mapPos', 'bg1scrollPos', 'worldPos', 'parentMapPos', 'airshipPos',
		--'charSavePos' <- is an array of 16 ...
	} do
		if ig.igButton('goto '..field..'...') then
			if field == 'airshipPos'
			or field == 'parentMapPos'
			or field == 'worldPos'
			then
				local inWoR = readbit(save.mapFlags, 164)
				app.mapWindow:open(inWoR and 1 or 0)
			else
				app.mapWindow:open(save.map)
			end
			app.tileWindow:setXY(save[field].x, save[field].y)
			app:centerView(save[field].x, save[field].y)
		end
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'character fields:' then
		ig.luatableInputInt('characterIndex', self, 'characterIndex')

		local r = save.roster.s + bit.band(15, self.characterIndex)
		for fieldname, ctype, field in r:fielditer() do
			self:editField(r, fieldname, ctype, field)
		end

		local character = save.characters.s + bit.band(15, self.characterIndex)
		for fieldname, ctype, field in character:fielditer() do
			self:editField(character, fieldname, ctype, field)
		end

		-- 12 x 54 spells saved...
		if self.characterIndex < numCharsCanLearnSpells then
			ig.igSeparator()
			if ig.igCollapsingHeader'spells learned:' then
				local charSpellLearns = save.spellsLearned + self.characterIndex * numLearnSpells
				local colSize = 6
				for i=0,numLearnSpells-1 do
					ig.igPushID_Int(i)
					ig.igSetNextItemWidth(32)
					ig.luatableTooltipInputFloatAsText(tostring(game.getSpellName(i)), charSpellLearns, i)
					ig.igSameLine()
					if ig.igButton'>' then
						app.spellWindow:open(i)
					end

					if i < numLearnSpells-1 and i % colSize < colSize-1 then
						ig.igSameLine()
					end
					ig.igPopID()
				end
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
		local hilite = args.hilite

assert.type(flagField, 'string')
		ig.igPushID_Str(flagField)

		local colSize = link and 8 or 16
		local flagobj = save[flagField]	-- should be a uint8_t[] array
		local count = bit.lshift(game.countof(flagobj), 3)
		for i=0,count-1 do
			ig.igPushID_Int(i)

			local enabled = readbit(flagobj, i)
			self.__tmp = enabled

			local thishilite
			if hilite then
				thishilite = hilite(i)
			end
			if thishilite ~= nil then
				ig.igPushStyleColor_U32(ig.ImGuiCol_FrameBg, thishilite)
			end

			local checkboxName = getname and getname(i) or flagField..' '..i
			if ig.luatableTooltipCheckbox(checkboxName, self, '__tmp') then
				writebit(flagobj, i, not enabled)
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

	ig.igSeparator()
	if ig.igCollapsingHeader'espers:' then
		showFlags{
			flagField = 'esperFlags',
			getname = function(i)
				return app.esperWindow:getIndexName(i)
			end,
			link = function(i)
				return app.esperWindow:open(i)
			end,
		}
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'skills:' then
		showFlags{
			flagField = 'swdtechFlags',
			getname = function(i)
				return tostring(game.swordTechNames[i])
			end,
		}

		showFlags{
			flagField = 'blitzFlags',
			getname = function(i)
				return game.getSpellName(i + 93)
			end,
			link = function(i)
				return app.spellWindow:open(i + 93)
			end,
		}

		showFlags{
			flagField = 'loreFlags',
			getname = function(i)
				return game.getSpellName(i + 139)
			end,
			link = function(i)
				return app.spellWindow:open(i + 139)
			end,
		}

		showFlags{
			flagField = 'danceFlags',
			getname = function(i)
				return tostring(game.mogDanceNames[i])
			end,
		}
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'map flags:' then
		showFlags{
			flagField = 'mapFlags',
		}
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'npc flags:' then
		showFlags{
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
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'treasure flags:' then
		showFlags{
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
	end

	ig.igSeparator()
	if ig.igCollapsingHeader'battle formation flags:' then
		showFlags{
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
	end

	ig.igSeparator()
	if self.ragesTitle then
		ig.igText(self.ragesTitle)
	end
	if ig.igCollapsingHeader'rages:' then
		showFlags{
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

	if self.index < 0 or self.index >= self:getCount() then return end
	local save = app.sram.saves.s + self.index

	self.ragesTitle = nil

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

	-- refresh character texs
	-- TODO this when you change the roster
	if self.charTexs then
		for _,tex in ipairs(self.charTexs) do
			tex:delete()
		end
	end
	--self.charTexs[1+save][1+charIndex]
	self.charTexs = range(0,15):mapi(function(charIndex)
		local ch = save.characters.s[charIndex]
		local img = game.getCharSpriteSheetImage(ch.sprite)
		img = img:copy{x=0, y=0, width=16, height=24}
		--local palette = bit.band(ch.character, 7)	-- character? sprite? palette? where to go for palette of sprite?
		--local palette = game.characterPaletteIndexes[charIndex]
		local palette = game.characterPaletteIndexes[ch.sprite]
		img.palette = makePalette(game, game.characterPalettes[palette], 4, 16)
		return self:makeTex(img)
	end)

	-- based on esper flags collect spells enabled flags
	self.spellsEnabled = {}	-- 0-based key
	local flagobj = save.esperFlags
	for esperIndex=0,game.numEspers-1 do
		if readbit(save.esperFlags, esperIndex) then
			for i=0,4 do
				local spellIndex = game.espers[esperIndex].spellLearn.s[i].spell.i
				if spellIndex >= 0 and spellIndex < numLearnSpells then
					self.spellsEnabled[spellIndex] = true
				end
			end
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
		if readbit(save.battleFormationFlags, i) then
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

	local ragesFound = 0
	local totalCanFind = 0
	local lookFor
	for i=0,255 do
		if self.monstersEnabled[i] then
			if readbit(save.rageFlags, i) then
				ragesFound = ragesFound + 1
			elseif i < 255 then	-- can't leap pugs...
				lookFor = i
			end
			totalCanFind = totalCanFind + 1
		end
	end

	self.ragesTitle = 'rages: '
		..ragesFound..' found / '
		..totalCanFind..' encountered'
		..(lookFor and (' ... look for '..game.monsterNames[lookFor]) or '')
end

return SRAMWindow
