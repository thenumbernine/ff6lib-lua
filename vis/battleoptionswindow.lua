local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local BattleOptionsWindow = ArrayWindow:subclass()

function BattleOptionsWindow:init(...)
	BattleOptionsWindow.super.init(self, ...)
	local game = self.app.game
end

function BattleOptionsWindow:getCount()
	local game = self.app.game
	return game.countof((assert.index(game, self.gameField)))
end

function BattleOptionsWindow:getIndexName(i)
	local game = self.app.game
	local battleEntries = game[self.gameField] + i
	local monsters = {}
	for j=0,battleEntries.dim-1 do
		local formationEntry = battleEntries.s[j]
		local formationIndex = formationEntry.formation
		if formationIndex < game.numFormations then
			local formation = game.formations + formationIndex
			for k=1,6 do
				if formation:getMonsterActive(k) then
					local monsterIndex = formation:getMonsterIndex(k)
					monsters[monsterIndex] = (monsters[monsterIndex] or 0) + 1
				end
			end
		end
	end
	return table.keys(monsters):sort():mapi(function(monsterIndex)
		return tostring(game.monsterNames[monsterIndex])
	end):concat', '
end

function BattleOptionsWindow:showIndexUI()
	local game = self.app.game
	local battleEntries = game[self.gameField] + self.index
	local formationCounts = {}
	ig.igPushID_Str('battleEntries '..self.name)
	for j=0,battleEntries.dim-1 do
		ig.igPushID_Int(j)

		local formationEntry = battleEntries.s[j]

		ig.igText(self:getProbStr(j))
		ig.igSameLine()

		self:editRef(self.app.battleFormationWindow, formationEntry, 'formation')

		ig.luatableCheckbox('choose from next four', formationEntry, 'chooseFromNextFour')

		ig.igPopID()
	end
	ig.igPopID()
end


-- subclasses:

local RandomBattleOptionsWindow = BattleOptionsWindow:subclass()
BattleOptionsWindow.RandomBattleOptionsWindow = RandomBattleOptionsWindow

RandomBattleOptionsWindow.name = 'random battle options'
RandomBattleOptionsWindow.gameField = 'monsterRandomBattles'	-- monsterRandomBattleEntry4_t

function RandomBattleOptionsWindow:getProbStr(j)
	return j == 3 and '1/16' or '5/16'
end

function RandomBattleOptionsWindow:showIndexUI(...)
	local app = self.app
	local game = app.game

	RandomBattleOptionsWindow.super.showIndexUI(self, ...)

	-- reverse-references:

	if self.mapsWithThis == nil then
		self.mapsWithThis = table()
		-- skip the first 3 maps for monsterRandomBattleOptionIndex
		for i=2,game.countof(game.maps)-1 do
			local mapInfo = game.getMap(i)		-- this wont bloat mem too much right?
			if mapInfo.monsterRandomBattleOptionIndex == self.index then
				self.mapsWithThis:insert(i)
			end
		end
	end

	ig.igSeparator()
	ig.igText'found as random battle in maps...'
	if #self.mapsWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'battleOptionsWindow-mapsWithThis'
		for j,i in ipairs(self.mapsWithThis) do
			ig.igPushID_Int(j)
			self.app.mapWindow:popupButton(i)
			ig.igPopID()
		end
		ig.igPopID()
	end

	self.worldSectorsWithThis = table()
	-- 128 of these ... 64 for WoB, 64 for WoR, each world is divided into 8x8 sectors
	for i=0,game.countof(game.worldSectorRandomBattlesPerTerrain)-1 do
		local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + i
		for _,terrain in ipairs(game.terrainTypes) do
			local battleIndex = randomBattlesPerTerrain[terrain]
			if battleIndex == self.index then
				self.worldSectorsWithThis:insert{sectorIndex=i, terrain=terrain}
			end
		end
	end

	ig.igText'found as a random world map battle...'
	if #self.worldSectorsWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'battleOptionsWindow-worldSectorsWithThis'
		local lastSectorIndex
		for j,info in ipairs(self.worldSectorsWithThis) do
			ig.igPushID_Int(j)

			local mapIndex = bit.rshift(info.sectorIndex, 6)
			local x = bit.lshift(bit.band(info.sectorIndex, 7), 5) + 16
			local y = bit.lshift(bit.band(bit.rshift(info.sectorIndex, 3), 7), 5) + 16

			if lastSectorIndex ~= info.sectorIndex then
				lastSectorIndex = info.sectorIndex
				ig.igText('map='..mapIndex..' sector='..x..','..y)
			end

			ig.igSameLine()
			if ig.igButton(info.terrain) then
				self.app.mapWindow.show[0] = true
				self.app.mapWindow:setIndex(mapIndex)

				-- just like doorWindow...
				-- and treasureWindow below ...
				self.app.worldEncounterSectorWindow.show[0] = true
				self.app.worldEncounterSectorWindow:setIndex(info.sectorIndex)

				-- just like doorWindow...
				-- new map should be loaded now
				local mapWidth, mapHeight = self.app.tileWindow:getMapSize()
				if mapWidth and mapHeight then
					self.app.tileWindow:setIndex(x + mapWidth * y)
				end
				self.app:centerView(x, y)
			end
			ig.igPopID()
		end
		ig.igPopID()
	end
end

function RandomBattleOptionsWindow:setIndex(...)
	RandomBattleOptionsWindow.super.setIndex(self, ...)

	-- clear cache
	self.mapsWithThis = nil
	self.worldSectorsWithThis = nil
end


local EventBattleOptionsWindow = BattleOptionsWindow:subclass()
BattleOptionsWindow.EventBattleOptionsWindow = EventBattleOptionsWindow

EventBattleOptionsWindow .name = 'event battle options'
EventBattleOptionsWindow.gameField = 'monsterEventBattles'		-- monsterRandomBattleEntry2_t

function EventBattleOptionsWindow:getProbStr(j)
	return '1/2'
end

function EventBattleOptionsWindow:showIndexUI(...)
	local app = self.app
	local game = app.game

	EventBattleOptionsWindow.super.showIndexUI(self, ...)

	-- reverse-references:

	if self.treasuresWithThis == nil then
		self.treasuresWithThis = table()
		for i=0,game.countof(game.maps)-1 do
			local mapInfo = game.getMap(i)		-- this wont bloat mem too much right?
			for j,treasure in ipairs(mapInfo.treasures) do
				if treasure.type == 1	-- monster
				and treasure.battleOrItemOrGP == self.index
				then
					self.treasuresWithThis:insert{
						mapIndex = i,
						treasureIndex = j,
						treasure = treasure,
					}	-- map is 0-based, treasure is 1-based
				end
			end
		end
	end
	ig.igSeparator()
	ig.igText'found as a monster-in-a-box ...'
	if #self.treasuresWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'battleOptionsWindow-treasuresWithThis'
		for j,info in ipairs(self.treasuresWithThis) do
			ig.igPushID_Int(j)
			if self.app.mapWindow:popupButton(
				info.mapIndex,
				'treasure #'..info.treasureIndex
			) then
				self.app.treasureWindow.show[0] = true
				self.app.treasureWindow:setIndex(info.treasureIndex-1)

				local t = info.treasure
				-- just like doorWindow...
				-- new map should be loaded now
				local mapWidth, mapHeight = self.app.tileWindow:getMapSize()
				if mapWidth and mapHeight then
					self.app.tileWindow:setIndex(t.pos.x + mapWidth * t.pos.y)
				end
				self.app:centerView(t.pos.x, t.pos.y)
			end
			ig.igPopID()
		end
		ig.igPopID()
	end

	if not self.eventScriptCmdsWithThis then
		self.eventScriptCmdsWithThis = table()
		for _,cmd in ipairs(game.eventScriptCmds) do
			local cmdname
			for _,checkcmdname in ipairs{'TouchBattle', 'Battle'} do
				if game.ScriptCmds[checkcmdname]:isa(cmd) then
--DEBUG:print(('battle at $%06x with eventBattleOptionsIndex %d'):format(cmd.addr, cmd.eventBattleOptionsIndex))
					cmdname = checkcmdname
					break
				end
			end
			if cmdname and cmd.eventBattleOptionsIndex == self.index then
				self.eventScriptCmdsWithThis:insert{addr=cmd.addr, cmdname=cmdname}
			end
		end
	end
	ig.igSeparator()
	ig.igText'event scripts ...'
	if #self.eventScriptCmdsWithThis == 0 then
		ig.igText'...none'
	else
		for _,info in ipairs(self.eventScriptCmdsWithThis) do
			app.scriptWindow:popupButtonForAddr(info.addr, info.cmdname)
		end
	end
	-- TODO I guess there's events, monsters, objects, vehicles, world scripts ... this is just events ...
end

function EventBattleOptionsWindow:setIndex(...)
	EventBattleOptionsWindow.super.setIndex(self, ...)

	-- clear cache
	self.treasuresWithThis = nil
	self.eventScriptCmdsWithThis = nil
end


return BattleOptionsWindow
