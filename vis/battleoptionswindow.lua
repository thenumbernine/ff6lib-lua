local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local BattleOptionsWindow = ArrayWindow:subclass()

function BattleOptionsWindow:init(...)
	BattleOptionsWindow.super.init(self, ...)
	local game = self.app.game
	self.array = range((game.countof((assert.index(game, self.gameField)))))
end

function BattleOptionsWindow:getArray()
	return self.array
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

function BattleOptionsWindow:showIndexUI(ar)
	local game = self.app.game
	local battleEntries = game[self.gameField] + self.index
	local formationCounts = {}
	for j=0,battleEntries.dim-1 do
		ig.igPushID_Str('battleEntries '..self.name)
		ig.igPushID_Int(j)

		local formationEntry = battleEntries.s[j]

		if self.gameField == 'monsterRandomBattles' then
			ig.igText(j == 3 and '1/16:' or '5/16:')
			ig.igSameLine()
		elseif self.gameField == 'monsterEventBattles' then
			ig.igText'1/2:'
			ig.igSameLine()
		end

		--self.app.battleFormationWindow:popupButton(formationEntry.formation)
		self:editRef(self.app.battleFormationWindow, formationEntry, 'formation')

		ig.luatableCheckbox('choose from next four', formationEntry, 'chooseFromNextFour')

		ig.igPopID()
		ig.igPopID()
	end

	-- reverse-references:

	if self.gameField == 'monsterRandomBattles' then

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
			for j,info in ipairs(self.worldSectorsWithThis) do
				ig.igPushID_Int(j)
				local mapIndex = bit.rshift(info.sectorIndex, 6)
				local x = bit.lshift(bit.band(info.sectorIndex, 7), 5) + 16
				local y = bit.lshift(bit.band(bit.rshift(info.sectorIndex, 3), 7), 5) + 16

				if self.app.mapWindow:popupButton(
					mapIndex,
					'sector='..x..','..y..' terrain='..info.terrain
				) then
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

	elseif self.gameField == 'monsterEventBattles' then

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
			ig.igPushID_Str'battleOptionsWindow-worldSectorsWithThis'
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

	else
		error'here'
	end
end

function BattleOptionsWindow:setIndex(...)
	BattleOptionsWindow.super.setIndex(self, ...)

	-- clear cache
	self.mapsWithThis = nil
	self.worldSectorsWithThis = nil
	self.treasuresWithThis = nil
end

local RandomBattleOptionsWindow = BattleOptionsWindow:subclass()
RandomBattleOptionsWindow.name = 'random battle options'
RandomBattleOptionsWindow.gameField = 'monsterRandomBattles'	-- monsterRandomBattleEntry4_t
BattleOptionsWindow.RandomBattleOptionsWindow = RandomBattleOptionsWindow

local EventBattleOptionsWindow = BattleOptionsWindow:subclass()
EventBattleOptionsWindow .name = 'event battle options'
EventBattleOptionsWindow.gameField = 'monsterEventBattles'		-- monsterRandomBattleEntry2_t
BattleOptionsWindow.EventBattleOptionsWindow = EventBattleOptionsWindow

return BattleOptionsWindow
