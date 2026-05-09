local table = require 'ext.table'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local ItemWindow = ArrayWindow:subclass()

ItemWindow.name = 'item'

function ItemWindow:init(...)
	ItemWindow.super.init(self, ...)
	local game = self.app.game
	self.array = range((game.countof(game.items)))
end

function ItemWindow:getArray()
	return self.array
end

function ItemWindow:getIndexName(i)
	return self.app.game.itemNames[i]
end

function ItemWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	local item = game.items + self.index
	ig.igText(' name = '..self:getIndexName(self.index))
	ig.igText(' desc = "'..game.gamezstr(game.itemDescBase + game.itemDescOffsets[self.index])..'"')
	for name in item:fielditer() do
		ig.igText(' '..name..' = '..tostring(item[name]))
	end

	ig.igText' colosseum info:'
	local colinfo = game.itemColosseumInfos[self.index]

	ig.igText('  monster fought =')
	ig.igSameLine()
	app.monsterWindow:popupButton(colinfo.monster.i)

	ig.igText('  item won =')
	ig.igSameLine()
	app.itemWindow:popupButton(colinfo.itemWon.i)

	ig.igText('  unknown = '..colinfo.unknown)
	ig.igText('  hideName = '..colinfo.hideName)

	if self.monstersWithThis == nil then
		self.monstersWithThis = table()
		for i=0,game.countof(game.monsterItems)-1 do
			local monsterItem = game.monsterItems + i
			for field in monsterItem:fielditer() do
				if monsterItem[field].i == self.index then
					self.monstersWithThis:insert{monsterIndex=i, field=field}
				end
			end
		end
	end

	ig.igSeparator()
	ig.igText'monsters with this item...'
	if #self.monstersWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-monstersWithThis'
		for j,info in ipairs(self.monstersWithThis) do
			ig.igPushID_Int(j)
			self.app.monsterWindow:popupButton(info.monsterIndex, info.field)
			ig.igPopID()
		end
		ig.igPopID()
	end

	if self.treasuresWithThis == nil then
		self.treasuresWithThis = table()
		for i=0,game.countof(game.maps)-1 do
			local mapInfo = game.getMap(i)		-- this wont bloat mem too much right?
			for j,treasure in ipairs(mapInfo.treasures) do
				if treasure.type == 2	-- item
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
	ig.igText'found in treasure chest ...'

	if #self.treasuresWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-treasuresWithThis'
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


end

function ItemWindow:setIndex(...)
	ItemWindow.super.setIndex(self, ...)

	-- clear cache
	self.monstersWithThis = nil
	self.treasuresWithThis = nil
	-- TODO metamorph set ...
end

return ItemWindow
