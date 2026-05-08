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
end

return ItemWindow
