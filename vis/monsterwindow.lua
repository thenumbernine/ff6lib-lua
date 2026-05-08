local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local MonsterWindow = ArrayWindow:subclass()

MonsterWindow.name = 'monster'

function MonsterWindow:init(...)
	MonsterWindow.super.init(self, ...)
	local game = self.app.game
	self.array = range((game.countof(game.monsters)))
end

function MonsterWindow:getArray()
	return self.array
end

function MonsterWindow:getIndexName(i)
	return self.app.game.monsterNames[i]
end

function MonsterWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	ig.igText(' name = "'..self:getIndexName(self.index)..'"')
	ig.igText(' attack name = "'..game.monsterAttackNames[self.index]..'"')
	local monster = game.monsters[self.index]
	for name in monster:fielditer() do
		ig.igText(' '..name..' = '..tostring(monster[name]))
	end
	ig.igText'attacks:'
	local monsterSpells = game.monsterSpells[self.index]
	for i=0,monsterSpells.dim-1 do
		ig.igPushID_Str'monsterSpells'
		ig.igPushID_Int(i)
		app.spellWindow:popupButton(monsterSpells.s[i].i)
		ig.igPopID()
		ig.igPopID()
	end
	ig.igText'items:'
	local monsterItem = game.monsterItems[self.index]
	for name in monsterItem:fielditer() do
		ig.igPushID_Str(name)
		ig.igText('  '..name..' = ')
		ig.igSameLine()
		app.itemWindow:popupButton(monsterItem[name].i)
		ig.igPopID()
	end
	local monsterSketches = game.monsterSketches[self.index]
	ig.igText'sketches:'
	for i=0,monsterSketches.dim-1 do
		ig.igPushID_Str'monsterSketches'
		ig.igPushID_Int(i)
		app.spellWindow:popupButton(monsterSketches.s[i].i)
		ig.igPopID()
		ig.igPopID()
	end
	if self.index < game.countof(game.monsterRages)then
		ig.igText'rages:'
		local monsterRages = game.monsterRages[self.index]
		for i=0,monsterRages.dim-1 do
			ig.igPushID_Str'monsterRages'
			ig.igPushID_Int(i)
			app.spellWindow:popupButton(monsterRages.s[i].i)
			ig.igPopID()
			ig.igPopID()
		end
	end
end

return MonsterWindow
