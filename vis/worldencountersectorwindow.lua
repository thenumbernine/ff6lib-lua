local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local WorldEncounterSectorWindow = ArrayWindow:subclass()

WorldEncounterSectorWindow.name = 'world encounter sectors'

function WorldEncounterSectorWindow:init(...)
	WorldEncounterSectorWindow.super.init(self, ...)
	self.array = range(0, bit.lshift(1, 7)-1)
end

function WorldEncounterSectorWindow:getArray()
	return self.array
end

function WorldEncounterSectorWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	ig.igText('world='..bit.rshift(self.index, 6))
	ig.igText('x='..bit.lshift(bit.band(self.index, 7), 5))
	ig.igText('y='..bit.lshift(bit.band(bit.rshift(self.index, 3), 7), 5))
	local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + self.index
	local encounterRateBits = game.worldSectorRandomBattleEncounterRatesPerTerrain[self.index]
	for i,terrain in ipairs(game.terrainTypes) do
		ig.igPushID_Str(terrain)
		ig.igText('terrain = '..tostring(terrain))
		local encounter = bit.band(3, bit.rshift(encounterRateBits, bit.lshift(i-1, 1)))
		local encounterRateName = game.encounterNames[encounter+1]
		ig.igText('rate = '..tostring(encounterRateName))

		ig.igText('battle options = ')
		ig.igSameLine()
		local battleIndex = randomBattlesPerTerrain[terrain]
		app.randomBattleOptionsWindow:popupButton(battleIndex)
		ig.igPopID()
	end
end

return WorldEncounterSectorWindow
