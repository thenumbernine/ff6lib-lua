local ffi = require 'ffi'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local WorldEncounterSectorWindow = ArrayWindow:subclass()

WorldEncounterSectorWindow.name = 'world encounter sectors'

function WorldEncounterSectorWindow:getCount()
	return bit.lshift(1, 7)
end

function WorldEncounterSectorWindow:showIndexUI()
	local app = self.app
	local game = app.game

	ig.igText(
		'world='..bit.rshift(self.index, 6)
		..' x='..bit.lshift(bit.band(self.index, 7), 5)
		..' y='..bit.lshift(bit.band(bit.rshift(self.index, 3), 7), 5)
	)

	local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + self.index
	local encounterRatePerTerrain = game.worldSectorRandomBattleEncounterRatesPerTerrain[self.index]
	for i,terrain in ipairs(game.terrainTypes) do
		ig.igPushID_Str(terrain)

		local encounterRate = encounterRatePerTerrain[terrain]
		ig.igSetNextItemWidth(100)
		-- luatableCombo is 1-based so ...
		self.tmpInt = self.tmpInt or ffi.new('int[1]')
		self.tmpInt[0] = encounterRatePerTerrain[terrain]
		if ig.igCombo('', self.tmpInt, game.encounterRateNames) then
			encounterRatePerTerrain[terrain] = self.tmpInt[0]
		end
		ig.igSameLine()

		self:editRef(app.randomBattleOptionsWindow, randomBattlesPerTerrain, terrain)
		ig.igPopID()
	end
end

return WorldEncounterSectorWindow
