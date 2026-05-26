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
	local save = app.sramWindow:getCurIndex()

	local x = bit.band(self.index, 7)
	local y = bit.band(bit.rshift(self.index, 3), 7)
	local xmin = bit.lshift(x, 5)
	local ymin = bit.lshift(y, 5)
	ig.igText(
		'world='..bit.rshift(self.index, 6)
		..' sector={'..x..', '..y..'}'
		..' bbox={{'..xmin..', '..ymin..'}, {'..(xmin + 32)..', '..(ymin + 32)..'}}'
	)

	local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + self.index
	local encounterRatePerTerrain = game.worldSectorRandomBattleEncounterRatesPerTerrain[self.index]
	for i,terrain in ipairs(game.terrainTypes) do
		-- count which of all options are encountered already
		local numFormationsFound = 0
		if save then
			local battleEntries = game.monsterRandomBattles + randomBattlesPerTerrain[terrain]
			for j=0,battleEntries.dim-1 do
				local formationEntry = battleEntries.s[j]
				if 0 ~= bit.band(
					bit.lshift(1, bit.band(formationEntry.formation, 7)),
					save.battleFormationFlags[bit.rshift(formationEntry.formation, 3)]
				) then
					numFormationsFound = numFormationsFound + 1
				end
			end
			if numFormationsFound == 0 then
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff0000ff)
			elseif numFormationsFound == 1 then
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff003fcf)
			elseif numFormationsFound == 2 then
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff007f7f)
			elseif numFormationsFound == 3 then
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff00cf3f)
			elseif numFormationsFound == 4 then
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff00ff00)
			else
				error'here'
			end
		end

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

		if save then
			ig.igPopStyleColor(1)
		end
	end
end

return WorldEncounterSectorWindow

--[[
world sector encounters in order, because I'm bored:

wob:
(2,1) = Narshe
(2,2) = Figaro desert east
(1,2) = Figaro desert west
(2,3) = South Figaro
(3,3) = mt Kolts
(3,2) = Returners
(3,1) = north of returners and Nikeah
(5,1) = Gau's dad's house
(5,0) = north of Gau's dad's house
(4,1) = over the bridge to Nikeah, (wait is it blocked initially?)
(6,1) = east of Gau's dad's house
(5,2) = Imperial Camp and Forest of Illusion
(4,2) = Doma castle
(5,3) = south of Forest of Illusion on the way to Baron Falls ... and Grenade Forest
(6,3) = Veldt
(6,4) = Veldt and Crescent Mountain
(5,4) = Veldt southwest

(0,1) = Kohlingen
(1,1) = desert east of Kohlingen
(1,0) = north of Kohlingen and Dragon's Neck house
(0,0) = dragon's ... nose?  and chocobo stable
(0,2) = walk south to Jidoor, and Zozo, and Mt Zozo
(0,3) = more walking south
(0,4) = Jidoor
(1,4) = Opera House
(1,3) = walk north to Zozo

(4,6) = Albrook
(5,6) = govt camp
(4,5) = north of Albrook
(3,6) = west of Albrook
(5,7) = nothing really, just some grass
(5,5) = more grass and nothing
(4,4) = more grass
(3,5) = Vector
(2,6) = more south continent wandering
(3,4) = Tzen
(2,5) = more wandering
(1,6) = more grass
(2,7) = Maranda
(1,7) = more grass

(7,4) = Thamasa
(7,3) = north of Thamasa

(7,1) = Triangle Island

(2,0) = mountains and ocean
(3,0) = mountains and ocean
(4,0) = mountains and ocean
(6,2) = mountains and ocean

(4,3) = ocean, and the tiniest bit of land
(6,6) = bit of mountains and ocean
(4,7) = tiniest bit of land and ocean

(6,0) = ocean
(7,0) = ocean
(7,2) = ocean
(2,4) = ocean
(0,5) = ocean
(1,5) = ocean
(6,5) = ocean
(7,5) = ocean
(0,6) = ocean
(7,6) = ocean
(0,7) = ocean
(3,7) = ocean
(6,7) = ocean
(7,7) = ocean
--]]
