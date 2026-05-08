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

		self.app.battleFormationWindow:popupButton(formationEntry.formation)
		if formationEntry.chooseFromNextFour ~= 0 then
			ig.igSameLine()
			ig.igText'... choose from next four'
		end
		ig.igPopID()
		ig.igPopID()
	end
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
