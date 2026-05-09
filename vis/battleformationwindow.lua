local table = require 'ext.table'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local BattleFormationWindow = ArrayWindow:subclass()

BattleFormationWindow.name = 'battle formation'

function BattleFormationWindow:init(...)
	BattleFormationWindow.super.init(self, ...)

	local game = self.app.game

	self.array = range((game.countof(game.formations)))
end

function BattleFormationWindow:getArray()
	return self.array
end

function BattleFormationWindow:getIndexName(i)
	local game = self.app.game
	local formation = game.formations + i
	local monsterCounts = {}
	for k=1,6 do
		if formation:getMonsterActive(k) then
			local monsterIndex = formation:getMonsterIndex(k)
			local key = '#'..monsterIndex
			if monsterIndex < game.numMonsters then
				key = key ..':'..tostring(game.monsterNames[monsterIndex])
			end
			monsterCounts[key] = (monsterCounts[key] or 0) + 1
		end
	end
	return table.keys(monsterCounts):sort():mapi(function(key)
		local count = monsterCounts[key]
		if count == 1 then return key end
		return key..' x'..count
	end):concat', '
end

function BattleFormationWindow:showIndexUI(ar)
	local game = self.app.game
	if self.index < game.countof(game.formationMPs) then
		ig.igText(' mp gained = '..tostring(game.formationMPs[self.index]))
	end
	local formation = game.formations + self.index
	for i=1,6 do
		ig.igPushID_Str('BattleFormationWindow')
		ig.igPushID_Int(i)
		ig.igText(' #'..i)
		local active = formation:getMonsterActive(i)
		ig.igText('  active = '..tostring(active))
		if active then
			self.app.monsterWindow:popupButton(formation:getMonsterIndex(i))
			-- pointer into another table I think?
			ig.igText('  pos = '..tostring(formation:getMonsterPos(i)))
			ig.igText('  size = '..tostring(formation:getFormationSize(i)))
		end
		ig.igPopID()
		ig.igPopID()
	end
	local formation2 = game.formation2s[self.index]
	for name in formation2:fielditer() do
		ig.igText(' '..name..' = '..formation2[name])
	end

	if not self.battlesWithThis then
		self.battlesWithThis = {}
		for _,field in ipairs{'monsterRandomBattles', 'monsterEventBattles'} do
			self.battlesWithThis[field] = table()
			for i=0,game.countof(game[field])-1 do
				local battleEntries = game[field] + i
				for j=0,battleEntries.dim-1 do
					local formationEntry = battleEntries.s + j
					if formationEntry.formation == self.index then
						self.battlesWithThis[field]:insertUnique(i)
					end
				end
			end
		end
	end
	for _,field in ipairs{'monsterRandomBattles', 'monsterEventBattles'} do
		ig.igPushID_Str('battleFormations-battlesWithThis')
		ig.igSeparator()
		ig.igText(field..'...')
		if #self.battlesWithThis[field] == 0 then
			ig.igText'...none'
		else
			local win
			if field == 'monsterRandomBattles' then
				win = self.app.randomBattleOptionsWindow
			elseif field == 'monsterEventBattles' then
				win = self.app.eventBattleOptionsWindow
			else
				error'here'
			end
			ig.igPushID_Str(field)
			for j,i in ipairs(self.battlesWithThis[field]) do
				ig.igPushID_Int(j)
				win:popupButton(i)
				ig.igPopID()
			end
			ig.igPopID()
		end
		ig.igPopID()
	end
end

function BattleFormationWindow:setIndex(...)
	BattleFormationWindow.super.setIndex(self, ...)

	-- clear cache
	self.battlesWithThis = nil
end

return BattleFormationWindow
