local range = require 'ext.range'
local table = require 'ext.table'
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

	if ig.igCollapsingHeader'fields' then
		ig.igText(' attack name = "'..game.monsterAttackNames[self.index]..'"')
		local monster = game.monsters[self.index]
		for fieldname, ctype, field in monster:fielditer() do
			if fieldname == 'metamorphSet' then
				self:editMetamorphRef(monster, 'metamorphSet')
			else
				self:editField(monster, fieldname, ctype, field)
			end
		end
	end

	ig.igSeparator()
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
	for fieldname, ctype, field in monsterItem:fielditer() do
		ig.igPushID_Str(fieldname)
		ig.igText('  '..fieldname..' = ')
		ig.igSameLine()
		app.itemWindow:popupButton(monsterItem[fieldname].i)
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

	-- reverse searches ...

	ig.igSeparator()

	if not self.battleFormationsWithThis then
		self.battleFormationsWithThis = table()
		for i=0,game.countof(game.formations)-1 do
			local formation = game.formations + i
			for j=1,6 do
				if formation:getMonsterActive(j) then
					local monsterIndex = formation:getMonsterIndex(j)
					if monsterIndex == self.index then
						self.battleFormationsWithThis:insertUnique(i)
					end
				end
			end
		end
	end
	ig.igText('found in battle formations...')
	if #self.battleFormationsWithThis == 0 then
		ig.igText('...none')
	else
		ig.igPushID_Str('monster-battleFormationsWithThis')
		for j,i in ipairs(self.battleFormationsWithThis) do
			ig.igPushID_Int(j)
			self.app.battleFormationWindow:popupButton(i)
			ig.igPopID()
		end
		ig.igPopID()
	end


	if self.colosseumBetsWithThis == nil then
		self.colosseumBetsWithThis = table()
		for i=0,game.countof(game.items)-1 do
			local colInfo = game.itemColosseumInfos + i
			if colInfo.monster.i == self.index then
				self.colosseumBetsWithThis:insert(i)
			end
		end
	end
	ig.igSeparator()
	ig.igText"colosseum bets where you fight this monster..."
	if #self.colosseumBetsWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'monsterWindow-colosseumBetsWithThis'
		for j,i in ipairs(self.colosseumBetsWithThis) do
			ig.igPushID_Int(j)
			app.itemWindow:popupButton(i)
			ig.igPopID()
		end
		ig.igPopID()
	end
end

function MonsterWindow:setIndex(...)
	MonsterWindow.super.setIndex(self, ...)

	-- clear cache
	self.battleFormationsWithThis = nil
	self.colosseumBetsWithThis = nil
end

return MonsterWindow
