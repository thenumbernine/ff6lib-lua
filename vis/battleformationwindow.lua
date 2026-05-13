local table = require 'ext.table'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'
local readMonsterSprite = require 'ff6.monstersprite'


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

local availSize = ig.ImVec2()
local drawSize = ig.ImVec2()
function BattleFormationWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	do
		local x = ig.igGetCursorPosX()
		local y = ig.igGetCursorPosY()

		-- same as in TileSheetWindow:showIndexUI
		local viewWidth = 256
		local viewHeight = 224
		ig.igGetContentRegionAvail(availSize)
		availSize.x = availSize.x - 16	-- make room for scrollbar
		availSize.y = availSize.y - 4
		local scale = math.max(1, availSize.x / viewWidth)--, availSize.y / viewHeight)

		local battleBgTex = app.mapWindow.battleBgTex
		if battleBgTex then
			drawSize.x = math.ceil(scale * battleBgTex.width)
			drawSize.y = math.ceil(scale * battleBgTex.height)
			-- TODO use uv1 to be 224/256, and drawSize accordingly ...
			ig.igImage(battleBgTex.id, drawSize)
		end
		if self.monsterSpriteTexs then
			-- key is number index 1-6
			for k,info in pairs(self.monsterSpriteTexs) do
				ig.igSetCursorPosX(math.ceil(x + 8 * info.pos.x * scale))
				ig.igSetCursorPosY(math.ceil(y + 8 * info.pos.y * scale))
				drawSize.x = math.ceil(scale * info.tex.width)
				drawSize.y = math.ceil(scale * info.tex.height)
				ig.igImage(info.tex.id, drawSize)
			end
		end
		ig.igSetCursorPosX(x)
		ig.igSetCursorPosY(y + math.ceil(viewHeight * scale))
	end

	if self.index < game.countof(game.formationMPs) then
		local tmp = {tonumber(game.formationMPs[self.index])}
		if ig.luatableInputFloatAsText('mp gained', tmp, 1) then
			game.formationMPs[self.index] = tmp[1]
		end
	end

	local formation = game.formations + self.index
	for i=1,6 do
		ig.igPushID_Str('BattleFormationWindow')
		ig.igPushID_Int(i)
		ig.igText(' #'..i)

		local info = formation:getMonsterInfo(i)
		if ig.luatableCheckbox('active', info, 'active') then
			formation['active'..i] = info.active and 1 or 0
		end
		if info.active then
			if self:editMonsterRef(info, 'monster') then
				formation['monster'..i] = info.monster
			end

			-- pointer into another table I think?
			if self:editField(info, 'pos', game.XY4b) then
				formation['pos'..i] = info.pos
			end

			-- this is in a whole other struct , so i'm not making it editable yet
			ig.igText(' size = '..formation:getFormationSize(i))
		end
		ig.igPopID()
		ig.igPopID()
	end
	local formation2 = game.formation2s[self.index]
	for fieldname, ctype, field in formation2:fielditer() do
		self:editField(formation2, fieldname, ctype, field)
	end

	-- reverse-references:

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
	local app = self.app
	local game = app.game

	BattleFormationWindow.super.setIndex(self, ...)

	-- clear cache
	self.battlesWithThis = nil

	-- get monster sprites
	if self.monsterSpriteTexs then
		for _,k in ipairs(table.keys(self.monsterSpriteTexs)) do
			self.monsterSpriteTexs[k].tex:delete()
		end
	end
	self.monsterSpriteTexs = {}

	local formation = game.formations + self.index
	for i=1,6 do
		-- tempting to just cache the whole info struct here ... hmm...
		local info = formation:getMonsterInfo(i)
		if info.active then
			self.monsterSpriteTexs[i] = {
				-- TODO NOTICE, this is YX, not XY
				-- should I change it in ff6.lua?
				pos = {x=tonumber(info.pos.y), y=tonumber(info.pos.x)},
				tex = self:makeTex(
					readMonsterSprite(game, info.monster)
				),
			}
		end
	end
end

return BattleFormationWindow
