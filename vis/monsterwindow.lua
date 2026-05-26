local table = require 'ext.table'
local ig = require 'imgui'
local readMonsterSprite = require 'ff6.monstersprite'
local ArrayWindow = require 'ff6.vis.arraywindow'


local MonsterWindow = ArrayWindow:subclass()

MonsterWindow.name = 'monster'

function MonsterWindow:getCount()
	local game = self.app.game
	return game.countof(game.monsters)
end

function MonsterWindow:getIndexName(i)
	return self.app.game.monsterNames[i]
end

local availSize = ig.ImVec2()
local drawSize = ig.ImVec2()
local uv0 = ig.ImVec2(0,0)
local uv1 = ig.ImVec2(1,1)
function MonsterWindow:showIndexUI()
	local app = self.app
	local game = app.game

	if app.sram then
		local save = app.sramWindow:getCurIndex()
		if save then
			if app.sramWindow.monstersEnabled[self.index] then
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff00ff00)
				ig.igText'- can encounter in veldt'
				ig.igPopStyleColor(1)

				local byteofs = bit.rshift(self.index, 3)
				local bitofs = bit.band(self.index, 7)
				local mask = bit.lshift(1, bitofs)
				local enabled = 0 ~= bit.band(mask, save.rageFlags[byteofs])
				if enabled then
					ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff00ff00)
					ig.igText'- gau has already found'
				else
					ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff0000ff)
					ig.igText'- gau has not found'
				end
				ig.igPopStyleColor(1)
			else
				ig.igPushStyleColor_U32(ig.ImGuiCol_Text, 0xff0000ff)
				ig.igText'- cannot encounter in veldt'
				ig.igPopStyleColor(1)
			end
		end
	end

	if self.monsterSpriteTex then
		--[[ fixed-size float-right
		local y = ig.igGetCursorPosY()
		ig.igSetCursorPosY(32)	-- or wherever y should be after the title bar
		local avail = ig.ImVec2()
		ig.igGetContentRegionAvail(avail)
		local desiredX = ig.igGetCursorPosX() + avail.x - self.monsterSpriteTex.width
		ig.igSetCursorPosX(desiredX)
		ig.igImage(self.monsterSpriteTex.id, self.monsterSpriteTex.imsize)
		ig.igSetCursorPosY(y)
		--]]
		-- [[ size based on image
		local y = ig.igGetCursorPosY()
		ig.igGetContentRegionAvail(availSize)
		availSize.x = availSize.x - 16	-- make room for scrollbar
		availSize.y = availSize.y - 4
		local scale = math.max(1, availSize.x / self.monsterSpriteTex.width)
		drawSize.x = math.ceil(scale * self.monsterSpriteTex.width)
		drawSize.y = math.ceil(scale * self.monsterSpriteTex.height)
		ig.igImage(self.monsterSpriteTex.id, drawSize, uv0, uv1)
		ig.igSetCursorPosY(y + math.ceil(self.monsterSpriteTex.height * scale) + 4)
		--]]
		--[[ size based on arbitrary specified width
				-- pick these to be the largest monster sprite size ...
		--[=[
		local viewWidth = 128
		local viewHeight = 128
		--]=]
		-- [=[
		local viewHeight = math.max(self.monsterSpriteTex.width, self.monsterSpriteTex.height)
		local viewWidth = viewHeight
		--]=]
		local y = ig.igGetCursorPosY()
		ig.igGetContentRegionAvail(availSize)
		availSize.x = availSize.x - 16	-- make room for scrollbar
		availSize.y = availSize.y - 4
		local scale = math.max(1, availSize.x / viewWidth)
		uv1.y = viewHeight / self.monsterSpriteTex.height
		drawSize.x = math.ceil(scale * self.monsterSpriteTex.width)
		drawSize.y = math.ceil(scale * viewHeight)
		ig.igImage(self.monsterSpriteTex.id, drawSize, uv0, uv1)
		ig.igSetCursorPosY(y + math.ceil(viewHeight * scale) + 4)
		--]]
	end

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

	-- pick the palette out of the MonsterSprite info and let the user edit it here ...
	-- (I think everything else in MonsterSprite is specific to the tile layout etc)
	if self.index >= 0 and self.index < game.countof(game.monsterSprites) then
		local monsterSprite = game.monsterSprites[self.index]
		self.__tmp = monsterSprite:getPaletteIndex()
		if ig.luatableInputInt('palette', self, '__tmp') then
			-- then set the palette ...
			monsterSprite.palLo = bit.band(0xff, self.__tmp)
			monsterSprite.palHi = bit.rshift(self.__tmp, 8)	-- only 7 bits
			-- if we changed the palette then regen the sprite ...
			self:refreshTex()
		end
	end

	ig.igSeparator()

	ig.igText'attacks:'
	ig.igPushID_Str'monsterSpells'
	local monsterSpells = game.monsterSpells[self.index]
	for i=0,monsterSpells.dim-1 do
		self:editField(monsterSpells.s, i, monsterSpells.ctype)
	end
	ig.igPopID()

	ig.igText'items:'
	ig.igPushID_Str'monsterItems'
	local monsterItem = game.monsterItems[self.index]
	for fieldname, ctype, field in monsterItem:fielditer() do
		self:editField(monsterItem, fieldname, ctype, field)
	end
	ig.igPopID()

	local monsterSketches = game.monsterSketches[self.index]
	ig.igText'sketches:'
	ig.igPushID_Str'monsterSketches'
	for i=0,monsterSketches.dim-1 do
		self:editField(monsterSketches.s, i, monsterSketches.ctype)
	end
	ig.igPopID()

	if self.index < game.countof(game.monsterRages)then
		ig.igText'rages:'
		ig.igPushID_Str'monsterRages'
		local monsterRages = game.monsterRages[self.index]
		for i=0,monsterRages.dim-1 do
			self:editField(monsterRages.s, i, monsterRages.ctype)
		end
		ig.igPopID()
	end

	-- reverse-references:

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

	-- clear reverse-reference cache
	self.battleFormationsWithThis = nil
	self.colosseumBetsWithThis = nil

	self:refreshTex()
end

function MonsterWindow:refreshTex()
	-- refresh monster sprite
	if self.monsterSpriteTex then
		self.monsterSpriteTex:delete()
		self.monsterSpriteTex = nil
	end
	if self.index < 0 or self.index >= self:getCount() then return end
	self.monsterSpriteTex = self:makeTex(readMonsterSprite(self.app.game, self.index))
end

return MonsterWindow
