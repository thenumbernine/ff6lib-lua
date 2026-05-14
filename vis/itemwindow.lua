local table = require 'ext.table'
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

	if ig.igCollapsingHeader'fields' then
		ig.igText(' desc = "'..game.gamezstr(game.itemDescBase + game.itemDescOffsets[self.index])..'"')
		for fieldname, ctype, field in item:fielditer() do
			if fieldname == 'spellLearn' then
				--if item.spellLearn.rate > 0 then
					--app.spellWindow:popupButton(item.spellLearn.spell.i)
					self:editField(item, fieldname, ctype, field)
				--end
				--ig.igText(' spell learn rate = '..item.spellLearn.rate)
				--self:editField(item, fieldname, ctype, field)
			elseif fieldname == 'spellCast' then
				-- is this the right condition?
				--if item.castOnAttack ~= 0 or item.castOnItemUse ~= 0 then
					--app.spellWindow:popupButton(item.spellCast)
					self:editSpellRef(item, fieldname, ctype, field)
				--end
			else
				self:editField(item, fieldname, ctype, field)
			end
		end
	end

	ig.igSeparator()
	ig.igText' colosseum info:'
	ig.igPushID_Str'itemColosseumInfos'
	local colinfo = game.itemColosseumInfos[self.index]
	for fieldname, ctype, field in colinfo:fielditer() do
		self:editField(colinfo, fieldname, ctype, field)
	end
	ig.igPopID()


	-- reverse-references:

	if not self.colosseumBetsWithThis then
		self.colosseumBetsWithThis = table()
		for i=0,game.countof(game.items)-1 do
			local otherColInfo = game.itemColosseumInfos + i
			if otherColInfo.itemWon.i == self.index then
				self.colosseumBetsWithThis:insert(i)
			end
		end
	end
	ig.igSeparator()
	ig.igText"colosseum bets that give this item..."
	if #self.colosseumBetsWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-colosseumBetsWithThis'
		for j,i in ipairs(self.colosseumBetsWithThis) do
			ig.igPushID_Int(j)
			self:popupButton(i)
			ig.igPopID()
		end
		ig.igPopID()
	end


	if not self.monstersWithThis then
		self.monstersWithThis = table()
		for i=0,game.countof(game.monsterItems)-1 do
			local monsterItem = game.monsterItems + i
			for fieldname in monsterItem:fielditer() do
				if monsterItem[fieldname].i == self.index then
					self.monstersWithThis:insert{monsterIndex=i, fieldname=fieldname}
				end
			end
		end
	end
	ig.igSeparator()
	ig.igText'monsters with this item...'
	if #self.monstersWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-monstersWithThis'
		for j,info in ipairs(self.monstersWithThis) do
			ig.igPushID_Int(j)
			app.monsterWindow:popupButton(info.monsterIndex, info.fieldname)
			ig.igPopID()
		end
		ig.igPopID()
	end


	if not self.treasuresWithThis then
		self.treasuresWithThis = table()
		for i=0,game.countof(game.maps)-1 do
			local mapInfo = game.getMap(i)		-- this wont bloat mem too much right?
			for j,treasure in ipairs(mapInfo.treasures) do
				if treasure.type == 2	-- item
				and treasure.battleOrItemOrGP == self.index
				then
					self.treasuresWithThis:insert{
						mapIndex = i,
						treasureIndex = j,
						treasure = treasure,
					}	-- map is 0-based, treasure is 1-based
				end
			end
		end
	end
	ig.igSeparator()
	ig.igText'found in treasure chest ...'
	if #self.treasuresWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-treasuresWithThis'
		for j,info in ipairs(self.treasuresWithThis) do
			ig.igPushID_Int(j)
			if app.mapWindow:popupButton(
				info.mapIndex,
				'treasure #'..info.treasureIndex
			) then
				app.treasureWindow.show[0] = true
				app.treasureWindow:setIndex(info.treasureIndex-1)

				local t = info.treasure
				-- just like doorWindow...
				-- new map should be loaded now
				local mapWidth, mapHeight = app.tileWindow:getMapSize()
				if mapWidth and mapHeight then
					app.tileWindow:setIndex(t.pos.x + mapWidth * t.pos.y)
				end
				app:centerView(t.pos.x, t.pos.y)
			end
			ig.igPopID()
		end
		ig.igPopID()
	end


	if not self.charsWithThis then
		self.charsWithThis = table()
		for i=0,game.numCharacters-1 do
			local name = tostring(game.characterNames[i])
			local ch = game.characters + i
			for _,field in ipairs{'lhand', 'rhand', 'head', 'body'} do
				if ch[field].i == self.index then
					self.charsWithThis:insert{charIndex=i, name=name, field=field}
				end
			end
			-- TODO time to not make these an array?
			for j=0,1 do
				if ch.relic.s[j].i == self.index then
					self.charsWithThis:insert{charIndex=i, name=name, field='relic'..(j+1)}
				end
			end
		end
	end
	ig.igSeparator()
	ig.igText'found in characters ...'
	if #self.charsWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-charsWithThis'
		local lastChar
		for j,info in ipairs(self.charsWithThis) do
			ig.igPushID_Int(j)
			if lastChar ~= info.name then
				lastChar = info.name
				ig.igText(lastChar)
			end
			ig.igSameLine()

			if ig.igButton(info.field) then
				self.app.charWindow.show[0] = true
				self.app.charWindow:setIndex(info.charIndex)
			end

			ig.igPopID()
		end
		ig.igPopID()
	end


	if not self.metamorphsWithThis then
		self.metamorphsWithThis = table()
		for i=0,game.countof(game.metamorphSets)-1 do
			local mmset = game.metamorphSets + i
			for j=0,mmset.dim-1 do
				if mmset.s[j].i == self.index then
					self.metamorphsWithThis:insert(i)
					break
				end
			end
		end
	end
	ig.igSeparator()
	ig.igText'found in metamorph sets ...'
	if #self.metamorphsWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'itemWindow-metamorphsWithThis'
		for _,metamorphIndex in ipairs(self.metamorphsWithThis) do
			app.metamorphWindow:popupButton(metamorphIndex)
		end
		ig.igPopID()
	end

	if not self.eventScriptCmdsWithThis then
		self.eventScriptCmdsWithThis = table()
		for _,cmd in ipairs(game.eventScriptCmds) do
			local cmdname
			for _,checkcmdname in ipairs{'GiveItem', 'TakeItem'} do
				if game.ScriptCmds[checkcmdname]:isa(cmd) then
					cmdname = checkcmdname
					break
				end
			end
			if cmdname and cmd.itemIndex == self.index then
				self.eventScriptCmdsWithThis:insert{addr=cmd.addr, cmdname=cmdname}
			end
		end
	end
	ig.igSeparator()
	ig.igText'event scripts ...'
	if #self.eventScriptCmdsWithThis == 0 then
		ig.igText'...none'
	else
		for _,info in ipairs(self.eventScriptCmdsWithThis) do
			app.scriptWindow:popupButtonForAddr(info.addr, info.cmdname)
		end
	end
	-- TODO I guess there's events, monsters, objects, vehicles, world scripts ... this is just events ...
end

function ItemWindow:setIndex(...)
	ItemWindow.super.setIndex(self, ...)

	-- clear cache
	self.colosseumBetsWithThis = nil
	self.monstersWithThis = nil
	self.treasuresWithThis = nil
	self.charsWithThis = nil
	self.metamorphsWithThis = nil
	self.eventScriptCmdsWithThis = nil
end

return ItemWindow
