#!/usr/bin/env luajit
local cmdline = require 'ext.cmdline'(...)
local ffi = require 'ffi'
local table = require 'ext.table'
local range = require 'ext.range'
local class = require 'ext.class'
local math = require 'ext.math'
local assert = require 'ext.assert'
local timer = require 'ext.timer'
local number = require 'ext.number'
local path = require 'ext.path'
local vec2d = require 'vec-ffi.vec2d'
local vec3d = require 'vec-ffi.vec3d'
local sdl = require 'sdl'
local gl = require 'gl.setup'(cmdline.gl)
local Image = require 'image'
local GLTex2D = require 'gl.tex2d'
local GLGeometry = require 'gl.geometry'
local GLSceneObject = require 'gl.sceneobject'
local ig = require 'imgui'

local startTime = timer.getTime()

local infn = cmdline[1]
assert(infn, "missing filename")
local game = require 'ff6'((assert(path(infn):read())))
local Game = game.Game
local rom = game.rom
local countof = game.countof

local function settableindex(t, i, ...)
	if select('#', ...) == 0 then return end
	t[i] = ...
	settableindex(t, i+1, select(2, ...))
end
local function settable(t, ...)
	settableindex(t, 1, ...)
end


local zAndLayersWithoutLayer3Priority = {
	{0,3},
	-- priority 0 sprites here
	--{1,3},	-- does layer 3 have a zlevel?  where is it?
	-- priority 1 sprites here
	{0,2},
	{0,1},
	-- priority 2 sprites here
	{1,2},
	{1,1},
	-- priority 3 sprites here
}

local zAndLayersWithLayer3Priority = {
	{0,2},
	{0,1},
	{1,2},
	{1,1},
	{0,3},
}


local numTilePropsBits = 18
local worldTilePropsNames = {
	'blocksChocobo',
	'airshipCantLand',
	'airshipShadow_0',
	'airshipShadow_1',
	'blocksWalking',
	'forest',
	'enemyEncounters',
	'unknown_0_7',
	'battleBG_0',
	'battleBG_1',
	'battleBG_2',
	'battleBG_3',
	'unknown_0_12',
	'veldt',
	'phoenixCave',
	'kefkasTower',
	'unused1',
	'unused2',
}
local mapTilePropsNames = {
	'zLevel_0',
	'zLevel_1',
	'zLevel_2',
	'topSpritePriority',
	'bottomSpritePriority',
	'door',
	'stairsUpRight',
	'stairsUpLeft',
	'passableRight',
	'passableLeft',
	'passableDown',
	'passableUp',
	'unknown_1_4',
	'unknown_1_5',
	'ladder',
	'passableNPC',
	'throughTile',
	'impassible',
}


local ArrayWindow = class()

function ArrayWindow:init(args)
	self.children = table(args.children)
	self.show = ffi.new('bool[1]', not not args.show)
	self.app = assert.index(args, 'app')
	self.getArray = args.getArray
	self:setIndex(args.index or 0)
end
function ArrayWindow:setIndex(index)
	if index == self.index then return false end
	self.index = index
end
function ArrayWindow:getIndexName() end
function ArrayWindow:popupButton(targetIndex)
	if targetIndex ~= nil then
		assert.type(targetIndex, 'number')
	end
	local result
	ig.igPushID_Str(self.name)
	ig.igPushID_Str'popup button'
	local ar = self:getArray()
	local has = ar and #ar > 0
	local indexName = has and targetIndex and self:getIndexName(targetIndex)
	local k = self.name..': '
		..(targetIndex and '#'..targetIndex..'/' or '')
		..(has and #ar or 'none')
		..(indexName and ' '..indexName or '')
	if not has then
		ig.igText(k)
		self.show[0] = false
	else
		if ig.igButton(k) then
			self.show[0] = true
			if targetIndex then
				print('setting', self.name, 'to', targetIndex)
				self:setIndex(targetIndex)
			end
			result = true
		end
	end
	ig.igPopID()
	ig.igPopID()
	return result
end

function ArrayWindow:update()
	if not self.show[0] then return end
	local ar = self:getArray()
	if not (ar and #ar > 0) then return end
	ig.igPushID_Str(self.name)
	if ig.igBegin(self.name, self.show, 0) then
		ig.igText(self.name..' #'..self.index..'/'..#ar)

		local pushIndex = self.index
		if ig.luatableInputInt('index', self, 'index') then
			local newIndex = self.index % #ar
			self.index = pushIndex	-- so setIndex registers a change
			self:setIndex(newIndex)
		end
		self:showIndexUI(ar)
	end
	ig.igEnd()
	ig.igPopID()
end


local MapWindow = ArrayWindow:subclass()
function MapWindow:init(...)
	MapWindow.super.init(self, ...)

	-- 'game.getMap' doensn't work so well since it has cache stuff like the texture layers ...
	self.array = range(countof(game.maps))

	self.mapIndexStack = table()
end
MapWindow.name = 'map'
function MapWindow:getMapInfo()
	return game.getMap(self.index)
end
function MapWindow:getArray()
	return self.array
end
function MapWindow:showIndexUI(ar)
	local app = self.app
	local mapInfo = self:getMapInfo()
	if not mapInfo then return end

	local map = mapInfo and mapInfo.map
	if map then
		ig.luatableTooltipCheckbox('useBlend', app, 'useBlend')

		ig.luatableTooltipCheckbox('showAnimTexs', app, 'showAnimTexs')
		-- hmm, i've got dif flags for anim layers and non-anim layers, why?
		if app.layerAnimTexs then
			local zAndLayers = map.layer3Priority == 0
				and zAndLayersWithoutLayer3Priority
				or zAndLayersWithLayer3Priority
			for _,zAndLayer in ipairs(zAndLayers) do
				local z, layer = table.unpack(zAndLayer)
				if app.layerAnimTexs[z]
				and app.layerAnimTexs[z][layer]
				then
					local k = 'showAnimTex_'..z..'_'..layer
					if app[k] == nil then app[k] = true end
					ig.igSameLine()
					ig.luatableTooltipCheckbox(k, app, k)
				end
			end
		elseif app.layerTexs then
			for i=1,#app.layerTexs do
				if i > 1 then
					ig.igSameLine()
				end
				local k = 'drawLayer'..i
				if app[k] == nil then app[k] = true end
				ig.luatableTooltipCheckbox('tex '..i, app, k)
			end
		end
	end

	local layouts = mapInfo and mapInfo.layouts
	local layout1Data = layouts and layouts[1] and layouts[1].data
	if layout1Data then
		-- based on MapTileProps:
		local tilePropsNames = self.index < 3
		-- WorldTileProps:
		and worldTilePropsNames
		-- MapTileProps:
		or mapTilePropsNames
assert.len(tilePropsNames, numTilePropsBits)
		for ip1,name in ipairs(tilePropsNames) do
			local i = ip1-1
			local mask = bit.lshift(1, i)
			self.__tmp = 0 ~= bit.band(app.showTileMask, mask)
			if bit.band(i, 7) ~= 0 then ig.igSameLine() end
			if ig.luatableTooltipCheckbox('tileProp '..i..': '..name, self, '__tmp') then
				app.showTileMask = bit.bxor(mask, app.showTileMask)
			end
		end
	end

	ig.luatableTooltipCheckbox('showTiles', app, 'showTiles')
	ig.igSameLine()
	app.tileWindow:popupButton()

	ig.luatableTooltipCheckbox('showTreasures', app, 'showTreasures')
	ig.igSameLine()
	app.treasureWindow:popupButton()

	ig.luatableTooltipCheckbox('showTouchTriggers', app, 'showTouchTriggers')
	ig.igSameLine()
	app.touchTriggerWindow:popupButton()

	ig.luatableTooltipCheckbox('showDoors', app, 'showDoors')
	ig.igSameLine()
	app.doorWindow:popupButton()

	ig.luatableTooltipCheckbox('showBigDoors', app, 'showBigDoors')
	ig.igSameLine()
	app.bigDoorWindow:popupButton()

	ig.luatableTooltipCheckbox('showNPCs', app, 'showNPCs')
	ig.igSameLine()
	app.npcWindow:popupButton()


	if self.index < 2 then
		ig.luatableTooltipCheckbox('showWorldEncounterSectors', app, 'showWorldEncounterSectors')
		ig.igSameLine()
		app.worldEncounterSectorWindow:popupButton()
	else
		app.randomBattleOptionsWindow:popupButton(mapInfo.monsterRandomBattleOptionIndex)
	end


	app.show16x16mapTiles = not not app.show16x16mapTiles
	ig.luatableTooltipCheckbox('show 16x16 tiles', app, 'show16x16mapTiles')

	local map = mapInfo.map
	if map then
		for name in map[0]:fielditer() do
			ig.igText(' '..name..' = '..tostring(map[0][name]))
		end
	end
end
function MapWindow:setIndex(newIndex, pushStack)
	local oldIndex = self.index

	-- special case for map 0x511 = parent map
	if newIndex == 0x1ff then
		newIndex = self.mapIndexStack:remove() or 0
		pushStack = false	-- can't push and pop at the same time
	end

	if MapWindow.super.setIndex(self, newIndex) == false then return false end

	if pushStack then
		self.mapIndexStack:insert(oldIndex)
	end

	local mapIndex = self.index
	local app = self.app

	-- reset windows that are dependent on the map
	for _,ch in ipairs(self.children) do
		ch:setIndex(0)
	end

	if app.layerTexs then
		for _,tex in ipairs(app.layerTexs) do
			tex:delete()
		end
		app.layerTexs = nil
	end

	collectgarbage()

	local mapInfo = game.getMap(mapIndex)
	if not mapInfo then
		print("map "..mapIndex.." missing")
		return
	end

	local map = mapInfo.map
	local paletteIndex = tonumber(map.palette)
	local gfxLayer3 = mapInfo.gfxLayer3
	local tilesetDatas = mapInfo.tilesetDatas
	local layerPos = mapInfo.layerPos
	local layerSizes = mapInfo.layerSizes
	local layouts = mapInfo.layouts
	local palette = mapInfo.palette
	local tilePropsData = mapInfo.tilePropsData

	print('maps[0x'..number.hex(mapIndex)..'] addr '
		..'0x'..number.hex(ffi.cast('uint8_t*', map) - rom)
		..' = '..map[0])

	local gfxstr = mapInfo.gfxIndexes:mapi(tostring):concat'/'
	for i=1,2 do
		local tilesetData = tilesetDatas[i]
		print('map tileset'..i..' data size', tilesetData and #tilesetData)
	end

	for i=1,3 do
		print('map layer '..i..' size', layerSizes[i], 'volume', layerSizes[i]:volume())
		print('map layout'..i..' data size', layouts[i] and #layouts[i].data)
		if i > 1 then
			print('map layer'..i..' pos', layerPos[i])
		end
	end

	local palette = mapInfo.palette

	if not palette then
		print("map "..mapIndex.." has no palette")
		return
	end

	local palData = ffi.new'uint8_t[128*4]'
	ffi.fill(palData, ffi.sizeof(palData))
	for i=0,127 do
		for j=0,3 do
			palData[bit.bor(j,bit.lshift(i, 2))] = palette[i+1][j+1]
		end
	end
	if self.mapPalTex then
		self.mapPalTex:delete()
	end
	self.mapPalTex = GLTex2D{
		width = 16*8,
		height = 1,
		internalFormat = gl.GL_RGBA,
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_NEAREST,
		data = palData,
	}:unbind()

	-- layer images already have 16x16 tiles baked into them...
	local imgToTex = function(img)
		local tex = GLTex2D{
			width = img.width,
			height = img.height,
			internalFormat = gl.GL_R8UI,
			minFilter = gl.GL_NEAREST,
			magFilter = gl.GL_NEAREST,
			data = img.buffer,
		}:unbind()
		tex.image = img
		return tex
	end
	local layerImgs, layerAnimImgs = mapInfo:getLayerImages()

	if app.layerTexs then
		for _,tex in ipairs(app.layerTexs) do
			tex:delete()
		end
	end
	app.layerTexs = layerImgs:mapi(imgToTex)

	if app.layerAnimTexs then
		for z, layerAnimImgs_z in pairs(app.layerAnimTexs) do
			for layer, layerAnimImgs_z_layer in pairs(layerAnimImgs_z) do
				for _,tex in ipairs(layerAnimImgs_z_layer) do
					tex:delete()
				end
			end
		end
	end
	app.layerAnimTexs = table()
	for z, layerAnimImgs_z in pairs(layerAnimImgs) do
		app.layerAnimTexs[z] = table()
		for layer, layerAnimImgs_z_layer in pairs(layerAnimImgs_z) do
			app.layerAnimTexs[z][layer] = layerAnimImgs_z_layer:mapi(imgToTex)
		end
	end

	if #app.layerTexs > 0 then
		app.mapSize:set(app.layerTexs[1].width, app.layerTexs[1].height)
	end

	if app.tilePropsTex then
		app.tilePropsTex:delete()
	end
	app.tilePropsTex = nil
	-- maybe another layout other than 1?
	local layout1Data = layouts[1] and layouts[1].data
	if layout1Data
	and tilePropsData
	then
		-- uint8_t into the tilePropsPtr table, which is a table of 2-byte-sized either WorldTileProps or MapTileProps
		local layoutptr = ffi.cast('uint8_t*', layout1Data)
		local tilePropsPtr = ffi.cast('uint16_t*', tilePropsData)
		local volume = layerSizes[1].x * layerSizes[1].y
		local data = ffi.new('uint32_t[?]', volume)
		ffi.fill(data, 0, volume * ffi.sizeof'uint32_t')
		for i=0,volume-1 do
			-- for non-world-maps,
			-- two special values:
			-- 0x7 = "through-tile"
			-- 0xfff7 = "impassible"
			-- i'll turn them into new flags
			local flags = tilePropsPtr[layoutptr[i]]
			if self.index >= 3 then
				if flags == 7 then
					flags = 0x10000	-- 'through-tile'
				elseif flags == 0xfff7 then
					flags = 0x20000	-- 'impassible'
				end
			end
			data[i] = flags
		end

		app.tilePropsTex = GLTex2D{
			width = layerSizes[1].x,
			height = layerSizes[1].y,
			internalFormat = gl.GL_R32UI,
			minFilter = gl.GL_NEAREST,
			magFilter = gl.GL_NEAREST,
			data = data,
		}:unbind()
	end

	-- [[ also load/show the 16x16 tiles?
	if app.map16x16tileTexs then
		for _,texs in ipairs(app.map16x16tileTexs) do
			for _,tex in ipairs(texs) do
				tex:delete()
			end
		end
		app.map16x16tileTexs = nil
	end
	if map then
		app.map16x16tileTexs = table()

		local gfxDatas = mapInfo.gfxIndexes:mapi(function(i)
			local gfx = game.getMapTileGraphics(i)
			if not gfx then return end
			return gfx.data
		end)

		for k=1,2 do
			app.map16x16tileTexs[k] = table()
			local tilesetIndex = tonumber(map['tileset'..k])
			-- TODO how to specify animation # as well?
			-- might have to just write these out based on mapindex ... and just skip unique ones?

			--for frameIndex=0,3 do
			-- but we have disableAnimationGeneration set so ...
			for frameIndex=0,0 do
				-- trying to load animated map frames...
				do--if not disableAnimationGeneration then
					local index = 0 --map.animatedLayers1And2
					local startOffset = game.mapAnimPropOfs[index]
					assert.eq(startOffset % ffi.sizeof(game.MapAnimProps), 0)
					local startIndex = startOffset / ffi.sizeof(game.MapAnimProps)
					local count = 32
					local animLayers1And2Props = table()
					for i=0,count-1 do
						local p = game.mapAnimProps + startIndex + i
						animLayers1And2Props:insert(p)
					end

					gfxDatas[5] = range(0,count-1):mapi(function(i)
						local p = game.mapAnimProps[startIndex + i]
						return ffi.string(game.mapAnimGraphics + p.frames.s[frameIndex ], 0x80)
					end):concat()
				end

				-- starting to wonder why key is just gfx1/2/3/4 and not /paletteIndex as well....
				local paletteIndex = tonumber(map.palette)

				local size = vec2d(16, 16)
				local img = Image(16 * size.x, 16 * size.y, 1, 'uint8_t'):clear()
				-- what is its format?
				local tile16x16 = 0
				for j=0,size.y-1 do
					local y = bit.lshift(j, 4)
					for i=0,size.x-1 do
						local x = bit.lshift(i, 4)
						game.layer1and2drawtile16x16(
							img,
							x,
							y,
							tile16x16,
							--map.tilesetDatas[layer]
							game.mapTilesetCache[tilesetIndex].data,
							nil,
							gfxDatas,
							nil -- mapInfo.gfxLayer3 and mapInfo.gfxLayer3.data
						)
						tile16x16 = tile16x16 + 1
					end
				end
				app.map16x16tileTexs[k]:insert(GLTex2D{
					--image = img,
					data = img.buffer,
					width = img.width,
					height = img.height,
					internalFormat = gl.GL_R8UI,

					minFilter = gl.GL_NEAREST,
					magFilter = gl.GL_NEAREST,
				})
			end
		end
	end
--]]

	-- start us off centered at the first door we find
	if mapInfo.doors then	
		local e = mapInfo.doors[1]
		if e then
			app:centerView(e.pos.x, e.pos.y)
		end
	end
end


local TileWindow = ArrayWindow:subclass()
TileWindow.name = 'tile'
function TileWindow:getMapSize()
	local mapInfo = game.getMap(self.app.mapWindow.index)
	if not mapInfo then return end
	local layerSizes = mapInfo.layerSizes
	if not layerSizes then return end
	return layerSizes[1].x, layerSizes[1].y
end
function TileWindow:getMapVolume()
	local w, h = self:getMapSize()
	if w and h then return w * h end
end
function TileWindow:getArray()
	local vol = self:getMapVolume()
	return vol and range(vol)
end
function TileWindow:showIndexUI(ar)
	local t = ar[1+self.index]
	if not t then return end

	local w, h = self:getMapSize()
	if not w then return end

	local x = self.index % w
	local y = (self.index - x) / w
	ig.igText(x..', '..y..' of '..w..', '..h)

	-- if getting x and y fail then mapInfo can't be accessed, so by here mapInfo is guaranteed
	local mapInfo = game.getMap(self.app.mapWindow.index)
	local layerPos = mapInfo.layerPos
	local layerSizes = mapInfo.layerSizes
	local tilePropsData = mapInfo.tilePropsData
	local layouts = mapInfo.layouts
	
	local layout1Data = layouts[1] and layouts[1].data
	if layout1Data then
		local layoutptr = ffi.cast('uint8_t*', layout1Data)
		if  tilePropsData then
			local tilePropsPtr = ffi.cast('uint16_t*', tilePropsData)
			local flags = tilePropsPtr[layoutptr[self.index]]
			ig.igText(('tile props = 0x%04x'):format(flags))
		end
	end
	for layer=1,3 do
		local layout = layouts[layer]
		local layerSize = layerSizes[layer]
		local layoutData = layout and layout.data
		if layoutData then
			
			local posx, posy = 0, 0
			if layerPos[layer]
			-- if we have a position for the layer, but we're using parallax, then the position is going to be relative to the view
			--and map.parallax == 0
			then
				posx, posy = layerPos[layer]:unpack()
			end

			local layoutptr = ffi.cast('uint8_t*', layoutData)
			local srcX = (x - posx) % layerSize.x
			local srcY = (y - posy) % layerSize.y
			local tile16x16 = layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]
			ig.igText(('layer %d tile: 0x%02x'):format(layer, tile16x16))
		end
	end
end


local TreasureWindow = ArrayWindow:subclass()
TreasureWindow.name = 'treasure'
function TreasureWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.treasures
end
function TreasureWindow:showIndexUI(ar)
	local t = ar[1+self.index]
	if not t then return end
	ig.igText(' pos = '..t.pos)
	ig.igText(' switch = '..t.switch)
	ig.igText(' empty = '..t.empty)
	ig.igText(' type = '..t.type)	-- combo: empty, monster, item, gp

	local app = self.app
	if t.type == 0 then	-- empty
		ig.igText(' empty = '..t.battleOrItemOrGP)
	elseif t.type == 1 then	-- monster
		app.eventBattleOptionsWindow:popupButton(t.battleOrItemOrGP)
	elseif t.type == 2 then	-- item
		app.itemWindow:popupButton(t.battleOrItemOrGP)
	elseif t.type == 3 then	-- GP
		ig.igText(' GP = '..(t.battleOrItemOrGP * 100))
	else
		ig.igText(' ??? = '..t.battleOrItemOrGP)
	end
end


local TouchTriggerWindow = ArrayWindow:subclass()
TouchTriggerWindow.name = 'event trigger'
function TouchTriggerWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.touchTriggers
end
function TouchTriggerWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end
	ig.igText(' pos = '..e.pos)
	-- absolute?
	local scriptAddr = e:getScriptAddr()
	if ig.igButton((' event script = $%06x'):format(scriptAddr)) then
		self.app.scriptWindow:openScriptAddr(scriptAddr)
	end
end


local DoorWindow = ArrayWindow:subclass()
DoorWindow.name = 'door'
function DoorWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.doors
end
function DoorWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end
	ig.igText(' pos = '..e.pos)
	if ig.igButton(' map = '..e.mapIndex) then
		self.app.mapWindow:setIndex(e.mapIndex, 0 ~= e.setParentMap)
		self.app:centerView(e.dest.x, e.dest.y)
	end
	ig.igText(' setParentMap = '..e.setParentMap)
	ig.igText(' zLevel = '..e.zLevel)
	ig.igText(' showDestName = '..e.showDestName)
	ig.igText(' destFacingDir = '..e.destFacingDir)
	ig.igText(' unknown_3_6 = '..e.unknown_3_6)
	ig.igText(' dest = '..e.dest)
end


local BigDoorWindow = ArrayWindow:subclass()
BigDoorWindow.name = 'big door'
function BigDoorWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.bigDoors
end
function BigDoorWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end
	ig.igText(' pos = '..e.pos)
	if ig.igButton(' map = '..e.mapIndex) then
		self.app.mapWindow:setIndex(e.mapIndex, 0 ~= e.setParentMap)
		self.app:centerView(e.dest.x, e.dest.y)
	end
	ig.igText(' length = '..(e.length+1))
	ig.igText(e.vertical==0 and ' horz' or ' vert')

	-- notice the rest is in common with typical Door:
	-- how about a common parent struct?
	ig.igText(' setParentMap = '..e.setParentMap)
	ig.igText(' zLevel = '..e.zLevel)
	ig.igText(' showDestName = '..e.showDestName)
	ig.igText(' destFacingDir = '..e.destFacingDir)
	ig.igText(' unknown_3_6 = '..e.unknown_3_6)
	ig.igText(' dest = '..e.dest)
end


local NPCWindow = ArrayWindow:subclass()
NPCWindow.name = 'npc'
function NPCWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.npcs
end
function NPCWindow:showIndexUI(ar)
	local n = ar[1+self.index]
	if not n then return end
	ig.igText(' pos = '..n.x..', '..n.y)

	local scriptAddr = n:getScriptAddr()
	if ig.igButton((' script $%06x'):format(scriptAddr)) then
		self.app.scriptWindow:openScriptAddr(scriptAddr)
	end
	ig.igText(' movement = '..n.movement)
	ig.igText(' speed = '..n.speed)

	ig.igText(' graphics = '..n.graphics)
	ig.igText(' palette = '..n.palette)

	-- "speed" when vehicle == 0
	-- "vehicle" otherwise
	ig.igText(' vehicle_or_speed = '..n.vehicle_or_speed)	-- what's this speed vs the other speed?

	ig.igText(' spritePriority = '..n.spritePriority)

	-- "direction" when animation == 0
	-- "type" otherwise
	ig.igText(' direction_or_type = '..n.direction_or_type)

	-- "size" when vehicle == 0 && special npc != 0
	-- otherwise "talkDoesntTurn"
	ig.igText(' size_or_talkDoesntTurn = '..n.size_or_talkDoesntTurn)

	ig.igText(' layerPriority = '..n.layerPriority)

	ig.igText(' animation = '..n.animation)
end


local WorldEncounterSectorWindow = ArrayWindow:subclass()
WorldEncounterSectorWindow.name = 'world encounter sectors'
function WorldEncounterSectorWindow:init(...)
	WorldEncounterSectorWindow.super.init(self, ...)
	self.array = range(0, bit.lshift(1, 7)-1)
end
function WorldEncounterSectorWindow:getArray()
	return self.array
end
WorldEncounterSectorWindow.terrains = {'grass', 'forest', 'desert', 'dirt'}	-- also fields of WorldSectorBattles_t
WorldEncounterSectorWindow.encounterNames = {'normal', 'low', 'high', 'none'}
function WorldEncounterSectorWindow:showIndexUI(ar)
	ig.igText('world='..bit.rshift(self.index, 6))
	ig.igText('x='..bit.lshift(bit.band(self.index, 7), 5))
	ig.igText('y='..bit.lshift(bit.band(bit.rshift(self.index, 3), 7), 5))
	local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + self.index
	local encounterRateBits = game.worldSectorRandomBattleEncounterRatesPerTerrain[self.index]
	for i,terrain in ipairs(self.terrains) do
		ig.igText('terrain = '..tostring(terrain))
		local encounter = bit.band(3, bit.rshift(encounterRateBits, bit.lshift(i-1, 1)))
		local encounterRateName = self.encounterNames[encounter+1]
		ig.igText('rate = '..tostring(encounterRateName))

		ig.igText('battle options = ')
		ig.igSameLine()
		local battleIndex = randomBattlesPerTerrain[terrain]
		self.app.randomBattleOptionsWindow:popupButton(battleIndex)
	end
end


local ScriptWindow = ArrayWindow:subclass()
function ScriptWindow:init(...)
	ScriptWindow.super.init(self, ...)
	self.clipper = ig.ImGuiListClipper_ImGuiListClipper()
	self.availableSpace = ig.ImVec2()
	-- on gc, but only before app shutdown:
	--ig.ImGuiListClipper_destroy(self.clipper)
end
ScriptWindow.name = 'script'
function ScriptWindow:getArray()
	return game.eventScriptCmds
end
function ScriptWindow:showIndexUI(ar)
	-- TODO index isn't necessary for this window
	-- tho better TODO is to fix the scroll area of the clipper and make it jump correctly
	--if ig.igBeginChild('ScriptWindowEvents', ig.ImVec2(0, #ar), true) then
	-- this is even worse, now single wheel or scrollbar scrolls up and down jump over an entire page, and i can't find the item i'm looking for, and the clipper seek funciton doesn't work.
	ig.igGetContentRegionAvail(self.availableSpace)
	self.availableSpace.x = 0
	if ig.igBeginChild('ScriptWindowEvents', self.availableSpace, true) then

		if self.jumpRequested then
			ig.igSetScrollY_Float(self.jumpRequested)
			self.jumpRequested = nil
		end

		--[[ not working, very arbitrary
		if ig.igIsWindowHovered(0) then
			local wheel = ig.igGetIO().MouseWheel
			if wheel ~= 0 then
				ig.igSetScrollY_Float(ig.igGetScrollY() - wheel * 10)
			end
		end
		--]]

		ig.ImGuiListClipper_Begin(self.clipper, #ar, 1)
		while ig.ImGuiListClipper_Step(self.clipper) do
			for i=self.clipper.DisplayStart,self.clipper.DisplayEnd-1 do
				local cmd = ar[1+i]
				if cmd then
					ig.igText(
						(i == self.index and '>' or ' ')
						..('%06x: '):format(cmd.addr)
						..tostring(cmd)
							:gsub('\n', '\\n')
					)
				end
			end
		end
		ig.ImGuiListClipper_End(self.clipper)
	end
	ig.igEndChild()
end
function ScriptWindow:setIndex(newIndex)
	if ScriptWindow.super.setIndex(self, newIndex) == false then return false end
	local cmd = self:getArray()[1+self.index]
	if cmd then
		self:openScriptAddr(cmd.addr)
	end
end
function ScriptWindow:openScriptAddr(scriptAddr)
	self.index = game.eventScriptCmdIndexForAddr[scriptAddr]
	if not self.index then
		print(("couldn't find event script command at address $%06x"):format(scriptAddr))
		self.index = 0
	else
		self.index = self.index - 1
	end
	self.show[0] = true
	--[[
	ig.ImGuiListClipper_SeekCursorForItem(self.clipper, self.index)
	--]]
	-- [[
	self.jumpRequested = self.index
	--]]
end


local SpellWindow = ArrayWindow:subclass()
SpellWindow.name = 'spell'
function SpellWindow:init(...)
	SpellWindow.super.init(self, ...)
	self.array = range((countof(game.spells)))
end
function SpellWindow:getArray()
	return self.array
end
function SpellWindow:getIndexName(i)
	return game.getSpellName(i)
end
function SpellWindow:showIndexUI(ar)
	ig.igText(' name = '..self:getIndexName(self.index))
	if self.index < 54 then
		ig.igText(' desc = "'..game.gamezstr(game.spellDescBase + game.spellDescOffsets[self.index])..'"')
	elseif self.index >= 54 and self.index < 64 then
		-- should I put esper descs here, or in the esper output, or both?
	end
	local spell = game.spells[self.index]
	for name in spell:fielditer() do
		ig.igText(' '..name..' = '..tostring(spell[name]))
	end
end


local ItemWindow = ArrayWindow:subclass()
ItemWindow.name = 'item'
function ItemWindow:init(...)
	ItemWindow.super.init(self, ...)
	self.array = range((countof(game.items)))
end
function ItemWindow:getArray()
	return self.array
end
function ItemWindow:getIndexName(i)
	return game.itemNames[i]
end
function ItemWindow:showIndexUI(ar)
	local item = game.items + self.index
	ig.igText(' name = '..self:getIndexName(self.index))
	ig.igText(' desc = "'..game.gamezstr(game.itemDescBase + game.itemDescOffsets[self.index])..'"')
	for name in item:fielditer() do
		ig.igText(' '..name..' = '..tostring(item[name]))
	end

	ig.igText' colosseum info:'
	local colinfo = game.itemColosseumInfos[self.index]

	ig.igText('  monster fought =')
	ig.igSameLine()
	self.app.monsterWindow:popupButton(colinfo.monster.i)

	ig.igText('  item won =')
	ig.igSameLine()
	self.app.itemWindow:popupButton(colinfo.itemWon.i)

	ig.igText('  unknown = '..colinfo.unknown)
	ig.igText('  hideName = '..colinfo.hideName)
end


local MonsterWindow = ArrayWindow:subclass()
MonsterWindow.name = 'monster'
function MonsterWindow:init(...)
	MonsterWindow.super.init(self, ...)
	self.array = range((countof(game.monsters)))
end
function MonsterWindow:getArray()
	return self.array
end
function MonsterWindow:getIndexName(i)
	return game.monsterNames[i]
end
function MonsterWindow:showIndexUI(ar)
	ig.igText(' name = "'..self:getIndexName(self.index)..'"')
	ig.igText(' attack name = "'..game.monsterAttackNames[self.index]..'"')
	local monster = game.monsters[self.index]
	for name in monster:fielditer() do
		ig.igText(' '..name..' = '..tostring(monster[name]))
	end
	local monsterSpells = game.monsterSpells[self.index]
	for i=0,monsterSpells.dim-1 do
		ig.igPushID_Str'monsterSpells'
		ig.igPushID_Int(i)
		self.app.spellWindow:popupButton(monsterSpells.s[i].i)
		ig.igPopID()
		ig.igPopID()
	end
	local monsterItem = game.monsterItems[self.index]
	for name in monsterItem:fielditer() do
		ig.igPushID_Str(name)
		ig.igText('  '..name..' = ')
		ig.igSameLine()
		self.app.itemWindow:popupButton(monsterItem[name].i)
		ig.igPopID()
	end
	local monsterSketches = game.monsterSketches[self.index]
	for i=0,monsterSketches.dim-1 do
		ig.igPushID_Str'monsterSketches'
		ig.igPushID_Int(i)
		self.app.spellWindow:popupButton(monsterSketches.s[i].i)
		ig.igPopID()
		ig.igPopID()
	end
	if self.index < countof(game.monsterRages)then
		local monsterRages = game.monsterRages[self.index]
		for i=0,monsterRages.dim-1 do
			ig.igPushID_Str'monsterRages'
			ig.igPushID_Int(i)
			self.app.spellWindow:popupButton(monsterRages.s[i].i)
			ig.igPopID()
			ig.igPopID()
		end
	end
end


local BattleFormationWindow = ArrayWindow:subclass()
BattleFormationWindow.name = 'battle formation'
function BattleFormationWindow:init(...)
	BattleFormationWindow.super.init(self, ...)
	self.array = range((countof(game.formations)))
end
function BattleFormationWindow:getArray()
	return self.array
end
function BattleFormationWindow:getIndexName(i)
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
	if self.index < countof(game.formationMPs) then
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
end


local BattleOptionsWindow = ArrayWindow:subclass()
function BattleOptionsWindow:init(...)
	BattleOptionsWindow.super.init(self, ...)
	self.array = range((countof((assert.index(game, self.gameField)))))
end
function BattleOptionsWindow:getArray()
	return self.array
end
function BattleOptionsWindow:getIndexName(i)
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

local EventBattleOptionsWindow = BattleOptionsWindow:subclass()
EventBattleOptionsWindow .name = 'event battle options'
EventBattleOptionsWindow.gameField = 'monsterEventBattles'		-- monsterRandomBattleEntry2_t



local App = require 'imgui.appwithorbit'()

App.hasFocus = true
App.title = 'FF6 Data Visualizer'

function App:initGL(...)
	App.super.initGL(self, ...)

	self.view.ortho = true
	self.view.orthoSize = 16
	self.animSpeed = 15
	self.useBlend = true
	self.showTileMask = 0	--0xffff
	self.showAnimTexs = true


	self.layerDrawObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
in vec2 vertex;
out vec2 tcv;
uniform mat4 mvProjMat;
void main() {
	tcv = vertex;
	gl_Position = mvProjMat * vec4(vertex, 0., 1.);
}
]],
			fragmentCode = [[
uniform usampler2D tex;
uniform sampler2D palTex;
in vec2 tcv;
out vec4 fragColor;
void main() {
	int index = int(texture(tex, tcv, 0).r);
	fragColor = texelFetch(palTex, ivec2(index, 0), 0);
}
]],
			uniforms = {
				tex = 0,
				palTex = 1,
			},
		},
		vertexes = {
			data = {
				0, 0,
				1, 0,
				0, 1,
				1, 1,
			},
			dim = 2,
		},
		geometry = {
			mode = gl.GL_TRIANGLE_STRIP,
		},
	}


	local colorsForBits = table{
		vec3d(1,0,0),
		vec3d(0,1,0),
		vec3d(0,0,1),
		vec3d(0,1,1),
		vec3d(1,0,1),
		vec3d(1,1,0),
		vec3d(1,1,1),
		vec3d(.75, .75, .75),

		vec3d(1, .5, .5),
		vec3d(.5, 1, .5),
		vec3d(0,0,.5),
		vec3d(0,.5,.5),
		vec3d(.5,0,.5),
		vec3d(.5,.5,0),
		vec3d(.5,.5,.5),
		vec3d(.25, .25, .25),

		vec3d(0,.25,0),
		vec3d(.25,0,0),
	}
	local tilePropsPalSize = 32
	assert.ge(tilePropsPalSize, numTilePropsBits)	-- TODO rup2 or something
	local tilePropPalData = ffi.new('uint8_t[?]', tilePropsPalSize * 4)
	ffi.fill(tilePropPalData, ffi.sizeof(tilePropPalData))
	for i=0,tilePropsPalSize-1 do
		local c = colorsForBits[(i % #colorsForBits) + 1]
		tilePropPalData[bit.bor(0, bit.lshift(i, 2))] = 255*c.x
		tilePropPalData[bit.bor(1, bit.lshift(i, 2))] = 255*c.y
		tilePropPalData[bit.bor(2, bit.lshift(i, 2))] = 255*c.z
		tilePropPalData[bit.bor(3, bit.lshift(i, 2))] = 1
	end
	self.tilePropsPalTex = GLTex2D{
		width = tilePropsPalSize,
		height = 1,
		internalFormat = gl.GL_RGBA,
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_NEAREST,
		data = tilePropPalData,
	}:unbind()

	self.flagsDrawObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
in vec2 vertex;
out vec2 tcv;
uniform mat4 mvProjMat;
void main() {
	tcv = vertex;
	gl_Position = mvProjMat * vec4(vertex, 0., 1.);
}
]],
			fragmentCode = [[
uniform usampler2D tex;
uniform sampler2D palTex;
uniform int palIndex;
uniform float alpha;
uniform float t;
in vec2 tcv;
out vec4 fragColor;
void main() {
	
	if (((int(gl_FragCoord.x) - int(gl_FragCoord.y) - int(t)) & 15) < 8) discard;
	//if (mod((gl_FragCoord.x + gl_FragCoord.y) / 10., 1.) < .5) discard;

	int flags = int(texture(tex, tcv, 0).r);
	flags &= 1 << palIndex;
	if (flags == 0) discard;
	fragColor = texelFetch(palTex, ivec2(palIndex, 0), 0);
	fragColor.a = alpha;
}
]],
			uniforms = {
				tex = 0,
				palTex = 1,
				alpha = .3,
			},
		},
		vertexes = {
			data = {
				0, 0,
				1, 0,
				0, 1,
				1, 1,
			},
			dim = 2,
		},
		geometry = {
			mode = gl.GL_TRIANGLE_STRIP,
		},
	}


	self.rectObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
in vec2 vertex;
uniform mat4 mvProjMat;
uniform vec4 bbox;
void main() {
	gl_Position = mvProjMat * vec4(
		bbox.x + bbox.z * vertex.x,
		bbox.y + bbox.w * vertex.y,
		0.,
		1.);
}
]],
			fragmentCode = [[
out vec4 fragColor;
uniform vec4 color;
void main() {
	fragColor = color;
}
]],
		},
		vertexes = {
			data = {
				0, 0,
				1, 0,
				0, 1,
				1, 1,
			},
			dim = 2,
		},
		geometry = {
			mode = gl.GL_TRIANGLE_STRIP,
		},
		uniforms = {
			bbox = {0,0,1,1},
			color = {1,1,1,1},
		},
	}

	self.mapSize = vec2d()


	-- make mapWindow's windows first:
	self.showTiles = true
	self.tileWindow = TileWindow{app=self}
	
	self.showTreasures = true
	self.treasureWindow = TreasureWindow{app=self}
	
	self.showTouchTriggers = true
	self.touchTriggerWindow = TouchTriggerWindow{app=self}

	self.showDoors = true
	self.doorWindow = DoorWindow{app=self}

	self.showBigDoors = true
	self.bigDoorWindow = BigDoorWindow{app=self}

	self.showNPCs = true
	self.npcWindow = NPCWindow{app=self}

	self.showWorldEncounterSectors = true
	self.worldEncounterSectorWindow = WorldEncounterSectorWindow{app=self}

	-- then make mapWindow:
	self.mapWindow = MapWindow{
		app = self,
		index = cmdline[2] and assert(tonumber(cmdline[2])) or 0,
		show = true,
		children = {
			self.tileWindow,
			self.treasureWindow,
			self.touchTriggerWindow,
			self.doorWindow,
			self.bigDoorWindow,
			self.npcWindow,
			self.worldEncounterSectorWindow,
		},
	}

	self.itemWindow = ItemWindow{app=self}

	self.spellWindow = SpellWindow{app=self}

	self.scriptWindow = ScriptWindow{app=self}

	self.monsterWindow = MonsterWindow{app=self}

	self.battleFormationWindow = BattleFormationWindow{app=self}

	self.randomBattleOptionsWindow = RandomBattleOptionsWindow{app=self}
	self.eventBattleOptionsWindow = EventBattleOptionsWindow{app=self}

	-- base-level not dependent on another window:
	self.baseWindows = table{
		self.mapWindow,
		self.itemWindow,
		self.spellWindow,
		self.scriptWindow,
		self.monsterWindow,
		self.battleFormationWindow,
		self.randomBattleOptionsWindow,
		self.eventBattleOptionsWindow,
	}

	self.scriptWindow.show[0] = false	-- who keeps opening this?
end


local function mat4x4mul(m, x, y, z, w)
	x = tonumber(x)
	y = tonumber(y)
	z = tonumber(z) or 0
	w = tonumber(w) or 1
	return
		m[0] * x + m[4] * y + m[ 8] * z + m[12] * w,
		m[1] * x + m[5] * y + m[ 9] * z + m[13] * w,
		m[2] * x + m[6] * y + m[10] * z + m[14] * w,
		m[3] * x + m[7] * y + m[11] * z + m[15] * w
end

require 'vec-ffi.vec4f'
local vec4x4fcol = require 'vec-ffi.create_vec4x4'{
	vectype = 'vec4x4fcol',
	ctype = 'vec4f',
	colMajor = true,
}
local mvInv = vec4x4fcol():setIdent()
local projInv = vec4x4fcol():setIdent()
function App:invTransform(x,y,z)
	x = tonumber(x)
	y = tonumber(y)
	z = tonumber(z) or 0
	x = -1 + 2 * x / tonumber(self.width)
	y = 1 - 2 * y / tonumber(self.height)
	mvInv:inv4x4(self.view.mvMat)
	projInv:inv4x4(self.view.projMat)
	local w = 1
	x,y,z,w = mat4x4mul(projInv.ptr,x,y,z,w)
	x,y,z,w = mat4x4mul(mvInv.ptr,x,y,z,w)
	return x,y,z,w
end

function App:centerView(x, y)
	self.view.pos.x = tonumber(x)+.5
	self.view.pos.y = -tonumber(y)-.5
end


-- called by update and called by save ...
function App:draw(animFrameIndex)
	self.layerDrawObj.uniforms.mvProjMat = self.view.mvProjMat.ptr

	local drawTex = function(tex)
		local blend = tex.image.blend
		if self.useBlend and blend then
			gl.glEnable(gl.GL_BLEND)
			if bit.band(blend, 2) ~= 0 then -- sub
				gl.glBlendEquation(gl.GL_FUNC_REVERSE_SUBTRACT)	-- framebuffer minus sprite
			else
				gl.glBlendEquation(gl.GL_FUNC_ADD)
			end
			local half = bit.band(blend, 1) ~= 0
			gl.glBlendColor(1, 1, 1, half and .5 or 1)
			if half then
				gl.glBlendFunc(gl.GL_CONSTANT_ALPHA, gl.GL_CONSTANT_ALPHA)
			else
				gl.glBlendFunc(gl.GL_ONE, gl.GL_ONE)
			end
		end

		gl.glEnable(gl.GL_ALPHA_TEST)
		gl.glAlphaFunc(gl.GL_GREATER, .5)

		self.layerDrawObj.texs[1] = tex
		self.layerDrawObj.texs[2] = self.mapWindow.mapPalTex
		self.layerDrawObj:draw()

		gl.glDisable(gl.GL_BLEND)
		gl.glDisable(gl.GL_ALPHA_TEST)
	end

	local mapInfo = self.mapWindow:getMapInfo()
	local map = mapInfo and mapInfo.map
	if self.showAnimTexs then
		if map
		and self.layerAnimTexs
		then
			-- also in maps.lua ...
			local zAndLayers = map.layer3Priority == 0
				and zAndLayersWithoutLayer3Priority
				or zAndLayersWithLayer3Priority
			for _,zAndLayer in ipairs(zAndLayers) do
				local z, layer = table.unpack(zAndLayer)
				local k = 'showAnimTex_'..z..'_'..layer
				if self[k] == nil then self[k] = true end
				if self[k]
				and self.layerAnimTexs[z]
				and self.layerAnimTexs[z][layer]
				then
					local animTexs = self.layerAnimTexs[z][layer]
					if animTexs and #animTexs > 0 then
						drawTex(animTexs[animFrameIndex % #animTexs + 1])
					end
				end
			end
		end
	else
		if self.layerTexs then
			for i,tex in ipairs(self.layerTexs) do
				local k = 'drawLayer'..i
				if self[k] == nil or self[k] == true then
					drawTex(tex)
				end
			end
		end
	end
end

function App:update()
	if not self.hasFocus then
		sdl.SDL_Delay(100)
		return
	end

	gl.glClear(gl.GL_COLOR_BUFFER_BIT)

	local t = timer.getTime() - startTime
	self.frameIndex = math.floor(t * self.animSpeed)
	local view = self.view
	self.layerDrawObj.uniforms.mvProjMat = self.view.mvProjMat.ptr

	self.tooltipText = nil

	if self.show16x16mapTiles then
		for k=1,2 do
			if self.map16x16tileTexs then
				local texs = self.map16x16tileTexs[k]
				local tex = texs[(self.frameIndex % #texs) + 1]

				view:setupModelView()
				view.mvMat:applyScale(1, -1)
				view.mvMat:applyTranslate((k-1) * (tex.width/16 + 1), 0, 0)
				view.mvMat:applyScale(tex.width/16, tex.height/16)
				view.mvProjMat:mul4x4(view.projMat, view.mvMat)

				self.layerDrawObj.texs[1] = tex
				self.layerDrawObj.texs[2] = self.mapWindow.mapPalTex
				self.layerDrawObj:draw()
			end
		end
	else


		-- mapSize is in texels
		-- so now coords are in 16x16 tiles
		view.mvMat:applyScale(1, -1)
		view.mvMat:applyScale(self.mapSize.x / 16, self.mapSize.y / 16, 1)
		view.mvProjMat:mul4x4(view.projMat, view.mvMat)

		-- draw layers blended together
		self:draw(bit.band(self.frameIndex, 7))	-- mod by max anim frame gcd

		-- draw overlays of things in the map:

		gl.glEnable(gl.GL_BLEND)
		gl.glBlendEquation(gl.GL_FUNC_ADD)
		gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE)

		if self.layerDrawObj
		and self.tilePropsTex
		then
			local totalBits = 0
			for i=0,numTilePropsBits-1 do
				if 0 ~= bit.band(bit.lshift(1, i), self.showTileMask) then
					totalBits = totalBits + 1
				end
			end
			for i=0,numTilePropsBits-1 do
				if 0 ~= bit.band(bit.lshift(1, i), self.showTileMask) then
					self.flagsDrawObj.texs[1] = self.tilePropsTex
					self.flagsDrawObj.texs[2] = self.tilePropsPalTex
					self.flagsDrawObj.uniforms.t = 20 * t - i
					self.flagsDrawObj.uniforms.mvProjMat = view.mvProjMat.ptr
					self.flagsDrawObj.uniforms.palIndex = i
					self.flagsDrawObj.uniforms.alpha = 1 / math.max(1, totalBits)
					self.flagsDrawObj:draw()
				end
			end
		end

		gl.glBlendFunc(gl.GL_ONE, gl.GL_ONE)

		local mapInfo = self.mapWindow:getMapInfo()
		if mapInfo then
			view:setupModelView()
			view.mvMat:applyScale(1, -1)
			view.mvProjMat:mul4x4(view.projMat, view.mvMat)

			local rectObj = self.rectObj
			local uniforms = rectObj.uniforms
			uniforms.mvProjMat = view.mvProjMat.ptr

			local function showHL()
				local x,y,w,h = table.unpack(uniforms.bbox)
				local eps = .1
				settable(uniforms.color, 1,1,1,1)
				settable(uniforms.bbox, x-eps, y-eps, 2*eps, h+2*eps)
				rectObj:draw()
				settable(uniforms.bbox, x-eps, y-eps, w+2*eps, 2*eps)
				rectObj:draw()
				settable(uniforms.bbox, x+w-eps, y-eps, 2*eps, h+2*eps)
				rectObj:draw()
				settable(uniforms.bbox, x-eps, y+h-eps, w+2*eps, 2*eps)
				rectObj:draw()
			end

			local mx, my, mz, mw = self:invTransform(self.mouse.pos.x * self.width, self.mouse.pos.y * self.height)
	mx = mx + self.view.pos.x
	my = my + self.view.pos.y	-- why isn't htis in the matrix and therefore in invTransform ?
	my = -my	-- oonce again, why ???? it's like i'm uisng the wrong mv matrix
self.tooltipText = math.floor(mx)..', '..math.floor(my)

			local leftPress = self.mouse.leftPress

			if (self.mapWindow.index == 0 or self.mapWindow.index == 1)
			and self.showWorldEncounterSectors
			then
				for sectorIndex=0,0x3f do
					local x = bit.lshift(bit.band(sectorIndex, 7), 5)
					local y = bit.lshift(bit.band(bit.rshift(sectorIndex, 3), 7), 5)
					local w, h = 32, 32

					local i = bit.bor(sectorIndex, bit.lshift(self.mapWindow.index, 6))
					if leftPress
					and x <= mx and mx <= x+w
					and y <= my and my <= y+h
					then
						self.worldEncounterSectorWindow:setIndex(i)
						self.worldEncounterSectorWindow.show[0] = true
					end
					settable(uniforms.color, .7, .7, .7, 1)
					settable(uniforms.bbox, x, y, w, h)
					--rectObj:draw()
					if i == self.worldEncounterSectorWindow.index then
						showHL()
					end
				end
			end

			if self.showTiles then
				local mapWidth, mapHeight = self.tileWindow:getMapSize()
				if mapWidth and mapHeight then
					local x = math.floor(mx)
					local y = math.floor(my)
					if x >= 0 and my >= 0
					and x < mapWidth
					and y < mapHeight
					then
						local i = x + mapWidth * y
						settable(uniforms.bbox, x, y, 1, 1)
						settable(uniforms.color, 1,1,1,1)
						showHL()
						if leftPress then
							self.tileWindow:setIndex(i)
							self.tileWindow.show[0] = true
						end
					end
				end
			end
			if self.showTreasures then
				for i,t in ipairs(mapInfo.treasures) do
					local x, y = tonumber(t.pos.x), tonumber(t.pos.y)
					if leftPress
					and x <= mx and mx <= x+1
					and y <= my and my <= y+1
					then
						self.treasureWindow:setIndex(i-1)
						self.treasureWindow.show[0] = true
					end
					settable(uniforms.bbox, x, y, 1, 1)
					settable(uniforms.color, 0,0,1,1)
					rectObj:draw()
					if i-1 == self.treasureWindow.index then
						showHL()
					end
				end
			end
			if self.showTouchTriggers then
				for i,e in ipairs(mapInfo.touchTriggers) do
					local x, y = tonumber(e.pos.x), tonumber(e.pos.y)
					if leftPress
					and x <= mx and mx <= x+1
					and y <= my and my <= y+1
					then
						self.touchTriggerWindow:setIndex(i-1)
						self.touchTriggerWindow.show[0] = true
					end
					settable(uniforms.bbox, x, y, 1, 1)
					settable(uniforms.color, 0,0,1,1)
					rectObj:draw()
					if i-1 == self.touchTriggerWindow.index then
						showHL()
					end
				end
			end
			if self.showDoors then
				for i,e in ipairs(mapInfo.doors) do
					local x, y = tonumber(e.pos.x), tonumber(e.pos.y)
					if leftPress
					and x <= mx and mx <= x+1
					and y <= my and my <= y+1
					then
						self.doorWindow:setIndex(i-1)
						self.doorWindow.show[0] = true
					end
					settable(uniforms.bbox, x, y, 1, 1)
					settable(uniforms.color, 1,0,0,1)
					rectObj:draw()
					if i-1 == self.doorWindow.index then
						showHL()
					end
				end
			end
			if self.showBigDoors then
				for i,e in ipairs(mapInfo.bigDoors) do
					local x, y = tonumber(e.pos.x), tonumber(e.pos.y)
					local w, h
					if e.vertical == 0 then
						w, h = e.length+1, 1
					else
						w, h = 1, e.length+1
					end
					if leftPress
					and x <= mx and mx <= x+w
					and y <= my and my <= y+h
					then
						self.bigDoorWindow:setIndex(i-1)
						self.bigDoorWindow.show[0] = true
					end
					settable(uniforms.bbox, x, y, w, h)
					settable(uniforms.color, 1,0,0,1)
					rectObj:draw()
					if i-1 == self.bigDoorWindow.index then
						showHL()
					end
				end
			end
			if self.showNPCs then
				for i,n in ipairs(mapInfo.npcs) do
					local x, y = tonumber(n.x), tonumber(n.y)
					if leftPress
					and x <= mx and mx <= x+1
					and y <= my and my <= y+1
					then
						self.npcWindow:setIndex(i-1)
						self.npcWindow.show[0] = true
					end
					settable(uniforms.bbox, x, y, 1, 1)
					settable(uniforms.color, 0,1,0,1)
					rectObj:draw()
					if i-1 == self.npcWindow.index then
						showHL()
					end
				end
			end
		end

		gl.glDisable(gl.GL_BLEND)
	end


	-- draw gui
	App.super.update(self)
end

function App:updateGUI()
	local mapInfo = self.mapWindow:getMapInfo()
	local map = mapInfo and mapInfo.map

	if ig.igBeginMainMenuBar() then

		if ig.igBeginMenu'view' then
			for _,w in ipairs(self.baseWindows) do
				if ig.igButton(w.name) then
					w.show[0] = true
				end
			end

			ig.igEndMenu()
		end

		if ig.igBeginMenu'map' then

			if self.layerAnimTexs then
				local doSaveLayerPNGs = ig.igButton'save layer pngs'
				local doSaveGIF = ig.igButton'save animated gif'
				if doSaveLayerPNGs
				or doSaveGIF
				then
					local animScreenshotPath = path'vis-map-animframes'
					animScreenshotPath:mkdir(true)

					local numFrames = 1
					local zAndLayers = map.layer3Priority == 0
						and zAndLayersWithoutLayer3Priority
						or zAndLayersWithLayer3Priority
					for _,zAndLayer in ipairs(zAndLayers) do
						local z, layer = table.unpack(zAndLayer)
						if self.layerAnimTexs[z]
						and self.layerAnimTexs[z][layer]
						then
							numFrames = math.max(numFrames, #self.layerAnimTexs[z][layer])
							if doSaveLayerPNGs then
								for frameIndex,frameTex in ipairs(self.layerAnimTexs[z][layer]) do
									frameTex.image:save((animScreenshotPath/(
										'map'..self.mapWindow.index
										..'_z='..z
										..'_layer='..layer
										..'_frame='..frameIndex
										..'.png'
									)).path)
								end
							end
						end
					end

					-- save a composite image while we're here
					local GLPingPong = require 'gl.pingpong'
					local pp = GLPingPong{
						numBuffers = 1,
						width = tonumber(self.mapSize.x),
						height = tonumber(self.mapSize.y),
						internalFormat = gl.GL_RGBA,
						minFilter = gl.GL_NEAREST,
						magFilter = gl.GL_NEAREST,
					}
					local fboTex = pp:cur()


					local pushMvMat = self.view.mvMat:clone()
					local pushProjMat = self.view.projMat:clone()
					local view = self.view
					view.mvMat
						:setIdent()
						:applyScale(1, -1)
						:applyScale(fboTex.width / 16, fboTex.height / 16, 1)
					view.projMat:setOrtho(0, fboTex.width / 16, -fboTex.height / 16, 0, -1000, 1000)
					view.mvProjMat:mul4x4(view.projMat, view.mvMat)

					gl.glViewport(0, 0, fboTex.width, fboTex.height)

					local compositeImgs = table()
					for frameIndex=1,numFrames do
						local fbo = pp.fbo
						fbo:bind()
							:setColorAttachmentTex2D(fboTex.id, 0)
							:drawBuffers(gl.GL_COLOR_ATTACHMENT0)
						assert(fbo:check())

						gl.glClearColor(0,0,0,1)
						gl.glClear(gl.GL_COLOR_BUFFER_BIT)

						self:draw(frameIndex-1)

						-- readpixels while we're here ...
						local image = Image(fboTex.width, fboTex.height, 4, 'uint8_t')	-- TODO tex :getChannels() :getCType()
						gl.glReadBuffer(gl.GL_COLOR_ATTACHMENT0)
						gl.glReadPixels(
							0, 0, fboTex.width, fboTex.height,
							gl.GL_RGBA, --fboTex.format,
							gl.GL_UNSIGNED_BYTE, --fboTex.type,
							image.buffer
						)
						gl.glReadBuffer(gl.GL_BACK)

						image = image:flip()
						compositeImgs:insert(image)
						if doSaveLayerPNGs then
							image:save((animScreenshotPath/(
								'map'..self.mapWindow.index
								..'_composite'
								..'_frame='..frameIndex
								..'.png'
							)).path)
						end

						fbo:unbind()
					end

					-- save the whole as an animation
					if doSaveGIF then
						compositeImgs[1]:save(
							(animScreenshotPath/('map'..self.mapWindow.index..'_animated.gif')).path,
							compositeImgs:unpack(2)
						)
					end

					gl.glViewport(0, 0, self.width, self.height)

					self.view.mvMat:copy(pushMvMat)
					self.view.projMat:copy(pushProjMat)
					self.view.mvProjMat:mul4x4(self.view.projMat, self.view.mvMat)
				end
			end

			ig.luatableInputFloat('animSpeed', self, 'animSpeed')

			ig.igEndMenu()
		end

		if ig.igBeginMenu'view' then
			if ig.igButton'reset view' then
				self.view.ortho = true
				self.view.orthoSize = 256
				self.view.pos:set(0,0,10)
				self.view.orbit:set(0,0,0)
				self.view.angle:set(0,0,0,1)
			end

			ig.igEndMenu()
		end

		ig.igEndMainMenuBar()
	end

	local function updateRecursive(chs)
		for _,ch in ipairs(chs) do
			ch:update()
			if ch.children
			and #ch.children > 0
			then
				updateRecursive(ch.children)
			end
		end
	end
	updateRecursive(self.baseWindows)

	if self.tooltipText then
		ig.igBeginTooltip()
		ig.igText(self.tooltipText)
		ig.igEndTooltip()
	end
end

function App:event(e)
	if e[0].type == sdl.SDL_EVENT_WINDOW_FOCUS_GAINED then
		self.hasFocus = true
		return
	elseif e[0].type == sdl.SDL_EVENT_WINDOW_FOCUS_LOST then
		self.hasFocus = false
		return
	end

	App.super.event(self, e)
	--local canHandleMouse = not ig.igGetIO()[0].WantCaptureMouse
	local canHandleKeyboard = not ig.igGetIO()[0].WantCaptureKeyboard

	if canHandleKeyboard then
		if e.type == sdl.SDL_EVENT_KEY_UP then
			if e.key.key == sdl.SDLK_LEFT then
				self.mapWindow:setIndex(math.clamp(math.floor(self.mapWindow.index - 1), 0, countof(game.maps)-1))
			elseif e.key.key == sdl.SDLK_RIGHT then
				self.mapWindow:setIndex(math.clamp(math.floor(self.mapWindow.index + 1), 0, countof(game.maps)-1))
			end
		end
	end
end

return App():run()
