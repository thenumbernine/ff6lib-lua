local ffi = require 'ffi'
local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local number = require 'ext.number'
local vec2d = require 'vec-ffi.vec2d'
local Image = require 'image'
local gl = require 'gl'
local GLTex2D = require 'gl.tex2d'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local vis_util = require 'ff6.vis.util'
local zAndLayersWithoutLayer3Priority = vis_util.zAndLayersWithoutLayer3Priority
local zAndLayersWithLayer3Priority = vis_util.zAndLayersWithLayer3Priority
local numTilePropsBits = vis_util.numTilePropsBits
local worldTilePropsNames = vis_util.worldTilePropsNames 
local mapTilePropsFlagForName = vis_util.mapTilePropsFlagForName
local mapTilePropsNames = vis_util.mapTilePropsNames 


local MapWindow = ArrayWindow:subclass()

MapWindow.name = 'map'

function MapWindow:init(...)
	MapWindow.super.init(self, ...)

	local game = self.app.game

	-- 'game.getMap' doensn't work so well since it has cache stuff like the texture layers ...
	self.array = range(game.countof(game.maps))

	self.mapIndexStack = table()
end

function MapWindow:getMapInfo()
	return self.app.game.getMap(self.index)
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

	-- [[ here or in TileWindow?
	if app.map16x16tileTexs then
		local first = true
		for i=1,2 do
			if app.map16x16tileTexs[i] then
				if not first then ig.igSameLine() end
				first = false
				local win = app.layerTileSheetWindows[i]
				win:popupButton()
			end
		end
	end
	--]]

	local map = mapInfo.map
	if map then
		for name in map[0]:fielditer() do
			ig.igText(' '..name..' = '..tostring(map[0][name]))
		end
	end
end

function MapWindow:setIndex(newIndex, pushStack)
	local app = self.app
	local game = app.game

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

	-- refresh the monster random battle options to reflect the map's
	if self.index < 2 then
		local sectorIndex = app.worldEncounterSectorWindow.index
		print('sectorIndex', app.worldEncounterSectorWindow.index)
		--[[ segfaulting
		if sectorIndex >= 0 and sectorIndex < #app.worldEncounterSectorWindow:getArray() then
			local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + sectorIndex
			local battleIndex = randomBattlesPerTerrain.grass
			app.randomBattleOptionsWindow:popupButton(battleIndex)
		end
		--]]
	else
		app.randomBattleOptionsWindow:setIndex(mapInfo.monsterRandomBattleOptionIndex)
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
		..'0x'..number.hex(ffi.cast('uint8_t*', map) - game.rom)
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
					flags = mapTilePropsFlagForName.throughTile
				elseif flags == 0xfff7 then
					flags = mapTilePropsFlagForName.impassible
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

		local worldInfo = game.worldInfos[mapIndex+1]	-- worldInfos is 1-based

		local maxLayers = worldInfo and 1 or 2
		for layer=1,maxLayers do
			app.map16x16tileTexs[layer] = table()
			local tilesetIndex = tonumber(map['tileset'..layer])
			-- TODO how to specify animation # as well?
			-- might have to just write these out based on mapindex ... and just skip unique ones?

			local maxFrames = worldInfo and 1 or 4

			for frameIndex=0,maxFrames-1 do
				do
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
				local tileImg = Image(16, 16, 1, 'uint8_t'):clear()
				local img = Image(16 * size.x, 16 * size.y, 4, 'uint8_t'):clear()
				-- what is its format?
				local tile16x16 = 0
				for j=0,size.y-1 do
					local y = bit.lshift(j, 4)
					for i=0,size.x-1 do
						local x = bit.lshift(i, 4)
						
						if worldInfo then
							game.layer1worlddrawtile16x16(
								--layerImg, x, y,
								tileImg, 0, 0,
								tile16x16,
								worldInfo.tilesetdata,	--game.tilesetDatas[layer],
											--game.mapTilesetCache[tilesetIndex].data,
								worldInfo.gfxdata	--gfxDatas[layer]
							)
						else
							game.layer1and2drawtile16x16(
								--img, x, y,
								tileImg, 0, 0,
								tile16x16,
								--map.tilesetDatas[layer]
								game.mapTilesetCache[tilesetIndex].data,
								nil,
								gfxDatas,
								nil -- mapInfo.gfxLayer3 and mapInfo.gfxLayer3.data
							)
						end

						-- bake palette into rgba (so imgui can use it)
						for srcy=0,15 do
							for srcx=0,15 do
								for ch=0,img.channels-1 do
									local palIndex = tileImg.buffer[bit.bor(srcx, bit.lshift(srcy, 4))]
									local dstp = img.buffer + img.channels * (x + srcx + bit.lshift(size.x, 4) * (y + srcy))
									dstp[0], dstp[1], dstp[2], dstp[3] = table.unpack(palette[palIndex+1])
								end
							end
						end

						tile16x16 = tile16x16 + 1
					end
				end
				app.map16x16tileTexs[layer]:insert(GLTex2D{
					--image = img,
					data = img.buffer,
					width = img.width,
					height = img.height,
					--internalFormat = gl.GL_R8UI,
					internalFormat = gl.GL_RGBA,

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

return MapWindow 
