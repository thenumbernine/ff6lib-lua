local ffi = require 'ffi'
local assert = require 'ext.assert'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local vis_util = require 'ff6.vis.util'
local mapTilePropsFlagForName = vis_util.mapTilePropsFlagForName
local mapTilePropsNames = vis_util.mapTilePropsNames
local worldTilePropsNames = vis_util.worldTilePropsNames
local numTilePropsBits = vis_util.numTilePropsBits


local uint8_t_p = ffi.typeof'uint8_t*'
local uint16_t_p = ffi.typeof'uint16_t*'


-- convert from 4-bit xy ff6 16x16 tilemap index to numo9 5-bit xy 8x8 tilemap index
local function tile44to55(index)
	return bit.bor(
		bit.lshift(bit.band(0x0f, index), 1),
		bit.lshift(bit.band(0xf0, index), 2)
	)
end


local ArrayWindow = require 'ff6.vis.arraywindow'

local TileWindow = ArrayWindow:subclass()

TileWindow.name = 'tile'

function TileWindow:getMapSize()
	local mapInfo = self.app.game.getMap(self.app.mapWindow.index)
	if not mapInfo then return end
	local layerSizes = mapInfo.layerSizes
	if not layerSizes then return end
	return layerSizes[1].x, layerSizes[1].y
end

function TileWindow:getMapVolume()
	local w, h = self:getMapSize()
	if w and h then return w * h end
end

function TileWindow:getCount()
	return self:getMapVolume()
end

function TileWindow:showIndexUI()
	local app = self.app
	local game = app.game

	local count = self:getCount()
	if self.index < 0 or self.index >= count then return end

	local w, h = self:getMapSize()
	if not w then return end

	local x = self.index % w
	local y = (self.index - x) / w
	ig.igText(x..', '..y..' of '..w..', '..h)

	-- if getting x and y fail then mapInfo can't be accessed, so by here mapInfo is guaranteed
	local mapIndex = app.mapWindow.index
	local mapInfo = game.getMap(mapIndex)
	local layerPos = mapInfo.layerPos
	local layerSizes = mapInfo.layerSizes
	local tilePropsData = mapInfo.tilePropsData
	local layouts = mapInfo.layouts
	local mapWidth, mapHeight = layerSizes[1].x, layerSizes[1].y

	local layout1Data = layouts[1] and layouts[1].data
	if layout1Data then
		local layoutptr = ffi.cast(uint8_t_p, layout1Data)
		if tilePropsData then
			local tilePropsPtr = ffi.cast(uint16_t_p, tilePropsData)

			local tilePropsIndex = layoutptr[self.index]
			local flags = tilePropsPtr[tilePropsIndex]
			--ig.igText(('tile props = 0x%04x'):format(flags))


			-- based on MapTileProps:
			local tilePropsNames = mapIndex < 3
				-- WorldTileProps:
				and worldTilePropsNames
				-- MapTileProps:
				or mapTilePropsNames
			assert.len(tilePropsNames, numTilePropsBits)
			-- only show real flags, not my extra custom ones for the shader
			for i=0,15 do
				local name = tilePropsNames[i+1]
				local mask = bit.lshift(1, i)
				self.__tmp = 0 ~= bit.band(flags, mask)
				if bit.band(i, 7) ~= 0 then ig.igSameLine() end
				if ig.luatableTooltipCheckbox('tileProp '..i..': '..name, self, '__tmp') then
					flags = bit.bxor(mask, flags)
					tilePropsPtr[tilePropsIndex] = flags

					if mapIndex < 2
					-- TODO and if we change a background bit ...
					then
						app.mapWindow:refreshBattleBgTex()
					end
				end
			end
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

			local layoutptr = ffi.cast(uint8_t_p, layoutData)
			local srcX = (x - posx) % layerSize.x
			local srcY = (y - posy) % layerSize.y
			local tile16x16 = layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]
			ig.igText('layer '..layer)

			local tileSheetWin = app.layerTileSheetWindows[layer]
			if tileSheetWin then
				ig.igSameLine()
				tileSheetWin:popupButton(tile16x16)
			end
			--ig.igText(('layer %d tile: 0x%02x'):format(layer, tile16x16))
		end
	end

	ig.igSeparator()

	local function getTile16x16(x, y, layer)	-- layer is 1-based
		local layout = layouts[layer]
		local layoutData = layout and layout.data
		if not layoutData then return end

		local posx, posy = 0, 0
		if layerPos[layer]
		-- if we have a position for the layer, but we're using parallax, then the position is going to be relative to the view
		--and map.parallax == 0
		then
			posx, posy = layerPos[layer]:unpack()
		end

		local layoutptr = ffi.cast(uint8_t_p, layoutData)
		local layerSize = layerSizes[layer]
		local srcX = (x - posx) % layerSize.x
		local srcY = (y - posy) % layerSize.y
		return layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]
	end

	local function getTileProps(x,y)
		if not layout1Data then return end
		if not tilePropsData then return end
		local layoutptr = ffi.cast(uint8_t_p, layout1Data)
		local tilePropsPtr = ffi.cast(uint16_t_p, tilePropsData)
		return tilePropsPtr[layoutptr[x + mapWidth * y]]
	end

	local function getFloorTile(x,y)
		if x < 0 or y < 0 or x >= mapWidth or y >= mapHeight then return end
		-- which layer do I use?
		--- I guess I use layer1
		--   except when it's transparent
		--   (same as when topSpritePriority is set?)
		--  and then I use layer2?
		-- [[
		local props = getTileProps(x, y)
		if props ~= 7
		and props ~= 0xfff7
		then
			local topSpritePriority = 0 ~= bit.band(mapTilePropsFlagForName.topSpritePriority, props)
			--local bottomSpritePriority = 0 ~= bit.band(mapTilePropsFlagForName.bottomSpritePriority, props)
			if topSpritePriority then
				return getTile16x16(x, y, 2)
			end
		end
		--]]
		local tile = getTile16x16(x, y, 1)
		--if tile == 0 then tile = getTile16x16(x, y, 2) end
		return tile
	end

	local function getWallOrCeilingTile(x,y)
		if x < 0 or y < 0 or x >= mapWidth or y >= mapHeight then return end
		-- always layer 1?
		-- layer 2 will sometimes have overhead ceiling stuff
		return getTile16x16(x, y, 1)
	end
end

function TileWindow:setIndex(index, ...)
	local app = self.app
	local game = app.game

	TileWindow.super.setIndex(self, index, ...)

	-- also set layer-tile-sheet-window's index...
	if not app.mapWindow then return end
	local mapIndex = app.mapWindow.index
	local mapInfo = game.getMap(mapIndex)
	if not mapInfo then return end
	local w, h = self:getMapSize()
	local x = self.index % w
	local y = (self.index - x) / w
	local layerPos = mapInfo.layerPos
	local layerSizes = mapInfo.layerSizes
	local layouts = mapInfo.layouts
	for layer=1,3 do
		local tileSheetWin = app.layerTileSheetWindows[layer]
		if tileSheetWin then
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
				local layoutptr = ffi.cast(uint8_t_p, layoutData)
				local srcX = (x - posx) % layerSize.x
				local srcY = (y - posy) % layerSize.y
				local tile16x16 = layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]
				tileSheetWin:setIndex(tile16x16)
			end
		end
	end

	if app.mapWindow.index < 2 then
		app.mapWindow:refreshBattleBgTex()
	end
end

return TileWindow
