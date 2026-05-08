local ffi = require 'ffi'
local table = require 'ext.table'
local range = require 'ext.range'
local path = require 'ext.path'
local vec2i = require 'vec-ffi.vec2i'
local vec3i = require 'vec-ffi.vec3i'
local box2i = require 'vec-ffi.box2i'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'
local mapTilePropsFlagForName = require 'ff6.vis.util'.mapTilePropsFlagForName


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

function TileWindow:getArray()
	local vol = self:getMapVolume()
	return vol and range(vol)
end

function TileWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	local t = ar[1+self.index]
	if not t then return end

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

	local layout1Data = layouts[1] and layouts[1].data
	if layout1Data then
		local layoutptr = ffi.cast('uint8_t*', layout1Data)
		if tilePropsData then
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
			ig.igText('layer '..layer)

			local tileSheetWin = app.layerTileSheetWindows[layer]
			if tileSheetWin then
				ig.igSameLine()
				tileSheetWin:popupButton(tile16x16)
			end
			--ig.igText(('layer %d tile: 0x%02x'):format(layer, tile16x16))
		end
	end

	if mapInfo then
		ig.igSeparator()
		local mapWidth, mapHeight = app.tileWindow:getMapSize()

		if app.tilePropsTex
		and app.tilePropsTex.data
		then
			local data = app.tilePropsTex.data
			if ig.igButton'Flood Fill Tile...' then
				local x = app.tileWindow.index % mapWidth
				local y = (app.tileWindow.index - x) / mapWidth

				local found
				for _,ff in ipairs(app.floodFillTilesPerMap[mapIndex] or {}) do
					if ff.filled[x + mapWidth * y] then
						print'already flood-filled this region!'
						found = true
					end
				end
				if not found then

					-- now flood-fill against bit #17 (the fake one for the special-value of 'impassible')
					local dirs = {
						{1,0},
						{0,1},
						{-1,0},
						{0,-1},
					}

					local layerPos = mapInfo.layerPos
					local layerSizes = mapInfo.layerSizes
					local layouts = mapInfo.layouts

					local filled = {}
					local bbox = box2i()
					bbox.min:set(0x10000, 0x10000)	-- some max bound
					bbox.max:set(-1, -1)				-- some min bound
					local function writeFilled(x,y)
						bbox:stretch(vec2i(x,y))
						local i = x + mapWidth * y
						-- read 16x16's here ...
						local value = 0
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

								value = bit.bor(value, bit.lshift(
									tile16x16,
									bit.lshift(layer-1, 3)	-- shift our 16x16 byte left 8 bits per layer...
								))
							end
						end

						filled[i] = value
					end
					local function canTraverse(x,y)
						return 0 == bit.band(mapTilePropsFlagForName.impassible, data[x + mapWidth * y])
					end
					if canTraverse(x,y) then
						writeFilled(x, y)
						local fillstack = table()
						fillstack:insert{x, y}
						while #fillstack > 0 do
							local x0, y0 = table.unpack(fillstack:remove())
							for _,dir in ipairs(dirs) do
								local x1, y1 = x0 + dir[1], y0 + dir[2]
								if x1 >= 0 and x1 < mapWidth
								and y1 >= 0 and y1 < mapHeight
								then
									local i = x1 + mapWidth * y1
									if not filled[i]
									and canTraverse(x1, y1)
									then
										writeFilled(x1, y1)
										fillstack:insert{x1, y1}
									end
								end
							end
						end
					end

					local hist = {}
					for index, tileValue in pairs(filled) do
						hist[tileValue] = (hist[tileValue] or 0) + 1
					end
					local tileValues = table.keys(hist):sort(function(a,b)
						return hist[a] > hist[b]
					end)
					-- hmm TODO
					-- histogram on layer1 tile or layer2 or on both?
					local floorTile = bit.band(0xff, tileValues[1])
					app.floodFillTilesPerMap[mapIndex] = app.floodFillTilesPerMap[mapIndex] or table()
					app.floodFillTilesPerMap[mapIndex]:insert{
						filled = filled,	-- key = tile index, value = int with each byte a layer's tile16x16 index
						hist = hist,		-- key = int of tile16x16 layers (value of filled), value = occurrence count
						tileValues = tileValues,	-- int of tile16x16 layers, sorted by occurrence
						bbox = bbox,
						floorTile = floorTile,
						wallTile = 0,
						ceilingTile = 0,
					}
				end
			end
		end

		local floodFillTilesForThisMap = app.floodFillTilesPerMap[mapIndex]
		if floodFillTilesForThisMap then
			for ffIndex,floodFillTiles in ipairs(floodFillTilesForThisMap) do
				ig.igSeparator()
				if ig.igButton'Clear Fill Tiles' then
					floodFillTilesForThisMap:remove(ffIndex)
					return	-- iterated table is invalidated
				end

				ig.igText'flood fill bounds:'
				local bbox = floodFillTiles.bbox
				ig.luatableInputInt('flood fill min x', bbox.min, 'x')
				ig.luatableInputInt('flood fill min y', bbox.min, 'y')
				ig.luatableInputInt('flood fill max x', bbox.max, 'x')
				ig.luatableInputInt('flood fill max y', bbox.max, 'y')

				-- AsText so it can handle all lua number parsing, including 0x's
				ig.luatableInputFloatAsText('floor tile', floodFillTiles, 'floorTile')
				ig.luatableInputFloatAsText('wall tile', floodFillTiles, 'wallTile')
				ig.luatableInputFloatAsText('ceiling tile', floodFillTiles, 'ceilingTile')

				if ig.igButton'export voxelmap' then
					-- 1) make our voxelmap
					local vector = require 'stl.vector-lua'
					local size = vec3i(bbox.max.x - bbox.min.x, bbox.max.y - bbox.min.y, 8)

					local voxelmap
					do
						local v = vector(Voxel, 3 + size:volume())
						ffi.cast('vec3i*', v.v)[0] = size
						for i=3,3+size:volume()-1 do
							v.v[i].intval = 0xffffffff
						end
						voxelmap = BlobVoxelMap(v:dataToStr())
					end
					-- 2) copy ground tiles across
					for y=0,tonumber(size.y)-1 do
						for x=0,tonumber(size.x)-1 do
							local vox = voxelmap:getVoxelBlobPtr(x, y, 0)
							vox.intval = 0	-- reset from clear
							vox.spriteIndex = tile44to55(floodFillTiles.floorTile)
							vox.mesh3DIndex = 0	-- hmm todo, floor voxel mesh index
							--vox.orientation = 32	-- flip x in voxel orientation?

							-- if this is a non-traversible tile then grow the walls
							local i = x + bbox.min.x + mapWidth * ((size.y - 1 - y) + bbox.min.y)
							if not floodFillTiles.filled[i] then
								for z=1,size.z-1 do
									local vox = voxelmap:getVoxelBlobPtr(x, y, z)
									vox.intval = 0	-- reset from clear
									-- TODO if it's a north wall then copy source tiles going y+
									-- and if it's any other wall then copy from the north tile wall profile
									vox.spriteIndex = z == size.z-1
										and tile44to55(floodFillTiles.ceilingTile)
										or tile44to55(floodFillTiles.wallTile)	-- TODO make this the most-prominent-wall-tile
									-- idk how to find that out ... trace from top of traversible filled region to the ceiling tile and count up most-prevalent
									vox.mesh3DIndex = 0	-- hmm todo, wall voxel mesh index
									--vox.orientation = 32	-- flip x in voxel orientation?
								end
							else

								--[[ TODO read the real tile instad of the most prominent
								vox.spriteIndex = tile44to55(


								)
								--]]
							end
						end
					end
					path('exported-voxelmap-map'..mapIndex..'.vox'):write(voxelmap:toBinStr())
				end

				ig.igText('flood fill histogram:')
				for _,tileValue in ipairs(floodFillTiles.tileValues) do
					ig.igText('\t'..('0x%06x'):format(tileValue)..' = '..floodFillTiles.hist[tileValue])
				end
			end
		end
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
				local layoutptr = ffi.cast('uint8_t*', layoutData)
				local srcX = (x - posx) % layerSize.x
				local srcY = (y - posy) % layerSize.y
				local tile16x16 = layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]
				tileSheetWin:setIndex(tile16x16)
			end
		end
	end
end

return TileWindow
