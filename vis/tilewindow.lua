local ffi = require 'ffi'
local table = require 'ext.table'
local string = require 'ext.string'
local range = require 'ext.range'
local path = require 'ext.path'
local fromlua = require 'ext.fromlua'
local vec2i = require 'vec-ffi.vec2i'
local vec3i = require 'vec-ffi.vec3i'
local box2i = require 'vec-ffi.box2i'
local sdl = require 'sdl'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'
local mapTilePropsFlagForName = require 'ff6.vis.util'.mapTilePropsFlagForName


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

function TileWindow:getArray()
	local vol = self:getMapVolume()
	return vol and range(vol)
end


-- goes with the floodfill stuff
-- maybe that belongs in its own window...
_G.sdlSaveFileDialog_chooseOutputFilenameFunction = function(userdata, filelist, filter)
print"does print kill jit, and why doesn't jit.off work?"
	xpcall(function()
		require 'sdl.assert'.nonnull(filelist)	-- error
		if filelist[0] == nil then return end	-- no file picked
		ffInfo.destFilename = ffi.string(filelist[0])
	end, function(err)
		print(err..'\n'..debug.traceback())
	end)
end
jit.off(_G.sdlSaveFileDialog_chooseOutputFilenameFunction)	-- not working?
_G.sdlSaveFileDialog_chooseOutputFilenameClosure = ffi.cast('SDL_DialogFileCallback', _G.sdlSaveFileDialog_chooseOutputFilenameFunction)


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
	local mapWidth, mapHeight = layerSizes[1].x, layerSizes[1].y

	local layout1Data = layouts[1] and layouts[1].data
	if layout1Data then
		local layoutptr = ffi.cast(uint8_t_p, layout1Data)
		if tilePropsData then
			local tilePropsPtr = ffi.cast(uint16_t_p, tilePropsData)
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

	if app.tilePropsTex
	and app.tilePropsTex.data
	then
		local data = app.tilePropsTex.data
		if ig.igButton'Flood Fill Tile...' then
			local x = self.index % mapWidth
			local y = (self.index - x) / mapWidth

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
					{1, 0},
					{0, 1},
					{-1, 0},
					{0, -1},
				}

				local filled = {}
				local bbox = box2i()
				bbox.min:set(32767, 32767)	-- some max bound
				bbox.max:set(-32768, -32768)				-- some min bound
				local function writeFilled(x,y)
					bbox:stretch(vec2i(x,y))
					local i = x + mapWidth * y
					-- read 16x16's here ...
					local value = 0
					for layer=1,3 do
						local tile16x16 = getTile16x16(x, y, layer) or 0
						value = bit.bor(value, bit.lshift(
							tile16x16,
							bit.lshift(layer-1, 3)	-- shift our 16x16 byte left 8 bits per layer...
						))
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
					tileIsNorthSlope = '',
					tileRemapping = '',	-- lua key->value code for a table for remapping 32x32 of 8x8 (numo9-index) tile-indexes
					destFilename = 'exported-voxelmap-map'..mapIndex..'-'..(#app.floodFillTilesPerMap[mapIndex]+1)..'.vox',
					maxAlt = 8,
				}
			end
		end
	end

	local floodFillTilesForThisMap = app.floodFillTilesPerMap[mapIndex]
	if floodFillTilesForThisMap then
		for ffIndex,ffInfo in ipairs(floodFillTilesForThisMap) do
			ig.igSeparator()
			if ig.igButton'Remove' then
				floodFillTilesForThisMap:remove(ffIndex)
				return	-- iterated table is invalidated
			end

			ig.luatableTooltipInputText('output filename', ffInfo, 'destFilename')
			ig.igSameLine()
			if ig.igButton'...' then
				-- TODO do a SDL open file dialog here
				-- and save the resutl as our output location
				sdl.SDL_ShowSaveFileDialog(
					_G.sdlSaveFileDialog_chooseOutputFilenameClosure,
					nil,				-- userdata
					self.app.window,	-- window
					nil,				-- filters
					0,					-- nfilters
					path(ffInfo.destFilename):getdir():exists()
						and path(ffInfo.destFilename):getdir().path
						or path:cwd().path		-- default_location
				)
			end

			-- text here, or tolua/fromlua here to verify syntax?
			ig.luatableInputText('tile remapping', ffInfo, 'tileRemapping')

			ig.luatableInputInt('voxelmap height', ffInfo, 'maxAlt')

			ig.igText'flood fill bounds:'
			local bbox = ffInfo.bbox
			ig.luatableInputInt('flood fill min x', bbox.min, 'x')
			ig.luatableInputInt('flood fill min y', bbox.min, 'y')
			ig.luatableInputInt('flood fill max x', bbox.max, 'x')
			ig.luatableInputInt('flood fill max y', bbox.max, 'y')

			-- AsText so it can handle all lua number parsing, including 0x's
			ig.luatableInputFloatAsText('floor tile', ffInfo, 'floorTile')
			ig.luatableInputFloatAsText('wall tile', ffInfo, 'wallTile')
			ig.luatableInputFloatAsText('ceiling tile', ffInfo, 'ceilingTile')

			ig.luatableInputText('north slopes:', ffInfo, 'tileIsNorthSlope')

			if ig.igButton'export voxelmap' then
				-- 1) make our voxelmap
				local vector = require 'stl.vector-lua'
				local size = vec3i(bbox.max.x - bbox.min.x, bbox.max.y - bbox.min.y, ffInfo.maxAlt)


				local tileIsNorthSlope = string.split(ffInfo.tileIsNorthSlope, ','):mapi(function(x)
					return true, (assert(tonumber(x)))
				end)

				local tileRemap
				if string.trim(ffInfo.tileRemapping) ~= '' then
					xpcall(function()
						tileRemap = assert(fromlua(ffInfo.tileRemapping))
					end, function(err)
						print(err..'\n'..debug.traceback())
					end)
				end

				-- in ff6, tile 1 <-> mine 2 is empty, but in mine it is where i put the torch animations (which are not present in the ff6 tile sheet)
				local function applyRemaps(tileIndex)
					return tileRemap and tileRemap[tileIndex] or tileIndex 
				end


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
				--local altForCol = range(size.x):mapi(function() return 0 end)
				local alt = 0
				for y=0,tonumber(size.y)-1 do	-- from bottom to top of 2D map, per-row (important for north-facing-stairs sake)
					local nextAlt
					for x=0,tonumber(size.x)-1 do
						for z=0,alt do
							local vox = voxelmap:getVoxelBlobPtr(x, y, z)
							vox.intval = 0	-- reset from clear
							vox.spriteIndex = applyRemaps(tile44to55(ffInfo.floorTile))
							vox.mesh3DIndex = 0	-- hmm todo, floor voxel mesh index
							--vox.orientation = 32	-- flip x in voxel orientation?
							vox.orientation = 34	-- flip y
						end

						local mapx = x + bbox.min.x
						local mapy = size.y - 1 - y + bbox.min.y
						local i = mapx + mapWidth * mapy
						if not ffInfo.filled[i] then
							-- if this is a non-traversible tile then grow the walls
							-- TODO determine which wall,
							--  if it's north-facing then use y-z tile for walls
							--  if it's south/east/west then reuse a previosly-created north-facing wall tile profile.
							for z=alt+1,size.z-1 do
								local vox = voxelmap:getVoxelBlobPtr(x, y, z)
								vox.intval = 0	-- reset from clear
								-- TODO if it's a north wall then copy source tiles going y+
								-- and if it's any other wall then copy from the north tile wall profile
								vox.spriteIndex = applyRemaps(tile44to55(
									getWallOrCeilingTile(mapx, mapy - z)
									or (
										z == size.z-1
										and ffInfo.ceilingTile
										or ffInfo.wallTile	-- TODO make this the most-prominent-wall-tile
									)
								))
								-- idk how to find that out ... trace from top of traversible filled region to the ceiling tile and count up most-prevalent
								vox.mesh3DIndex = 0	-- hmm todo, wall voxel mesh index
								--vox.orientation = 32	-- flip x in voxel orientation?
								vox.orientation = 34	-- flip y
							end
						else
							local floorTile16x16 = getFloorTile(mapx, mapy)
							local slope = tileIsNorthSlope[floorTile16x16]
							if slope then
								nextAlt = alt + 1
							end
							-- read the real tile and use it instead of the most prominent
							local vox = voxelmap:getVoxelBlobPtr(x, y, math.min(nextAlt or alt, size.z-1))
							vox.intval = 0	-- reset from clear
							vox.spriteIndex = applyRemaps(tile44to55(floorTile16x16 or ffInfo.floorTile))
							if not slope then
								vox.mesh3DIndex = 0	-- hmm todo, floor voxel mesh index
								vox.orientation = 34	-- flip y
							else
								vox.mesh3DIndex = 5		-- "slope with sides"
								-- TODO I need a mesh that is y-slope-up with texcoords aligned ...
								-- ... or maybe I need extra bits for applying orientation2D to the texcoords?
								-- TODO use the voxelmap rotation lookup tables for a left-applied z-rotation on 34 (aka flip-y)...
								vox.orientation = 35
							end
						end
					end
					alt = math.min(nextAlt or alt, size.z-1)
				end
				path(ffInfo.destFilename):write(voxelmap:toBinStr())
			end

			ig.igText('flood fill histogram:')
			for _,tileValue in ipairs(ffInfo.tileValues) do
				ig.igText('\t'..('0x%06x'):format(tileValue)..' = '..ffInfo.hist[tileValue])
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
