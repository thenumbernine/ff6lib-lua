local ffi = require 'ffi'
local table = require 'ext.table'
local string = require 'ext.string'
local path = require 'ext.path'
local tolua = require 'ext.tolua'
local fromlua = require 'ext.fromlua'
local LiteThread = require 'thread.lite'
local Semaphore = require 'thread.semaphore'
local vec2i = require 'vec-ffi.vec2i'
local vec3i = require 'vec-ffi.vec3i'
local box2i = require 'vec-ffi.box2i'
local sdl = require 'sdl'
local ig = require 'imgui'
local Window = require 'ff6.vis.window'

local vis_util = require 'ff6.vis.util'
local mapTilePropsFlagForName = vis_util.mapTilePropsFlagForName
local worldTilePropsFlagForName = vis_util.worldTilePropsFlagForName


local intptr_t = ffi.typeof'intptr_t'


local floodFillSavePath = path'floodfill-save.lua'

local VoxelmapWindow = Window:subclass()

VoxelmapWindow.name = 'voxelmap'

function VoxelmapWindow:init(...)
	VoxelmapWindow.super.init(self, ...)

	-- load the floodFillTilesPerMap here...
	self.floodFillTilesPerMap = table()
	if floodFillSavePath:exists() then
		self.floodFillTilesPerMap = table(
			(assert(fromlua(
				(assert(
					floodFillSavePath:read()
				)),
				nil, nil, {
					box2i = box2i,
				}
			)))
		)
		for k,v in pairs(self.floodFillTilesPerMap) do
			setmetatable(v, table)
		end
	end

	self.semOpen = Semaphore()
	self.sdlSaveFileDialog_chooseOutputFilenameThread = LiteThread{
		init = function(thread)
			thread.lua[[
-- set required modules as globals
ffi = require 'ffi'
Semaphore = require 'thread.semaphore'
require 'sdl'	-- load SDL_DialogFileCallback
sdlAssertNonNull = require 'sdl.assert'.nonnull
]]
			thread.lua([[
local semOpenID = ffi.cast('void*', ...)
semOpen = Semaphore:wrap(semOpenID)
]], ffi.cast(intptr_t, self.semOpen.id+0))
		end,
		threadFuncTypeName = 'SDL_DialogFileCallback',
		code = [[
local userdata, filelist, filter = ...
xpcall(function()
	sdlAssertNonNull(filelist)	-- error
	if filelist[0] == nil then return end	-- no file picked

	-- set a global for the app to read
	_G.openfilename  = ffi.string(filelist[0])

	-- tell the app to record it
	semOpen:post()

end, function(err)
	print(err..'\n'..debug.traceback())
end)
]],
	}
end

function VoxelmapWindow:exit()
	assert(floodFillSavePath:write(
		(assert(tolua(
			self.floodFillTilesPerMap,
			{
				serializeForType = {
					cdata = function(state, x, tab, luapath, keyRef)
						if box2i:isa(x) then
							return 'box2i'..tostring(x)
						else
							error("tolua got unknown cdata "..tostring(x))
						end
					end,
				},
			}
		)))
	))

end

function VoxelmapWindow:updateWindow()
	local app = self.app
	local game = app.game

	local mapIndex = app.mapWindow.index
	local mapInfo = game.getMap(mapIndex)
	if not mapInfo then return end

	local layerSizes = mapInfo.layerSizes
	local mapWidth, mapHeight = layerSizes[1].x, layerSizes[1].y

	local tileIndex = app.tileWindow.index

	if app.tilePropsTex
	and app.tilePropsTex.data
	then
		local data = app.tilePropsTex.data
		if ig.igButton'Flood Fill Tile...' then
			local x = tileIndex % mapWidth
			local y = (tileIndex - x) / mapWidth

			local found
			for _,ff in ipairs(self.floodFillTilesPerMap[mapIndex] or {}) do
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

					filled[i] = {
						tileValue = value,
						alt = 0,
					}
				end
				local function canTraverse(x,y)
					local flags = data[x + mapWidth * y]
					if mapIndex < 2 then
						return 0 == bit.band(worldTilePropsFlagForName.blocksWalking, flags)
					else
						return 0 == bit.band(mapTilePropsFlagForName.impassible, flags)
					end
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

				-- hmm TODO
				-- histogram on layer1 tile or layer2 or on both?
				local hist = {}
				for index, ft in pairs(filled) do
					hist[ft.tileValue] = (hist[ft.tileValue] or 0) + 1
				end
				local tileValues = table.keys(hist):sort(function(a,b)
					return hist[a] > hist[b]
				end)
				local floorTile = bit.band(0xff, tileValues[1])

				self.floodFillTilesPerMap[mapIndex] = self.floodFillTilesPerMap[mapIndex] or table()
				self.floodFillTilesPerMap[mapIndex]:insert{
					filled = filled,	-- key = tile index, value = int with each byte a layer's tile16x16 index
					hist = hist,		-- key = int of tile16x16 layers (value of filled), value = occurrence count
					tileValues = tileValues,	-- int of tile16x16 layers, sorted by occurrence
					bbox = bbox,
					floorTile = floorTile,
					wallTile = 0,
					ceilingTile = 0,
					tileIsNorthSlope = '',
					tileRemapping = '',	-- lua key->value code for a table for remapping 32x32 of 8x8 (numo9-index) tile-indexes
					destFilename = 'exported-voxelmap-map'..mapIndex..'-'..(#self.floodFillTilesPerMap[mapIndex]+1)..'.vox',
					maxAlt = 8,
				}
			end
		end
	end

	local floodFillTilesForThisMap = self.floodFillTilesPerMap[mapIndex]
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
					self.sdlSaveFileDialog_chooseOutputFilenameThread.funcptr,	-- callback
					nil,				-- userdata
					self.app.window,	-- window
					nil,				-- filters
					0,					-- nfilters
					path(ffInfo.destFilename):getdir():exists()
						and path(ffInfo.destFilename):getdir().path
						or path:cwd().path		-- default_location
				)
			end
			-- if we do the save then whatever ffInfo is open gets it, so don't change ffInfo's until you pick your file!
			if self.semOpen:trywait() then
				ffInfo.destFilename = self.sdlSaveFileDialog_chooseOutputFilenameThread.lua.global.openfilename
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

			--[[
			ig.igText('flood fill histogram:')
			for _,tileValue in ipairs(ffInfo.tileValues) do
				ig.igText('\t'..('0x%06x'):format(tileValue)..' = '..ffInfo.hist[tileValue])
			end
			--]]
		end
	end
end

return VoxelmapWindow
