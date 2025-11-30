--[[
problems remaining:
- animated tiles don't work yet.  noticeable with map 38's torches and figaro castle.
- map 8 blackjack parlor has tile glitches (and in everything's ff6tool too)
- map 47 south figaro water layer is bad and proly needs animation.
- map 54 figaro castle needs animation for fans and stuff.
- map 61 figaro engine has tile glitches (and in everything's ff6tool too)
- map 92 south figaro cave behind turtle has tile glitches (and in everything's ff6tool too)
- map 103 (and in everything's ff6tool)
--]]
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
local string = require 'ext.string'
local range = require 'ext.range'
local assert = require 'ext.assert'
local tolua = require 'ext.tolua'
local vec2i = require 'vec-ffi.vec2i'
local Image = require 'image'
local makePalette = require 'ff6.graphics'.makePalette
local makePaletteSets = require 'ff6.graphics'.makePaletteSets
local tileWidth = require 'ff6.graphics'.tileWidth
local tileHeight = require 'ff6.graphics'.tileHeight
local readTile = require 'ff6.graphics'.readTile

return function(rom, game, romsize)

local countof = game.countof
local decompress = game.decompress

-- this holds the info of the 16x16 map blocks, interaction with player, etc
-- cache decompressed data
for i=0,countof(game.mapLayoutOffsets)-1 do
	local mapLayout = game.getMapLayout(i)
	if mapLayout then
		print('mapLayouts[0x'..i:hex()..']'
			..' offset=0x'..mapLayout.offset:hex()
			..' addr=0x'..('%06x'):format(mapLayout.addr)
			..(mapLayout.data and ' size=0x'..(#mapLayout.data):hex() or '')
		)
		--if mapLayout.data then print(mapLayout.data:hexdump()) end
	end
end

for i=0,countof(game.mapTilePropsOffsets)-1 do
	local tileProps = game.getMapTileProps(i)
	if tileProps then
		print('mapTileProps[0x'..i:hex()..']'
			..' offset=0x'..tileProps.offset:hex()
			..' addr=0x'..('%06x'):format(tileProps.addr)
			..(tileProps.data and ' size=0x'..(#tileProps.data):hex() or ''))
		--if data then print(data:hexdump()) end
	end
end

--[[
This holds the mapping from 16x16 to 8x8 tiles.
A (layer 1 & 2) tileset is 2048 bytes in size = 256 * 2*2 * 2
(256 different mapLayout[] values) x (2x2 of the 8x8 subtiles) x (2 bytes for describing rendering the 8x8 subtile)
The 2 bytes describing rendering the 8x8 subtile provides a 10-bit index for lookup into the gfx1+2+3+4 set per map.
That means a map's tileset is unique wrt its gfx1+2+3+4 (+ palette)
--]]
local mapGfxStrs = {}	-- maps from gfxstr = gfx1/2/2/3 to sets of pairs of tileset/palette
for i=0,countof(game.mapTilesetOffsets)-1 do
	local tileset = game.getMapTileset(i)
	if tileset then
		tileset.mapIndexes = table()
		tileset.palettes = table()
		tileset.gfxstrs = {}	-- gfx1/gfx2/gfx3/gfx4
		tileset.paletteForGfxStr = {}
		print('mapTilesets[0x'..i:hex()..'] offset=0x'
			..tileset.offset:hex()
			..' addr=0x'..('%06x'):format(tileset.addr)
			..(data and ' size=0x'..(#tileset.data):hex() or '')
		)
		--if data then print(data:hexdump()) end
	end
end
print()

local mappath = path'maps'
mappath:mkdir()

local maptilespath = path'maptiles8x8'
maptilespath:mkdir()

local maptilesetpath = path'maptiles16x16'
maptilesetpath:mkdir()

local mappalpath = path'mappals'
mappalpath:mkdir()

-- map tile graphics, 8x8x4bpp tiles for layers 1 & 2
-- the last 3 are 0xffffff
for i=0,countof(game.mapTileGraphicsOffsets)-1 do
	local gfx = game.getMapTileGraphics(i)
	if gfx then
		-- this is times something and then a pointer into game.mapTileGraphics
		print('mapTileGraphics[0x'..i:hex()..'] = 0x'..gfx.offset:hex()
-- the space between them is arbitrary
--		..(i>0 and ('\tdiff=0x'..(game.mapTileGraphicsOffsets[i]:value() - game.mapTileGraphicsOffsets[i-1]:value()):hex()) or '')
		)

		-- bookkeeping:
		gfx.mapIndexes = table()
		gfx.palettes = table()
	end
end
do	-- here decompress all 'mapTileGraphics' tiles irrespective of offset table
	-- 0x30c8 tiles of 8x8x4bpp = 32 bytes in game.mapTileGraphics
	local bpp = 4
	local numTiles = ffi.sizeof(game.mapTileGraphics) / bit.lshift(bpp, 3)	-- = 0x30c8
	-- 128 is just over sqrt numTiles
	local masterTilesWide = 16 -- 128
	local masterTilesHigh = math.ceil(numTiles / masterTilesWide)
	local im = Image(masterTilesWide*tileWidth, masterTilesHigh*tileHeight, 1, 'uint8_t'):clear()
	for i=0,numTiles-1 do
		local x = i % masterTilesWide
		local y = (i - x) / masterTilesWide
		readTile(im,
			x * tileWidth,
			y * tileHeight,
			game.mapTileGraphics + bit.lshift(bpp, 3),
			bpp)
	end
	-- alright where is the palette info stored?
	-- and I'm betting somewhere is the 16x16 info that points into this 8x8 tile data...
	-- and I'm half-suspicious it is compressed ...
	im.palette = makePalette(game.mapPalettes + 0xc, 4, 16 * 8)
	im:save((maptilespath/'tiles-layers1and2.png').path)
end

-- each points to compressed data, which decompressed is of size 0x1040
for i=0,countof(game.mapTileGraphicsLayer3Offsets)-1 do
	local gfxLayer3 = game.getMapTileGraphicsLayer3(i)
	if gfxLayer3 then
		gfxLayer3.mapIndexes = table()
		gfxLayer3.palettes = table()
		print('mapTileGraphicsLayer3[0x'..i:hex()..'] offset=0x'
			..gfxLayer3.offset:hex()
			..' addr=0x'..('%06x'):format(gfxLayer3.addr)
			..' size=0x'..(#gfxLayer3.data):hex()
		)
		--if data then print(data:hexdump()) end
	end
end
-- each is 0x1040 in size .....
-- wait, is the first 0x40 the tileset?
-- leaving 0x1000 = 0x100 x 8x8x2bpp tiles
do
	local bpp = 2
	local n = countof(game.mapTileGraphicsLayer3Offsets)
	local im = Image(16*tileWidth, 16*n*tileHeight, 1, 'uint8_t'):clear()
	for i=0,n-1 do
		local gfxLayer3 = game.mapTileGraphicsLayer3Cache[i]
		for x=0,15 do
			for y=0,15 do
				readTile(im,
					x * tileWidth,
					(y + 16 * i) * tileHeight,
					-- how come, for 4bpp, the tiles are 0x20 = 8*4 bytes = 8*8*4 bits = 1<<5 bytes apart
					-- but for 2bpp the tiles are 8*16 = 8*2 bytes = 8*8*2 bits = 1<<4 bytes apart ...
					ffi.cast('uint8_t*', gfxLayer3.data) + 0x40 + (x + 16 * y) * bit.lshift(bpp, 3),
					bpp)
			end
		end
	end
	-- alright where is the palette info stored?
	-- and I'm betting somewhere is the 16x16 info that points into this 8x8 tile data...
	-- and I'm half-suspicious it is compressed ...
	im.palette = makePalette(game.mapPalettes + 0xc, 4, 16 * 8)
	im:save((maptilespath/'tiles-layer3.png').path)
end

makePaletteSets(
	mappalpath,
	game.mapPalettes,
	ffi.sizeof(game.mapPalettes) / ffi.sizeof'color_t',
	function(index) return bit.band(0xf, index) == 0 end
)

local mappalworldspath = path'mappalworld'
mappalworldspath:mkdir()
makePaletteSets(
	mappalworldspath,
	game.WoBPalettes,
	(
		ffi.sizeof(game.WoBPalettes)
		+ ffi.sizeof(game.WoRPalettes)
		+ ffi.sizeof(game.setzerAirshipPalette)
	) / ffi.sizeof'color_t',
	function(index) return bit.band(0xf, index) == 0 end
)

for i,worldInfo in ipairs(game.worldInfos) do
	print('world', i, worldInfo.prefix)
	print('	gfxstr', ('0x%x'):format(#worldInfo.gfxstr))
	print('	layoutstr', ('0x%x'):format(#worldInfo.layoutstr))
	--print(worldInfo.gfxstr:hexdump())

	-- while we're here ...
	if worldInfo.palette then
		local img = Image(256, 256, 1, 'uint8_t'):clear()
		img.palette = worldInfo.palette
		for j=0,15 do
			for i=0,15 do
				game.layer1worlddrawtile16x16(img, i*16, j*16, i+16*j, worldInfo.tilesetdata, worldInfo.gfxdata)
			end
		end
		img:save((maptilesetpath('world'..i..'.png')).path)
	end
end

-- key by layout1, layout2, layout3 ...
-- I think I can safely group maps by layout1 alone ...
local mapsForLayout1 = table()

for mapIndex=0,countof(game.maps)-1 do
	local mapInfo = game.getMap(mapIndex)
	if mapInfo then
		local map = mapInfo.map
		local paletteIndex = tonumber(map.palette)
		local gfxLayer3 = mapInfo.gfxLayer3
		local tilesetDatas = mapInfo.tilesetDatas
		local layerPos = mapInfo.layerPos
		local layerSizes = mapInfo.layerSizes
		local layouts = mapInfo.layouts
		local palette = mapInfo.palette
		local tilePropsData = mapInfo.tilePropsData

		print('maps[0x'..mapIndex:hex()..'] = '..map[0])
		-- map.gfx* points into mapTileGraphicsOffsets into mapTileGraphics
		-- these are 8x8 tiles

		--mapsForLayout1[table{map.layout1, map.layout2, map.layout3}:concat'/'] = true
		mapsForLayout1[map.layout1] = mapsForLayout1[map.layout1] or table()
		mapsForLayout1[map.layout1]:insert(mapIndex)

		local gfxstr = mapInfo.gfxIndexes:mapi(tostring):concat'/'
		for i=1,2 do
			local tilesetIndex = tonumber(map['tileset'..i])
			local tileset = game.getMapTileset(tilesetIndex)
			local tilesetData = tilesetDatas[i]
			print('map tileset'..i..' data size', tilesetData and #tilesetData)
			if tileset then
				tileset.mapIndexes[mapIndex] = true
				tileset.palettes[paletteIndex] = true
				tileset.gfxstrs[gfxstr] = true
				tileset.paletteForGfxStr[gfxstr] = paletteIndex
				-- map from gfxstr to tilesetIndex/paletteIndex
				-- this is unique for each set of 640 tile8x8's rendered (used by tilesets)
				mapGfxStrs[gfxstr] = mapGfxStrs[gfxstr] or {}
				mapGfxStrs[gfxstr][tilesetIndex..'/'..paletteIndex] = true
			end
		end

		if gfxLayer3 then
			gfxLayer3.palettes[paletteIndex] = true
			gfxLayer3.mapIndexes[mapIndex] = true
		end

		for i=1,3 do
			print('map layer '..i..' size', layerSizes[i], 'volume', layerSizes[i]:volume())
			print('map layout'..i..' data size', layouts[i] and #layouts[i].data)
			if i > 1 then
				print('map layer'..i..' pos', layerPos[i])
			end
		end

		if not palette then
			print(' map has invalid palette!')
		end

		for i=1,4 do
			local gfx = mapInfo.gfxs[i]
			if gfx then
				if gfx.palettes then gfx.palettes[paletteIndex] = true end
				if gfx.mapIndexes then gfx.mapIndexes[mapIndex] = true end
			end
		end

		local layer1Size = layerSizes[1]
		local compositeImg = Image(
			-- map size is in 16x16 tiles, right?
			-- and should I size it by the first layer, or by the max of all layers?
			bit.lshift(layer1Size.x, 4),
			bit.lshift(layer1Size.y, 4),
			1,
			'uint8_t'
		):clear()
		compositeImg.palette = palette

		local layerImgs = mapInfo:getLayerImages()
		for _,layerImg in ipairs(layerImgs) do
			-- now apply layerImg to compositeImg
			-- ... maybe later have an option for doing it RGBA using proper blending
			-- ... but for now I want to preserve palettes so
			for i=0,layerImg.width * layerImg.height - 1 do
				local colorIndex = layerImg.buffer[i]
				local color = palette[colorIndex+1]
				if color[4] > 0 then
					compositeImg.buffer[i] = colorIndex
				end
			end
		end

		compositeImg:save((mappath/('map'..mapIndex..'.png')).path)


		local layout1Data = layouts[1] and layouts[1].data
		-- draw tile properties while we're here
		if tilePropsData then
			if not layout1Data then
				print('!!! got tile properties without layer 1 layout data !!!')
			else
				-- each is 0x200 in size ... draw it I guess
				local tileLayoutVisImg = Image(
					bit.lshift(layer1Size.x, 4),
					bit.lshift(layer1Size.y, 4),
					1,
					'uint8_t'
				):clear()

				-- RGB (16 bits used)
				local tileLayoutImg = Image(layer1Size.x, layer1Size.y, 3, 'uint8_t')

	--assert.eq(ffi.sizeof'mapTileProps_t', 2)	-- make sure writing uint16_t's works
	--local tilePropsMap = ffi.new('uint16_t[?]', layer1Size.x * layer1Size.y)
				local layoutptr = ffi.cast('uint8_t*', layout1Data)
				local tilePropsPtr = ffi.cast('uint16_t*', tilePropsData)
				for dstY=0,layer1Size.y-1 do
					for dstX=0,layer1Size.x-1 do
						local props = tilePropsPtr + layoutptr[0]

						-- write to the 16:1 8bppIndexed image
						for py=0,15 do
							for px=0,15 do
								local pix = tileLayoutVisImg.buffer + px + 16 * dstX + tileLayoutVisImg.width * (py + 16 * dstY)
								pix[0] = ffi.cast('uint8_t*', props)[bit.band(px + py, 1)]
							end
						end

						-- write to the 1:1 RGB image
						local pix = tileLayoutImg.buffer + 3 * (dstX + tileLayoutImg.width * dstY)
						ffi.cast('uint16_t*', pix)[0] = props[0]
	--tilePropsMap[dstX + tileLayoutImg.width * dstY] = props[0]

						layoutptr = layoutptr + 1
					end
				end

	--if mapIndex == 0 or mapIndex == 1 then
	--	print('tile props:')
	--	print(ffi.string(tilePropsMap, ffi.sizeof(tilePropsMap)):hexdump())
	--end

				tileLayoutVisImg.palette = range(256):mapi(function(i)
					return {
						math.random(0,255),
						math.random(0,255),
						math.random(0,255),
						255
					}
				end)

				tileLayoutVisImg:save((mappath/('maplayoutviz'..mapIndex..'.png')).path)
				tileLayoutImg:save((mappath/('maplayout'..mapIndex..'.png')).path)
			end
		end

		-- [[ write out maps 0 and 1 as RGB (16 bits used) tilemaps
		if mapIndex == 0
		or mapIndex == 1
		then
			local tilemap = Image(layer1Size.x, layer1Size.y, 3, 'uint8_t'):clear()
			local dst = tilemap.buffer
			local src = ffi.cast('uint8_t*', layout1Data)
			for y=0,layer1Size.y-1 do
				for x=0,layer1Size.x-1 do
					ffi.cast('uint16_t*', dst)[0] = bit.bor(
						bit.lshift(bit.band(0xf, src[0]), 1),
						bit.lshift(bit.band(0xf0, src[0]), 2)
					)
					dst = dst + tilemap.channels
					src = src + 1
				end
			end
			tilemap:save((mappath('tilemap'..mapIndex..'.png')).path)
		end
		--]]
	end
end
print()

--[[
415 unique maps[]
352 unique mapLayoutOffsets[]
183 unique combinations of maps[]' layout1/layout2/layout3
148 unique values of layout1's of maps
--]]
-- [[
print'maps for layout1s:'
for _,layoutIndex in ipairs(mapsForLayout1:keys():sort()) do
	print(layoutIndex, ':', mapsForLayout1[layoutIndex]:concat', ')
end
print()
--]]

for _,tilesetIndex in ipairs(game.mapTilesetCache:keys():sort()) do
	local tileset = game.mapTilesetCache[tilesetIndex]
	local gfxstrs = table.keys(tileset.gfxstrs):sort()
	if tileset then
		print('mapTilesets[0x'..tilesetIndex:hex()..']')
		print('','mapIndexes='..tolua(tileset.mapIndexes:keys():sort()))
		print('','palettes='..tolua(tileset.palettes:keys():sort()))
		print('','gfxstrs='..tolua(gfxstrs))
	end
	for tilesetGfxPerm,gfxstr in ipairs(gfxstrs) do
		local paletteIndex = tileset.paletteForGfxStr[gfxstr] or 0
		local palette = makePalette(game.mapPalettes + paletteIndex, 4, 16*8)

		local gfxDatas = string.split(gfxstr, '/')
			:mapi(function(s)
				local i = tonumber(s)
				local gfx = game.getMapTileGraphics(i)
				if not gfx then return end
				return gfx.data
			end)

--print('drawing '..gfxstr..' with palette '..paletteIndex)

		local size = vec2i(16, 16)
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
					game.mapTilesetCache[tilesetIndex].data,
					nil,
					gfxDatas
				)
				tile16x16 = tile16x16 + 1
			end
		end
		img.palette = palette
		img:save((maptilesetpath/('tileset_'..tilesetIndex
			..(#gfxstrs > 1 and ('_'..tilesetGfxPerm) or '')
			..'.png')).path)
	end
end

-- 8x8 tiles are going to be 16x40 = 640 in size
-- depending on whether it is pointed to by gfx1/2/3/4, or what combo are used, this might be 128 or 256 entries
for _,gfxIndex in ipairs(game.mapTileGraphicsCache:keys():sort()) do
	local bpp = 4
	local gfx = game.mapTileGraphicsCache[gfxIndex]
	if gfx then
		-- draw the 8x8 of all tiles here
		-- notice, some gfxs only ever use only 128 of them

		local paletteIndex = gfx.palettes:keys():sort()[1] or 0
		local palette = makePalette(game.mapPalettes + paletteIndex, 4, 16*8)

		local size = vec2i(16, 16)
		local img = Image(8 * size.x, 8 * size.y, 1, 'uint8_t'):clear()
		local tile8x8 = 0
		for j=0,size.y-1 do
			for i=0,size.x-1 do
				local tileptr = rom + gfx.addr + tile8x8 * bit.lshift(bpp, 3)
				readTile(
					img,
					bit.lshift(i, 3),
					bit.lshift(j, 3),
					tileptr,
					bpp
				)
				tile8x8 = tile8x8 + 1
			end
		end
		img.palette = palette
		img:save((maptilespath/('tilegfx4bpp_'..gfxIndex..'.png')).path)
	end
end

for _,gfxLayer3Index in ipairs(game.mapTileGraphicsLayer3Cache:keys():sort()) do
	local gfxLayer3 = game.mapTileGraphicsLayer3Cache[gfxLayer3Index]
	if gfxLayer3 and gfxLayer3.data then
		-- layer3 always has the same layout, so it has no tileset, so just use that layout for the graphics tiles
		local paletteIndex = gfxLayer3.palettes:keys():sort()[1] or 0
		local palette = makePalette(game.mapPalettes + paletteIndex, 4, 16*8)

		local size = vec2i(16, 16)
		local img = Image(16 * size.x, 16 * size.y, 1, 'uint8_t'):clear()
		local tile16x16 = 0
		for j=0,size.y-1 do
			for i=0,size.x-1 do
				game.layer3drawtile16x16(
					img,
					bit.lshift(i, 4),
					bit.lshift(j, 4),
					tile16x16,
					gfxLayer3.data
				)
				tile16x16 = tile16x16 + 1
			end
		end
		img.palette = palette
		img:save((maptilespath/('tilegfx2bpp_'..gfxLayer3Index..'.png')).path)
	end
end


print()
for i=0,ffi.sizeof(game.mapEventTriggerOfs)/2-1 do
	local addr = game.mapEventTriggerOfs[i] + ffi.offsetof('game_t', 'mapEventTriggerOfs')
	local mapEventTrigger = ffi.cast('mapEventTrigger_t*', rom + addr)
	print('mapEventTrigger[0x'..i:hex()..'] ='
		--..' $'..('%04x'):format(addr)
		..' '..mapEventTrigger)
end
print()

-- there are less entrance trigger offsets than entrance triggers
-- all the entranceTriggerOfs point into entranceTriggers aligned to entranceTrigger_t:
-- so I could dump the whole list of 0x469 entranceTrigger_t's
--  instead of just the offset list
-- These are for the world map I guess?
-- The very first one is the chocobo stable next to Dragons Neck Colloseum in WoB.
-- but I don't see the Narshe trigger in this list ...
for i=0,game.numEntranceTriggerOfs-1 do
	-- TODO use ref_t or whateever
	local addr = game.entranceTriggerOfs[i] + ffi.offsetof('game_t', 'entranceTriggerOfs')
	--assert.eq((addr - ffi.offsetof('game_t', 'entranceTriggers')) % ffi.sizeof'entranceTrigger_t', 0)
	local entranceTrigger = ffi.cast('entranceTrigger_t*', rom + addr)
	print('entranceTriggerOfs[0x'..i:hex()..'] ='
		..' addr: $'..('%06x'):format(addr))
		--..' '..entranceTrigger)
end
print()

--[[
ok so
entrance trigger #4 is Narshe on the WoB map
but it's not in the entranceTriggerOfs[] list
so I'm betting the entranceTriggerOfs[] list is the start of each map.
its size is 513 while the map size is 415 (and everything's ff6tool says the entranceTriggerOfs[] is just 415 so...)
so if entranceTriggerOfs is the start per map then what is the length per map?
Sure enough this matches with everything's tool, which says 44 <-> 0x2c entrances on world map, and I show Ofs[1] points to 0x2d ...
--]]
for i=0,countof(game.entranceTriggers)-1 do
	-- TODO use ref_t or whateever
	local entranceTrigger = game.entranceTriggers + i
	local addr = ffi.cast('uint8_t*', entranceTrigger) - rom
	print('entranceTriggers[0x'..i:hex()..'] ='
		..' addr: $'..('%06x'):format(addr)
		..' '..entranceTrigger)
end
print()

-- there are more entrance area trigger offsets than entrance area triggers
for i=0,game.numEntranceTriggerOfs-1 do
	local addr = game.entranceAreaTriggerOfs[i] + ffi.offsetof('game_t', 'entranceAreaTriggerOfs')
	--assert.eq((addr - ffi.offsetof('game_t', 'entranceAreaTriggers')) % ffi.sizeof'entranceAreaTrigger_t', 0)
	print('entranceAreaTrigger[0x'..i:hex()..']')
	print(' addr: $'..('%06x'):format(addr))
	local entranceAreaTrigger = ffi.cast('entranceAreaTrigger_t*', rom + addr)
	print(' '..entranceAreaTrigger)
end

for i=0,countof(game.treasureOfs)-1 do
	local addr = game.treasureOfs[i] + ffi.offsetof('game_t', 'treasures')
	print('treasureOfs[0x'..i:hex()..']'
		..' addr=$'..('%06x'):format(addr))
end
print()

for i=0,countof(game.treasures) do
	local addr = ffi.cast('uint8_t*', game.treasures + i) - rom
	print('treasures[0x'..i:hex()..'] ='
		..' addr=$'..('%06x'):format(addr)
		..' '..game.treasures[i])
end
print()

for i=0,countof(game.npcOfs)-1 do
	local addr = game.npcOfs[i] + ffi.offsetof('game_t', 'npcOfs')
	print('npcOfs[0x'..i:hex()..']'
		..' addr=$'..('%06x'):format(addr))
end
print()

for i=0,countof(game.npcs) do
	local addr = ffi.cast('uint8_t*', game.npcs + i) - rom
	print('npcs[0x'..i:hex()..']'
		..' addr=$'..('%06x'):format(addr)
		..' '..game.npcs[i])
end
print()


print()
print(game.mapNames)

for i=0,0xff do
	print('WoBTileProps[0x'..i:hex()..'] = '..game.WoBTileProps[i])
end
for i=0,0xff do
	print('WoRTileProps[0x'..i:hex()..'] = '..game.WoRTileProps[i])
end
print()


end
