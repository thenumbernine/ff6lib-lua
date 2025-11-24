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
local makePalette = require 'graphics'.makePalette
local makePaletteSets = require 'graphics'.makePaletteSets
local tileWidth = require 'graphics'.tileWidth
local tileHeight = require 'graphics'.tileHeight
local readTile = require 'graphics'.readTile
local drawTile = require 'graphics'.drawTile
local drawTileLinear = require 'graphics'.drawTileLinear
local decompress0x800 = require 'decompress'

-- util? ext.ffi or something?
local function countof(array)
	return ffi.sizeof(array) / (ffi.cast('uint8_t*', array+1) - ffi.cast('uint8_t*', array+0))
end

return function(rom, game, romsize)

-- this holds the info of the 16x16 map blocks, interaction with player, etc
-- cache decompressed data
local mapLayouts = table()	-- 0-based
for i=0,countof(game.mapLayoutOffsets)-1 do
	local offset = game.mapLayoutOffsets[i]:value()
	local addr = 0xffffff
	local data
	if offset ~= 0xffffff then
		addr = offset + ffi.offsetof('game_t', 'mapLayoutsCompressed')
		data = decompress0x800(rom + addr, ffi.sizeof(game.mapLayoutsCompressed))
		mapLayouts[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
		}
	end
	print('mapLayouts[0x'..i:hex()..']'
		..' offset=0x'..offset:hex()
		..' addr=0x'..('%06x'):format(addr)
		..(data and ' size=0x'..(#data):hex() or '')
	)
	--if data then print(data:hexdump()) end
end

local mapTileProps = table()	-- 0-based
for i=0,countof(game.mapTilePropsOfs)-1 do
	local offset = game.mapTilePropsOfs[i]
	local addr = 0xffffff
	local data
	if offset ~= 0xffff then
		addr = offset + ffi.offsetof('game_t', 'mapTilePropsCompressed')
		data = decompress0x800(rom + addr, ffi.sizeof(game.mapTilePropsCompressed))
		mapTileProps[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
		}
	end
	print('mapTileProps[0x'..i:hex()..']'
		..' offset=0x'..offset:hex()
		..' addr=0x'..('%06x'):format(addr)
		..(data and ' size=0x'..(#data):hex() or ''))
	--if data then print(data:hexdump()) end
end

--[[
This holds the mapping from 16x16 to 8x8 tiles.
A (layer 1 & 2) tileset is 2048 bytes in size = 256 * 2*2 * 2
(256 different mapLayout[] values) x (2x2 of the 8x8 subtiles) x (2 bytes for describing rendering the 8x8 subtile)
The 2 bytes describing rendering the 8x8 subtile provides a 10-bit index for lookup into the gfx1+2+3+4 set per map.
That means a map's tileset is unique wrt its gfx1+2+3+4 (+ palette)
--]]
local mapTilesets = table()	-- 0-based
local mapGfxStrs = {}	-- maps from gfxstr = gfx1/2/2/3 to sets of pairs of tileset/palette
for i=0,countof(game.mapTilesetOffsets)-1 do
	local offset = game.mapTilesetOffsets[i]:value()
	local addr = 0xffffff
	local data
	if offset ~= 0xffffff then
		addr = offset + ffi.offsetof('game_t', 'mapTilesetsCompressed')
		data = decompress0x800(rom + addr, ffi.sizeof(game.mapTilesetOffsets))
		mapTilesets[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
			mapIndexes = table(),
			palettes = table(),
			gfxstrs = {},	-- gfx1/gfx2/gfx3/gfx4
			paletteForGfxStr = {},
		}
	end
	print('mapTilesets[0x'..i:hex()..'] offset=0x'
		..offset:hex()
		..' addr=0x'..('%06x'):format(addr)
		..(data and ' size=0x'..(#data):hex() or '')
	)
	--if data then print(data:hexdump()) end
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
local mapTileGraphics = table()	-- 0-based
for i=0,countof(game.mapTileGraphicsOffsets)-1 do
	local offset = game.mapTileGraphicsOffsets[i]:value()
	local addr = offset + ffi.offsetof('game_t', 'mapTileGraphics')
	-- this is times something and then a pointer into game.mapTileGraphics
	print('mapTileGraphics[0x'..i:hex()..'] = 0x'..offset:hex()
-- the space between them is arbitrary
--		..(i>0 and ('\tdiff=0x'..(game.mapTileGraphicsOffsets[i]:value() - game.mapTileGraphicsOffsets[i-1]:value()):hex()) or '')
	)
	mapTileGraphics[i] = {
		index = i,
		offset = offset,
		addr = addr,
		data = rom + addr,
		-- bookkeeping:
		mapIndexes = table(),
		palettes = table(),
	}
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
local mapTileGraphicsLayer3 = table()	--0-based
for i=0,countof(game.mapTileGraphicsLayer3Offsets)-1 do
	local offset = game.mapTileGraphicsLayer3Offsets[i]:value()
	local addr = offset + ffi.offsetof('game_t', 'mapTileGraphicsLayer3')
	local data = decompress0x800(rom + addr, ffi.sizeof(game.mapTileGraphicsLayer3))
	mapTileGraphicsLayer3[i] = {
		index = i,
		offset = offset,
		addr = addr,
		data = data,
		mapIndexes = table(),
		palettes = table(),
	}
	print('mapTileGraphicsLayer3[0x'..i:hex()..'] offset=0x'
		..offset:hex()
		..' addr=0x'..('%06x'):format(addr)
		..' size=0x'..(#data):hex()
	)
	--if data then print(data:hexdump()) end
end
-- each is 0x1040 in size .....
-- wait, is the first 0x40 the tileset?
-- leaving 0x1000 = 0x100 x 8x8x2bpp tiles
do
	local bpp = 2
	local n = countof(game.mapTileGraphicsLayer3Offsets)
	local im = Image(16*tileWidth, 16*n*tileHeight, 1, 'uint8_t'):clear()
	for i=0,n-1 do
		local gfxLayer3 = mapTileGraphicsLayer3[i]
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

local function layer1and2tile8x8toptr(tile8x8, gfxDatas)

	-- first 256 is gfx1
	if tile8x8 < 0x100 then
		local bpp = 4
		local gfxData = gfxDatas[1]
		if not gfxData then return end
		local tileptr = gfxData + tile8x8 * bit.lshift(bpp, 3)
		return tileptr, bpp
	end

	-- next 256 belong to gfx2?
	-- or only 128 of it?
	--if tile8x8 < 0x200 then
	-- (what does bit-7 here represent?)
	if tile8x8 < 0x180 then
		local bpp = 4
		local gfxData = gfxDatas[2]
		if not gfxData then return end
		tile8x8 = bit.band(0x7f, tile8x8)
		local tileptr = gfxData + tile8x8 * bit.lshift(bpp, 3)
		return tileptr, bpp
	end

	-- if gfx3 == gfx4 then gfx3's tiles are 0x180-0x27f
	if gfxDatas[3] == gfxDatas[4] then
		local bpp = 4
		local gfxData = gfxDatas[3]
		if not gfxData then return end
		-- is it 0x180 -> 0 or 0x180 -> 0x80?
		tile8x8 = bit.band(0xff, tile8x8 - 0x80)
		local tileptr = gfxData + tile8x8 * bit.lshift(bpp, 3)
		return tileptr, bpp
	end

	-- [[ from 0x180 to 0x200 I'm getting discrepencies as well...
	-- (what does bit-7 here represent?)
	if tile8x8 < 0x200 then
		local bpp = 4
		local gfxData = gfxDatas[3]
		if not gfxData then return end
		tile8x8 = bit.band(0x7f, tile8x8)
		local tileptr = gfxData + tile8x8 * bit.lshift(bpp, 3)
		return tileptr, bpp
	end
	--]]

	-- gfx3 doesn't use indexes 0x80 and over (reserved for something else?)
	-- (what does bit-7 here represent?)
	if tile8x8 < 0x280 then
		local bpp = 4
		local gfxData = gfxDatas[4]
		if not gfxData then return end
		tile8x8 = bit.band(0x7f, tile8x8)
		local tileptr = gfxData + tile8x8 * bit.lshift(bpp, 3)
		return tileptr, bpp
	end

	-- extra notes to remember for later:
	-- animated tiles start at 0x280
	-- dialog graphics start at 0x2e0
	-- tiles 0x300-0x3ff aren't used by bg1 & bg2
end

local function layer1and2drawtile16x16(img, x, y, tile16x16, tilesetData, zLevelFlags, gfxDatas, palette)
	if not tilesetData then return end
	assert.len(tilesetData, 0x800)
	zLevelFlags = zLevelFlags or 3
	local tilesetptr = ffi.cast('uint8_t*', tilesetData)
	for yofs=0,1 do
		for xofs=0,1 do
			local i = bit.lshift(bit.bor(xofs, bit.lshift(yofs, 1)), 8)
			local tilesetTile = bit.bor(
				tilesetptr[tile16x16 + i],
				bit.lshift(tilesetptr[tile16x16 + bit.bor(0x400, i)], 8)
			)
			local tileZLevel = bit.band(bit.rshift(tilesetTile, 13), 1)
			if bit.band(bit.lshift(1, tileZLevel), zLevelFlags) ~= 0 then
				local tile8x8 = bit.band(tilesetTile, 0x3ff)
				local tileptr, bpp = layer1and2tile8x8toptr(tile8x8, gfxDatas)
				if tileptr then
					local highPal = bit.band(7, bit.rshift(tilesetTile, 10))
					local hFlip8 = bit.band(0x4000, tilesetTile) ~= 0
					local vFlip8 = bit.band(0x8000, tilesetTile) ~= 0
					drawTile(img,
						x + bit.lshift(xofs, 3),
						y + bit.lshift(yofs, 3),
						tileptr,
						bpp,
						hFlip8,
						vFlip8,
						bit.lshift(highPal, bpp),
						palette
					)
				end
			end
		end
	end
end

local function layer3tile8x8toptr(tile8x8, gfxLayer3Data)
	local bpp = 2
	if not gfxLayer3Data then return end
	-- 0x40 at the beginning of all layer3 tiles
	-- also there's only 0x40 unique tiles (only 6 bits are used), so i'm betting this is an extra bitflag that goes along with them ... maybe zLevel?
	local ofs = 0x40 + bit.band(0xff, tile8x8) * bit.lshift(bpp, 3)
	assert.lt(ofs, #gfxLayer3Data)
	local tileptr = ffi.cast('uint8_t*', gfxLayer3Data) + ofs
	return tileptr, bpp
end

local function layer3drawtile16x16(img, x, y, tile16x16, gfxLayer3Data, palette)
	for yofs=0,1 do
		for xofs=0,1 do
			local hFlip = bit.band(0x40, tile16x16) ~= 0
			local vFlip = bit.band(0x80, tile16x16) ~= 0
			-- wait because tile16x16 << 2 has to be 8 bits
			-- that means tile16x16 can only be 6 bits
			-- and it also means that zLevel, hFlip, vFlip, highPal all must be 0
			-- nope, in fact, hFlip is bit 6, vFlip  is bit 7
			-- zLevel might be the bit of the extra 0x40 bytes at the beginning ...
			-- or in fact 0x40 bytes means 8 bits per unique tile, so idk what goes in there ...
			local tilesetTile = bit.bor(
				bit.lshift(bit.band(0x3f, tile16x16), 2),
				bit.lshift(yofs, 1),
				xofs
			)
			tilesetTile = bit.band(tilesetTile, 0xff)
			local tile8x8 = bit.band(tilesetTile, 0x3ff)
			-- bpp is always 2 for layer3
			local tileptr, bpp = layer3tile8x8toptr(tile8x8, gfxLayer3Data)
			if tileptr then
				drawTile(img,
					x + bit.lshift(hFlip and (1-xofs) or xofs, 3),
					y + bit.lshift(vFlip and (1-yofs) or yofs, 3),
					tileptr,
					bpp,
					hFlip,
					vFlip,
					nil,	-- palor
					palette
				)
			end
		end
	end
end

-- gfxData for world is just 0x400 + tilesetData ... they are combined in the same compressed blob
local function layer1worlddrawtile16x16(img, x, y, tile16x16, tilesetData, gfxData, palette)
	if not tilesetData then return end
	if not gfxData then return end
	assert.eq(#tilesetData, 0x2480)	-- but we only use the first 0x400
	local tilesetptr = ffi.cast('uint8_t*', tilesetData)
	assert.eq(tilesetptr + 0x400, gfxData)
	local highPalData = tilesetptr + 0x2400
	for yofs=0,1 do
		for xofs=0,1 do
			local tile8x8 = tilesetptr[bit.bor(
				bit.lshift(xofs, 0),
				bit.lshift(yofs, 1),
				bit.lshift(tile16x16, 2)
			)]
			
			local bpp = 4
			local ofs = tile8x8 * bit.lshift(bpp, 3)
			assert.ge(ofs, 0)
			assert.lt(ofs, 0x2000)
			local tileptr = gfxData + ofs

			local highPal = highPalData[bit.rshift(tile8x8, 1)]
			if 0 ~= bit.band(1, tile8x8) then highPal = bit.rshift(highPal, 4) end
			highPal = bit.band(0xf, highPal)

			local hFlip8 = false
			local vFlip8 = false
			drawTileLinear(img,
				x + bit.lshift(xofs, 3),
				y + bit.lshift(yofs, 3),
				tileptr,
				bpp,
				hFlip8,
				vFlip8,
				bit.lshift(highPal, bpp),
				palette
			)
		end
	end
end

local worldInfos = table{
	'WoB',				-- gfxstr is 0x2480, layoutstr is 0x10000
	'WoR',				-- gfxstr is 0x2480, layoutstr is 0x10000
	'SerpentTrench',	-- gfxstr is 0x2480, layoutstr is 0x4000
}:mapi(function(prefix, i)
	local gfxdatacompressed = game[prefix..'GfxDataCompressed']
	local gfxstr = decompress0x800(
		ffi.cast('uint8_t*', gfxdatacompressed),
		ffi.sizeof(gfxdatacompressed)
	)
	local layoutcompressed = game[prefix..'LayoutCompressed']
	local layoutstr = decompress0x800(
		ffi.cast('uint8_t*', layoutcompressed),
		ffi.sizeof(layoutcompressed)
	)

	-- [[
	local palsrc = op.safeindex(game, prefix..'Palettes')
	local palette = palsrc and makePalette(palsrc, 4, 16*8)
	--]]
	--[[
	local palette = makePalette(game.mapPalettes, 4, 16 * 8)
	--]]

	print('world', i, prefix)
	print('	gfxstr', ('0x%x'):format(#gfxstr))
	print('	layoutstr', ('0x%x'):format(#layoutstr))
	print(gfxstr:hexdump())
	local tilesetdata = gfxstr
	local gfxdata = ffi.cast('uint8_t*', gfxstr) + 0x400

	local tilePropsData = op.safeindex(game, prefix..'TileProps')

	-- [[ while we're here ...
	if palette then
		local img = Image(256, 256, 1, 'uint8_t'):clear()
		img.palette = palette
		for j=0,15 do
			for i=0,15 do
				layer1worlddrawtile16x16(img, i*16, j*16, i+16*j, tilesetdata, gfxdata, palette)
			end
		end
		img:save((maptilesetpath('world'..i..'.png')).path)
	end
	--]]

	return {
		prefix = prefix,
		gfxstr = gfxstr,
		tilesetdata = tilesetdata,							-- 0-0x400
		gfxdata = gfxdata,	-- 0x400 - 0x2400
		layoutstr = layoutstr,
		layoutdata = ffi.cast('uint8_t*', layoutstr),
		layoutSize = i == 3 and vec2i(128, 128) or vec2i(256, 256),
		palette = palette,
		tilePropsData = tilePropsData,
	}
end)



-- key by layout1, layout2, layout3 ...
-- I think I can safely group maps by layout1 alone ...
local mapsForLayout1 = table()

for mapIndex=0,countof(game.maps)-1 do
--do local mapIndex=19
	local map = game.maps + mapIndex
	print('maps[0x'..mapIndex:hex()..'] = '..game.maps[mapIndex])
	-- map.gfx* points into mapTileGraphicsOffsets into mapTileGraphics
	-- these are 8x8 tiles

	--mapsForLayout1[table{map.layout1, map.layout2, map.layout3}:concat'/'] = true
	mapsForLayout1[map.layout1] = mapsForLayout1[map.layout1] or table()
	mapsForLayout1[map.layout1]:insert(mapIndex)

	local paletteIndex = tonumber(map.palette)

	local gfxIndexes = range(4):mapi(function(i)
		return tonumber(map['gfx'..i])
	end)
	local gfxstr = gfxIndexes:mapi(tostring):concat'/'
	local gfxs = range(4):mapi(function(i)
		return mapTileGraphics[gfxIndexes[i]]
	end)

	local gfxLayer3Index =  tonumber(map.gfxLayer3)
	local gfxLayer3 = mapTileGraphicsLayer3[gfxLayer3Index]
	if gfxLayer3 then
		gfxLayer3.palettes[paletteIndex] = true
		gfxLayer3.mapIndexes[mapIndex] = true
	end

	local tilesetDatas = table()
	for i=1,2 do
		local tilesetIndex = tonumber(map['tileset'..i])
		local tileset = mapTilesets[tilesetIndex]
		local tilesetData = tileset.data
		tilesetDatas[i] = tilesetData
		print('map tileset'..i..' data size', tileset and #tilesetData)
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

	local layerPos = table()
	local layerSizes = table()
	local layouts = table()
	for i=1,3 do
		local width = bit.lshift(1, 4 + map['layer'..i..'WidthLog2Minus4'])
		local height = bit.lshift(1, 4 + map['layer'..i..'HeightLog2Minus4'])
		layerSizes[i] = vec2i(width, height)
		local layoutIndex = tonumber(map['layout'..i])
		layouts[i] = layoutIndex > 0 and mapLayouts[layoutIndex] or nil
		print('map layer '..i..' size', layerSizes[i], 'volume', layerSizes[i]:volume())
		print('map layout'..i..' data size', layouts[i] and #layouts[i].data)
		if i > 1 then
			local ofs = map['layer'..i..'Pos']
			layerPos[i] = vec2i(ofs.x, ofs.y)
			print('map layer'..i..' pos', layerPos[i])
		end
	end

	local palette
	if paletteIndex >= 0 and paletteIndex < countof(game.mapPalettes) then
		palette = makePalette(game.mapPalettes + paletteIndex, 4, 16*8)
	else
		print(' map has invalid palette!')
	end

	local tilePropsData = op.safeindex(mapTileProps, tonumber(map.tileProps), 'data')

	-- [[ special maps
	if mapIndex >= 0 
	and mapIndex < #worldInfos
	then
		local worldInfo = worldInfos[mapIndex+1]
		local gfx = {}
		gfx.data = worldInfo.gfxdata
		gfxs[1] = gfx

		layerSizes[1] = worldInfo.layoutSize

		local layout = {}
		layout.data = worldInfo.layoutstr
		layouts[1] = layout

		tilesetDatas[1] = worldInfo.tilesetdata

		palette = worldInfo.palette or palette

		tilePropsData = worldInfo.tilePropsData

		gfxs[2] = nil
		gfxs[3] = nil
		gfxs[4] = nil
		gfxLayer3 = nil
		layouts[2] = nil
		layouts[3] = nil
		tilesetDatas[2] = nil
	end
	--]]

	local gfxDatas = range(4):mapi(function(i)
		local gfx = gfxs[i]
		if not gfx then return end

		-- bookkeeping:
		if gfx.palettes then gfx.palettes[paletteIndex] = true end
		if gfx.mapIndexes then gfx.mapIndexes[mapIndex] = true end

		return gfx.data
	end)

	local layout1Data = layouts[1] and layouts[1].data
	local layer1Size = layerSizes[1]

	-- TODO sometimes the map uses layer2/3's size ... not layer1's ...
	-- check the .size variable?
	local img = Image(
		-- map size is in 16x16 tiles, right?
		-- and should I size it by the first layer, or by the max of all layers?
		bit.lshift(layer1Size.x, 4),
		bit.lshift(layer1Size.y, 4),
		1,
		'uint8_t'
	):clear()

	for _,zAndLayer in ipairs(
		map.layer3Priority == 0
		and {
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
		or {
			{0,2},
			{0,1},
			{1,2},
			{1,1},
			{0,3},
		}
	)do
		local z, layer = table.unpack(zAndLayer)
		local blend = -1

		-- layer 3 avg
		if map.colorMath == 1 and layer == 3 then
			blend = 1
		-- layer 2 avg
		elseif map.colorMath == 4 and layer == 2 then
			blend = 1
		-- layer 3 add
		elseif map.colorMath == 5 and layer == 3 then
			blend = 0
		-- layer 1 avg
		elseif map.colorMath == 8 and layer == 1 then
			blend = 1
		-- there's more ofc but meh
		end
		-- TODO NOTICE blend does nothing at the moment
		-- because I'm outputting 8bpp-indexed

		local layerSize = layerSizes[layer]
		local layout = layouts[layer]
		local layoutData = layout and layout.data
		if not layout or not layoutData then
			print("missing layout "..layer, layout, layoutData)
		--elseif layerSize:volume() ~= #layouts[layer].data then
		--	print("map layout"..layer.." data size doesn't match layer size")
		-- I guess just modulo?
		--elseif layout1Data 	-- if we're missing layout[1].data then we are in the dark as to volume check
		--and #layoutData ~= #layout1Data
		--then
			-- sometimes happens like with map 6 Blackjack Exterior
			--print("layer "..layer.."'s data size doesn't match layer 1's data size:", #layoutData, #layout1Data)
		else
			local posx, posy = 0, 0
			if layerPos[layer]
			-- if we have a position for the layer, but we're using parallax, then the position is going to be relative to the view
			--and map.parallax == 0
			then
				posx, posy = layerPos[layer]:unpack()
			end
			local layoutptr = ffi.cast('uint8_t*', layoutData)
			for dstY=0,layer1Size.y-1 do
				local y = bit.lshift(dstY, 4)
				for dstX=0,layer1Size.x-1 do
					local x = bit.lshift(dstX, 4)
					local srcX = (dstX + posx) % layerSize.x
					local srcY = (dstY + posy) % layerSize.y
					local tile16x16 = layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]

					if mapIndex < #worldInfos then
						if z == 0 and layer == 1 then
							layer1worlddrawtile16x16(img, x, y, tile16x16, tilesetDatas[layer], gfxDatas[layer], palette)
						end
					elseif layer == 3 then
						layer3drawtile16x16(img, x, y, tile16x16, gfxLayer3.data, palette)
					else
						layer1and2drawtile16x16(img, x, y, tile16x16, tilesetDatas[layer], bit.lshift(1, z), gfxDatas, palette)
					end
				end
			end
		end
	end

	img.palette = palette
	img:save((mappath/('map'..mapIndex..'.png')).path)


	-- draw tile properties while we're here
	if tilePropsData then
		if not layout1Data then
			print('!!! got tile properties without layer 1 layout data !!!')
		else
			-- each is 0x200 in size ... draw it I guess
			local img = Image(
				bit.lshift(layer1Size.x, 4),
				bit.lshift(layer1Size.y, 4),
				1,
				'uint8_t'
			):clear()

			local layoutptr = ffi.cast('uint8_t*', layout1Data)
			local tilePropsPtr = ffi.cast('mapTileProps_t*', tilePropsData)
			for dstY=0,layer1Size.y-1 do
				for dstX=0,layer1Size.x-1 do
					local props = tilePropsPtr + layoutptr[0]
					for py=0,15 do
						for px=0,15 do
							local pix = img.buffer + px + 16 * dstX + img.width * (py + 16 * dstY)
							pix[0] = ffi.cast('uint8_t*', props)[bit.band(px + py, 1)]
						end
					end
					layoutptr = layoutptr + 1
				end
			end

			img.palette = range(256):mapi(function(i)
				return {
					math.random(0,255),
					math.random(0,255),
					math.random(0,255),
					255
				}
			end)
			img:save((mappath/('tileprops'..mapIndex..'.png')).path)
		end
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

for _,tilesetIndex in ipairs(mapTilesets:keys():sort()) do
	local tileset = mapTilesets[tilesetIndex]
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
				local gfx = mapTileGraphics[i]
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
				layer1and2drawtile16x16(
					img,
					x,
					y,
					tile16x16,
					mapTilesets[tilesetIndex].data,
					nil,
					gfxDatas,
					palette
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
for _,gfxIndex in ipairs(mapTileGraphics:keys():sort()) do
	local bpp = 4
	local gfx = mapTileGraphics[gfxIndex]
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

for _,gfxLayer3Index in ipairs(mapTileGraphicsLayer3:keys():sort()) do
	local gfxLayer3 = mapTileGraphicsLayer3[gfxLayer3Index]
	if gfxLayer3 and gfxLayer3.data then
		-- layer3 always has the same layout, so it has no tileset, so just use that layout for the graphics tiles
		local paletteIndex = gfxLayer3.palettes:keys():sort()[1] or 0
		local palette = makePalette(game.mapPalettes + paletteIndex, 4, 16*8)

		local size = vec2i(16, 16)
		local img = Image(16 * size.x, 16 * size.y, 1, 'uint8_t'):clear()
		local tile16x16 = 0
		for j=0,size.y-1 do
			for i=0,size.x-1 do
				layer3drawtile16x16(
					img,
					bit.lshift(i, 4),
					bit.lshift(j, 4),
					tile16x16,
					gfxLayer3.data,
					palette
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
