local ffi = require 'ffi'
local op = require 'ext.op'
local assert = require 'ext.assert'
local class = require 'ext.class'
local table = require 'ext.table'
local range = require 'ext.range'
local vec2i = require 'vec-ffi.vec2i'
local Image = require 'image'
local makePalette = require 'ff6.graphics'.makePalette
local readTile = require 'ff6.graphics'.readTile
local readTileLinear = require 'ff6.graphics'.readTileLinear

return function(game)
	local rom = ffi.cast('uint8_t*', game.s)
	local countof = game.countof
	local decompress = game.decompress
	local romsize = game.romsize

	local mapLayoutCache = table()	-- 0-based
	game.mapLayoutCache = mapLayoutCache
	function game.getMapLayout(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapLayoutOffsets) then return end

		if mapLayoutCache[i] then return mapLayoutCache[i] end
		local offset = game.mapLayoutOffsets[i]:value()

		if offset == 0xffffff then return end

		-- is 0xffffff the only invalid value?
		assert.ge(offset, 0)
		assert.lt(offset, ffi.sizeof(game.mapLayoutsCompressed))

		local addr = offset + ffi.offsetof('game_t', 'mapLayoutsCompressed')
		local data = decompress(rom + addr, ffi.sizeof(game.mapLayoutsCompressed))
		mapLayoutCache[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
		}
		return mapLayoutCache[i]
	end

	local mapTilePropsCache = table()	-- 0-based
	game.mapTilePropsCache = mapTilePropsCache
	function game.getMapTileProps(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTilePropsOffsets) then return end

		if mapTilePropsCache[i] then return mapTilePropsCache[i] end
		local offset = game.mapTilePropsOffsets[i]
		if offset == 0xffff then return end

		assert.ge(offset, 0, 'mapTilePropsOffsets['..i..']')
		assert.lt(offset, ffi.sizeof(game.mapTilePropsCompressed), 'mapTilePropsOffsets['..i..']')

		local addr = offset + ffi.offsetof('game_t', 'mapTilePropsCompressed')
		local data = decompress(rom + addr, ffi.sizeof(game.mapTilePropsCompressed))
		mapTilePropsCache[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
		}
		return mapTilePropsCache[i]
	end

	local mapTilesetCache = table()
	game.mapTilesetCache = mapTilesetCache
	function game.getMapTileset(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTilesetOffsets) then return end

		if mapTilesetCache[i] then return mapTilesetCache[i] end
		local offset = game.mapTilesetOffsets[i]:value()
		if offset == 0xffffff then return end

		assert.ge(offset, 0)
		assert.le(offset, ffi.sizeof(game.mapTilesetsCompressed))

		local addr = offset + ffi.offsetof('game_t', 'mapTilesetsCompressed')
		local data = decompress(rom + addr, ffi.sizeof(game.mapTilesetOffsets))
		mapTilesetCache[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
		}
		return mapTilesetCache[i]
	end

	local mapTileGraphicsCache = table()	-- 0-based
	game.mapTileGraphicsCache = mapTileGraphicsCache
	function game.getMapTileGraphics(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTileGraphicsOffsets) then return end

		if mapTileGraphicsCache[i] then return mapTileGraphicsCache[i] end
		local offset = game.mapTileGraphicsOffsets[i]:value()

		assert.ge(offset, 0, 'mapTileGraphicsOffset['..i..']')
		assert.lt(offset, ffi.sizeof(game.mapTileGraphics), 'mapTileGraphicsOffset['..i..']')

		local addr = offset + ffi.offsetof('game_t', 'mapTileGraphics')
		mapTileGraphicsCache[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = rom + addr,
		}
		return mapTileGraphicsCache[i]
	end

	local mapTileGraphicsLayer3Cache = table()
	game.mapTileGraphicsLayer3Cache = mapTileGraphicsLayer3Cache
	function game.getMapTileGraphicsLayer3(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTileGraphicsLayer3Offsets) then return end

		if mapTileGraphicsLayer3Cache[i] then return mapTileGraphicsLayer3Cache[i] end
		local offset = game.mapTileGraphicsLayer3Offsets[i]:value()

		assert.ge(offset, 0, 'mapTileGraphicsLayer3Offsets['..i..']')
		assert.lt(offset, ffi.sizeof(game.mapTileGraphicsLayer3), 'mapTileGraphicsLayer3Offsets['..i..']')

		local addr = offset + ffi.offsetof('game_t', 'mapTileGraphicsLayer3')
		local data = decompress(rom + addr, ffi.sizeof(game.mapTileGraphicsLayer3))
		mapTileGraphicsLayer3Cache[i] = {
			index = i,
			offset = offset,
			addr = addr,
			data = data,
		}
		return mapTileGraphicsLayer3Cache[i]
	end

	-- NOTICE this decompresses always
	-- maybe decompress upon request only?
	game.worldInfos = table{
		'WoB',				-- gfxstr is 0x2480, layoutstr is 0x10000
		'WoR',				-- gfxstr is 0x2480, layoutstr is 0x10000
		'SerpentTrench',	-- gfxstr is 0x2480, layoutstr is 0x4000
	}:mapi(function(prefix, i)
		local gfxdatacompressed = game[prefix..'GfxDataCompressed']
		local gfxstr = decompress(
			ffi.cast('uint8_t*', gfxdatacompressed),
			ffi.sizeof(gfxdatacompressed)
		)
		local layoutcompressed = game[prefix..'LayoutCompressed']
		local layoutstr = decompress(
			ffi.cast('uint8_t*', layoutcompressed),
			ffi.sizeof(layoutcompressed)
		)

		local palsrc
		if i == 3 then
			palsrc = decompress(
				game.SerpentTrenchPalettesCompressed,
				ffi.sizeof(game.SerpentTrenchPalettesCompressed)
			)
		else
			palsrc = op.safeindex(game, prefix..'Palettes')
		end
		local palette = palsrc and makePalette(palsrc, 4, 16*8)

		local tilesetdata = gfxstr
		local gfxdata = ffi.cast('uint8_t*', gfxstr) + 0x400

		local tilePropsData = op.safeindex(game, prefix..'TileProps')

		local worldInfo = {
			prefix = prefix,
			gfxstr = gfxstr,
			tilesetdata = tilesetdata,							-- 0-0x400
			gfxdata = gfxdata,	-- 0x400 - 0x2400
			layoutstr = layoutstr,
			layoutdata = ffi.cast('uint8_t*', layoutstr),
			layoutSize = i == 3
				and vec2i(128, 128)
				or vec2i(256, 256),
			palette = palette,
			tilePropsData = tilePropsData,
		}
		return worldInfo 
	end)

	-- useful function for maps
	function game.layer1and2tile8x8toptr(tile8x8, gfxDatas)

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

	function game.layer1and2drawtile16x16(img, x, y, tile16x16, tilesetData, zLevelFlags, gfxDatas)
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
					local tileptr, bpp = game.layer1and2tile8x8toptr(tile8x8, gfxDatas)
					if tileptr then
						local highPal = bit.band(7, bit.rshift(tilesetTile, 10))
						local hFlip8 = bit.band(0x4000, tilesetTile) ~= 0
						local vFlip8 = bit.band(0x8000, tilesetTile) ~= 0
						readTile(img,
							x + bit.lshift(xofs, 3),
							y + bit.lshift(yofs, 3),
							tileptr,
							bpp,
							hFlip8,
							vFlip8,
							bit.lshift(highPal, bpp)
						)
					end
				end
			end
		end
	end

	function game.layer3tile8x8toptr(tile8x8, gfxLayer3Data)
		local bpp = 2
		if not gfxLayer3Data then return end
		-- 0x40 at the beginning of all layer3 tiles
		-- also there's only 0x40 unique tiles (only 6 bits are used), so i'm betting this is an extra bitflag that goes along with them ... maybe zLevel?
		local ofs = 0x40 + bit.band(0xff, tile8x8) * bit.lshift(bpp, 3)
		assert.lt(ofs, #gfxLayer3Data)
		local tileptr = ffi.cast('uint8_t*', gfxLayer3Data) + ofs
		return tileptr, bpp
	end

	function game.layer3drawtile16x16(img, x, y, tile16x16, gfxLayer3Data)
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
				local tileptr, bpp = game.layer3tile8x8toptr(tile8x8, gfxLayer3Data)
				if tileptr then
					readTile(
						img,
						x + bit.lshift(hFlip and (1-xofs) or xofs, 3),
						y + bit.lshift(vFlip and (1-yofs) or yofs, 3),
						tileptr,
						bpp,
						hFlip,
						vFlip
					)
				end
			end
		end
	end

	-- gfxData for world is just 0x400 + tilesetData ... they are combined in the same compressed blob
	function game.layer1worlddrawtile16x16(img, x, y, tile16x16, tilesetData, gfxData)
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
				readTileLinear(img,
					x + bit.lshift(xofs, 3),
					y + bit.lshift(yofs, 3),
					tileptr,
					bpp,
					hFlip8,
					vFlip8,
					bit.lshift(highPal, bpp)
				)
			end
		end
	end

	local MapInfo = class()
	MapInfo.init = table.union
	function MapInfo:getLayerImages()
		if self.layerImgs then return self.layerImgs end
		local map = self.map
		if not map then return end

		local gfxDatas = range(4):mapi(function(i)
			local gfx = self.gfxs[i]
			return gfx and gfx.data
		end)

		local layer1Size = self.layerSizes[1]

		local layerImgs = table()
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
			local blend = nil

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

			local layerSize = self.layerSizes[layer]
			local layout = self.layouts[layer]
			local layoutData = layout and layout.data
			if not (layout and layoutData) then
				print("missing layout "..layer, layout, layoutData)
			else
				local layerImg = Image(
					bit.lshift(layer1Size.x, 4),
					bit.lshift(layer1Size.y, 4),
					1,
					'uint8_t'
				):clear()
				layerImg.blend = blend
				layerImgs:insert(layerImg)
				layerImg.palette = self.palette

				local posx, posy = 0, 0
				if self.layerPos[layer]
				-- if we have a position for the layer, but we're using parallax, then the position is going to be relative to the view
				--and map.parallax == 0
				then
					posx, posy = self.layerPos[layer]:unpack()
				end
				local layoutptr = ffi.cast('uint8_t*', layoutData)
				for dstY=0,layer1Size.y-1 do
					local y = bit.lshift(dstY, 4)
					for dstX=0,layer1Size.x-1 do
						local x = bit.lshift(dstX, 4)
						local srcX = (dstX + posx) % layerSize.x
						local srcY = (dstY + posy) % layerSize.y
						local tile16x16 = layoutptr[((srcX + layerSize.x * srcY) % #layoutData)]

						if self.index < #game.worldInfos then
							if z == 0 and layer == 1 then
								game.layer1worlddrawtile16x16(layerImg, x, y, tile16x16, self.tilesetDatas[layer], gfxDatas[layer])
							end
						elseif layer == 3 then
							game.layer3drawtile16x16(layerImg, x, y, tile16x16, self.gfxLayer3 and self.gfxLayer3.data)
						else
							game.layer1and2drawtile16x16(layerImg, x, y, tile16x16, self.tilesetDatas[layer], bit.lshift(1, z), gfxDatas)
						end
					end
				end
			end
		end
		self.layerImgs = layerImgs
		return layerImgs
	end

	local mapInfoCache = table()
	game.mapInfoCache = mapInfoCache
	function game.getMap(mapIndex)
		mapIndex = math.floor((assert(tonumber(mapIndex))))
		if mapIndex < 0 or mapIndex >= countof(game.maps) then return end
		if game.mapInfoCache[mapIndex] then return game.mapInfoCache[mapIndex] end

		local map = game.maps + mapIndex
		local paletteIndex = tonumber(map.palette)

		local gfxIndexes = range(4):mapi(function(i)
			return tonumber(map['gfx'..i])
		end)
		local gfxs = range(4):mapi(function(i)
			return game.getMapTileGraphics(gfxIndexes[i])
		end)

		local gfxLayer3 = game.getMapTileGraphicsLayer3(tonumber(map.gfxLayer3))

		local tilesetDatas = table()
		for i=1,2 do
			local tilesetIndex = tonumber(map['tileset'..i])
			local tileset = game.getMapTileset(tilesetIndex)
			tilesetDatas[i] = tileset and tileset.data
		end

		local layerPos = table()
		local layerSizes = table()
		local layouts = table()
		for i=1,3 do
			local width = bit.lshift(1, 4 + map['layer'..i..'WidthLog2Minus4'])
			local height = bit.lshift(1, 4 + map['layer'..i..'HeightLog2Minus4'])
			layerSizes[i] = vec2i(width, height)
			local layoutIndex = tonumber(map['layout'..i])
			layouts[i] = layoutIndex > 0 and game.getMapLayout(layoutIndex) or nil
			if i > 1 then
				local ofs = map['layer'..i..'Pos']
				layerPos[i] = vec2i(ofs.x, ofs.y)
			end
		end

		local palette
		if paletteIndex >= 0 
		and paletteIndex < countof(game.mapPalettes)
		then
			palette = makePalette(game.mapPalettes + paletteIndex, 4, 16*8)
		end

		local tileProps = game.getMapTileProps(map.tileProps)
		local tilePropsData = tileProps and tileProps.data

		-- [[ special maps
		if mapIndex >= 0
		and mapIndex < #game.worldInfos
		then
			local worldInfo = game.worldInfos[mapIndex+1]
			local gfx = {}
			gfx.data = worldInfo.gfxdata
			gfxs[1] = gfx

			layerSizes[1] = worldInfo.layoutSize

			local layout = {}
			layout.data = worldInfo.layoutstr
			layouts[1] = layout

			tilesetDatas[1] = worldInfo.tilesetdata

			if worldInfo.palette then
				palette = worldInfo.palette
			end

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

--TODO just adding this print line makes a lot of maps work that otherwise didn't
-- hmmmmmm
print('mapInfo', mapIndex, 'palette', palette)
		local mapInfo = MapInfo{
			index = mapIndex,
			map = map,
			gfxIndexes = gfxIndexes,
			gfxs = gfxs,
			gfxLayer3 = gfxLayer3,
			tilesetDatas = tilesetDatas,
			layerPos = layerPos,
			layerSizes = layerSizes,
			layouts = layouts,
			palette = palette,
			tilePropsData = tilePropsData,
		}
		game.mapInfoCache[mapIndex] = mapInfo
		return mapInfo
	end
end
