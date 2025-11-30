local ffi = require 'ffi'
local op = require 'ext.op'
local assert = require 'ext.assert'
local vec2i = require 'vec-ffi.vec2i'
local makePalette = require 'ff6.graphics'.makePalette

return function(game)
	local rom = ffi.cast('uint8_t*', game.s)
	local countof = game.countof
	local decompress = game.decompress
	local romsize = game.romsize

	local mapLayoutCache = table()	-- 0-based
	game.mapLayoutCache = mapLayoutCache 
	game.getMapLayout = function(i)
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
	game.getMapTileProps = function(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTilePropsOffsets) then return end
		
		if mapTilePropsCache[i] then return mapTilePropsCache[i] end
		local offset = game.mapTilePropsOffsets[i]
		if offset == 0xffff then return end
		
		assert.ge(offset, 0)
		assert.lt(offset, ffi.sizeof(game.mapTilePropsCompressed))

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
	game.getMapTileset = function(i)
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
	game.getMapTileGraphics = function(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTileGraphicsOffsets) then return end

		if mapTileGraphicsCache[i] then return mapTileGraphicsCache[i] end
		local offset = game.mapTileGraphicsOffsets[i]:value()
		
		assert.ge(offset, 0)
		assert.lt(offset, ffi.sizeof(game.mapTileGraphics))

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
	game.getMapTileGraphicsLayer3 = function(i)
		i = math.floor((assert(tonumber(i))))
		if i < 0 or i >= countof(game.mapTileGraphicsLayer3Offsets) then return end

		if mapTileGraphicsLayer3Cache[i] then return mapTileGraphicsLayer3Cache[i] end
		local offset = game.mapTileGraphicsLayer3Offsets[i]:value()
		
		assert.ge(offset, 0)
		assert.lt(offset, ffi.sizeof(game.mapTileGraphicsLayer3))

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

		return {
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
	end)
end
