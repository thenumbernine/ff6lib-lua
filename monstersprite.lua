local ffi = require 'ffi'
local graphics = require 'ff6.graphics'
local makeTiledImageWithMask = graphics.makeTiledImageWithMask

local function readMonsterSprite(
	game,
	index
)
	local Game = game.Game
	local rom = game.rom
	local monsterSprite = game.monsterSprites[index]

	local offset = monsterSprite.offset

	local bitsPerPixel
	if monsterSprite._3bpp ~= 0 then
		bitsPerPixel = 3
	else
		bitsPerPixel = 4
	end

	-- now find a monster image with a matching offset...

	-- weird that the tile-is-16-pixels bit is at the end of the 1st and not the 2nd byte ...
	local paletteIndex = monsterSprite:getPaletteIndex()

	-- there are 0x300 x palettes of 8 colors each = 0x1800 colors
	-- = 0x18 = 24 palettes of x 0x100 = 256 colors
	-- and it looks like the last few aren't even used ...
	local pal = game.monsterPalettes + paletteIndex

	local tileMaskData8, tileMaskData16
	do
		local baseaddr = ffi.offsetof(Game, 'battleAnimGraphicsSets3bpp')	-- 0x120000 = battleAnimGraphicsSets3bpp
		local addr1 = game.monsterSpriteTileMask8Ofs + baseaddr
		local addr2 = game.monsterSpriteTileMask16Ofs + baseaddr
		local addr3 = ffi.offsetof(Game, 'itemNames')	-- 0x12b300 = the next section, so end of addr2's space

		local numTileMasks8 = bit.rshift((addr2 - addr1), 3)
		local numTileMasks16 = bit.rshift((addr3 - addr2), 5)

--print('addr1', addr1:hex(), 'numTileMasks8', numTileMasks8:hex())
--print('addr2', addr2:hex(), 'numTileMasks16', numTileMasks16:hex())
--print('addr3', addr3:hex())

		-- 8 bytes, each byte is a row, each bit is a column flag
		tileMaskData8 = rom + addr1
		-- by default points to start of monsterSpriteTileMaskData
		--, numTileMasks8 * 8)

		-- 16 shorts, each short is a row, each bit is a column flag
		tileMaskData16 = rom + addr2
		-- by default points inside of monsterSpriteTileMaskData
		--, numTileMasks16 * 32)
	end

	local tile16 = monsterSprite.tile16 == 1

	-- bitflags of which 8x8 tiles are used
	local tileMaskData
	if not tile16 then
		tileMaskData = tileMaskData8
	else
		tileMaskData = tileMaskData16
	end

	local tilesWide, tilesHigh
	if not tile16 then
		tilesWide = 8
		tilesHigh = 8
	else
		tilesWide = 16
		tilesHigh = 16
	end

	local im = makeTiledImageWithMask(
		game,
		tilesWide,		-- tiles wide
		tilesHigh,		-- tiles high
		bitsPerPixel,	-- 3 or 4
		tileMaskData,	-- mask bitvector
		monsterSprite.tileMaskIndex,	-- start of mask bitvector
		game.monsterSpriteData + offset * 8,

		-- hmm, all pal does is be used to create a palette table, so maybe I don't have to pass it?
		pal
	)

	return im
end
return readMonsterSprite
