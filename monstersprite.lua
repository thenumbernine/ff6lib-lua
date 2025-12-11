local ffi = require 'ffi'
local graphics = require 'graphics'
local makeTiledImageWithMask = graphics.makeTiledImageWithMask

local function writeMonsterSprite(
	game,
	index
)
	path'monsters':mkdir()

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

	-- weir that the tile-is-16-pixels bit is at the end of the 1st and not the 2nd byte ...
	local paletteIndex = bit.bor(monsterSprite.palLo, bit.lshift(monsterSprite.palHi, 8))
	local pal = game.monsterPalettes + paletteIndex

	local tileMaskData8, tileMaskData16
	do
		local addr1 = game.monsterSpriteTileMask8Ofs + 0x120000	-- 0x120000 = battleAnimGraphicsSets3bpp
		local addr2 = game.monsterSpriteTileMask16Ofs + 0x120000
		local addr3 = 0x12b300		-- item names -- the next section, so end of addr2's space

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
		tilesWide,		-- tiles wide
		tilesHigh,		-- tiles high
		bitsPerPixel,	-- 3 or 4
		tileMaskData,	-- mask bitvector
		monsterSprite.tileMaskIndex,	-- start of mask bitvector
		game.monsterSpriteData + offset * 8,
		pal
	)

	im:save('monsters/monster'..('%03d'):format(index)..' '..game.monsterNames[index]..'.png')

	return im.width * im.height
end
return writeMonsterSprite
