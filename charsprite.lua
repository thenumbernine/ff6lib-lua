local ffi = require 'ffi'
local Image = require 'image'
local graphics = require 'ff6.graphics'
local readTile = graphics.readTile
local tileWidth = graphics.tileWidth
local tileHeight = graphics.tileHeight

-- TODO get this from the game?
local spriteNames = {
	'terra',
	'locke',
	'cyan',
	'shadow',
	'edgar',
	'sabin',
	'celes',
	'strago',
	'relm',
	'setzer',
	'mog',
	'gau',
	'gogo',
	'umaro',
	'soldier',
	'imp',
	'leo',
	'banon',
	'morphedTerra',
	'merchant',
	'ghost',
	'kefka',
}
-- TODO get this from ... where?
local frameNames = {
	'walkd1',
	'standd',
	'walkd2',
	'walku1',
	'standu',
	'walku2',
	'walkl1',
	'standl',
	'walkl2',
	'wound',

	'ready',
	'pain',
	'stand',
	'swing',
	'handsupl1',
	'handsupl2',
	'cast1',
	'cast2',
	'dead',
	'eyesclosed',

	'winkd',
	'winkl',
	'handsupd',
	'handsupu',
	'growl',
	'saluted1',
	'saluted2',
	'saluteu1',
	'saluteu2',
	'laugh1',

	'laugh2',
	'startled',
	'sadd',
	'sadu',
	'sadl',
	'peeved',
	'finger1',
	'finger2',
	'jikuu',
	'tent',

	'dead2',
}

local tilesWide = 2
local tilesHigh = 3

local function readFrame(charIndex, im, charBasePtr, frameTileOffset, bitsPerPixel)
	local tileCount =
		charIndex < 87 and 6
		or charIndex < 116 and 5
		or 4

	-- characters have a set of ptrs-to-tiles (cuz they are reused often)
	-- no flags on/off (cuz the sprites are often dense/with no 8x8 holes)
	for spriteTileIndex=0,tileCount-1 do
		local x, y
		if charIndex < 87 then
			x = spriteTileIndex % 2
			y = (spriteTileIndex - x) / 2
		elseif charIndex < 116 then
			x = (spriteTileIndex+1) % 2
			y = (spriteTileIndex+1 - x) / 2
		else
			x = spriteTileIndex % 2
			y = (spriteTileIndex - x) / 2
		end

		local tile = charBasePtr + frameTileOffset[spriteTileIndex]
		readTile(im, x*tileWidth, y*tileHeight, tile, bitsPerPixel)
	end
end


local function readCharSprite(game, charIndex, processFrame)
	local rom = game.rom
	assert(charIndex >= 0 and charIndex < game.numCharacterSprites)

	local width = tileWidth*tilesWide
	local height = tileHeight*tilesHigh

	local palIndex = game.characterPaletteIndexes[charIndex]
--print('charIndex', charIndex, 'palIndex', palIndex)
--[[
	if palIndex >= 8 then palIndex = 0 end		-- TODO or idk
	if charIndex == 18 then palIndex = 8 end	-- special for morphed terra
--]]
-- [[
	palIndex = bit.band(palIndex, 7)
--]]

	local bitsPerPixel = 4

	local numFrames = game.getNumFramesForCharSpriteSheet(charIndex)

	for frameIndex=0,numFrames-1 do
-- points into fieldSpriteGraphics == 0x150000 ?
		local charBaseOffset = bit.band(
			bit.bnot(0xc00000),
			bit.bor(
				game.characterSpriteOffsetLo[charIndex],
				bit.lshift(game.characterSpriteOffsetHiAndSize[charIndex].hi, 16)
			))
		local im = Image(width, height, 1, 'uint8_t')
			:clear()
		readFrame(charIndex, im,
			rom + charBaseOffset,
			game.characterFrameTileOffsets + frameIndex * tilesWide * tilesHigh,
			bitsPerPixel)

		processFrame(charIndex, frameIndex, im, palIndex)
	end
end

return readCharSprite
