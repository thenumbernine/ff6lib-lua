#!/usr/bin/env luajit
--[[
oh yeah, 0x150000 - 0x185000 is where the tile data for character sprites goes
but the end of it is vehicles
so this is now the vehicle-extraction script
--]]
local path = require 'ext.path'
local table = require 'ext.table'

local function run(game)

	local Image = require 'image'
	local makePalette = require 'ff6.graphics'.makePalette
	local readTile = require 'ff6.graphics'.readTile
	-- 8x8 right?
	local tileWidth = require 'ff6.graphics'.tileWidth
	local tileHeight = require 'ff6.graphics'.tileHeight
	local bpp = 4

	--4bpp means 32 bytes per 8x8 tile ...
	--[[ 0x150000 - 0x185000 has 6784 8x8 tiles = 1696 16x16 tiles
	local ptr = game.fieldSpriteGraphics
	local tilesWide = 64
	local tilesHigh = 106
	--]]
	-- [[ 0x183000 - 0x185000 has 256 8x8 tiles = 64 16x16 tiles
	local ptr = game.rom + 0x183000
	local tilesWide = 16
	local tilesHigh = 16
	--]]

	local im = Image(tileWidth*tilesWide, tileHeight*tilesHigh, 1, 'uint8_t'):clear()

	--everything8215 vehicleGraphics says mapSpritePalettes[7] and [11]
	im.palette = table.append(
		makePalette(game, game.characterPalettes + 7, 4, 16),
		makePalette(game, game.characterPalettes + 11, 4, 16)
	)

	local tileIndex = 0
	for ty=0,tilesHigh-1 do
		for tx=0,tilesWide-1 do
			local palor = tileIndex < 32 and 0x10 or 0
			readTile(im, tx*tileWidth, ty*tileHeight, ptr, bpp, false, false, palor)
			ptr = ptr + 32
			tileIndex = tileIndex + 1
		end
	end

	im:save'field-gfx.png'
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local game = require 'ff6'(( assert(path((...)):read()) ))
	run(game)
end

return run
