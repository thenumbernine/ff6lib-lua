#!/usr/bin/env luajit
--[[
oh yeah, 0x150000 - 0x185000 is where the tile data for character sprites goes
but the end of it is vehicles
so this is now the vehicle-extraction script
--]]
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
local assert = require 'ext.assert'

local uint8_t = ffi.typeof'uint8_t'

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

	local tilesImg = Image(tileWidth*tilesWide, tileHeight*tilesHigh, 1, uint8_t):clear()

	--everything8215 vehicleGraphics says mapSpritePalettes[7] and [11]
	tilesImg.palette = table.append(
		makePalette(game, game.characterPalettes + 7, 4, 16),
		makePalette(game, game.characterPalettes + 11, 4, 16)
	)

	local tileImgs = table()
	local tileIndex = 0
	for ty=0,tilesHigh-1 do
		for tx=0,tilesWide-1 do
			local tileImg = Image(8, 8, 1, uint8_t):clear()
			local palor = tileIndex < 32 and 0x10 or 0
			readTile(tileImg, 0, 0, ptr, bpp, false, false, palor)
			tilesImg:pasteInto{image=tileImg, x=tx*tileWidth, y=ty*tileHeight}
			ptr = ptr + 32
			tileIndex = tileIndex + 1
			tileImgs:insert(tileImg)
		end
	end

	tilesImg:save'vehicle-tiles.png'

	-- now rearrange them and put them into a 256x256 sprite-sheet (this is ff6t3d-specific)
	-- is it just me or does it look like the tiles were meant to be laid out in rows of 16 to make a 128x128 sheet?
	-- i'll just go with that ....
	local tile16x16Imgs = table()
	for j=0,7 do
		for i=0,7 do
			tile16x16Imgs:insert(tilesImg:copy{x=i*16, y=j*16, width=16, height=16})
		end
	end
	assert.len(tile16x16Imgs, 64)

	local sheetImg = Image(256,256,1,uint8_t):clear()
	sheetImg.palette = tilesImg.palette

	local i = 0
	local function pasteNext(args)
		i = i + 1
		args.image = tile16x16Imgs[i]
		sheetImg:pasteInto(args)
	end
	-- raft up/down
	pasteNext{x=6*16, y=0*16}
	pasteNext{x=6*16, y=1*16}
	pasteNext{x=7*16, y=0*16}
	pasteNext{x=7*16, y=1*16}
	-- raft left/right
	pasteNext{x=6*16, y=2*16}
	pasteNext{x=6*16, y=3*16}
	pasteNext{x=7*16, y=2*16}
	pasteNext{x=7*16, y=3*16}

	pasteNext{x=80, y=96}	-- chocobo head
	pasteNext{x=0, y=96}	-- headless chocobo tail facing down
	pasteNext{x=0, y=112}	--
	pasteNext{x=16, y=96}	-- headless chocobo tail facing down #2
	pasteNext{x=16, y=112}	--
	pasteNext{x=80, y=112}	-- chocobo tail
	pasteNext{x=32, y=96}	-- headless chocobo tail facing up
	pasteNext{x=32, y=112}	--
	pasteNext{x=48, y=96}	-- headless chocobo tail facing up #2
	pasteNext{x=48, y=112}	--
	-- chocobo standing left
	pasteNext{x=0, y=128}
	pasteNext{x=0, y=144}
	pasteNext{x=16, y=128}
	pasteNext{x=16, y=144}
	-- chocobo run left #1
	pasteNext{x=32, y=128}
	pasteNext{x=32, y=144}
	pasteNext{x=48, y=128}
	pasteNext{x=48, y=144}
	-- chocobo run left #2
	pasteNext{x=64, y=128}
	pasteNext{x=64, y=144}
	pasteNext{x=80, y=128}
	pasteNext{x=80, y=144}

	pasteNext{x=64, y=96}	-- chocobo wark
	pasteNext{x=64, y=112}	-- chocobo eyes closed

	-- magitek stand d
	pasteNext{x=0, y=0}
	pasteNext{x=0, y=16}
	-- and then the last two, hflipped

	-- magitek walk d
	pasteNext{x=32, y=0}
	pasteNext{x=32, y=16}
	-- and then walk d #2 hflipped

	-- magitek walk d #2
	pasteNext{x=64, y=0}
	pasteNext{x=64, y=16}
	-- and then walk d #1 hflipped

	-- magitek stand u
	pasteNext{x=0, y=32}
	pasteNext{x=0, y=48}
	-- and then the last two, hflipped

	-- magitek walk u
	pasteNext{x=32, y=32}
	pasteNext{x=32, y=48}
	-- and then walk u #2 hflipped

	-- magitek walk u #2
	pasteNext{x=64, y=32}
	pasteNext{x=64, y=48}
	-- and then walk u #1 hflipped

	-- stand l
	pasteNext{x=0, y=64}
	pasteNext{x=16, y=64}
	pasteNext{x=0, y=80}
	pasteNext{x=16, y=80}
	-- walk l #1
	pasteNext{x=32, y=64}
	pasteNext{x=48, y=64}
	pasteNext{x=32, y=80}
	pasteNext{x=48, y=80}
	-- walk l #2
	pasteNext{x=64, y=64}
	pasteNext{x=80, y=64}
	pasteNext{x=64, y=80}
	pasteNext{x=80, y=80}
	-- walk l #3
	pasteNext{x=96, y=64}
	pasteNext{x=112, y=64}
	pasteNext{x=96, y=80}
	pasteNext{x=112, y=80}

	sheetImg:save'vehicle-sheet.png'
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local game = require 'ff6'(( assert(path((...)):read()) ))
	run(game)
end

return run
