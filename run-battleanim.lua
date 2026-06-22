local ffi = require 'ffi'
local Image = require 'image'
local makePalette = require 'graphics'.makePalette
local makePaletteSets = require 'graphics'.makePaletteSets
local tileWidth = require 'graphics'.tileWidth
local tileHeight = require 'graphics'.tileHeight
local readTile = require 'graphics'.readTile

--[[
lets try to make sense of this mess...

battleAnimScriptOffsets[i]
	points to byte offset within battleAnimScripts[]

battleAnimScripts[i]
	handles frame playback of battleAnimSets[]
	(TODO make structs out of this like I do for event-scripts in scripts.lua)

battleAnimSets[i]
	.wait is how long to wait
	.sound is what sound to play
	for j in 0,1,2:
		.palette[j] points into battleAnimPalettes[]
		.effect[j] points into battleAnimEffects[]
battleAnimEffects[i] ... this is one animated sequence, i.e. a collection of frames.
	.numFrames = how many frames in this animation-set's animation-effect's animation
	.width, .height = frame size, in 8x8 tiles
	._2bpp is true for 2bpp, false for 3bpp
	.graphicSet | (.graphicSetHighBit<<8)
		points into BattleAnim8x8Tile list, which are 16x4 x 8x8 tiles
			2bpp list addr is * 0x40 + 0x12C000
			3bpp list addr is * 0x40 + 0x120000
	.frameIndexBase
		index into battleAnimFrame16x16TileOffsets[] to get effectFrame16x16TileOffsetPtr
	for frameIndex in 0..numFrames-1:
		frame16x16TilesPtr addr =
			0x110000
			+ effectFrame16x16TileOffsetPtr[frameIndex]
		... notice all these addrs span from 0x110141 to 0x11e96b,
			so it is the 'battleAnimFrame16x16Tiles'
frame16x16TilesPtr points to a list of BattleAnim16x16Tile's = list of 16x16 tiles
	.x, .y = in 16x16 tile units, destination into this frame to draw this 16x16 tile
	.tile = index into graphicSet's 64x4 location of 8x8 tiles
		tile8x8DataBaseAddr + tileLen * graphicSetTile.tile
	.hflip16, .vflip16 = how to flip the 16x16 tile
BattleAnim8x8Tile list holds:
	.tile = address into tile8x8DataBaseAddr + tileLen * graphicSetTile.tile
	.hflip = hflip 8x8
	.vflip = vflip 8x8


alltiles-2bpp is 512x184
alltiles-3bpp is 512x616
total is 512x800
i.e. 256x1600 i.e. 6.25 x 256x256 sheets
--]]

local battleAnimGraphicSetsPath = path'battleanim_graphicsets'
battleAnimGraphicSetsPath:mkdir()

return function(game)
	local rom = game.rom
	local romsize = game.romsize
	local countof = game.countof
	local graphicSetsUsed = table()
	local paletteForTileIndex = {}
	local palettesUsed = table()
	local frame16x16TileAddrInfo = table()


	-- [==[ interface layer for game.*

	--[[ using the originals?
	local battleAnimSets = game.battleAnimSets
	local battleAnimEffects = game.battleAnimEffects
	local battleAnimFrame16x16TileOffsets = game.battleAnimFrame16x16TileOffsets
	local battleAnimGraphicsSets2bpp = game.battleAnimGraphicsSets2bpp
	local battleAnimGraphicsSets3bpp = game.battleAnimGraphicsSets3bpp
	local battleAnimFrame16x16Tiles = game.battleAnimFrame16x16Tiles
	--]]
	-- [[ lets try to separate the blobs and still reconstruct the same data correctly
	local battleAnimSets = ffi.new(ffi.typeof('$[?]', game.BattleAnimSet), countof(game.battleAnimSets))
	ffi.copy(battleAnimSets, game.battleAnimSets, ffi.sizeof(battleAnimSets))
	battleAnimGraphicSetsPath'animsets.bin':write(ffi.string(battleAnimSets, ffi.sizeof(battleAnimSets)))

	local battleAnimEffects = ffi.new(ffi.typeof('$[?]', game.BattleAnimEffect), countof(game.battleAnimEffects))
	ffi.copy(battleAnimEffects, game.battleAnimEffects, ffi.sizeof(battleAnimEffects))
	battleAnimGraphicSetsPath'animeffects.bin':write(ffi.string(battleAnimEffects, ffi.sizeof(battleAnimEffects)))

	local battleAnimFrame16x16TileOffsets = ffi.new('uint16_t[?]', 4194)
	ffi.copy(battleAnimFrame16x16TileOffsets, game.battleAnimFrame16x16TileOffsets, ffi.sizeof(battleAnimFrame16x16TileOffsets))
	battleAnimGraphicSetsPath'animframe16x16offsets.bin':write(ffi.string(battleAnimFrame16x16TileOffsets, ffi.sizeof(battleAnimFrame16x16TileOffsets)))

	local battleAnimGraphicsSets2bpp = ffi.new(ffi.typeof('$[?]', game.BattleAnim8x8Tile), 0x20 * 0xb0)
	ffi.copy(battleAnimGraphicsSets2bpp, game.battleAnimGraphicsSets2bpp, ffi.sizeof(battleAnimGraphicsSets2bpp))
	battleAnimGraphicSetsPath'graphicsets2bpp.bin':write(ffi.string(battleAnimGraphicsSets2bpp, ffi.sizeof(battleAnimGraphicsSets2bpp)))

	local battleAnimGraphicsSets3bpp = ffi.new(ffi.typeof('$[?]', game.BattleAnim8x8Tile), 0x20 * 0x180)
	ffi.copy(battleAnimGraphicsSets3bpp, game.battleAnimGraphicsSets3bpp, ffi.sizeof(battleAnimGraphicsSets3bpp))
	battleAnimGraphicSetsPath'graphicsets3bpp.bin':write(ffi.string(battleAnimGraphicsSets3bpp, ffi.sizeof(battleAnimGraphicsSets3bpp)))

	local battleAnimFrame16x16Tiles = ffi.new(ffi.typeof('$[?]', game.BattleAnim16x16Tile), 0x74cb)
	ffi.copy(battleAnimFrame16x16Tiles, game.battleAnimFrame16x16Tiles, ffi.sizeof(battleAnimFrame16x16Tiles))
	battleAnimGraphicSetsPath'animframe16x16tiles.bin':write(ffi.string(battleAnimFrame16x16Tiles, ffi.sizeof(battleAnimFrame16x16Tiles)))

	-- no need to save 'battleAnimGraphics2bpp/3bpp, because that's tile data, stored in the tile sheets
	--]]
	--]==]

	local battleAnimGraphicSetsPerBpp = {
		[2] = battleAnimGraphicsSets2bpp,
		[3] = battleAnimGraphicsSets3bpp,
	}

	local infoPerBpp = {
		[2] = {
			-- 16x16 info in 8x8 partitions
			graphicSetBaseAddr = 0x12C000,	-- battleAnimGraphicsSets2bpp
			-- 8x8 tile data:
			tile8x8DataBaseAddr = 0x187000,	-- battleAnimGraphics2bpp
			tile8x8DataEndAddr = 0x18c9a0,
		},
		[3] = {
			graphicSetBaseAddr = 0x120000, 	-- battleAnimGraphicsSets3bpp
			tile8x8DataBaseAddr = 0x130000,	-- battleAnimGraphics3bpp
			tile8x8DataEndAddr = 0x14c998,
		},
	}

	-- total # of 8x8 tiles saved
	-- to give me a rough texture-atlas idea if I want to save the expanded tiles
	local totalTilesSaved = 0

	local battleAnimSetPath = path'battleanim'
	battleAnimSetPath:mkdir()
	for battleAnimSetIndex=0,countof(game.battleAnimSets)-1 do
		local battleAnim = battleAnimSets + battleAnimSetIndex
		print('battleAnimSet['..battleAnimSetIndex..'] = '..battleAnim)

		for j=0,2 do
			-- TODO array plz, but then TODO serialzie arrays in 'struct' please
			local effectIndex = battleAnim['effect'..(j+1)]
			local paletteIndex = battleAnim['palette'..(j+1)]

			if effectIndex ~= 0xffff then
				local dontRunScript = 0 ~= bit.band(0x8000, effectIndex)
				effectIndex = bit.band(0x7fff, effectIndex)
				if effectIndex >= countof(game.battleAnimEffects) then
					-- NO MORE OF THESE ERRORS BEING HIT, NICE
					print('!!! effect is oob !!! '..('%x'):format(effectIndex))
				else
					local effect = battleAnimEffects + effectIndex
					print('\t\teffect'..(j+1)..'='..effect)

					local effectFrame16x16TileOffsetPtr = battleAnimFrame16x16TileOffsets + effect.frameIndexBase
					local graphicSetIndex = bit.bor(effect.graphicSet, bit.lshift(effect.graphicSetHighBit, 8))

					graphicSetsUsed[graphicSetIndex] = graphicSetsUsed[graphicSetIndex]
						or {
							effectDisplayIndex = {},
							palettes = {},
						}
					graphicSetsUsed[graphicSetIndex].effectDisplayIndex[j] = true
					graphicSetsUsed[graphicSetIndex].palettes[paletteIndex] = true
					palettesUsed[paletteIndex] = true

					local bpp = effect._2bpp == 1 and 2 or 3
					local info = infoPerBpp[bpp]

					-- number of BattleAnim8x8Tile entries into the battleAnimGraphicSets[bpp] array (2 bytes each)
					local graphicSetOffset = graphicSetIndex * 0x20
					local graphicSetAddr = info.graphicSetBaseAddr + graphicSetOffset * ffi.sizeof(game.BattleAnim8x8Tile)
					--local graphicSetTiles = ffi.cast(ffi.typeof('$*', game.BattleAnim8x8Tile), rom + graphicSetAddr)
					local graphicSetTiles = battleAnimGraphicSetsPerBpp[bpp] + graphicSetOffset

					local tileLen = bit.lshift(bpp, 3)
					print('\t\teffectAddr=0x'..graphicSetAddr:hex()
						..', tileLen=0x'..tileLen:hex())

					local numFrames = effect.numFrames
					-- https://web.archive.org/web/20190907020126/https://www.ff6hacking.com/forums/thread-925.html
					-- ... says dont use the last 2 bits
					numFrames = bit.band(0x3f, numFrames)
					for frameIndex=0,numFrames-1 do
						print('\t\t\tframeIndex=0x'..frameIndex:hex()..':')

						local frame16x16TilesAddr = 0x110000 + effectFrame16x16TileOffsetPtr[frameIndex]  -- somewhere inside battleAnimFrame16x16Tiles
						print('\t\t\t\tframe16x16TilesAddr=0x'..frame16x16TilesAddr:hex())

						--[[
						ok i've got a theory.
						that the frame16x16TilesAddr (list of BattleAnim16x16Tile's)
						 is going to be the unique identifier of an animation frame (except palette swaps).
						lets see if each frame16x16TilesAddr maps to always use the same graphicSetIndex
						i.e. they will have the same .bpp and .graphicSetIndex
						--]]
						frame16x16TileAddrInfo[frame16x16TilesAddr] = frame16x16TileAddrInfo[frame16x16TilesAddr] or table()
						local key = '0x'..graphicSetIndex:hex()..'/'..bpp
						frame16x16TileAddrInfo[frame16x16TilesAddr][key] = true

						-- convert the 0x110000-relative address into an battleAnimFrame16x16Tiles[] index since thats where it points after all
						local battleAnimFrame16x16TilesAddr = ffi.cast('uint8_t*', game.battleAnimFrame16x16Tiles) - ffi.cast('uint8_t*', rom)
						local animFrame16x16TileOffset = frame16x16TilesAddr - battleAnimFrame16x16TilesAddr
						assert.le(0, animFrame16x16TileOffset)
						assert.lt(animFrame16x16TileOffset, ffi.sizeof(game.battleAnimFrame16x16Tiles))	-- will this sizeof work?
						-- make sure its aligned to BattleAnim16x16Tile
						assert.eq(0, bit.band(1, animFrame16x16TileOffset))
						local animFrame16x16TileIndex = bit.rshift(animFrame16x16TileOffset, 1)

						-- using the 0x110000 address offset:
						--local frame16x16TilesPtr = ffi.cast(ffi.typeof('$*', game.BattleAnim16x16Tile), rom + frame16x16TilesAddr)
						-- using the battleAnimFrame16x16Tiles struct offset (where the ptr goes anyways):
						--local frame16x16TilesPtr = game.battleAnimFrame16x16Tiles + animFrame16x16TileIndex
						-- using the extracted binary blob:
						local frame16x16TilesPtr = battleAnimFrame16x16Tiles + animFrame16x16TileIndex

						local im = Image(
							2*tileWidth * effect.width,
							2*tileHeight * effect.height,
							1,
							'uint8_t'
						)
							:clear()

						-- looking for ways to test the tile count per-frame
						-- I think tracking the tile order is the best way
						local lastTileOrder
						for frameTile16x16Index=0,math.huge-1 do
							local battleAnim16x16Tile = frame16x16TilesPtr + frameTile16x16Index
							local x = battleAnim16x16Tile.x
							local y = battleAnim16x16Tile.y
							-- is an oob tile an end as well?
							if x >= effect.width then break end
							if y >= effect.height then break end
							local tileOrder = x + effect.width * y
							if lastTileOrder and lastTileOrder >= tileOrder then break end
							lastTileOrder = tileOrder

							totalTilesSaved = totalTilesSaved + 1

							print('\t\t\t\t\tbattleAnim16x16Tile='..battleAnim16x16Tile)
							-- paste into image
							for yofs=0,1 do
								for xofs=0,1 do
									-- this makes a lot more sense if you look it up in the 'alltiles' image below
									-- TLDR: Make a 16x8 tile display out of the 8x8 tiles pointed to by graphicSetTiles[]
									-- You'll see they make up 16x16-pixel regions
									-- Those are what we are indexing here, hence why you have to pick apart battleAnim16x16Tile.tile into its lower 3 bits and its upper 5
									local graphicSetTile = graphicSetTiles + bit.bor(
										-- bit 0 is xofs
										xofs,
										-- bits 123 is tile bits 012
										bit.lshift(
											bit.band(
												battleAnim16x16Tile.tile,
												7
											),
											1
										),
										-- bit 4 is yofs
										bit.lshift(yofs, 4),
										-- bits 567 is tile bits 345
										bit.lshift(
											-- .tile is 6 bits, so truncate 3 lower bits <-> & 0x38
											bit.band(
												battleAnim16x16Tile.tile,
												0x38
											),
										2)
									)
									local vflip = 0 ~= graphicSetTile.vflip
									local hflip = 0 ~= graphicSetTile.hflip
--print('xofs', xofs:hex(), 'yofs', yofs:hex(), 'graphicSetTile', graphicSetTile)
									-- 16384 indexable, but points to 0x130000 - 0x14c998, which only holds 4881
									paletteForTileIndex[graphicSetTile.tile] = paletteIndex
									local tile8x8DataAddr = info.tile8x8DataBaseAddr + tileLen * graphicSetTile.tile
									local xformxofs = xofs
									if battleAnim16x16Tile.hflip16 ~= 0 then
										xformxofs = 1 - xformxofs
										hflip = not hflip
									end
									local xformyofs = yofs
									if battleAnim16x16Tile.vflip16 ~= 0 then
										xformyofs = 1  - xformyofs
										vflip = not vflip
									end
									readTile(
										im,
										bit.bor(bit.lshift(x, 1), xformxofs)*tileWidth,
										bit.bor(bit.lshift(y, 1), xformyofs)*tileHeight,
										rom + tile8x8DataAddr,
										bpp,
										hflip,
										vflip
									)
								end
							end
						end

						local paltable = makePalette(game, game.battleAnimPalettes + paletteIndex, bpp, bit.lshift(1, bpp))
						im.palette = paltable
						im:save(battleAnimSetPath(
							('%03d'):format(battleAnimSetIndex)
							..('-%d'):format(j)	-- effect1,2,3
							..('-%02d'):format(frameIndex)
							..'.png').path)
					end
					print()
				end
			end
		end
	end
	print()

	print('total 8x8 tiles used for battle animations:', totalTilesSaved)
	print()

	local uniquePalettesUsed = palettesUsed:keys():sort()
	print('palettes used #:', uniquePalettesUsed:mapi(function(i) return '0x'..i:hex() end):concat', ')
	print('...'..#uniquePalettesUsed..' unique palettes')
	-- TODO these are palette8's used for 3bpp, but 2bpp just needs 4 colors ...
	-- 175 of 240 palettes are used ...
	-- ... I'll just copy them all into palette blobs.
	print()

	-- [[ graphic sets for effect #3 is supposed to have a different base address, hmmm...
	local uniqueGraphicSets = graphicSetsUsed:keys():sort()
	print('graphicSets used', uniqueGraphicSets:mapi(function(i) return '0x'..i:hex() end):concat', ')
	print('...'..#uniqueGraphicSets..' unique graphic sets')
	print()
	--]]

	-- so this is basically a plot of the entire pointer table at 0x120000
	--
	-- honestly this comes from a unique combo of graphicSet & effect 123 index (3 has a dif base)
	-- so I don't need to make so many copies ...
	--
	-- also each 'graphicSet' number is just 8 tiles worth of 16x16 tiles
	-- each 'graphicSet' is only addressible by 64 8x8 tiles = 16 16x16 tiles
	-- (because it's a byte, and its high two bits are used for hflip & vflip, so 64 values)
	-- so each 'graphicSet' is going to share 8 16x16 tiles in common with the next 'graphicSet'
	-- so 'graphicSet' is really '8x start of location in 8-tile-rows of 16x16 tileset'
	--
	-- so 1 'graphicSet' tiles is 16x4 of 8x8 = 8x2 of 16x16 = 128x32
	-- so 256 'graphicSets' with their even overlapping rows excluded is 128 x (32x128) = 128 x 4096
	-- but I could square this circle to be 512 x 1024
	-- ... but are there more than 256 addressible?
	-- yup there are.  so how do you address them, with just 1 byte?
	for bpp=2,3 do
		-- graphicSet * 0x40 + 0x120000 points to the table of u16 offsets
		-- the region 0x120000-0x126000 is for 'monster sprite tile mask data' ... nah, that's really just for this data.
		--		it's named 'monster' cuz monsters use 120000 as the base addr for their tile mask data,
		-- 		but really theirs always addresses into 'monsterSpriteTileMaskData'
		-- so that means there's only room for 0x180 of 0x40 within this region.
		-- so max graphics set is 0x180 ?  but how to index beyond 1 bytes worth?
		-- also
		-- monsters use 12a824-12ac24 for 8x8 tile masks
		-- and 12ac24-12b300 for 16x16 tile masks
		--local maxGraphicSet = 256
		local maxGraphicSet = assert.index({
			-- ??? wait, if its 2bpp then inc by 0x40 means skipping a full graphicsSet instead of just half...
			[2] = 0xb0,
			[3] = 384,
		}, bpp)

		-- 1 graphic set is (8x8) x (16x4)
		local setWidth = 16 * tileWidth
		local setHeight = 4 * tileHeight
		local masterSetsWide = 4	-- i'll make 4 cols of them
		local masterSetsHigh = math.ceil(maxGraphicSet/2/masterSetsWide)
		local master = Image(
			setWidth * masterSetsWide,
			setHeight * masterSetsHigh,
			4,	-- rgba
			'uint8_t'
		):clear()

		-- only plot the even graphicSetIndex tiles cuz the odd ones have a row in common
		for graphicSetIndex=0,maxGraphicSet-1,2 do
			assert.eq(bit.band(graphicSetIndex, 1), 0, "this wont be aligned in the master image")

			local halfGraphicsSetIndex = bit.rshift(graphicSetIndex, 1)
			local masterRow = halfGraphicsSetIndex % masterSetsHigh
			local masterCol = (halfGraphicsSetIndex - masterRow) / masterSetsHigh

			local graphicSetInfo = graphicSetsUsed[graphicSetIndex]
--print('graphicSet '..graphicSetIndex)
			local paletteIndex = 0
			local j = 0
			if graphicSetInfo then
				local effectDisplayIndexes = table.keys(graphicSetInfo.effectDisplayIndex):sort()
--print(' uses effect display indexes: '..effectDisplayIndexes:concat', ')
				j = effectDisplayIndexes[1] or 0
				local palettes = table.keys(graphicSetInfo.palettes):sort()
--print(' uses palettes: '..palettes:concat', ')
				paletteIndex = palettes:last() or 0
			end

			local tileLen = bit.lshift(bpp, 3)
			local info = infoPerBpp[bpp]
			local graphicSetTiles = battleAnimGraphicSetsPerBpp[bpp] + graphicSetIndex * 0x20

			local im = Image(
				0x10*tileWidth,
				4*tileHeight,
				1, 'uint8_t'
			)
			im.palette = makePalette(game, game.battleAnimPalettes + paletteIndex, bpp, bit.lshift(1, bpp))
			for y=0,3 do
				for x=0,15 do
					local graphicSetTile = graphicSetTiles + (x + 0x10 * y)
					local tile8x8DataAddr = info.tile8x8DataBaseAddr + tileLen * graphicSetTile.tile
					readTile(
						im,
						x * tileWidth,
						y * tileHeight,
						rom + tile8x8DataAddr,
						bpp,
						0 ~= graphicSetTile.hflip,
						0 ~= graphicSetTile.vflip
					)
				end
			end
			master:pasteInto{
				image = im:rgba(),
				x = masterCol * 128,
				y = masterRow * 32,
			}
			--im:save(battleAnimGraphicSetsPath(('%03d'):format(graphicSetIndex)..'.png').path)
		end
		master:save(battleAnimGraphicSetsPath('battle_anim_graphic_sets_'..bpp..'bpp.png').path)
	end

	print'frame16x16TileAddrInfo={'
	for _,addr in ipairs(frame16x16TileAddrInfo:keys():sort()) do
		print('\t[0x'..addr:hex()..'] = {'
			..frame16x16TileAddrInfo[addr]:keys():sort():concat', '
			..'},')
	end
	print'}'
	print()

	-- what about plotting the entire tile data?
	-- this is the data at 0x130000 - 0x14c998
	-- it's going to be 3bpp 8x8 data , so there will be 4881 of them
	do
		local tilesPerSheetInBits = 5
		local tilesPerSheetSize = bit.lshift(1, tilesPerSheetInBits)
		local sheetSize = tilesPerSheetSize * tileWidth	-- == tileHeight
		local tilesPerSheetMask = tilesPerSheetSize-1

		local tileImg = Image(tileWidth, tileHeight, 1, 'uint8_t')

		for bpp=2,3 do
			local info = infoPerBpp[bpp]
			tileImg:clear()

			local allTileSheets = table()

			local tileSizeInBytes = bit.lshift(bpp, 3)
			local totalTiles = math.floor((info.tile8x8DataEndAddr - info.tile8x8DataBaseAddr) / tileSizeInBytes)

			for tileIndex=0,totalTiles-1 do
				local tileX = bit.band(tileIndex, tilesPerSheetMask)
				local tileYAndSheetIndex = bit.rshift(tileIndex, tilesPerSheetInBits)
				local tileY = bit.band(tileYAndSheetIndex, tilesPerSheetMask)
				local sheetIndex = bit.rshift(tileYAndSheetIndex, tilesPerSheetInBits)

				readTile(
					tileImg,
					0,
					0,
					rom + info.tile8x8DataBaseAddr + tileSizeInBytes * tileIndex,
					bpp
				)
				local sheet = allTileSheets[sheetIndex+1]
				if not sheet then
					sheet = Image(sheetSize, sheetSize, 1, 'uint8_t'):clear()
					allTileSheets[sheetIndex+1] = sheet
				end

				-- use whatever's last as the palette
				local paletteIndex = paletteForTileIndex[tileIndex] or 0
				sheet.palette = makePalette(game, game.battleAnimPalettes + paletteIndex, bpp, bit.lshift(1, bpp))

				sheet:pasteInto{
					image = tileImg,
					x = bit.lshift(tileX, 3),	-- tx << 3 == tx * 8 == tx * tileWidth
					y = bit.lshift(tileY, 3),
				}
			end
			for sheetIndexPlus1,sheet in ipairs(allTileSheets) do
				sheet:save(battleAnimGraphicSetsPath('alltiles-'..bpp..'bpp-sheet'..sheetIndexPlus1..'.png').path)
			end
		end
	end

	local bpp = 3
	local mask = bit.lshift(1, bpp) - 1
	local numColors = bit.lshift(countof(game.battleAnimPalettes), bpp)
	makePaletteSets(
		game,
		battleAnimGraphicSetsPath,
		game.battleAnimPalettes,
		numColors,
		function(index)
			return bit.band(mask, index) == 0
		end
	)
end
