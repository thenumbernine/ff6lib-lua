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
		points into battleAnim8x8Tile_t list, which are 16x4 x 8x8 tiles
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
frame16x16TilesPtr points to a list of battleAnim16x16Tile_t's = list of 16x16 tiles
	.x, .y = in 16x16 tile units, destination into this frame to draw this 16x16 tile
	.tile = index into graphicSet's 64x4 location of 8x8 tiles
		tile8x8DataBaseAddr + tileLen * graphicSetTile.tile
	.hflip16, .vflip16 = how to flip the 16x16 tile
battleAnim8x8Tile_t list holds:
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

return function(rom, game, romsize)
	local graphicSetsUsed = table()
	local paletteForTileIndex = {}
	local palettesUsed = table()
	local frame16x16TileAddrInfo = table()


	-- [==[ interface layer for game.*

	local numBattleAnimSets = game.numBattleAnimSets
	local numBattleAnimEffects = game.numBattleAnimEffects

	--[[ using the originals?
	local battleAnimSets = game.battleAnimSets
	local battleAnimEffects = game.battleAnimEffects
	local battleAnimFrame16x16TileOffsets = game.battleAnimFrame16x16TileOffsets
	local battleAnimGraphicsSets2bpp = game.battleAnimGraphicsSets2bpp
	local battleAnimGraphicsSets3bpp = game.battleAnimGraphicsSets3bpp
	local battleAnimFrame16x16Tiles = game.battleAnimFrame16x16Tiles
	--]]
	-- [[ lets try to separate the blobs and still reconstruct the same data correctly
	local battleAnimSets = ffi.new('battleAnimSet_t[?]', game.numBattleAnimSets)
	ffi.copy(battleAnimSets, game.battleAnimSets, ffi.sizeof(battleAnimSets))
	battleAnimGraphicSetsPath'animsets.bin':write(ffi.string(battleAnimSets, ffi.sizeof(battleAnimSets)))

	local battleAnimEffects = ffi.new('battleAnimEffect_t[?]', game.numBattleAnimEffects)
	ffi.copy(battleAnimEffects, game.battleAnimEffects, ffi.sizeof(battleAnimEffects))
	battleAnimGraphicSetsPath'animeffects.bin':write(ffi.string(battleAnimEffects, ffi.sizeof(battleAnimEffects)))

	local battleAnimFrame16x16TileOffsets = ffi.new('uint16_t[?]', 4194)
	ffi.copy(battleAnimFrame16x16TileOffsets, game.battleAnimFrame16x16TileOffsets, ffi.sizeof(battleAnimFrame16x16TileOffsets))
	battleAnimGraphicSetsPath'animframe16x16offsets.bin':write(ffi.string(battleAnimFrame16x16TileOffsets, ffi.sizeof(battleAnimFrame16x16TileOffsets)))

	local battleAnimGraphicsSets2bpp = ffi.new('battleAnim8x8Tile_t[?]', 0x20 * 0xb0)
	ffi.copy(battleAnimGraphicsSets2bpp, game.battleAnimGraphicsSets2bpp, ffi.sizeof(battleAnimGraphicsSets2bpp))
	battleAnimGraphicSetsPath'graphicsets2bpp.bin':write(ffi.string(battleAnimGraphicsSets2bpp, ffi.sizeof(battleAnimGraphicsSets2bpp)))

	local battleAnimGraphicsSets3bpp = ffi.new('battleAnim8x8Tile_t[?]', 0x20 * 0x180)
	ffi.copy(battleAnimGraphicsSets3bpp, game.battleAnimGraphicsSets3bpp, ffi.sizeof(battleAnimGraphicsSets3bpp))
	battleAnimGraphicSetsPath'graphicsets3bpp.bin':write(ffi.string(battleAnimGraphicsSets3bpp, ffi.sizeof(battleAnimGraphicsSets3bpp)))

	local battleAnimFrame16x16Tiles = ffi.new('battleAnim16x16Tile_t[?]', 0x74cb)
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
	for battleAnimSetIndex=0,numBattleAnimSets-1 do
		local battleAnim = battleAnimSets + battleAnimSetIndex
		print('battleAnimSet['..battleAnimSetIndex..'] = '..battleAnim)

		for j=0,2 do
			-- TODO array plz, but then TODO serialzie arrays in 'struct' please
			local effectIndex = battleAnim['effect'..(j+1)]
			local paletteIndex = battleAnim['palette'..(j+1)]

			if effectIndex ~= 0xffff then
				local dontRunScript = 0 ~= bit.band(0x8000, effectIndex)
				effectIndex = bit.band(0x7fff, effectIndex)
				if effectIndex >= numBattleAnimEffects then
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

					-- number of battleAnim8x8Tile_t entries into the battleAnimGraphicSets[bpp] array (2 bytes each)
					local graphicSetOffset = graphicSetIndex * 0x20
					local graphicSetAddr = info.graphicSetBaseAddr + graphicSetOffset * ffi.sizeof'battleAnim8x8Tile_t'
					--local graphicSetTiles = ffi.cast('battleAnim8x8Tile_t*', rom + graphicSetAddr)
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
						that the frame16x16TilesAddr (list of battleAnim16x16Tile_t's)
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
						-- make sure its aligned to battleAnim16x16Tile_t
						assert.eq(0, bit.band(1, animFrame16x16TileOffset))
						local animFrame16x16TileIndex = bit.rshift(animFrame16x16TileOffset, 1)

						-- using the 0x110000 address offset:
						--local frame16x16TilesPtr = ffi.cast('battleAnim16x16Tile_t*', rom + frame16x16TilesAddr)
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

						local paltable = makePalette(game.battleAnimPalettes + paletteIndex, bpp, bit.lshift(1, bpp))
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
			im.palette = makePalette(game.battleAnimPalettes + paletteIndex, bpp, bit.lshift(1, bpp))
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
				sheet.palette = makePalette(game.battleAnimPalettes + paletteIndex, bpp, bit.lshift(1, bpp))

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

	-- space for 660 entries
	-- but if they are 1:1 with battleAnimEffects
	--  then just 650 entries
	local battleScriptAddrs = table()
	print()
	--for i=0,660-1 do
	for i=0,numBattleAnimEffects-1 do
		local offset = game.battleAnimScriptOffsets[i]
		local addr = offset + 0x100000
		--print('battleAnimScript['..i..']: offset=0x'..offset:hex()..', addr=0x'..addr:hex())
		battleScriptAddrs[addr] = battleScriptAddrs[addr] or table()
		battleScriptAddrs[addr]:insert(i)
	end
	print()

	-- of 660, 566 unique entries
	-- of 650, 565 as well
	local addrsInOrder = battleScriptAddrs:keys():sort()
	for i,addr in ipairs(addrsInOrder) do
		local addrend = addrsInOrder[i+1] or 0x107fb2
		print('battleAnimScript addr=0x'..addr:hex()..':')
		print(' used by effect #s: '..battleScriptAddrs[addr]:mapi(function(addr) return ('0x%04x'):format(addr) end):concat', ')
		local pc = addr
		local linepc
		local lhs
		local ifblockstack = table()
		local tab = 0
		local rhsprint
		local function startline()
			lhs = ''
			linepc = pc
			repeat
				if #ifblockstack == 0 then break end
				local last = ifblockstack:last()
				if bit.band(0xffff, pc) < last then break end
				ifblockstack:remove()
				tab = tab - 1
				rhsprint'end if'
			until false
		end
		local function read()
--print(debug.traceback())
			assert.le(0, pc)
			assert.lt(pc, romsize)
			local cmd = ffi.cast('uint8_t*', rom + pc)[0]
			--local cmd = rom[pc] --ffi.cast('uint8_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%02x'):format(cmd)	-- number.tostring arg is max # decimal digits ... i should do args for # lhs padding as well ...
			pc = pc + 1
assert.type(cmd, 'number')
			return cmd
		end
		local function reads8()
--print(debug.traceback())
			assert.le(0, pc)
			assert.lt(pc, romsize)
			local cmd = ffi.cast('int8_t*', rom + pc)[0]
			--local cmd = rom[pc] --ffi.cast('int8_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%02x'):format(cmd)	-- number.tostring arg is max # decimal digits ... i should do args for # lhs padding as well ...
			pc = pc + 1
assert.type(cmd, 'number')
			return cmd
		end
		local function readu16()
--print(debug.traceback())
			assert.le(0, pc)
			assert.lt(pc, romsize-2)
			local cmd = ffi.cast('uint16_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%04x'):format(cmd)
			pc = pc + 2
assert.type(cmd, 'number')
			return cmd
		end
		local function readu32()
			assert.le(0, pc)
			assert.lt(pc, romsize-4)
--print(debug.traceback())
			local cmd = ffi.cast('uint32_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%08x'):format(cmd)
			pc = pc + 4
assert.type(cmd, 'number')
			return cmd
		end
		function rhsprint(...)
			assert.eq(select('#', ...), 1)
			local s = ...
--print(debug.traceback())
			print(
				('$%04x: '):format(bit.band(0xffff, linepc))
				--('$%06x: '):format(linepc)
				..lhs
				..(' '):rep(20 - #lhs), ('  '):rep(tab)..s)
			startline()
		end
		startline()

		local movedirs = {
			[0] = 'down/forward',
			'down',
			'down/back',
			'forward',
			'back',
			'up/forward',
			'up',
			'up/back'
		}

		local function u8(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('uint8_t', b))
			return ('0x%02x'):format(b)
		end
		local function s8(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('int8_t', b))
			if b < 0 then return '-0x'..math.abs(b):hex() end
			return '0x'..b:hex()
			--return ('0x%02x'):format(b)
		end
		local function u16(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('uint16_t', b))
			return ('0x%04x'):format(b)
		end
		local function u32(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('uint32_t', b))
			return ('0x%08x'):format(b)
		end
		local function addrtostr(x)
			return ('$%04x'):format(bit.band(x, 0xffff))
		end

		while pc < addrend do
			local cmd = read()
			-- is this always and only for the first two bytes?
			if pc == addr+1 then
				local subcmd = read()
				local speed = 1 + bit.rshift(cmd, 4)
				local x = bit.band(cmd, 0xf)
				local align = bit.rshift(subcmd, 4)
				-- 0 = bottom of character
				-- 2 = center of character
				-- 4 = top of character
				-- 6 = unused
				-- 8 = initial position
				-- 0xa = center of screen
				-- 0xc = front of character
				-- 0xe = "align to character" (which side?)
				local y = bit.band(subcmd, 4)
				assert.eq(x, 0)
				assert.eq(y, 0)
				rhsprint('speed '..speed..' align '..align)
			elseif cmd >= 0 and cmd < 0x20 then
				rhsprint('show frame '..u8(cmd))
			elseif cmd >= 0x20 and cmd < 0x80 then
				rhsprint('-')	-- that's all it has in the notes ... '-'
			elseif cmd == 0x80 then
				local subcmd = read()
				if subcmd == 0x00 then
					rhsprint('quadra slam/quadra slice')
				elseif subcmd == 0x01 then
					rhsprint('')
				elseif subcmd == 0x02 then
					rhsprint('')
				elseif subcmd == 0x03 then
					rhsprint('')
				elseif subcmd == 0x04 then
					rhsprint('randomize vector angle and position (init fire dance sprites)')
				elseif subcmd == 0x05 then
					rhsprint('bum rush')
				elseif subcmd == 0x06 then
					rhsprint('init tornado (w wind/spiraler)')
				elseif subcmd == 0x07 then
					rhsprint('move tornado to thread position (w wind/spiraler)')
				elseif subcmd == 0x08 then
					rhsprint('move thread to vector position (w wind/spiraler)')
				elseif subcmd == 0x09 then
					rhsprint('update character/monster sprite tile priority for tornado (w wind/spiraler)')
				elseif subcmd == 0x0A then
					rhsprint('white/effect magic intro')
				elseif subcmd == 0x0B then
					rhsprint('update esper pre-animation balls position')
				elseif subcmd == 0x0C then
					rhsprint('')
				elseif subcmd == 0x0D then
					rhsprint('')
				elseif subcmd == 0x0E then
					rhsprint('')
				elseif subcmd == 0x0F then
					rhsprint('')
				elseif subcmd == 0x10 then
					rhsprint('move to target position')
				elseif subcmd == 0x11 then
					rhsprint('randomize vector angle')
				elseif subcmd == 0x12 then
					rhsprint('')
				elseif subcmd == 0x13 then
					rhsprint('toggle imp graphics for target (imp)')
				elseif subcmd == 0x14 then
					rhsprint('make target vanish (vanish)')
				elseif subcmd == 0x15 then
					rhsprint('move circle to thread position')
				elseif subcmd == 0x16 then
					rhsprint('')
				elseif subcmd == 0x17 then
					rhsprint('update sprite layer priority based on target')
				elseif subcmd == 0x18 then
					rhsprint('load sketched monster palette')
				elseif subcmd == 0x19 then
					rhsprint('sketch')
				elseif subcmd == 0x1A then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x1B then
					rhsprint('transform into magicite')
				elseif subcmd == 0x1C then
					rhsprint('decrement screen brightness')
				elseif subcmd == 0x1D then
					rhsprint('transform into magicite')
				elseif subcmd == 0x1E then
					rhsprint('')
				elseif subcmd == 0x1F then
					rhsprint('')
				elseif subcmd == 0x20 then
					rhsprint('')
				elseif subcmd == 0x21 then
					rhsprint('update rotating sprite layer priority')
				elseif subcmd == 0x22 then
					rhsprint('pearl wind')
				elseif subcmd == 0x23 then
					rhsprint('pearl wind')
				elseif subcmd == 0x24 then
					rhsprint('clear BG3 HDMA scroll data')
				elseif subcmd == 0x25 then
					rhsprint('clear BG1 HDMA scroll data')
				elseif subcmd == 0x26 then
					local arg = read()
					rhsprint(''
						..(arg == 0 and 'enable' or 'disable')
						..' character color palette updates')
				elseif subcmd == 0x27 then
					local arg = read()
					rhsprint(''
						..(arg == 0 and 'show' or 'hide')
						..' characters for esper attack')
				elseif subcmd == 0x28 then
					local arg = read()
					rhsprint('affects all characters. sprite priority = '
						..bit.band(3, bit.rshift(arg, 4)))
				elseif subcmd == 0x29 then
					local arg = read()
					rhsprint(''
						..(arg == 0 and 'show' or 'hide')
						..' cursor sprites (esper attack)')
				elseif subcmd == 0x2A then
					local arg = read()
					rhsprint('load animation palette '
						..u8(arg)..', sprite')
				elseif subcmd == 0x2B then
					local arg = read()
					rhsprint('load animation palette '
						..u8(arg)
						..', bg1 (inferno)')
				elseif subcmd == 0x2C then
					local arg = read()
					rhsprint('load animation palette '
						..u8(arg)
						..', BG3 (justice, earth aura)')
				elseif subcmd == 0x2D then
					local x = readu16()
					local y = readu16()
					local z = readu16()
					rhsprint('jump to '
						..addrtostr(x)..' for normal attack, '
						..addrtostr(y)..' for back attack (or side and attacker is #3 or #4), '
						..addrtostr(z)..' for pincer attack (or side and attacker is #1 or #2 or monster)')
				elseif subcmd == 0x2E then
					local x = read()
					local y = read()
					rhsprint('move sprite to ('
						..u8(x)..', '
						..u8(y)..')')
				elseif subcmd == 0x2F then
					rhsprint('')
				elseif subcmd == 0x30 then
					local arg = read()
					rhsprint('load animation palette '
						..u8(arg)
						..' for character 1')
				elseif subcmd == 0x31 then
					local arg = read()
					rhsprint('move in wide vertical sine wave with speed '
						..u8(arg)
						..' (hope song, sea song)')
				elseif subcmd == 0x32 then
					local x = readu16()
					local y = readu16()
					if x == y then
						rhsprint('jump to '..addrtostr(x))
					else
						rhsprint('jump to '
							..addrtostr(x)..' if facing left, '
							..addrtostr(y)..' if facing right')
					end
				elseif subcmd == 0x33 then
					local arg = read()
					rhsprint('update rainbow gradient lines')
				elseif subcmd == 0x34 then
					rhsprint('copy monster palettes to character palettes (hope song)')
				elseif subcmd == 0x35 then
					rhsprint('use character palettes for monster sprite data (hope song)')
				elseif subcmd == 0x36 then
					rhsprint('restore palettes for monster sprite data (hope song)')
				elseif subcmd == 0x37 then
					rhsprint('clear fixed color value hdma data ($2132)')
				elseif subcmd == 0x38 then
					rhsprint('enable high priority BG3 (justice)')
				elseif subcmd == 0x39 then
					local arg = read()
					rhsprint('update blue gradient lines (S. Cross, Carbunkl, Odin/Raiden)')
				elseif subcmd == 0x3A then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x3B then
					rhsprint('set target color palette to animation palette')
				elseif subcmd == 0x3C then
					rhsprint('set target color palette to normal')
				elseif subcmd == 0x3D then
					rhsprint('quadra slam/quadra slice')
				elseif subcmd == 0x3E then
					local arg = read()
					rhsprint('set main screen designation ($212C)')
				elseif subcmd == 0x3F then
					rhsprint('sonic dive')
				elseif subcmd == 0x40 then
					local arg = read()
					rhsprint('set screen mode ($2105) to '
						..u8(arg))
				elseif subcmd == 0x41 then
					local cx, cy, dx, dy = reads8(), reads8(), reads8(), reads8()
					rhsprint('shrink BG1 by ('
						..s8(cx)..','..s8(cy)..') and move ('
						..s8(dx)..','..s8(dy)..')')
				elseif subcmd == 0x42 then
					local vh = read()
					rhsprint('set MODE7 Settings register ($211A)'
						..' vflip='..tostring(0 ~= bit.band(2, vh))
						..' hflip='..tostring(0 ~= bit.band(1, vh)))
				elseif subcmd == 0x43 then
					rhsprint('moon song/charm')
				elseif subcmd == 0x44 then
					rhsprint('fire beam/bolt beam/ice beam')
				elseif subcmd == 0x45 then
					local arg = read()
					rhsprint('set BG1/BG2 mask settings hardware register ($2123)')
				elseif subcmd == 0x46 then
					rhsprint('')
				elseif subcmd == 0x47 then
					rhsprint('')
				elseif subcmd == 0x48 then
					rhsprint('clear')
				elseif subcmd == 0x49 then
					rhsprint('ink hit/virite')
				elseif subcmd == 0x4A then
					rhsprint('')
				elseif subcmd == 0x4B then
					rhsprint('update red/yellow gradient lines (megazerk)')
				elseif subcmd == 0x4C then
					rhsprint('move triangle to thread position')
				elseif subcmd == 0x4D then
					rhsprint('set vector from triangle to target')
				elseif subcmd == 0x4E then
					rhsprint('')
				elseif subcmd == 0x4F then
					rhsprint('')
				elseif subcmd == 0x50 then
					rhsprint('')
				elseif subcmd == 0x51 then
					rhsprint('rippler')
				elseif subcmd == 0x52 then
					rhsprint('stone')
				elseif subcmd == 0x53 then
					rhsprint('r.polarity')
				elseif subcmd == 0x54 then
					rhsprint('r.polarity')
				elseif subcmd == 0x55 then
					rhsprint('quasar')
				elseif subcmd == 0x56 then
					rhsprint('goner')
				elseif subcmd == 0x57 then
					local arg = read()
					rhsprint('set BG3/BG4 window mask settings ($2124) to '..u8(arg))
				elseif subcmd == 0x58 then
					local arg = read()
					rhsprint('set circle shape to '..u8(arg))
				elseif subcmd == 0x59 then
					rhsprint('goner/flare star')
				elseif subcmd == 0x5A then
					rhsprint('mind blast')
				elseif subcmd == 0x5B then
					rhsprint('mind blast')
				elseif subcmd == 0x5C then
					rhsprint('mind blast')
				elseif subcmd == 0x5D then
					rhsprint('')
				elseif subcmd == 0x5E then
					rhsprint('overcast')
				elseif subcmd == 0x5F then
					local arg = reads8()
					rhsprint('increase blue backdrop gradient by '..s8(arg)..' (used by Overcast)')
				elseif subcmd == 0x60 then
					local flags = readu32()	-- TODO is aabbccdd 4 sets of 2 bits or 4 bytes?
					rhsprint('toggle attacker status'
						..u32(flags)
						..' (morph/revert)')
				elseif subcmd == 0x61 then
					local xx, yy, zz = read(), read(), read()
					rhsprint('')
				elseif subcmd == 0x62 then
					rhsprint('evil toot/fader')
				elseif subcmd == 0x63 then
					local arg = read()
					rhsprint('move in narrow vertical sine wave with speed '
						..u8(arg)
						..' (evil toot)')
				elseif subcmd == 0x64 then
					rhsprint('purifier/inviz edge')
				elseif subcmd == 0x65 then
					rhsprint('')
				elseif subcmd == 0x66 then
					rhsprint('shock wave')
				elseif subcmd == 0x67 then
					rhsprint('load extra esper palette (purifier)')
				elseif subcmd == 0x68 then
					rhsprint('purifier')
				elseif subcmd == 0x69 then
					rhsprint('update sprite layer priority based on attacker')
				elseif subcmd == 0x6A then
					rhsprint('align bottom of thread with bottom of target (ice 3)')
				elseif subcmd == 0x6B then
					rhsprint('l? pearl')
				elseif subcmd == 0x6C then
					rhsprint('overcast')
				elseif subcmd == 0x6D then
					rhsprint('disable battle menu')
				elseif subcmd == 0x6E then
					rhsprint('')
				elseif subcmd == 0x6F then
					rhsprint('')
				elseif subcmd == 0x70 then
					rhsprint('')
				elseif subcmd == 0x71 then
					rhsprint('restore character palettes (purifier/hope song)')
				elseif subcmd == 0x72 then
					local arg = read()
					rhsprint('if attack hit then branch to '..addrtostr(pc + arg)..' (+'..u8(arg)..')')
				elseif subcmd == 0x73 then
					local arg = read()
					rhsprint('set graphics for dice roll (die index = '
						..u8(arg)..')')
				elseif subcmd == 0x74 then
					rhsprint('')
				elseif subcmd == 0x75 then
					rhsprint('super ball')
				elseif subcmd == 0x76 then
					local arg = read()
					rhsprint('seize')
				elseif subcmd == 0x77 then
					rhsprint('seize')
				elseif subcmd == 0x78 then
					rhsprint('discard')
				elseif subcmd == 0x79 then
					rhsprint('characters run to left side of screen (takes 56 loops to reach other side)')
				elseif subcmd == 0x7A then
					rhsprint('characters run to right side of screen (takes 56 loops to reach other side)')
				elseif subcmd == 0x7B then
					rhsprint('flip all characters (after running to opposite side of screen)')
				elseif subcmd == 0x7C then
					rhsprint('swap target and attacker')
				elseif subcmd == 0x7D then
					local arg = read()
					rhsprint('if dragon horn effect is active then branch to '..addrtostr(pc + arg)..' (+'..u8(arg)..')')
				elseif subcmd == 0x7E then
					rhsprint('flip target character vertically')
				elseif subcmd == 0x7F then
					rhsprint('hide all monsters')
				elseif subcmd == 0x80 then
					rhsprint('boss death')
				elseif subcmd == 0x81 then
					rhsprint('')
				elseif subcmd == 0x82 then
					rhsprint('boss death')
				elseif subcmd == 0x83 then
					rhsprint('')
				elseif subcmd == 0x84 then
					rhsprint('chadarnook exit')
				elseif subcmd == 0x85 then
					rhsprint('chadarnook exit')
				elseif subcmd == 0x86 then
					local arg = read()
					rhsprint('play sound effect '
						..u8(arg)..', pan based on sprite X position')
				elseif subcmd == 0x87 then
					local arg = read()
					rhsprint('play sound effect '
						..u8(arg)..', pan based on sprite Y position')
				elseif subcmd == 0x88 then
					rhsprint('')
				elseif subcmd == 0x89 then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x8A then
					rhsprint('set target monster sprite priority to 0')
				elseif subcmd == 0x8B then
					rhsprint('play ching sound effect')
				elseif subcmd == 0x8C then
					local arg = read()
					rhsprint('play sound effect '
						..u8(arg)..', pan center')
				else
					rhsprint('!!! uncharted subcmd')
				end
			elseif cmd == 0x81 then
				local xx, yy = read(), read()
				if xx == yy then
					rhsprint('set attacking character graphic to '..u8(xx))
				else
					rhsprint('set attacking character graphic to '..u8(xx)..' if facing left, '..u8(yy)..' if facing right')
				end
			elseif cmd == 0x82 then
				local xx, yy = read(), read()
				if xx == yy then
					rhsprint('set targetted character graphic to '..u8(xx))
				else
					rhsprint('set targetted character graphic to '..u8(xx)..' if facing left, '..u8(yy)..' if facing right')
				end
			elseif cmd == 0x83 then
				local arg = read()
				rhsprint('set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x84 then
				local xx = read()
				rhsprint('set animation speed to '..u8(xx))
			elseif cmd == 0x85 then
				rhsprint('move thread to attacker position')
			elseif cmd == 0x86 then
				local arg = read()
				rhsprint('for attacker, set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x87 then
				local arg = read()
				rhsprint('for target, set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x88 then
				local arg = read()
				rhsprint([[$F71D fight: set frame to ]]
					..u8(arg)..[[ and jump forward with weapon]])
			elseif cmd == 0x89 then
				local arg = read()
				rhsprint('loop from 0 to '..u8(arg-1))
				tab = tab + 1
			elseif cmd == 0x8A then
				tab = tab - 1
				rhsprint('end loop')
			elseif cmd == 0x8B then
				local arg = read()
				rhsprint('animated loop frame offset from +0 to +'..u8(arg-1))
				tab = tab + 1
			elseif cmd == 0x8C then
				tab = tab - 1
				rhsprint('end animated loop')
			elseif cmd == 0x8D then
				local arg = read()
				rhsprint('if animation is hflipped then set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x8E then
				local arg = read()
				rhsprint('show thread'
					..(0 ~= bit.band(0x80, arg) and ' below' or ' above')
					..(0 ~= bit.band(0x40, arg) and ' front' or ' back')
					..' other sprites (sprite priority) with'
					..(0 ~= bit.band(1, arg) and '' or ' opposite')..' weapon hand')
			elseif cmd == 0x8F then
				-- TODO either 0x8D or 0x8F should probably be 'vflipped'
				local arg = read()
				rhsprint('if animation is hflipped then set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x90 then
				local arg = read()
				rhsprint('set thread sprite tile priority to '
					..bit.band(3, bit.rshift(arg, 4))
					..' (tile priority)')
			elseif cmd == 0x91 then
				rhsprint('move this thread to attacker thread position')
			elseif cmd == 0x92 then
				local speed, branch = read(), read()
				rhsprint('move thread along vector (speed '
					..u8(speed)..', code branch '
					..u8(branch)..')')
			elseif cmd == 0x93 then
				local arg = read()
				rhsprint('set position on vector to '..u8(arg))
			elseif cmd == 0x94 then
				rhsprint('set vector from attacker to a random location on the target (GP Rain, AutoCrossbow)')
			elseif cmd == 0x95 then
				rhsprint('set vector from attacker to target')
			elseif cmd == 0x96 then
				local xx, yy = read(), read()
				rhsprint('if ??? then jump backwards '..u8(xx))
			elseif cmd == 0x97 then
				rhsprint('boomerang/wing edge/full moon/rising sun')
			elseif cmd == 0x98 then
				local arg, arg2 = read(), read()
				rhsprint('increment graphic index offset every '
					..u8(arg)
					..' frame(s), '..u8(arg2))
			elseif cmd == 0x99 then
				local arg = read()
				rhsprint('set thread palette to '..u8(bit.band(7, bit.rshift(arg, 1))))
			elseif cmd == 0x9A then
				rhsprint('set thread facing direction to match attacker')
			elseif cmd == 0x9B then
				rhsprint('')
			elseif cmd == 0x9C then
				local xx = read()
				rhsprint('')
			elseif cmd == 0x9D then
				local xx = read()
				rhsprint('')
			elseif cmd == 0x9E then
				rhsprint('')
			elseif cmd == 0x9F then
				local arg = read()
				rhsprint('animated loop start (loop count equal to the number of active threads, '
					..u8(arg)..' = 0) (autocrossbow)')
				tab = tab + 1
			elseif cmd == 0xA0 then
				local arg, arg2 = read(), read()
				rhsprint('jump forward along vector (speed '
					..u8(arg)..', code branch '..u8(arg)..')')
			elseif cmd == 0xA1 then
				local arg, arg2 = read(), read()
				rhsprint('jump backward along vector (speed '
					..u8(arg)..', code branch '..u8(arg2)..')')
			elseif cmd == 0xA2 then
				rhsprint('drill')
			elseif cmd == 0xA3 then
				local xxxx = readu16()
				rhsprint('shift color palette left')
			elseif cmd == 0xA4 then
				local arg, arg2 = read(), read()
				rhsprint('shift color palette right'
					..' numcolors='..bit.band(0xf, arg)
					..' offset='..bit.band(0xf, bit.rshift(arg, 4))
					..' speed='..bit.band(0xf, arg2)
					..' paletteIndex='..bit.band(0xf, bit.rshift(arg, 4)))
			elseif cmd == 0xA5 then
				local aa, bb, cc, xx, yyyy, zz = read(), read(), read(), read(), readu16(), read()
				rhsprint('circle origin ('
					..u8(aa+0x80)..','..u8(bb+0x80)..')'
					..' growspeed?='..u8(cc)
					..' maxsize='..u16(yyyy))
			elseif cmd == 0xA6 then
				local dx, dy, dr = read(), read(), read()
				rhsprint('move circle ('..s8(dx)..','..s8(dy)..'), circle size += '..s8(dr))
			elseif cmd == 0xA7 then
				rhsprint('update circle?')
			elseif cmd == 0xA8 then
				rhsprint('move circle to attacker')
			elseif cmd == 0xA9 then
				local dx, dy = read(), read()
				rhsprint('move circle ('..s8(dx)..','..s8(dy)..') (based on character facing direction)')
			elseif cmd == 0xAA then
				local arg = read()
				rhsprint('set sprite palette 3 color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xAB then
				local arg = read()
				rhsprint('set sprite palette 3 color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xAC then
				local xx, yy = read(), read()
				rhsprint([[$EE9C set background scroll HDMA data 123fffff vhaaaaaa 1: affect BG1 2: affect BG2 3: affect BG3 f: frequency v: vertical h: horizontal a: amplitude (max 14, must be even ???)]])
			elseif cmd == 0xAD then
				local arg = read()
				rhsprint('set BG scroll HDMA index: BG='..bit.rshift(arg, 6)
					..' index='..u8(bit.band(arg, 0x3f)))
			elseif cmd == 0xAE then
				local vh___123 = read()
				rhsprint('vh---123    $ED86 Update Scroll HDMA data v: vertical h: horizontal 1: affect BG1 2: affect BG2 3: affect BG3')
			elseif cmd == 0xAF then
				local arg = read()
				rhsprint('set background palette color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xB0 then
				local arg = read()
				rhsprint('set background palette color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xB1 then
				local arg = read()
				rhsprint('set sprite palette 1 color subtraction (absolute)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB2 then
				local arg = read()
				rhsprint('Set sprite palette 1 color addition (absolute)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB3 then
				local arg = read()
				rhsprint('add color to sprite palette 3 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB4 then
				local arg = read()
				rhsprint('subtract color from sprite palette 3 palette (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB5 then
				local arg = read()
				rhsprint('add color to background palette (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB6 then
				local arg = read()
				rhsprint('subtract color from background palette (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB7 then
				local arg = read()
				rhsprint('add color to sprite palette 1 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB8 then
				local arg = read()
				rhsprint('subtract color from sprite palette 1 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB9 then
				local arg = read()
				rhsprint('set monster palettes color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xBA then
				local arg = read()
				rhsprint('set monster palettes color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xBB then
				local arg = read()
				rhsprint('add color to monster palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xBC then
				local arg = read()
				rhsprint('subtract color from monster palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xBD then
				local arg = read()
				rhsprint(
					(0 ~= bit.band(0x80, arg) and
						(
							(0 ~= bit.band(0x20, arg) and 'hide' or 'show')
							..' BG1'
						)
						or ''
					)
					..(0 ~= bit.band(0x40, arg) and
						(
							(0 ~= bit.band(0x10, arg) and 'hide' or 'show')
							..' BG3'
						)
						or ''
					)
					..' animation thread graphics')
			elseif cmd == 0xBE then
				local arg = read()
				rhsprint('set screen mosaic to '..u8(arg)..' ($2106)')
			elseif cmd == 0xBF then
				local arg = readu16()
				rhsprint('call '..addrtostr(arg))
			elseif cmd == 0xC0 then
				rhsprint('return'..(
					#ifblockstack == 0 and '\n' or ''
				))
			elseif cmd == 0xC1 then
				local xx, yy = read(), read()
				rhsprint('vector movement speed? = '..u8(xx)..', branch to '..addrtostr(pc - yy)..' (-'..u8(yy)..')')
			elseif cmd == 0xC2 then
				local abc_____ = read()
				rhsprint('unpause animation a: unpause BG1 b: unpause BG3 c: unpause sprites')
			elseif cmd == 0xC3 then
				rhsprint('move circle to target')
			elseif cmd == 0xC4 then
				local arg = read()
				rhsprint('move'
					..(0 ~= bit.band(arg, 0x80) and ' BG1' or '')
					..(0 ~= bit.band(arg, 0x40) and ' BG3' or '')
					..' thread to this thread position')
			elseif cmd == 0xC5 then
				local a, b, c, d = readu16(), readu16(), readu16(), readu16()
				rhsprint('jump based on swdtech hit: {'
					..table{addrtostr(a), addrtostr(b), addrtostr(c), addrtostr(d)}:concat', '..'}'
					)
			elseif cmd == 0xC6 then
				local xx, yy = read(), read()
				rhsprint('quadra slam/quadra slice')
			elseif cmd == 0xC7 then
				local subcmd = read()
				if subcmd == 0x00 then
					local arg = read()
					rhsprint('set attacking character direction to face '
						..(arg == 0 and 'left' or 'right'))
				elseif subcmd == 0x01 then
					rhsprint('reset position offsets for attacking character')
				elseif subcmd == 0x02 then
					rhsprint('save attacking character position')
				elseif subcmd == 0x03 then
					rhsprint('restore attacking character position and reset offsets')
				elseif subcmd == 0x04 then
					rhsprint('restore attacking character position')
				elseif subcmd == 0x05 then
					local arg = read()
					rhsprint('(unused)')
				elseif subcmd == 0x06 then
					local arg, arg2 = read(), read()
					rhsprint('')
				elseif subcmd == 0x07 then
					rhsprint('update character action based on vector direction (walking)')
				elseif subcmd == 0x08 then
					local x, y = read(), read()
					rhsprint('set vector target ('..u8(x)..','..u8(y)..') from attacker')
				elseif subcmd == 0x09 then
					rhsprint('update character action based on vector direction (arms up)')
				elseif subcmd == 0x0A then
					local xx = read()
					rhsprint('(unused)')
				elseif subcmd == 0x0B then
					local x, y, z = read(), read(), read()
					rhsprint('spc('
						..table{u8(x), u8(y), u8(z)}:concat', '..')'
					)
				elseif subcmd == 0x0C then
					local arg, arg2 = read(), read()
					rhsprint('set actor '..u8(arg)..' graphic index to '..u8(arg2))
				elseif subcmd == 0x0D then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x0E then
					local arg = read()
					rhsprint('screen shaking ($6285) = '..u8(arg))
				elseif subcmd == 0x0F then
					rhsprint('(unused)')
				elseif subcmd == 0x10 then
					local xx = read()
					rhsprint('')
				elseif subcmd == 0x11 then
					rhsprint('disable run from battle')
				else
					rhsprint('!!! unknown subcmd')
				end
			elseif cmd == 0xC8 then
				local arg = read()
				rhsprint('set attacker graphic = '..u8(arg))
			elseif cmd == 0xC9 then
				local arg = read()
				rhsprint(''..(arg == 0
					and ('play animation default sound effect')
					or 'play sound effect '..u8(arg)
				))
			elseif cmd == 0xCA then
				rhsprint('')
			elseif cmd == 0xCB then
				local eddddddd = read()
				rhsprint('enable/disable echo sprites (4 copies of character sprite) e: 1 = enable, 0 = disable d: frame delay between echo sprites (bitmask)')
			elseif cmd == 0xCC then
				local arg = read()
				rhsprint('set sprite palette 2 color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xCD then
				local arg = read()
				rhsprint('set sprite palette 2 color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xCE then
				local arg = read()
				rhsprint('add color to sprite palette 2 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xCF then
				local arg = read()
				rhsprint('subtract color from sprite palette 2 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xD0 then
				local vhftpppm = read()
				rhsprint('set sprite data for all character/monster sprites')
			elseif cmd == 0xD1 then
				local arg = read()
				rhsprint(
					(arg == 0 and 'validate' or 'invalidate')
					..' character/monster order priority')
			elseif cmd == 0xD2 then
				local xx, yy = read(), read()
				rhsprint('set target position ('..u8(xx)..','..u8(yy)..') without moving')
			elseif cmd == 0xD3 then
				rhsprint('move circle to attacking character')
			elseif cmd == 0xD4 then
				local xxxx, yy = readu16(), read()
				-- hmm cant tell bit/byte order from desc...
				rhsprint('set color addition/subtraction data')
			elseif cmd == 0xD5 then
				local arg = read()
				rhsprint('set monster'
					..(0 ~= bit.band(1, arg) and ' hflip' or '')
					..(0 ~= bit.band(2, arg) and ' vflip' or ''))
			elseif cmd == 0xD6 then
				rhsprint('')
			elseif cmd == 0xD7 then
				local xx = read()
				rhsprint('move fire dance sprites')
			elseif cmd == 0xD8 then
				local xx, yy, zz = read(), read(), read()
				rhsprint('x speed='..u8(xx)..' y speed='..u8(yy)..' ??? = '..u8(zz))
			elseif cmd == 0xD9 then
				local xx = read()
				rhsprint('(bum rush)')
			elseif cmd == 0xDA then
				local xxxx = readu16()
				rhsprint('update tornado (w wind/spiraler)')
			elseif cmd == 0xDB then
				local arg = read()
				rhsprint('if character already stepped forward to attack then branch to '..addrtostr(pc - arg)..' (+'..u8(arg)..')')
			elseif cmd == 0xDC then
				rhsprint('rotate triangle 2D')
			elseif cmd == 0xDD then
				local xx, yy, dd, rr = read(), read(), read(), read()
				rhsprint('init triangle')
			elseif cmd == 0xDE then
				rhsprint('move triangle to attacker position')
			elseif cmd == 0xDF then
				rhsprint('move triangle to target position')
			elseif cmd == 0xE0 then
				local xx, yy, dd, rr = read(), read(), read(), read()
				rhsprint('modify triangle')
			elseif cmd == 0xE1 then
				local xx = read()
				rhsprint('show/hide attacker sprite')
			elseif cmd == 0xE2 then
				rhsprint('')
			elseif cmd == 0xE3 then
				rhsprint('')
			elseif cmd == 0xE4 then
				rhsprint('')
			elseif cmd == 0xE5 then
				local xx, yy, zz = read(), read(), read()
				rhsprint('branch to '..addrtostr(pc - yy)..' (-'..u8(yy)..')')
			elseif cmd == 0xE6 then
				local xx, yy, zz = read(), read(), read()
				rhsprint('branch to '..addrtostr(pc - yy)..' (-'..u8(yy)..')')
			elseif cmd == 0xE7 then
				rhsprint('calculate vector from attacking character to target')
			elseif cmd == 0xE8 then
				local rr, tt = read(), read()
				rhsprint('move to polar coordinates r='..u8(rr)..' theta='..u8(tt))
			elseif cmd == 0xE9 then
				local dx, dy = read(), read()
				-- TODO should this be 0..dx or 0..(dx-1) ?
				rhsprint('move randomly (0..'..u8(dx)..', 0..'..u8(dy)..')')
			elseif cmd == 0xEA then
				local _13__xxxx = read()
				rhsprint('set BG tile data quadrants 1 = affect bg1 3 = affect bg1 x = quadrant')
			elseif cmd == 0xEB then
				-- TODO count as many as threads ... how to determine # of threads?
				local addrs = range(3):mapi(function() return readu16() end)
				rhsprint('jump based on thread to {'
					..addrs:mapi(function(addr) return addrtostr(addr) end):concat', '
					..'}')
			elseif cmd == 0xEC then
				local xx = read()
				rhsprint('set thread layer (0 = sprite, 1 = BG1, 2 = BG3)')
			elseif cmd == 0xED then
				rhsprint('')
			elseif cmd == 0xEE then
				local __oo____ = read()
				rhsprint('set target sprite tile priority')
			elseif cmd == 0xEF then
				local rr, tt = read(), read()
				rhsprint('move to polar coordinates r='..u8(rr)..' theta='..u8(tt)..' (similar to $E8)')
			elseif cmd == 0xF0 then
				local a,b,c,d,e = readu16(), readu16(), readu16(), readu16(), readu16()
				rhsprint('jump based on current target'
					..' char1='..addrtostr(a)
					..' char2='..addrtostr(b)
					..' char3='..addrtostr(c)
					..' char4='..addrtostr(d)
					..' monster='..addrtostr(e))
			elseif cmd == 0xF1 then
				local xx = read()
				rhsprint('')
			elseif cmd == 0xF2 then
				rhsprint('set a trajectory from target center to attacker')
			elseif cmd == 0xF3 then
				local a,b,c,d,e = readu16(), readu16(), readu16(), readu16(), readu16()
				rhsprint('jump based on current attacker'
					..' char1='..addrtostr(a)
					..' char2='..addrtostr(b)
					..' char3='..addrtostr(c)
					..' char4='..addrtostr(d)
					..' monster='..addrtostr(e))
			elseif cmd == 0xF4 then
				local _______t = read()
				rhsprint('set sprite layer priority')
			elseif cmd == 0xF5 then
				rhsprint('until no threads are active')
			elseif cmd == 0xF6 then
				rhsprint('rotate triangle 3D')
			elseif cmd == 0xF7 then
				local arg = read()
				rhsprint('wait until vertical scanline position '..u8(arg))
			elseif cmd == 0xF8 then
				--[[
				local arg, arg2 = readu16(), readu16()
				rhsprint('if magitek mode is enabled then jump to '
					..addrtostr(arg)
					..' else '..addrtostr(arg2))
				--]]
				-- [[
				local arg, arg2 = readu16(), readu16()
				-- funny thing, arg2 always matches script PC
				assert.eq(arg2, bit.band(0xffff, pc))
				ifblockstack:insert(arg)
				-- this is always a command "if magitek mode is enabled then jump to" (with no else)
				-- and that means this is really an if-block: "if magitek mode is not enabled"
				--  that we can pop once we reach that address
				rhsprint'if not magitek mode then'
				tab = tab + 1
				--]]
			elseif cmd == 0xF9 then
				local xx,yy,zz = read(), read(), read()
				rhsprint('')
			elseif cmd == 0xFA then
				local arg = readu16()
				rhsprint('jump to '..addrtostr(arg))
			elseif cmd == 0xFB then
				local arg = read()
				rhsprint('set character palettes color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xFC then
				local arg = read()
				rhsprint('set character palettes color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xFD then
				local arg = read()
				rhsprint('add color to character palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xFE then
				local arg = read()
				rhsprint('subtract color from character palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xFF then
				rhsprint('end animation'..(
					#ifblockstack == 0 and '\n' or ''
				))
			end
		end
	end
	print()

	local bpp = 3
	local mask = bit.lshift(1, bpp) - 1
	local numColors = bit.lshift(game.numBattleAnimPalettes, bpp)
	makePaletteSets(
		battleAnimGraphicSetsPath,
		game.battleAnimPalettes,
		numColors,
		function(index)
			return bit.band(mask, index) == 0
		end
	)
end
