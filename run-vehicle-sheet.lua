#!/usr/bin/env luajit
--[[
this has turned into a replacement for charsprites.lua
so TODO get rid of charsprites.lua

oh yeah, 0x150000 - 0x185000 is where the tile data for character sprites goes
but the end of it is vehicles
so this is now the vehicle-extraction script

and it's gonna use `characterFrameTileOffset`
... and the next struct that I haven't charted yet

--]]
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
local range = require 'ext.range'
local assert = require 'ext.assert'
local tolua = require 'ext.tolua'
local vec2i = require 'vec-ffi.vec2i'

local int8_t_p = ffi.typeof'int8_t*'
local uint8_t = ffi.typeof'uint8_t'
local uint8_t_p = ffi.typeof'uint8_t*'

-- 0-15 is character-palette-7 for magitek and chocobo
local saveRaftIndexOffset = 16		-- 16-31 is character palette 11
local saveBlackjackIndexOffset = 32	-- 32-47 is blackjack palette
local saveFalconIndexOffset = 48	-- 48-63 is falcon palette

local function run(game)
	local rom = game.rom

	local worldspritedir = path'worldgfx'
	worldspritedir:mkdir(true)

	-- points into fieldSpriteGraphics == 0x150000-0x183000
	local function getTileOffsetForSprite(sprite)
		if sprite < 0 or sprite >= game.numCharacterSprites then return end
		return bit.band(
			bit.bnot(0xc00000),
			-- these are only for sprite < 87?
			bit.bor(
				game.characterSpriteOffsetLo[sprite],
				bit.lshift(game.characterSpriteOffsetHiAndSize[sprite].hi, 16)
			))
	end

	local Image = require 'image'
	local makePalette = require 'ff6.graphics'.makePalette
	local readTile = require 'ff6.graphics'.readTile
	local drawTile = require 'ff6.graphics'.drawTile
	-- 8x8 right?
	local tileWidth = require 'ff6.graphics'.tileWidth
	local tileHeight = require 'ff6.graphics'.tileHeight
	local bpp = 4

	local spriteNames = game.spriteNames
	local spriteIndexes = game.spriteIndexes
	local spriteFrameNames = game.spriteFrameNames
	local spriteFrameIndexes = game.spriteFrameIndexes

	--[[
	alright now to track what sprites use what frames
	since it doesn't look too obvious...
	--]]
	local spriteFrames = {}
	for i=0,game.numCharacterSprites-1 do
		spriteFrames[i] = {}
	end
	-- terra-imp have only up to finger2, then tent, then two riding
	for sprite=spriteIndexes.terra,spriteIndexes.kefka do	-- terra - imp
		for frame=0,spriteFrameIndexes.finger2 do
			spriteFrames[sprite][frame] = true
		end
		spriteFrames[sprite][spriteFrameIndexes.tent] = true
		-- these also have dead_horz but it's a copy of dead...
		-- leo-kefka same but no riding
		if sprite <= spriteIndexes.imp then
			spriteFrames[sprite][spriteFrameIndexes.riding_left_1] = true
			spriteFrames[sprite][spriteFrameIndexes.riding_left_2] = true
		end
	end
	-- terra alone has special
	spriteFrames[spriteIndexes.terra][spriteFrameIndexes.special] = true
	-- these only have 9 walking frames, then some extras
	for sprite=spriteIndexes.gestahl,spriteIndexes.emperor_servant do
		for frame=0,8 do
			spriteFrames[sprite][frame] = true
		end
		-- gestahl and old man have this:
		if sprite <= spriteIndexes.elder then
			spriteFrames[sprite][spriteFrameIndexes.npc_special_1] = true
		end
		if sprite <= spriteIndexes.man then
			spriteFrames[sprite][spriteFrameIndexes.npc_waving_1] = true
			spriteFrames[sprite][spriteFrameIndexes.npc_waving_2] = true
			spriteFrames[sprite][spriteFrameIndexes.npc_head_down_down] = true
		end
	end
	spriteFrames[spriteIndexes.dog][spriteFrameIndexes.handsupl1] = true
	spriteFrames[spriteIndexes.dog][spriteFrameIndexes.npc_head_down_down] = true
	for sprite=spriteIndexes.celes_in_dress,spriteIndexes.draco do
		spriteFrames[sprite][spriteFrameIndexes.opera_singer_mouth_open] = true
		spriteFrames[sprite][spriteFrameIndexes.opera_singer_mouth_closed] = true
		spriteFrames[sprite][spriteFrameIndexes.opera_singer_unused] = true
	end
	for sprite=spriteIndexes.celes_in_dress,spriteIndexes.maduin do
		if sprite ~= spriteIndexes.gau_dressed_up
		and sprite ~= spriteIndexes.elder_esper
		and sprite ~= spriteIndexes.cid
		then
			spriteFrames[sprite][spriteFrameIndexes.npc_special_2] = true
		end
	end
	spriteFrames[spriteIndexes.figaro_guard][spriteFrameIndexes.riding_left_1] = true
	spriteFrames[spriteIndexes.figaro_guard][spriteFrameIndexes.riding_left_2] = true
	-- the rest are single-frame
	for sprite=spriteIndexes.ramuh,spriteIndexes.atma do
		spriteFrames[sprite][0] = true
	end
	spriteFrames[spriteIndexes.ramuh][spriteFrameIndexes.ramuh_staff_raised] = true
	spriteFrames[spriteIndexes.ramuh][spriteFrameIndexes.ramuh_eyes_closed] = true
	-- or it's just a coincicdence that this matches figaro_guard_dead
	--spriteFrames[spriteIndexes.figaro_guard_riding][spriteFrameIndexes.handsupu] = true

	-- animated don't use 0, but do use special thru special + #frames-1 enabled
	-- find these by searching everything8215/ff6/src/event/npc_prop.asm
	--  and looking for all npcs flagged with npc_anim, then note the npc_gfx, and note the animation count and the frame name
	local animationFrameCount = {
		flying_bird_1 = 2,
		flying_bird_2 = 2,
		big_sparkle = 2,
		multi_sparkles = 2,
		small_sparkle = 2,
		coin = 2,
		rat = 2,
		turtle = 2,
		small_bird_up = 2,
		save_point = 4,
		flame = 4,
		explosion = 4,
		tentacle_1 = 4,
		tentacle_2 = 4,
	}
	for sprite=spriteIndexes.small_statue,spriteIndexes.tentacle_2 do
		local frameCount = animationFrameCount[spriteNames[sprite]] or 1
		for i=0,frameCount-1 do
			spriteFrames[sprite][spriteFrameIndexes.special_anim_1+i] = true
		end
	end

	-- ok now the only way to find if a sprite is 16x24, 16x16, or 32x32 is to look at all the NPCs that use it
	-- these are everything8215/ff6/src/event/npc_prop.asm flagged as 'special_npc_prop'
	-- kind of like how to tell what palettes it works with
	-- true means 16x16
	-- 32x32 means 32x32
	-- and '2 frames' is me noticing they are set to a npc_anim, but i'm lazy about getting into that just yet
	local spriteSpecial = {
		air_force = true,
		falcon_1 = '32x32',
		falcon_2 = true,
		falcon_3 = true,
		flying_terra_1 = '2 frames',
		flying_terra_2 = '2 frames',	-- not in the list, hmm
		flying_terra_3 = '2 frames',
		ending_terra_1 = '2 frames',
		ending_terra_2 = '2 frames',
		ending_terra_3 = true,
		tritoch = '32x32',
		leo_sword = true,
		diving_helmet = true,
		chadarnook_3 = '32x32',
		chadarnook_1 = true,
		chadarnook_2 = true,
		small_bird_left = '2 frames',
		gate_1 = '32x32',
		gate_2 = true,
		gate_3 = true,
		crane_hook_2 = true,
		crane_hook_1 = true,
		crane_hook_3 = true,
		magitek_train_1 = true,
		magitek_train_3 = true,
		magitek_train_2 = true,
		magitek_train_4 = true,
		crane_1 = true,
		crane_2 = true,
		crane_3 = true,
		guardian_1 = true,
		guardian_2 = true,
		guardian_3 = true,
		guardian_4 = true,
		guardian_5 = true,
		guardian_6 = true,
		floor_switch = true,
		big_switch = true,
		rock = true,
		magitek_machine = '32x32',
		elevator = '32x32',
		poltergeist_1 = '32x32',
		doom_1 = '32x32',
		doom_2 = true,
		doom_3 = true,
		goddess_1 = '32x32',
		goddess_2 = true,
		goddess_3 = true,
		odin = '32x32',
	}

	do
		--4bpp means 32 bytes per 8x8 tile ...
		--  0x150000 - 0x185000 has 6784 8x8 tiles = 1696 16x16 tiles
		local ptr = game.fieldSpriteGraphics

		print'spritePalettes = {'
		for sprite=0,game.numCharacterSprites-1 do
			local palIndexes = game.palettesForSprites[spriteNames[sprite]]
			local palIndex = palIndexes and palIndexes:last() or 0
			print('\t['..sprite..'] = '..palIndex..',\t-- '..spriteNames[sprite]..' = '..tostring(game.spritePaletteNames[palIndex]))
		end
		print'}'


		-- collect into a tilesImgWriter-list and into a multiple-sheet list
		local tilesImgWriter = {}
		function tilesImgWriter:init()
			self.tilesWide = 58*2+1
			self.tilesHigh = 356
			self.img = Image(tileWidth*self.tilesWide, tileHeight*self.tilesHigh, 4, uint8_t):clear()
			self.dst = vec2i()
		end
		function tilesImgWriter:writeSprite(args)
			local allFramesWidths = args.frameImgs:mapi(function(img) return img.width end):sum()
			local maxFrameHeight = math.max(args.frameImgs:mapi(function(img) return img.height end):unpack())
			if self.dst.x + allFramesWidths >= self.img.width then
				self.dst.x = 0
				self.dst.y = self.dst.y + maxFrameHeight
			end
			for framePlus1,frameImg in ipairs(args.frameImgs) do
				self:writeFrame{
					sprite = args.sprite,
					frame = framePlus1-1,
					frameImg = frameImg,
				}
			end
			self.dst.x = self.dst.x + 8
		end
		function tilesImgWriter:writeFrame(args)
			local img = self.img

			-- frame to our tilesImgWriter sheet...
			if self.dst.x > img.width then
				print('!!! WARNING !!! dst.x='..self.dst.x..' > img.y='..img.width)
			end
			if self.dst.y > img.height then
				print('!!! WARNING !!! dst.y='..self.dst.y..' > img.y='..img.height)
			end
			img:pasteInto{
				image = args.frameImg:rgba(),
				x = self.dst.x,
				y = self.dst.y,
			}

			if not spriteFrames[args.sprite][args.frame] then
				for j=0,args.frameImg.height-1 do
					for i=0,args.frameImg.width-1 do
						local ofs = 4 * (self.dst.x+i + img.width * (self.dst.y+j))
						if img.buffer[3 + ofs] < 127 then
							img.buffer[0 + ofs] = 0
							img.buffer[1 + ofs] = 255
							img.buffer[2 + ofs] = 255
							img.buffer[3 + ofs] = 255
						end
					end
				end
			end

			self.dst.x = self.dst.x + args.frameImg.width
		end

		function tilesImgWriter:endSprite()
			self.dst.x = self.dst.x + 8
		end
		function tilesImgWriter:done()
			self.img:save'all-field-tiles.png'
		end


		local sheetImgWriter = {}
		function sheetImgWriter:init()
			self.anims = table()
			self.outDir = path'npc_sprites'
			self.outDir:mkdir()
			self.img = Image(256, 256, 1, uint8_t)
			self.sheetIndex = 0
			self.dst = vec2i()
			self.rowmaxheight = 0
		end
		function sheetImgWriter:flushCharSheet()
			local basename = 'sheet'..self.sheetIndex
			self.img:save(self.outDir(basename..'.png'))
			self.img:clear()
			self.sheetIndex = self.sheetIndex + 1
			self.dst = vec2i()
			self.anims:insert'_new_sheet_\n'
			self.rowmaxheight = 0
		end
		function sheetImgWriter:writeSprite(args)
			-- filter out garbage/unused frames
			local frameImgs = table(args.frameImgs)
			local frameNums = range(0,#frameImgs-1)
			assert.eq(#frameImgs, #frameNums)
			for framePlus1=#frameImgs,1,-1 do
				if not spriteFrames[args.sprite][framePlus1-1] then
					frameImgs:remove(framePlus1)
					frameNums:remove(framePlus1)
				end
			end
			assert.eq(#frameImgs, #frameNums)
			-- simulate frame inc across all frames
			-- see if we are still in this sheet
			-- if not then advance early
			local newdst = self.dst:clone()
			local reset
			for _,frameImg in ipairs(frameImgs) do
				newdst, reset = self:dstinc(newdst, frameImg)
				if reset then break end
			end
			if reset then
				self:flushCharSheet()
			end

			--[[ proper
			self.anims:insert'do\n'
			self.anims:insert'\tlocal anim = {\n'
			self.anims:insert('\t\tname = '..tolua(spriteNames[args.sprite])..',\n')
			self.anims:insert'\t\tframes = {\n'
			--]]
			-- [[ concise
			self.anims:insert(spriteNames[args.sprite])
			--]]

			for i,frameImg in ipairs(frameImgs) do
				self:writeFrame{
					sprite = args.sprite,
					frame = frameNums[i],
					frameImg = frameImg,
				}
			end

			--[[ proper
			self.anims:insert'\t\t},\n'
			self.anims:insert'\t\tseqs = table.union({}, charSeqs),\n'
			self.anims:insert'\t}\n'
			self.anims:insert'\tanim.frame0 = anim.frames!.standd\n'
			self.anims:insert'\tanim.seq0 = anim.seqs!.stand\n'
			self.anims:insert'\ttable.insert(anims, anim)\n'
			self.anims:insert'end\n'
			--]]
			-- [[ concise
			self.anims:insert'\n'
			--]]
		end
		function sheetImgWriter:dstinc(dst, img)
			local rowmaxheight = self.rowmaxheight
			dst = dst:clone()
			local reset
			dst.x = dst.x + img.width
			rowmaxheight = math.max(rowmaxheight, img.height)
			if dst.x + img.width > self.img.width then
				dst.x = 0
				dst.y = dst.y + rowmaxheight
				if dst.y + 24 > self.img.height then
					dst.y = 0
					reset = true
				end
			end
			return dst, reset, rowmaxheight
		end
		function sheetImgWriter:writeFrame(args)
			local frameImg = args.frameImg
			self.img.palette = frameImg.palette
			self.img:pasteInto{
				image = frameImg,
				x = self.dst.x,
				y = self.dst.y,
			}
			local frameName = spriteFrameNames[args.frame]
			if args.sprite >= spriteIndexes.ramuh then
				-- starting with ramuh ... the 1st frame is no longer stepping-down but is standing looking down...
				if frameName == 'special_anim_1' then
					frameName = 'stand1'
				elseif frameName == 'special_anim_2' then
					frameName = 'stand2'
				elseif frameName == 'special_anim_3' then
					frameName = 'stand3'
				elseif frameName == 'special_anim_4' then
					frameName = 'stand4'
				elseif frameName == 'walkd1' then
					frameName = 'stand1'
				else
					frameName = 'stand2'
				end
			end
			--[[ proper
			self.anims:insert('\t\t\t'..frameName..' = {\t-- '..args.frame..'\n')
			self.anims:insert("\t\t\t\tspriteIndex = "..(self.dst.x / 8)
				.." | ("..(self.dst.y / 8).." << 5)"
				.." | (("..(self.sheetIndex+1).." + sheetBlobIndexForName!['Characters #1']) << 10),\n")
			self.anims:insert('\t\t\t\tspriteWidth = '..(frameImg.width / 8)..',\n')
			self.anims:insert('\t\t\t\tspriteHeight = '..(frameImg.height / 8)..',\n')
			self.anims:insert'\t\t\t},\n'
			--]]
			-- [[ concise
			if frameImg.width == 16 and frameImg.height == 24 then
				self.anims:insert(' '..frameName)
			elseif frameImg.width == 16 and frameImg.height == 16 then
				self.anims:insert(' +'..frameName)
			elseif frameImg.width == 32 and frameImg.height == 32 then
				self.anims:insert(' *'..frameName)
			else
				error("idk how to classify this frame size")
			end
			--]]

			local reset
			self.dst, reset, self.rowmaxheight = self:dstinc(self.dst, frameImg)
			if reset then
				self:flushCharSheet()
			end
		end
		function sheetImgWriter:done()
			self:flushCharSheet()
			self.outDir('anims.lua'):write(self.anims:concat())
		end

		local writers = table{tilesImgWriter, sheetImgWriter}
		local function writeCall(fk)
			return function(...)
				for _,wr in ipairs(writers) do
					local f = wr[fk]
					if f then f(wr, ...) end
				end
			end
		end

		writeCall'init'()

		for sprite=0,game.numCharacterSprites-1 do
			local palIndexes = game.palettesForSprites[spriteNames[sprite]]
			local palIndex = palIndexes and palIndexes:last()
-- see all?
--print('sprite', spriteNames[sprite], 'using palettes', palIndexes and palIndexes:concat', ')
			palIndex = bit.band(palIndex or 0, 0x1f)
			local palette = game.getCharPal(palIndex)

			-- output all frames but assume all are 2x3
			-- reveals a few extra singing frames that I didn't output before
			local maxFrames = game.countof(game.characterFrameTileOffsets) / 6
			-- TODO who determines this? and who determines where the offset info is?
			-- ... turns out the NPC data does. great.
			local spriteTileCount = 6
			-- max tile placement, used for bounds in the sheet
			local frameTilesWide = 2
			local frameTilesHigh = 3

			-- get the offset from charBaseAddr to the 8x8x4bpp tile data
			local function getFrameTileOffset(frame, tileIndex)
				-- TODO sometimes this is characterFrameTileOffsets, sometimes I bet it is what's next ...
				return game.characterFrameTileOffsets[tileIndex + frame * spriteTileCount]
			end

			local special = spriteSpecial[spriteNames[sprite]]
			if special then
				if special == true then
					spriteTileCount = 4
					maxFrames = 1
					frameTilesWide = 2
					frameTilesHigh = 2
				elseif special == '32x32' then
					spriteTileCount = 16
					maxFrames = 1
					frameTilesWide = 4
					frameTilesHigh = 4
				elseif special == '2 frames' then
					spriteTileCount = 4
					maxFrames = 2
					frameTilesWide = 2
					frameTilesHigh = 2
				elseif special ~= nil then
					error'here'
				end
				-- enable our frames
				for i=0,maxFrames-1 do
					spriteFrames[sprite][i] = true
				end
				-- change our frame tile offset getter to linear?
				getFrameTileOffset = function(frame, tileIndex)
					return 0x20 * (tileIndex + spriteTileCount * frame)
				end
			end

			local frameImgs = table()

			local charBaseAddr = getTileOffsetForSprite(sprite)

--[[
-- tiles are 8x8x4bpp = 32 bytes = 0x20 bytes ...
-- for a 16x16 that is 0x80 bytes
local tileDataSize = (getTileOffsetForSprite(sprite+1) or 0x183000) - charBaseAddr
print(
	'sprite', sprite, spriteNames[sprite],
	--'tile ofs', ('%x'):format(charBaseAddr),
	--'size', ('%x'):format(tileDataSize),
	(tileDataSize/0x20)..' tiles'
)
--]]

			for frame=0,maxFrames-1 do
				local frameImg = Image(frameTilesWide*tileWidth, frameTilesHigh*tileHeight, 1, uint8_t):clear()
				frameImgs:insert(frameImg)

				frameImg.palette = palette

				-- blit to our 8x8 with palette set up
				for spriteTileIndex=0,spriteTileCount-1 do
					local x = spriteTileIndex % frameTilesWide
					local y = (spriteTileIndex - x) / frameTilesWide

					local tile = rom + charBaseAddr + getFrameTileOffset(frame, spriteTileIndex)

					-- tile to our frame...
					readTile(frameImg, 8*x, 8*y, tile, bpp)
				end
			end
			writeCall'writeSprite'{
				sprite = sprite,
				frameImgs = frameImgs,
			}
		end

		writeCall'done'()
	end


-----------------------------------------------------------------------


	-- use this for later making vehicle-sheet
	local tilesImg
	do
		-- 0x183000 - 0x185000 has 256 8x8 tiles = 64 16x16 tiles
		local ptr = game.rom + 0x183000
		local tilesWide = 16
		local tilesHigh = 16

		tilesImg = Image(tileWidth*tilesWide, tileHeight*tilesHigh, 1, uint8_t):clear()

		--everything8215 vehicleGraphics says mapSpritePalettes[7] and [11]
		tilesImg.palette = table():append(
			game.getCharPal(7),
			game.getCharPal(11)
		)

		local tileIndex = 0
		for ty=0,tilesHigh-1 do
			for tx=0,tilesWide-1 do
				local tileImg = Image(8, 8, 1, uint8_t):clear()
				local palor = tileIndex < 32 and saveRaftIndexOffset or 0	-- shift raft palette to use +16
				readTile(tileImg, 0, 0, ptr, bpp, false, false, palor)
				tilesImg:pasteInto{image=tileImg, x=tx*tileWidth, y=ty*tileHeight}
				ptr = ptr + 32
				tileIndex = tileIndex + 1
			end
		end

		--tilesImg:save'vehicle-tiles.png'
	end

	-- now rearrange them and put them into a 256x256 sprite-sheet (this is ff6t3d-specific)
	-- is it just me or does it look like the tiles were meant to be laid out in rows of 16 to make a 128x128 sheet?
	-- i'll just go with that ....
	local sheetImg = Image(256, 256, 1, uint8_t):clear()

	sheetImg.palette = table():append(
		tilesImg.palette,	-- char 7 & char 11 pals
		makePalette(game, game.blackjackPalette, bpp, 16),
		makePalette(game, game.falconPalette, bpp, 16)
	)
	assert.len(sheetImg.palette, 64)
	do
		local tile16x16Imgs = table()
		for j=0,7 do
			for i=0,7 do
				tile16x16Imgs:insert(tilesImg:copy{x=i*16, y=j*16, width=16, height=16})
			end
		end
		assert.len(tile16x16Imgs, 64)

		local i = 1
		local function pasteNext(args)
			args.image = tile16x16Imgs[i]
			i = i + 1
			sheetImg:pasteInto(args)
		end
		-- raft up/down
		pasteNext{x=0*16+32, y=0*16+96}
		pasteNext{x=0*16+32, y=1*16+96}
		pasteNext{x=1*16+32, y=0*16+96}
		pasteNext{x=1*16+32, y=1*16+96}
		-- raft left/right
		pasteNext{x=2*16+32, y=0*16+96}
		pasteNext{x=2*16+32, y=1*16+96}
		pasteNext{x=3*16+32, y=0*16+96}
		pasteNext{x=3*16+32, y=1*16+96}

		pasteNext{x=80, y=96+32}	-- chocobo head
		pasteNext{x=0, y=96+32}	-- headless chocobo tail facing down
		pasteNext{x=0, y=112+32}	--
		pasteNext{x=16, y=96+32}	-- headless chocobo tail facing down #2
		pasteNext{x=16, y=112+32}	--
		pasteNext{x=80, y=112+32}	-- chocobo tail
		pasteNext{x=32, y=96+32}	-- headless chocobo tail facing up
		pasteNext{x=32, y=112+32}	--
		pasteNext{x=48, y=96+32}	-- headless chocobo tail facing up #2
		pasteNext{x=48, y=112+32}	--
		-- chocobo standing left
		pasteNext{x=0, y=128+32}
		pasteNext{x=0, y=144+32}
		pasteNext{x=16, y=128+32}
		pasteNext{x=16, y=144+32}
		-- chocobo run left #1
		pasteNext{x=32, y=128+32}
		pasteNext{x=32, y=144+32}
		pasteNext{x=48, y=128+32}
		pasteNext{x=48, y=144+32}
		-- chocobo run left #2
		pasteNext{x=64, y=128+32}
		pasteNext{x=64, y=144+32}
		pasteNext{x=80, y=128+32}
		pasteNext{x=80, y=144+32}

		pasteNext{x=64, y=96+32}	-- chocobo wark
		pasteNext{x=64, y=112+32}	-- chocobo eyes closed

		-- magitek stand d
		local magitek = i
		pasteNext{x=0, y=0}
		pasteNext{x=0, y=16}
		-- and then the last two, hflipped
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+0]:mirror(), x=16, y=0}
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+1]:mirror(), x=16, y=16}

		-- magitek walk d
		pasteNext{x=32, y=0}
		pasteNext{x=32, y=16}
		-- and then walk d #2 hflipped
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+4]:mirror(), x=48, y=0}
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+5]:mirror(), x=48, y=16}

		-- magitek walk d #2
		pasteNext{x=64, y=0}
		pasteNext{x=64, y=16}
		-- and then walk d #1 hflipped
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+2]:mirror(), x=80, y=0}
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+3]:mirror(), x=80, y=16}

		-- magitek stand u
		pasteNext{x=0, y=32}
		pasteNext{x=0, y=48}
		-- and then the last two, hflipped
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+6]:mirror(), x=16, y=32}
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+7]:mirror(), x=16, y=48}

		-- magitek walk u
		pasteNext{x=32, y=32}
		pasteNext{x=32, y=48}
		-- and then walk u #2 hflipped
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+10]:mirror(), x=48, y=32}
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+11]:mirror(), x=48, y=48}

		-- magitek walk u #2
		pasteNext{x=64, y=32}
		pasteNext{x=64, y=48}
		-- and then walk u #1 hflipped
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+8]:mirror(), x=80, y=32}
		sheetImg:pasteInto{image=tile16x16Imgs[magitek+9]:mirror(), x=80, y=48}

		-- stand l
		pasteNext{x=0, y=64+32}
		pasteNext{x=16, y=64+32}
		pasteNext{x=0, y=80+32}
		pasteNext{x=16, y=80+32}
		-- walk l #1
		pasteNext{x=32-32, y=64}
		pasteNext{x=48-32, y=64}
		pasteNext{x=32-32, y=80}
		pasteNext{x=48-32, y=80}
		-- walk l #2
		pasteNext{x=64-32, y=64}
		pasteNext{x=80-32, y=64}
		pasteNext{x=64-32, y=80}
		pasteNext{x=80-32, y=80}
		-- walk l #3
		pasteNext{x=96-32, y=64}
		pasteNext{x=112-32, y=64}
		pasteNext{x=96-32, y=80}
		pasteNext{x=112-32, y=80}
	end

	-- now write out all the world sprites
	local chocoImg = Image(256, 256, 1, uint8_t):clear()
	do
		-- airship animation data
		-- 5816
		-- game.worldAnimSpriteData

		-- each is 6144 = 32 bytes per 8x84bpp tile x 192 tiles
		local function save(filename, tileData, palData)
			local tilesWide = 16
			local tilesHigh = #tileData / 32 / tilesWide
			local img = Image(tilesWide * 8, tilesHigh * 8, 1, uint8_t):clear()
			img.palette = makePalette(game, palData, bpp, 16)
			local dataptr = ffi.cast(uint8_t_p, tileData)
			local tile = dataptr + 0
			for y=0,tilesHigh-1 do
				for x=0,tilesWide-1 do
					local i = x + tilesWide * y
					readTile(img, x*8, y*8, tile, bpp)
					tile = tile + 32
				end
			end
			img:save(worldspritedir/filename)
			assert.eq(tile, dataptr + #tileData)
		end
		local decompress = game.decompress
		local blackjackTiles = decompress(game.airship1Compressed, ffi.sizeof(game.airship1Compressed))
		--save('tiles_blackjack.png', blackjackTiles, game.blackjackPalette)
		local falconTiles = decompress(game.airship2Compressed, ffi.sizeof(game.airship2Compressed))
		--save('tiles_falcon.png', falconTiles, game.falconPalette)
		-- what palette do world-map chocobos use? 7 but as hi-pal 0x10, so tell them to use 6-8
		local chocobo1Tiles = decompress(game.worldChocobo1Compressed, ffi.sizeof(game.worldChocobo1Compressed))
		--save('tiles_chocobo1.png', chocobo1Tiles, game.characterPalettes + 7)
		local chocobo2Tiles = decompress(game.worldChocobo2Compressed, ffi.sizeof(game.worldChocobo2Compressed))
		--save('tiles_chocobo2.png', chocobo2Tiles, game.characterPalettes + 7)

		local data = decompress(game.worldAnimSpritesCompressed, ffi.sizeof(game.worldAnimSpritesCompressed))
		save('tiles_worldAnimSprites.png', data, game.WoBPalettes)
		local data = decompress(game.worldMiscSpritesCompressed, ffi.sizeof(game.worldMiscSpritesCompressed))
		save('tiles_worldMiscSprites.png', data, game.WoBPalettes)
		local data = decompress(game.worldBackdropCompressed, ffi.sizeof(game.worldBackdropCompressed))
		save('tiles_worldBackdrop.png', data, game.WoBPalettes)
		local data = decompress(game.WoBMinimapCompressed, ffi.sizeof(game.WoBMinimapCompressed))
		save('tiles_WoBMinimap.png', data, game.WoBPalettes)
		local data = decompress(game.WoRMinimapCompressed, ffi.sizeof(game.WoRMinimapCompressed))
		save('tiles_WoRMinimap.png', data, game.WoRPalettes)

		--[[ 16327 bytes...
		local data = decompress(game.magitekTrainCompressed, ffi.sizeof(game.magitekTrainCompressed))
		print('#magitekTrainCompressed', #data)
		--]]

		local function setPal(img, palData)
			img.palette = makePalette(game, palData, bpp, 16)
			return img
		end

		local function getFrame(i, tileData, palData)
			local palette = palData and makePalette(game, palData, bpp, 256)
			local ofs = game.worldAnimSpriteOfs[i]
			local p = ffi.cast(uint8_t_p, game.worldAnimSpriteData + ofs)
			local numTiles = p[0] p=p+1
			local tiles = table()
			for i=0,numTiles-1 do
				-- xy are signed about 0,0 origin
				local x = ffi.cast(int8_t_p, p)[0] p=p+1
				local y = ffi.cast(int8_t_p, p)[0] p=p+1
				local tileIndex = p[0] p=p+1
				local flags = p[0] p=p+1
--print('tile', x, y, tileIndex, flags)
				if 0 ~= bit.band(1, flags) then tileIndex = tileIndex + 0x100 end
				local palor = bit.band(0x70, bit.lshift(flags, 3))
				local hflip = 0 ~= bit.band(0x40, flags)
				local vflip = 0 ~= bit.band(0x80, flags)
				local zIndex = bit.band(3, bit.rshift(flags, 5))
				tiles:insert{x=x, y=y, tileIndex=tileIndex, palor=palor, hflip=hflip, vflip=vflip, zIndex=zIndex}
			end

			local xmin, xmax = math.huge, -math.huge
			local ymin, ymax = math.huge, -math.huge
			for _,tile in ipairs(tiles) do
				xmin = math.min(xmin, tile.x)
				ymin = math.min(ymin, tile.y)
				xmax = math.max(xmax, tile.x + 8)
				ymax = math.max(ymax, tile.y + 8)
			end

			local tileDataPtr = ffi.cast(uint8_t_p, tileData)

			local img = Image(xmax - xmin, ymax - ymin, 1, uint8_t):clear()
			img.palette = palette
			-- chocobo has shadow under it.  but all are at the same zIndex
			-- and if you draw in-order then the shadow takes precedence
			for i=#tiles,1,-1 do
				local tile = tiles[i]
				local f = palette and drawTile or readTile
				f(
					img,
					tile.x - xmin,
					tile.y - ymin,
					tileDataPtr + bit.lshift(tile.tileIndex, 5),
					bpp,
					tile.hflip,
					tile.vflip,
					tile.palor,
					palette
				)
			end
			return img
		end

		-- getting save order from everything8215/ff6/src/world/world_anim.asm

		-- I guess this means blackjackTiles starts at 0x40?
		-- and there were 0xc0 of them
		local blackjackTilesPadded = ('\0'):rep(64 * 32) .. blackjackTiles
		local falconTilesPadded = ('\0'):rep(64 * 32) .. falconTiles

		-- airship (back view)
		local airshipFrameNames = table{'idle1', 'idle2', 'l1', 'l2', 'r1', 'r2', 'u1', 'u2', 'ul1', 'ul2', 'ur1', 'ur2', 'd1', 'd2', 'dl1', 'dl2', 'dr1', 'dr2'}
		assert.len(airshipFrameNames, 0x12)
		for i=1,0x12 do
			--frame:save(worldspritedir/('blackjack_'..airshipFrameNames[i]..'.png'))
			sheetImg:pasteInto{
				image = getFrame(i, blackjackTilesPadded, game.blackjackPalette) + saveBlackjackIndexOffset,
				x = 96 + math.floor((i-1)%6)*24,
				y = math.floor((i-1)/6)*32,
			}
			--frame:save(worldspritedir/('falcon_'..airshipFrameNames[i]..'.png'))
			sheetImg:pasteInto{
				image = getFrame(i, falconTilesPadded, game.falconPalette) + saveFalconIndexOffset,
				x = 96 + math.floor((i-1+18)%6)*24,
				y = math.floor((i-1+18)/6)*32,
			}
		end
--[[ I don't need all of these if 3d hw can do the same ... 'ground' and 'liftoff3' are enough
		for i=0x45,0x48	do
			getFrame(i, blackjackTilesPadded, game.blackjackPalette)
				:save(worldspritedir/('blackjack_liftoff'..(i-0x44)..'.png'))
		end	-- blackjack on grond
		getFrame(0x4d, blackjackTilesPadded, game.blackjackPalette)	-- blackjack lift off
			:save(worldspritedir/('blackjack_ground.png'))

		for i=0x62,0x65 do
			getFrame(i, falconTilesPadded, game.falconPalette)
				:save(worldspritedir/('falcon_liftoff'..(i-0x61)..'.png'))
		end	-- falcon on ground
		getFrame(0x66, falconTilesPadded, game.falconPalette)	-- falcon lifting off
			:save(worldspritedir/'falcon_ground.png')
--]]
-- [[
		-- TODO for the world-map airships we can't use the blackjack or falcon palettes, gotta add a new palette to the sheet view save file
		-- these all ahve hi-pal 0x10
		sheetImg:pasteInto{image = getFrame(0x4d, blackjackTilesPadded, game.blackjackPalette) - 0x10 + saveBlackjackIndexOffset, x = 0, y = 192}
		sheetImg:pasteInto{image = getFrame(0x47, blackjackTilesPadded, game.blackjackPalette) - 0x10 + saveBlackjackIndexOffset, x = 40, y = 192}
		sheetImg:pasteInto{image = getFrame(0x66, falconTilesPadded, game.falconPalette) - 0x10 + saveFalconIndexOffset, x = 0, y = 216}
		sheetImg:pasteInto{image = getFrame(0x64, falconTilesPadded, game.falconPalette) - 0x10 + saveFalconIndexOffset, x = 40, y = 216}
--]]

		-- there's 0x200 tiles
		-- chocobo1 & chocobo2 uses 0x40 thru 0xc0
		-- their anim sprites use 0xa0-0xef
		local tile = chocobo1Tiles:sub(0x80*0x20+1, (0x80+1)*0x20)
		assert.len(tile, 32)
		local chocoboTilesPadded =
			-- [[
			('\0'):rep(0x40 * 0x20)
			.. chocobo1Tiles	-- 0x40-0xc0
			.. ('\0'):rep(0x40 * 0x20)
			.. chocobo2Tiles	-- 0x40-0xco
			.. ('\0'):rep(0x200 * 0x20)
			--]]
		local x, y = 0, 0
		local nextY = 0
		for _,i in ipairs(table():append(
			range(0x13, 0x25),	-- chocobo, uses hi-pal 0x10 & 0x20
			range(0x49,0x4c)
		)) do
			local frame = getFrame(i, chocoboTilesPadded, game.characterPalettes + 6)
			local frameWidthTileAligned = math.ceil(frame.width / 8) * 8
			local frameHeightTileAligned = math.ceil(frame.height / 8) * 8
			if x + frame.width >= chocoImg.width then
				x = 0
				y = nextY
			end
			chocoImg:pasteInto{image=frame, x=x, y=y}
			x = x + frameWidthTileAligned
			nextY = math.max(nextY, frameHeightTileAligned)
		end
		for i=0x26,0x2d do end	-- character
		for i=0x2e,0x32 do end
		for i=0x33,0x36 do end
		for i=0x37,0x3e do end	-- ship
		for i=0x3f,0x44 do end	-- arrows
		for i=0x49,0x4c do end	-- dismount chocobo
		for i=0x4e,0x53 do end	-- esper terra
		for i=0x54,0x55 do end
		for i=0x56,0x59 do end	-- smoking airship
		for i=0x5a,0x5e do end
		for i=0x5f,0x61 do end	-- bird
		for i=0x67,0x6b do end
	end

	sheetImg.palette = table.rep(sheetImg.palette, 256 / #sheetImg.palette)
	sheetImg:save(worldspritedir/'vehicle-sheet.png')
	-- now save another copy for exporting that has all but the lower 4 bits zeroed
	for i=0,sheetImg.width*sheetImg.height-1 do
		sheetImg.buffer[i] = bit.band(0xf, sheetImg.buffer[i])
	end
	sheetImg:save(worldspritedir/'vehicle-sheet-export.png')

	chocoImg.palette = table():append(
		game.getCharPal(6),
		game.getCharPal(7),
		game.getCharPal(8)
	)
	chocoImg.palette = table.rep(chocoImg.palette, 256 / #chocoImg.palette)
	chocoImg:save(worldspritedir/'chocobo.png')
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local game = require 'ff6'(( assert(path((...)):read()) ))
	run(game)
end

return run
