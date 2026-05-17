#!/usr/bin/env luajit
-- this is separate of everything else, just a quick and dirty sram viewer
-- https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:ram:sram&s[]=%2Asram%2A
local ffi = require 'ffi'
local table = require 'ext.table'
local path = require 'ext.path'
local assert = require 'ext.assert'
local createVec = require 'vec-ffi.create_vec'
local struct = require 'struct'

local ff6_util = require 'ff6.util'
local countof = ff6_util.countof
local arrayType = ff6_util.arrayType
local uint24_t = ff6_util.uint24_t
local Effect1 = ff6_util.Effect1
local Effect4 = ff6_util.Effect4
local Str6 = ff6_util.Str6
local CharacterName = ff6_util.CharacterName
local makefixedstr = ff6_util.makefixedstr


local uint8_t = ffi.typeof'uint8_t'
local uint8_t_p = ffi.typeof'uint8_t*'
local uint16_t = ffi.typeof'uint16_t'
local uint32_t = ffi.typeof'uint32_t'


local fn, romfn, saveIndex = ...
assert(fn, "expected <filename>")
local p = path(fn)
assert(p:exists(), "failed to find "..p)
local data = assert(p:read())
assert.len(data, 0x2000)
local ptr = ffi.cast(uint8_t_p, data)


local game
if romfn then
	game = require 'ff6'((assert(path(romfn):read())))
end

local menuNameRef4Type =
	game and game.MenuNameRef4
	or require 'vec-ffi.vec4ub'	-- TODO use MenuNameRef4 instead

local itemRefType = game and game.ItemRef or uint8_t

local Character = struct{
	ctypeOnly = true,
	packed = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	fields = {
		{name='character', type=uint8_t},	-- CharacterNameRef?
		{name='sprite', type=uint8_t},
		{name='name', type=CharacterName},
		{name='level', type=uint8_t},
		{name='hp', type=uint16_t},
		{name='hpMax', type=uint16_t},
		{name='mp', type=uint16_t},
		{name='mpMax', type=uint16_t},
		{name='exp', type=uint24_t},
		{name='effect1', type=Effect1},
		{name='effect4', type=Effect4},
		--[[
		{name='menus', type=menuNameRef4Type},
		--]]
		-- [[ MenuNameRef returns nil for oob, and I think it does this because the struct serialization tests for nil and skips the field entirely, but the underlying vector concats without tostring or test, so..
		{name='menu1', type=game and game.MenuNameRef or uint8_t},
		{name='menu2', type=game and game.MenuNameRef or uint8_t},
		{name='menu4', type=game and game.MenuNameRef or uint8_t},
		{name='menu5', type=game and game.MenuNameRef or uint8_t},
		--]]
		{name='vigor', type=uint8_t},
		{name='speed', type=uint8_t},
		{name='stamina', type=uint8_t},
		{name='magicPower', type=uint8_t},
		{name='esper', type=itemRefType},
		{name='rhand', type=itemRefType},
		{name='lhand', type=itemRefType},
		{name='head', type=itemRefType},
		{name='body', type=itemRefType},
		{name='relic1', type=itemRefType},
		{name='relic2', type=itemRefType},
	},
}
assert.eq(ffi.sizeof(Character), 0x25)

local Character16 = createVec{
	ctypeOnly = true,
	ctype = Character,
	dim = 0x10,
}

local vec16ub = createVec{
	ctypeOnly = true,
	ctype = uint8_t,
	dim = 16,
}

local Str12 = makefixedstr(12)

-- why just the first 4 names tho?
local Str12_4 = createVec{
	ctypeOnly = true,
	ctype = Str12,
	dim = 4,
}

local SaveSlot = struct{
	ctypeOnly = true,
	packed = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,

	fields = {
		{name='characters', type=Character16},								-- 0x000 - 0x250
		{name='raster', type=vec16ub},										-- 0x250 - 0x260
		{name='gold', type=uint24_t},										-- 0x260 - 0x263
		{name='time', type=uint24_t},										-- 0x263 - 0x266	-- in 30hz increments?
		{name='steps', type=uint24_t},										-- 0x266 - 0x269
		{name='itemTypes', type=arrayType(itemRefType, 256)},				-- 0x269 - 0x369
		{name='itemCounts', type=arrayType(uint8_t, 256)},					-- 0x369 - 0x469
		{name='esperFlags', type=uint32_t},									-- 0x469 - 0x46d
		{name='activeGroup', type=uint8_t},									-- 0x46d - 0x46e	-- for when you are in multi-party mode? like phoenix cave / kefka's tower?
		{name='spellsLearned', type=arrayType(uint8_t, 12 * 54)},			-- 0x46e - 0x6f6 ... spell learned[12][54]
		{name='unknown_6f6', type=uint8_t},									-- 0x6f6 - 0x6f7
		{name='swdtechFlags', type=uint8_t},								-- 0x6f7 - 0x6f8	-- wait, in-rom it is 12 bytes per swdtech (right?)
		{name='swdtechNames', type=Str12_4},								-- 0x6f6 - 0x728
		{name='blitzFlags', type=uint8_t},									-- 0x728 - 0x729
		{name='loreFlags', type=uint24_t},									-- 0x729 - 0x72c
		{name='rageFlags', type=arrayType(uint8_t, 32)},					-- 0x72c - 0x74c
		{name='danceFlags', type=uint8_t},									-- 0x74c - 0x74d

		{name='unknown_74d', type=arrayType(uint8_t, -(0x74d - 0x7c7))},	-- 0x74d - 0x7c7

		{name='numSaves', type=uint8_t},									-- 0x7c7 - 0x7c8

		{name='unknown_7c8', type=uint8_t},									-- 0x7c8 - 0x7c9

		-- 0x7c9 = 'battle variables'
		-- with doom gate's hp at 0x7d3-0x7d4
		-- cursed shield counter at 0x7d5
		{name='battleVars', type=arrayType(uint8_t, 20)},					-- 0x7c9 - 0x7dd

		{name='battleFormationFlags', type=arrayType(uint8_t, 0x40)},		-- 0x7dd - 0x81d

		{name='unknown_81d', type=arrayType(uint8_t, -(0x81d - 0x840))},	-- 0x81d - 0x840 = 23 bytes ...

		{name='treasureFlags', type=arrayType(uint8_t, 0x40)},				-- 0x840 - 0x880 = "treasure bits" here: https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:ram:field_ram&s[]=%2Asram%2A#fffsave_ram
		{name='mapFlags', type=arrayType(uint8_t, 0x60)},					-- 0x880 - 0x8e0 = 768 = 0x300 flags /8 = 0x60 bytes. everything8215's "mapSwitches", or this page's "event bits": https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:ram:field_ram&s[]=%2Asram%2A#fffsave_ram
		{name='npcFlags', type=arrayType(uint8_t, 0x80)},					-- 0x8e0 - 0x960

		{name='mapx', type=uint8_t},										-- 0x960 - 0x961
		{name='mapy', type=uint8_t},										-- 0x961 - 0x962
		{name='airshipx', type=uint8_t},									-- 0x962 - 0x963
		{name='airshipy', type=uint8_t},									-- 0x963 - 0x964

		{name='map', type=uint8_t},											-- 0x964 - 0x965

		{name='unknown_965', type=arrayType(uint8_t, -(0x965 - 0x96b))},	-- 0x965 - 0x96b

		{name='mapx2', type=uint8_t},										-- 0x96b
		{name='mapy2', type=uint8_t},										-- 0x96c
		{name='unknown_96d', type=arrayType(uint8_t, -(0x96d - 0xa00))},	-- 0x96d - 0xa00

		-- where are the npc flags? 1024 bits = 0x80 bytes
	},
}
assert.eq(ffi.sizeof(SaveSlot), 0xa00)

-- arrayType doesn't give you tostring support so
local SaveSlot3 = createVec{
	ctypeOnly = true,
	ctype = SaveSlot,
	dim = 3,
}

-- wait is this true? or was I looking at RAM, not SRAM?
local SRAM = struct{
	ctypeOnly = true,
	packed = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,

	fields = {
		{name='saves', type=SaveSlot3},						-- 0x0000 - 0x1e00
		{name='unknown_1e00', type=arrayType(uint8_t, -(0x1e00 - 0x1ff0))},	-- 0x1e00 - 0x1ff0
		{name='mostRecentSaveSlot', type=uint8_t},							-- 0x1ff0 - 0x1ff1
		{name='randomNumberSeed', type=uint8_t},							-- 0x1ff1 - 0x1ff2
		{name='unknown_1ff2', type=arrayType(uint8_t, -(0x1ff2 - 0x1ff8))},	-- 0x1ff2 - 0x1ff8

		-- previous 8 bytes ... first is 01, next is the ... save count?  maybe first is hi byte?
		{name='tailsig', type=arrayType(uint8_t, 8)},						-- 0x1ff8 - 0x2000 .. 1b e4 1b e4 1b e4 1b e4
	},
}
assert.eq(ffi.sizeof(SRAM), 0x2000)

assert.eq(ffi.sizeof(SRAM), #data)	-- make sure it fits

-- reverse-reference treasure # for its bits ...
local treasuresForFlagIndex
if game then
	-- [[ a lot is identical to maps.lua treasure generation, i could consolidate if I stored either pointers there or objects and addresses there
	local mapsForTreasure = {}	 -- key = treasure index, value = table of map indexes
	for mapIndex=0,countof(game.maps) do
		local startOfs = game.treasureOfs[mapIndex]
		assert.eq(startOfs % ffi.sizeof(game.Treasure), 0)
		local startIndex = startOfs / ffi.sizeof(game.Treasure)

		local endIndex
		if mapIndex == countof(game.treasureOfs)-1 then
			endIndex = countof(game.treasures)
		else
			local endOfs = game.treasureOfs[mapIndex+1]
			assert.eq(endOfs % ffi.sizeof(game.Treasure), 0)
			endIndex = endOfs / ffi.sizeof(game.Treasure)
		end
		for treasureIndex=startIndex,endIndex-1 do
			local t = game.treasures + treasureIndex
			mapsForTreasure[treasureIndex] = mapsForTreasure[treasureIndex] or table()
			mapsForTreasure[treasureIndex]:insert(mapIndex)
		end
	end
	--]]

	treasuresForFlagIndex = {}
	for treasureIndex=0,countof(game.treasures)-1 do
		local t = game.treasures + treasureIndex
		local flag = tonumber(t.flag)
		treasuresForFlagIndex[flag] = treasuresForFlagIndex[flag] or table()
		treasuresForFlagIndex[flag]:insert{
			treasure = game.Treasure(t[0]),
			maps = mapsForTreasure[treasureIndex],
		}
	end
end

local sram = ffi.cast(ffi.typeof('$*', SRAM), ptr)
--[[
print(sram)
--]]
-- [[
local saveMin, saveMax
if saveIndex then
	saveMin = assert(tonumber(saveIndex))	-- 0-based
	saveMax = saveMin
else
	saveMin, saveMax = 0, sram.saves.dim-1
end
for i=saveMin, saveMax do
	local save = sram.saves.s + i
	print()
	print('save', i)
	for fieldname in save:fielditer() do
		if fieldname ~= 'characters' then
			print('', fieldname, save[fieldname])
		end
	end
	for j=0,save.characters.dim-1 do
		local ch = save.characters.s + j
		print('', 'char['..j..']')
		for fieldname in ch:fielditer() do
			print('', '', fieldname, ch[fieldname])
		end
	end

	print()
	print'treasures:'
	for i=0,bit.lshift(countof(save.treasureFlags),3)-1 do
		local byteofs = bit.rshift(i,3)
		local bitofs = bit.band(i, 7)
		local mask = bit.lshift(1, bitofs)
		local enabled = 0 ~= bit.band(mask, save.treasureFlags[byteofs])
		local t = game.treasures + i
		io.write(i, '\t', enabled and '✅' or '❌')
		local sep = ''
		for _,t in ipairs(treasuresForFlagIndex[i] or {}) do
			io.write(sep, ' ', tostring(t.treasure)..' in maps '..t.maps:concat',')
			sep = ','
		end
		print()
	end

	-- formation count is 0x240
	-- that's 0x48 bytes of flags
	-- but save files only have 0x40 flags
	-- so ...
	print()
	print'battle formations:'
	for i=0,bit.lshift(countof(save.battleFormationFlags),3)-1 do
		local byteofs = bit.rshift(i,3)
		local bitofs = bit.band(i, 7)
		local mask = bit.lshift(1, bitofs)
		local enabled = 0 ~= bit.band(mask, save.battleFormationFlags[byteofs])
		print(i, enabled and '✅' or '❌', game.getFormationName(i))
	end
end
--]]
