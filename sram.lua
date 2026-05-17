#!/usr/bin/env luajit
-- this is separate of everything else, just a quick and dirty sram viewer
-- https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:ram:sram&s[]=%2Asram%2A
local ffi = require 'ffi'
local path = require 'ext.path'
local assert = require 'ext.assert'
local struct = require 'struct'

local ff6_util = require 'ff6.util'
local countof = ff6_util.countof
local arrayType = ff6_util.arrayType
local uint24_t = ff6_util.uint24_t
local Effect1 = ff6_util.Effect1
local Effect4 = ff6_util.Effect4
local CharacterName = ff6_util.CharacterName


local uint8_t = ffi.typeof'uint8_t'
local uint8_t_p = ffi.typeof'uint8_t*'
local uint16_t = ffi.typeof'uint16_t'


local fn, romfn = ...
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

local SaveSlot = struct{
	ctypeOnly = true,
	fields = {
		{name='characters', type=arrayType(Character, 0x10)},				-- 0x000 - 0x250
		-- where are the battle group encounter flags?

		{name='unknown_0250', type=arrayType(uint8_t, -(0x250 - 0x960))},	-- 0x250 - 0x960

		{name='mapx', type=uint8_t},										-- 0x960
		{name='mapy', type=uint8_t},										-- 0x961

		{name='unknown_0962', type=arrayType(uint8_t, -(0x962 - 0x96b))},	-- 0x962 - 0x96b
		{name='mapx2', type=uint8_t},										-- 0x96b
		{name='mapy2', type=uint8_t},										-- 0x96c
		{name='unknown_096d', type=arrayType(uint8_t, -(0x96d - 0xa00))},	-- 0x96d - 0xa00
	},
}
assert.eq(ffi.sizeof(SaveSlot), 0xa00)

-- wait is this true? or was I looking at RAM, not SRAM?
local SRAM = struct{
	ctypeOnly = true,
	fields = {
		{name='saves', type=arrayType(SaveSlot, 3)},						-- 0x0000 - 0x1e00
		{name='unknown_1e00', type=arrayType(uint8_t, -(0x1e00 - 0x1ff0))},	-- 0x1e00 - 0x1ff0
		{name='mostRecentSaveSlot', type=uint8_t},							-- 0x1ff0 - 0x1ff1
		{name='randomNumberSeed', type=uint8_t},							-- 0x1ff1 - 0x1ff2
		{name='unknown_1ff2', type=arrayType(uint8_t, -(0x1ff2 - 0x1ff8))},	-- 0x1ff2 - 0x1ff8
		{name='tailsig', type=arrayType(uint8_t, 8)},						-- 0x1ff8 - 0x2000
	},
}
assert.eq(ffi.sizeof(SRAM), 0x2000)

assert.eq(ffi.sizeof(SRAM), #data)	-- make sure it fits

local sram = ffi.cast(ffi.typeof('$*', SRAM), ptr)

for i=0,countof(sram.saves)-1 do
	local save = sram.saves + i
	print()
	print('save', i)
	for j=0,countof(save.characters)-1 do
		local ch = save.characters + j
		print('char['..j..'] = '..tostring(ch))
	end
end
