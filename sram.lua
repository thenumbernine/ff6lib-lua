#!/usr/bin/env luajit
-- this is separate of everything else, just a quick and dirty sram viewer
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

-- wait is this true? or was I looking at RAM, not SRAM?
local SRAM = struct{
	ctypeOnly = true,
	fields = {
		{name='characters', type=arrayType(Character, 0x10)},	-- 0x0000 - 0x0250
		-- where are the battle group encounter flags?
	},
}

local function assertOffset(name, addr)
	assert.eq(ffi.offsetof(SRAM, name), addr, name)
end

assertOffset('characters', 0)

assert.le(ffi.sizeof(SRAM), #data)	-- make sure it fits
local sram = ffi.cast(ffi.typeof('$*', SRAM), ptr)
for i=0,countof(sram.characters)-1 do
	print('char['..i..'] = '..tostring(sram.characters[i]))
end
