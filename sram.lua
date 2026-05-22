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

local uint8_t_p = ffi.typeof'uint8_t*'

local romfn, sramfn, saveIndex = ...

local game = require 'ff6'((assert(path(romfn):read())))

assert(sramfn, "expected <filename>")
local p = path(sramfn)
assert(p:exists(), "failed to find "..p)
local data = assert(p:read())
assert.len(data, 0x2000)
local ptr = ffi.cast(uint8_t_p, data)

assert.eq(ffi.sizeof(game.SRAM), #data)	-- make sure it fits


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

-- reverse-reference treasure # for its bits ...
local treasuresForFlagIndex = {}
for treasureIndex=0,countof(game.treasures)-1 do
	local t = game.treasures + treasureIndex
	local flag = tonumber(t.flag)
	treasuresForFlagIndex[flag] = treasuresForFlagIndex[flag] or table()
	treasuresForFlagIndex[flag]:insert{
		treasure = game.Treasure(t[0]),
		maps = mapsForTreasure[treasureIndex],
	}
end

local sram = ffi.cast(ffi.typeof('$*', game.SRAM), ptr)
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

	local maxCols = 64	-- TODO curses?

	print()
	print'treasures:'
	-- if sfc is provided then show long-form like we're doing
	--  but if it's not then just show a matrix of checkboxes, since we can't get the names/locations anyways
	for i=0,bit.lshift(countof(save.treasureFlags),3)-1 do
		local byteofs = bit.rshift(i,3)
		local bitofs = bit.band(i, 7)
		local mask = bit.lshift(1, bitofs)
		local enabled = 0 ~= bit.band(mask, save.treasureFlags[byteofs])
		io.write(i, '\t', enabled and '✅' or '❌')
		if treasuresForFlagIndex then
			local sep = ''
			for _,t in ipairs(treasuresForFlagIndex[i] or {}) do
				io.write(sep, ' ', tostring(t.treasure)..' in maps '..t.maps:concat',')
				sep = ';'
			end
		end
		print()
	end

	local function align(n, s)
		s = tostring(s)
		return s..(' '):rep(n - #s)
	end

	-- formation count is 0x240
	-- that's 0x48 bytes of flags
	-- but save files only have 0x40 flags
	-- so ...
	local formationsEnabled = {}
	local monstersEnabled = {}
	local numFormationFlags = bit.lshift(countof(save.battleFormationFlags),3)
	for i=0,numFormationFlags-1 do
		local byteofs = bit.rshift(i,3)
		local bitofs = bit.band(i, 7)
		local mask = bit.lshift(1, bitofs)
		local formationEnabled = 0 ~= bit.band(mask, save.battleFormationFlags[byteofs])
		formationsEnabled[i] = formationEnabled
		if formationEnabled then
			local formation = game.formations + i
			-- do I care about chooseNextFour as well?
			for j=1,6 do
				if formation:getMonsterActive(j) then
					local monsterIndex = formation:getMonsterIndex(j)
					monstersEnabled[monsterIndex] = true
				end
			end
		end
	end

	print()
	print(
		align(4+3+72, 'battle formations:')
		..'monsters:'
	)
	local rowCount = math.max(countof(game.monsters), numFormationFlags)
	for i=0,rowCount-1 do
		local monsterEnabled
		if i < countof(game.monsters) then
			monsterEnabled = not not monstersEnabled[i]
		end
		print(
			align(4, i)											-- biggest is 3
			..align(3, formationsEnabled[i] and '✅' or '❌')	-- biggest is 1 but it needs 2 spaces...
			..align(72, game.formations[i]:getDesc() or 'formation #'..i)			-- biggest is 70
			..(monsterEnabled ~= nil
				and align(4, i)
					..align(3, monsterEnabled and '✅' or '❌')
					..(game.monsterNames[i] or 'monster #'..i)
				or ''
			)
		)
	end

end
--]]
