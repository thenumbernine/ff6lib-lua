local table = require 'ext.table'

local function settableindex(t, i, ...)
	if select('#', ...) == 0 then return end
	t[i] = ...
	settableindex(t, i+1, select(2, ...))
end

local function settable(t, ...)
	settableindex(t, 1, ...)
end



local zAndLayersWithoutLayer3Priority = {
	{0,3},
	-- priority 0 sprites here
	--{1,3},	-- does layer 3 have a zlevel?  where is it?
	-- priority 1 sprites here
	{0,2},
	{0,1},
	-- priority 2 sprites here
	{1,2},
	{1,1},
	-- priority 3 sprites here
}

local zAndLayersWithLayer3Priority = {
	{0,2},
	{0,1},
	{1,2},
	{1,1},
	{0,3},
}


local numTilePropsBits = 18

local worldTilePropsNames = {
	'blocksChocobo',
	'airshipCantLand',
	'airshipShadow_0',
	'airshipShadow_1',
	'blocksWalking',
	'bottomHalfSemiTransparent',
	'enemyEncounters',
	'unknown_0_7',
	'battleBG_0',
	'battleBG_1',
	'battleBG_2',
	'battleBG_3',
	'unknown_0_12',
	'veldt',
	'phoenixCave',
	'kefkasTower',
	-- my extras ... here just to match mapTilePropsNames
	'unused1',
	'unused2',
}
local worldTilePropsFlagForName = table.map(worldTilePropsNames, function(name, index)
	return bit.lshift(1, index-1), name
end):setmetatable(nil)


local mapTilePropsNames = {
	'zLevel_0',
	'zLevel_1',
	'zLevel_2',
	'topSpritePriority',
	'bottomSpritePriority',
	'door',
	'stairsUpRight',
	'stairsUpLeft',
	'passableRight',
	'passableLeft',
	'passableDown',
	'passableUp',
	'unknown_1_4',
	'unknown_1_5',
	'ladder',
	'passableNPC',
	-- my extras ... here to use bitflags for special-values of map tile props (for the bitflag-overlay renderer)
	'throughTile',
	'impassible',
}
local mapTilePropsFlagForName = table.map(mapTilePropsNames, function(name, index)
	return bit.lshift(1, index-1), name
end):setmetatable(nil)


return {
	settable = settable,
	zAndLayersWithoutLayer3Priority = zAndLayersWithoutLayer3Priority,
	zAndLayersWithLayer3Priority = zAndLayersWithLayer3Priority,
	numTilePropsBits = numTilePropsBits,
	worldTilePropsNames = worldTilePropsNames,
	worldTilePropsFlagForName = worldTilePropsFlagForName,
	mapTilePropsNames = mapTilePropsNames,
	mapTilePropsFlagForName = mapTilePropsFlagForName,
}
