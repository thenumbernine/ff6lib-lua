#!/usr/bin/env luajit
require 'ext'
local ffi = require 'ffi'
local Image = require 'image'
local gnuplot = require 'gnuplot'
local makePalette = require 'ff6.graphics'.makePalette
local makePaletteSets = require 'ff6.graphics'.makePaletteSets
local tileWidth = require 'ff6.graphics'.tileWidth
local tileHeight = require 'ff6.graphics'.tileHeight
local readTile = require 'ff6.graphics'.readTile

local infn, outfn = ...
assert(infn, "missing filename")

-- you can only load this once, from there all the types' metatables are bound to the game pointer
-- so even if you load a second game file, all the metatypes will be bound to the first pointer
-- I can't change that without associating the C data with the Lua object.
-- TODO TODO TODO fix this
local game = require 'ff6'((assert(path(infn):read())))
local rom = game.rom
local romsize = game.romsize

local countof = game.countof

for i=0,game.numSpells-1 do
	print('spell #'..i)
	print('Name="'..game.getSpellName(i)..'"')
	if i < 54 then
		print('Desc="'..game.gamezstr(game.spellDescBase + game.spellDescOffsets[i])..'"')
	elseif i >= 54 and i < 64 then
		-- should I put esper descs here, or in the esper output, or both?
	end
	print(game.spells[i])
	print()
end

for i=0,game.numSpells-1 do
	if game.spells[i].isLore ~= 0 then
		print('Name="'..game.getSpellName(i)..'"')
		for j=0,game.numMonsters-1 do
			for k=0,1 do
				if game.monsterSketches[j].s[k].i == i then
					print('\tsketch '..game.monsterNames[j])
				end
			end
			if j < game.numRages then
				for k=0,1 do
					if game.monsterRages[j].s[k].i == i then
						print('\trage '..game.monsterNames[j])
					end
				end
			end
		end
	end
end
print()

for i=0,game.numEspers-1 do
	print('esper #'..i)
	print('Name="'..game.getEsperName(i)..'"')
	print('AttackName="'..game.esperAttackNames[i]..'"')
	print('Desc="'..game.gamezstr(game.esperDescBase + game.esperDescOffsets[i])..'"')
	print(game.espers[i])
	print()
end

for i=0,game.numEsperBonuses-1 do
	print('esper bonus #'..i)
	print('desc = "'..game.esperBonusDescs[i]..'"')
	print('long desc = "'..game.gamezstr(game.longEsperBonusDescBase  + game.longEsperBonusDescOffsets[i])..'"')
	print()
end

for i=0,game.numItems-1 do
	print('item #'..i)
	print('Name="'..game.itemNames[i]..'"')
	print('Desc="'..game.gamezstr(game.itemDescBase + game.itemDescOffsets[i])..'"')
	print(game.items[i])
	print(game.itemColosseumInfos[i])
	print()
end

for i=0,game.numItemTypes-1 do
	print('item type #'..i..' = '..game.itemTypeNames[i])
end
print()

for i=0,game.numRareItems-1 do
	print('rare item #'..i)
	print('name="'..game.rareItemNames[i]..'"')
	print('desc="'..game.gamezstr(game.rareItemDescBase + game.rareItemDescOffsets[i])..'"')
	print()
end

for i=0,game.numCharacterPalettes-1 do
	print('characterPalettes[0x'..i:hex()..'] = '..game.characterPalettes[i])
end
print()

do
	local unique = {}
	for i=0,game.numMonsterPalettes-1 do
		print('monsterPalettes[0x'..i:hex()..'] = '..game.monsterPalettes[i])
		unique[ffi.string(game.monsterPalettes + i, ffi.sizeof'palette8_t')] = true
	end
	-- 646 of 768 unique values used
	-- only indexes 0-656 are used, the rest are black
	-- overall, 256 x 20.5 palette-blobs are used
	print('# unique: '..#table.keys(unique))
	print()
end

for i=0,game.numMonsters-1 do
	print('monster #'..i)
	print('Name="'..game.monsterNames[i]..'"')
	print('AttackName="'..game.monsterAttackNames[i]..'"')
	print(game.monsters[i])
	print('spells = '..game.monsterSpells[i])
	print(game.monsterItems[i])
	print('sketches = '..game.monsterSketches[i])
	if i < game.numRages then print('rages = '..game.monsterRages[i]) end
	print()
end


-- [[ battle backgrounds
-- wonder where I should put this ...
-- with monster formations?
-- with world maps?
-- meh, it is here for now.
local decompress = require 'ff6.decompress'
local battleBgGfxs = table()	-- 0-based
for i=0,countof(game.battleBackgroundGraphicsOffsets)-1 do
	local addr = game.battleBackgroundGraphicsOffsets[i]:value()
	if addr ~= 0 then
		addr = addr - 0xc00000
		assert.ge(addr, 0)
		assert.lt(addr, romsize) 
		local data = decompress(rom + addr, romsize)
		-- size is very arbitrary and a lot look like bad data
		print('battle bg gfx [0x'..i:hex()..'] = 0x'..addr:hex()
			..(data and ' len=0x'..(#data):hex() or '')
		)
		battleBgGfxs[i] = {
			index = i,
			addr = addr,
			data = data,
		}
	end
end
print()

--[[
size is always 0x800
there's 32x19 8x8 tiles that I see, so 608 = 0x260 tiles ...
 2 bytes per tile = 0x4c0 bytes total
 3 bytes per tile = 0x720 bytes total
--]]
local battleBgLayouts = table()	-- 0-based
for i=0,countof(game.battleBackgroundLayoutOffsets)-1 do
	local offset = game.battleBackgroundLayoutOffsets[i]
	local addr = offset + 0x270000
	assert.ge(addr, ffi.offsetof('game_t', 'battleBackgroundLayoutCompressed'))
	assert.lt(addr, ffi.offsetof('game_t', 'battleBackgroundLayoutCompressed') + ffi.sizeof(game.battleBackgroundLayoutCompressed))
	local data = decompress(rom + addr, ffi.sizeof(game.battleBackgroundLayoutCompressed))
	print('battle bg layout [0x'..i:hex()..'] ='
		..' offset=0x'..addr:hex()
		..' addr=0x'..addr:hex()
		..(data and ' len=0x'..(#data):hex() or '')
	)
	if data then
		print(data:hexdump())
	end
	battleBgLayouts[i] = {
		index = i,
		offset = offset,
		addr = addr,
		data = data,
	}
end
print()

for i=0,countof(game.battleBackgroundProperties)-1 do
	local bg = game.battleBackgroundProperties + i
	print('battle bg 0x'..i:hex()..' = '..bg)
	print('\tmog dance', game.mogDanceNames[game.battleBackgroundDance[i]])

	-- same rendering as with maps?
	-- but use what sizes
end
os.exit()

--]]




local totalPixels = 0
-- [[
local writeMonsterSprite = require 'ff6.monstersprite'
for i=0,game.numMonsterSprites-1 do
	print('monsterSprites[0x'..i:hex()..'] = '..game.monsterSprites[i])
	totalPixels = totalPixels + writeMonsterSprite(game, i)
end
print('wrote monster pixels', totalPixels)
print()
--]]

-- [[ see how many unique monsters there are ...
local monsterSpriteOffsetSet = {}
--local monsterSpriteOffsets = table() -- from 1-based monster index to offset #

-- from palLo | palHi | _3bpp to monsterSprite.offset
-- so I can try to group monsterSprite.offset by matching palettes
local monsterPalettes_monsterSpriteIndexes = {}

for i=0,game.numMonsterSprites-1 do
	local monsterSprite = game.monsterSprites + i
	if not monsterSpriteOffsetSet[monsterSprite.offset] then
		monsterSpriteOffsetSet[monsterSprite.offset] = table()
	end
	monsterSpriteOffsetSet[monsterSprite.offset]:insert(i)
	--monsterSpriteOffsets[i+1] = monsterSpriteOffsetSet[monsterSprite.offset][1]

	local palIndex = bit.bor(
		monsterSprite.palLo,
		bit.lshift(monsterSprite.palHi, 8),
		bit.lshift(monsterSprite._3bpp, 15)
	)
	monsterPalettes_monsterSpriteIndexes[palIndex] = monsterPalettes_monsterSpriteIndexes[palIndex] or {}
	monsterPalettes_monsterSpriteIndexes[palIndex][i] = true
end

--print('monsterSpriteOffsets = '..tolua(monsterSpriteOffsets))

--[[
_3bpp, tile16, tileMaskIndex are constant per-offset
only palHi and palLo vary per offset
so here, write us only unique monsters with the first palette
--]]
for _,offset in ipairs(table.keys(monsterSpriteOffsetSet):sort()) do
	print('monsterSpriteOffset '..('0x%04x'):format(offset)..' has sprites: '..table.concat(monsterSpriteOffsetSet[offset], ', '))
end
print()

do
	local monsterPalettesUnique = table.keys(
		monsterPalettes_monsterSpriteIndexes
	):sort()

	-- what about palette indexes that are odd ...
	-- if a palette index is odd ....
	local transparents = {}
	local opaques = {}
	for _,pal in ipairs(monsterPalettesUnique) do
		local indexbase = bit.lshift(bit.band(pal, 0x7fff), 3)
		local bpp = bit.band(pal, 0x8000) ~= 0 and 3 or 4
		local numColors = bit.lshift(bpp, 1)
		transparents[indexbase] = true
		assert.eq(opaques[indexbase], nil)
		for i=indexbase+1,indexbase+numColors-1 do
			opaques[i] = true
			assert.eq(transparents[i], nil)
		end
	end
	for _,pal in ipairs(monsterPalettesUnique) do
		local indexes = table.keys(monsterPalettes_monsterSpriteIndexes[pal]):sort()
		io.write('monsterPalettes[0x'..('%04x'):format(pal)..'] is used by ')

		-- [=[ print indexes
		io.write('indexes ')
		for _,index in ipairs(indexes) do
			io.write(('0x%x'):format(index), ', ')
		end
		--]=]
		--[=[ print offsets, which correlate to unique images
		local offsets = {}
		for _,i in ipairs(table.keys(monsterPalettes_monsterSpriteIndexes[pal]):sort()) do
			local monsterSprite = game.monsterSprites + i
			offsets[monsterSprite.offset] = true
		end

		io.write('offsets ')
		for _,offset in ipairs(table.keys(offsets):sort()) do
			io.write(('0x%x'):format(offset), ', ')
		end
		--]=]
		print()
	end
	print()

	-- write monster palettes
	local p = path'monsters_graphicset'
	makePaletteSets(
		p,
		game.monsterPalettes,
		bit.lshift(game.numMonsterPalettes, 3),
		function(index)
			return transparents[index]
		end
	)
end
--]]

for i=0,game.numMetamorphSets-1 do
	print('metamorph set #'..i..' = '..game.metamorphSets[i])
end
print()

for i=0,game.numFormations-1 do
	print('formation=0x'..i:hex())
	if i < game.numFormationMPs then
		print('\tmp='..game.formationMPs[i])
	end

	local formation = game.formations + i
	for j=1,6 do
		io.write('\t#0x', j:hex(), ':')
		local active = formation:getMonsterActive(j)
		io.write(' active=', tostring(active))
		if active then
			local monsterIndex = formation:getMonsterIndex(j)
			io.write(', monster=0x', monsterIndex:hex())
			if monsterIndex < countof(game.monsterNames) then
				io.write(' "', tostring(game.monsterNames[monsterIndex]), '"')
			end
			io.write(', pos=', tostring(formation:getMonsterPos(j)))
			io.write(', size=', tostring(formation:getFormationSize(j)))
		end
		print()
	end
	print('',game.formation2s[i])
	print()
end

for i=0,game.numFormationSizeOffsets-1 do
	local offset = game.formationSizeOffsets[i]
	local addr = offset + 0x020000
	print('formation size offset #0x'..i:hex()
		..' offset='..('0x%04x'):format(offset)
		..' addr='..('0x%04x'):format(addr)
		..' formationSize='..ffi.cast('formationSize_t*', rom + addr)[0]
		..(i==0 and '' or (' delta=0x'..(game.formationSizeOffsets[i] - game.formationSizeOffsets[i-1]):hex()))
	)
end
print()

for i=0,game.numFormationSizes-1 do
	print('formation size #'..i..' = '
		..('0x%04x'):format(ffi.cast('uint8_t*', game.formationSizes + i) - rom)
		..' '..game.formationSizes[i])
end
print()

for i=0,0xff do
	print('monster random battle options 0x'..i:hex())
	local formationCounts = {}
	for j=0,3 do
		local formationEntry = game.monsterRandomBattles[i].s[j]
		local formationIndex = formationEntry.formation
		local formationDesc = 'formation=0x'..formationIndex:hex()..':'
		if formationIndex < game.numFormations then
			local formation = game.formations + formationIndex
			local monsterCounts = {}
			for k=1,6 do
				if formation:getMonsterActive(k) then
					local monsterIndex = formation:getMonsterIndex(k)
					local key = '#'..monsterIndex
					if monsterIndex < game.numMonsters then
						key = key ..':'..tostring(game.monsterNames[monsterIndex])
					end
					monsterCounts[key] = (monsterCounts[key] or 0) + 1
				end
			end
			formationDesc = formationDesc .. ' '
			..table.keys(monsterCounts):sort():mapi(function(key)
				local count = monsterCounts[key]
				if count == 1 then return key end
				return key..' x'..count
			end):concat', '
		end
		if formationEntry.chooseFromNextFour ~= 0 then
			formationDesc = formationDesc .. ' (chooseFromNextFour)'
		end
		formationCounts[formationDesc] = (formationCounts[formationDesc] or 0) + (j == 3 and 1 or 5)
	end
	for _,formationDesc in ipairs(table.keys(formationCounts):sort()) do
		local formationCount = formationCounts[formationDesc]
		print('', formationCount..'/16 '..formationDesc)
	end
end
print()

local terrains = {'grass', 'forest', 'desert', 'dirt'}	-- also fields of WorldSectorBattles_t
local encounterNames = {'normal', 'low', 'high', 'none'}
-- first 64 are each 32x32 tile segment in the WoB, next 64 are segments in the WoR
for i=0,0x7f do
	print('map sector encounter #0x'..i:hex()
		..' world='..bit.rshift(i, 6)
		..' x='..bit.band(i, 7)
		..' y='..bit.band(bit.rshift(i, 3), 7)
	)
	local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + i
	local encounterRateBits = game.worldSectorRandomBattleEncounterRatesPerTerrain[i]
	for i,terrain in ipairs(terrains) do
		local battleIndex = randomBattlesPerTerrain[terrain]
		local encounter = bit.band(3, bit.rshift(encounterRateBits, bit.lshift(i-1, 1)))
		local encounterName = encounterNames[encounter+1]
		print('', terrain, 'monster random battle options 0x'..battleIndex:hex(), 'rate='..encounterName)
	end
end
print()

-- wait if this is small map info rather than world map info
-- then why does it still have 4 entries? how does it pick them?  still by field/forest/desert/dirt?
--[[
for i=0,0x7f do
	print('map battle group #0x'..i:hex())
	for j=0,3 do
		print('\t'..terrains[j+1]..' = monster random battle 0x'..game.mapBattleGroups[i][j]:hex())
	end
	-- is it probability bits per group?
	print('\tprobability '..game.mapBattleProbability[i]:bin()..'b')
end
print()
--]]


-- [[ get some statistics on our structure fields
local mins = {}
local maxs = {}
local sums = {}
for i=0,game.numCharacters-1 do
	print('character #'..i)
	print('Name="'..game.characterNames[i]..'"')
	print(game.characters[i])
	print()
	for name,ctype,field in game.character_t:fielditer() do
		local value
		if ctype == 'uint8_t' then
			value = game.characters[i][name]
		elseif ctype == 'menuref4_t'
		or ctype == 'itemref_t'
		or ctype == 'itemref2_t'
		then
			-- TODO count unique values
			goto skip
		else
			error("don't know how to handle ctype "..ctype)
		end
		value = tonumber(value) or error("failed to convert "..tostring(value).." to a number")
		mins[name] = not mins[name] and value or math.min(mins[name], value)
		maxs[name] = not maxs[name] and value or math.max(maxs[name], value)
		sums[name] = not sums[name] and value or (sums[name] + value)
::skip::
	end
end
print((tolua({
	mins = mins,
	maxs = maxs,
	avgs = table.map(sums, function(v) return v/game.numCharacters end),
})
	:gsub(', ',',\n\t\t')
	:gsub('}','\n\t}')
	:gsub('={','={\n\t\t')
))
--]]

-- [[
local readCharSprite = require 'ff6.charsprite'
local totalPixels = 0
-- [=[
local chx, chy = 0, 0
local sheetIndex = 0
local charSheet = Image(256, 256, 1, 'uint8_t')
local function flushCharSheet()
	charSheet.palette = makePalette(game.characterPalettes, 4, 256)
	charSheet:save('characters/sheet'..sheetIndex..'.png')
	charSheet:clear()
	sheetIndex = sheetIndex + 1
	chx, chy = 0, 0
end
local function pushSpriteFrame(charIndex, frameIndex, im, palIndex)
	-- offset into our palette
	im = im + bit.lshift(palIndex, 4)
	-- [==[ hack to fit 4 chars into one sheet
	if frameIndex == 38 	-- only exists for Terra I think
	--or frameIndex == 39 	-- tent
	then return end
	--]==]
	charSheet:pasteInto{
		x = chx,
		y = chy,
		image = im,
	}
	chx = chx + im.width
	if chx + im.width > charSheet.width then
		chx = 0
		chy = chy + im.height
		if chy + im.height > charSheet.height then
			flushCharSheet()
		end
	end
end
--]=]
for charIndex=0,game.numCharacterSprites-1 do
	--local spriteName = spriteNames[charIndex+1] or 'char'..charIndex
	local spriteName = 'char'..('%03d'):format(charIndex)
	--[=[ save to char sheet
	local charSheet = Image(256, 256, 1, 'uint8_t')
		:clear()
	local chx, chy = 0, 0
	--]=]
	readCharSprite(game, charIndex, function(charIndex, frameIndex, im, palIndex)
		-- [=[ save each frame individually...
		--local frameName = frameNames[frameIndex+1] or tostring(frameIndex)
		--local frameName = tostring(frameIndex)
		local frameName = ('%02d'):format(frameIndex)
		local relname = spriteName..'_'..frameName..'.png'
		im.palette = makePalette(game.characterPalettes + palIndex, 4, 16)
		im:save('characters/'..relname)
		--]=]
		--[=[ save to our char sheet
		charSheet:pasteInto{
			x = chx,
			y = chy,
			image = im,
		}
		chx = chx + im.width
		if chx + im.width > charSheet.width then
			chx = 0
			chy = chy + im.height
		end
		--]=]
		-- [=[ compact sheets
		pushSpriteFrame(charIndex, frameIndex, im, palIndex)
		--]=]
		totalPixels = totalPixels + im.width * im.height
	end)
	--[=[ save to our char sheet
	charSheet:save('characters/sheet'..spriteName..'.png')
	--]=]
	--[=[ extra sheet condition to not wrap or something idk
	-- it is specific to the char # so meh gotta guess here
	if chy + 3*8*4 > charSheet.height then
		flushCharSheet()
	end
	--]=]
end
-- [=[
if chx > 0 or chy > 0 then
	flushCharSheet()
end
--]=]
print('wrote total pixels', totalPixels)
--]]
-- [[ while we're here , why not write out the char palettes
makePaletteSets(
	path'character_pals',
	game.characterPalettes,
	16 * game.numCharacterPalettes,
	function(index)
		return bit.band(0xf, index) == 0
	end
)
--]]

for i=0,game.numMenuNames-1 do
	print('menu #'..i..' = "'..game.menuNames[i]..'"')
end
print()

for i=0,game.numMogDances-1 do
	print('mog dance #'..i..' = '..game.mogDanceNames[i])
end
print()

for i=0,game.numSwordTechs-1 do
	print('sword tech #'..i..' name="'..game.swordTechNames[i]..'" desc="'..game.gamezstr(game.swordTechDescBase + game.swordTechDescOffsets[i])..'"')
end
print()

for i=0,game.numBlitzes-1 do
	io.write('blitz #'..i)
	print(' desc="'..game.gamezstr(game.blitzDescBase + game.blitzDescOffsets[i])..'"')
	print('blitz data: '..game.blitzData[i])
	print()
end
print()

for i=0,game.numLores-1 do
	print('lore #'..i..' desc="'..game.gamezstr(game.loreDescBase + game.loreDescOffsets[i])..'"')
end
print()

io.write('exp for level up: ')
for i=0,game.numExpLevelUps-1 do
	io.write(' ',game.expForLevelUp[i])
end
print()

io.write('hpmax+ per level up: ')
for i=0,game.numLevels-1 do
	io.write(' ',game.hpIncPerLevelUp[i])
end
print()

io.write('mpmax+ per level up: ')
for i=0,game.numLevels-1 do
	io.write(' ',game.mpIncPerLevelUp[i])
end
print()
print()

gnuplot{
	terminal = 'png size 1024,768',
	output = 'hp_inc_per_level.png',
	title = 'HP inc per level up',
	style = 'data lines',
	data = {
		range(1,game.numLevels),
		range(0,game.numLevels-1):mapi(function(i) return game.hpIncPerLevelUp[i] end),
	},
	{using='1:2', notitle=true},
}
gnuplot{
	terminal = 'png size 1024,768',
	output = 'mp_inc_per_level.png',
	title = 'MP inc per level up',
	style = 'data lines',
	data = {
		range(1,game.numLevels),
		range(0,game.numLevels-1):mapi(function(i) return game.mpIncPerLevelUp[i] end),
	},
	{using='1:2', notitle=true},
}

local totalHPIncPerLevel = range(1,game.numLevels):mapi(function(i,_,t)
	return (i==1 and 0 or t[i-1]) + game.hpIncPerLevelUp[i-1]
end)
local totalMPIncPerLevel = range(1,game.numLevels):mapi(function(i,_,t)
	return (i==1 and 0 or t[i-1]) + game.mpIncPerLevelUp[i-1]
end)

gnuplot{
	terminal = 'png size 1024,768',
	output = 'hp_total_at_level.png',
	title = 'HP total at level',
	style = 'data lines',
	data = {
		range(1,#totalHPIncPerLevel),
		totalHPIncPerLevel,
	},
	{using='1:2', notitle=true},
}
gnuplot{
	terminal = 'png size 1024,768',
	output = 'mp_total_at_level.png',
	title = 'MP total at level',
	style = 'data lines',
	data = {
		range(1,#totalMPIncPerLevel),
		totalMPIncPerLevel,
	},
	{using='1:2', notitle=true},
}



for i=0,game.numShops-1 do
	print('shop #'..i..': '..game.shops[i])
end
print()

require 'ff6.run-maps'(rom, game, romsize)

print(game.dialog)
print(game.battleDialog)
print(game.battleDialog2)
print(game.battleMessages)
print(game.positionedText)


print('setzer airship palette = '..game.setzerAirshipPalette)
print('daryl airship palette = '..game.darylAirshipPalette)
print('menuWindowPalettes = '..game.menuWindowPalettes)

--print('characterMenuImages = '..game.characterMenuImages)
do
	local bpp = 4
	local tilesWide = 5
	local tilesHigh = 5
	local menucharpath = path'characters_menu'
	menucharpath:mkdir()
	local ptr = game.characterMenuImages
	for charIndex=0,game.numMenuChars-1 do
		-- same same anyways?
		if charIndex < 16 then
			ptr = rom + 0x2d0000 + game.characterMenuImageOffsets[charIndex]
		else
			ptr = game.characterMenuImages + charIndex * (tilesWide * tilesHigh * 8 * bpp)
		end
		local baseptr = ptr
		local im = Image(tileWidth*tilesWide, tileHeight*tilesHigh, 1, 'uint8_t')
			:clear()
		local tileIndex = 0
		for ty=0,tilesHigh-1 do
			for tx=0,tilesWide-1 do
				ptr = baseptr + game.characterMenuImageTileLayout[tileIndex] * 8 * bpp
				readTile(im, tx*tileWidth, ty*tileHeight, ptr, bpp)
				tileIndex = tileIndex + 1
			end
		end
		im.palette = makePalette(game.menuPortraitPalette + charIndex, 4, 16)
		im:save(
			menucharpath('menu'..charIndex..'.png').path
		)
	end
	--for charIndex=0,game.numMenuChars-1 do
	--	print('menuPortraitPalette = '..game.menuPortraitPalette[charIndex])
	--end
end
--print('handCursorGraphics = '..game.handCursorGraphics)
print('battleWhitePalette = '..game.battleWhitePalette)
print('battleGrayPalette = '..game.battleGrayPalette)
print('battleYellowPalette = '..game.battleYellowPalette)
print('battleBluePalette = '..game.battleBluePalette)
print('battleEmptyPalette = '..game.battleEmptyPalette)
print('battleGrayPalette = '..game.battleGrayPalette)
print('battleGreenPalette = '..game.battleGreenPalette)
print('battleRedPalette = '..game.battleRedPalette)
print('battleMenuPalettes = '..game.battleMenuPalettes)
print()

require 'ff6.battleanim'(rom, game, romsize)

do
	local img = Image(8*16, 8*16, 1, 'uint8_t')
	for j=0,15 do
		for i=0,15 do
			local index = i + 16 * j
			for x=0,7 do
				for y=0,7 do
					img.buffer[
						(x + 8 * i) + img.width * (y + 8 * j)
					] = bit.bor(
						bit.band(bit.rshift(game.font[2*y + 0x10 * index], 7-x), 1),
						bit.lshift(bit.band(bit.rshift(game.font[2*y+1 + 0x10 * index], 7-x), 1), 1)
					)
				end
			end
		end
	end
	img.palette = range(0,3):mapi(function(i)
		local l = math.floor(i/3*255)
		return {l,l,l}
	end)
	img:save'font.png'

	print('font16 widths: '..range(0,0x7f):mapi(function(i)
		return ('%02x'):format(game.font16_widths[i])
	end):concat' ')
end

--[[
TODO
output font ...
	font16_20_to_7f

output audio ...
	spcMainCodeLoopLen
	spcMainCode
	spcMainCode
	brrSamplePtrs
	loopStartOfs
	pitchMults
	adsrData
	brrSamples
--]]

print('spcMainCodeLoopLen = '..game.spcMainCodeLoopLen)
--[[
print('spcMainCode = '..
	range(0,math.min(game.spcMainCodeLoopLen, ffi.sizeof(game.spcMainCode))-1)
	:mapi(function(i) return (' %02x'):format(game.spcMainCode[i]) end):concat()
)
--]]
require 'ff6.sounds'(rom, game, romsize)

print'end of rom output'

--print('0x047aa0: ', game.padding_047aa0)

if outfn then
	math.randomseed(os.time())

	--[[ randomize ...
	here's my idea:
	make swords randomly cast.
	make espers only do stat-ups
	make later-equipment teach you spells ... and be super-weak ... and cursed-shield-break into some crap item (turn Paladin Shield into dried meat)

list of spells in order of power / when you should get them

Osmose	1
Scan	3
Antdot	3
Poison	3
Fire	4
Ice	5
Cure	5
Slow	5
Sleep	5
Bolt	6
Muddle	8
Mute	8
Haste	10
Stop	10
Imp	10
Regen	10
Rasp	12
Safe	12
Shell	15
Remedy	15
Drain	15
Bserk	16
Float	17
Vanish	18
Warp	20
Fire 2	20
Ice 2	21
Bolt 2	22
Rflect	22
Cure 2	25
Dispel	25
Break	25
Slow 2	26
Bio	26
Life	30
Demi	33
Doom	35
Haste2	38
Cure 3	40
Pearl	40
Flare	45
Quartr	48
Life 3	50
Quake	50
Fire 3	51
Ice 3	52
Bolt 3	53
X-Zone	53
Life 2	60
Meteor	62
W Wind	75
Ultima	80
Merton	85
Quick	99
	--]]

	local itemsForType = table()
	for i=0,game.numItems-1 do
		local key = game.items[i].itemType
		itemsForType[key] = itemsForType[key] or table()
		itemsForType[key]:insert(ffi.new('itemref_t', i))
	end
	print(tolua(itemsForType))
	print('number of swords: '..#itemsForType[1])

-- [[ spells ... gobbleygook
-- this made the screen go garbage at the first battle
	for i=0,game.numSpells-1 do
		local spell = game.spells+i
		for j=0,ffi.sizeof'spell_t'-1 do
			spell.s[j] = math.random(0,255)
		end
		spell.unused_7_2 = 0
		-- should this be a percent?
		spell.killsCaster = 0
	end
--]]

-- [[ swords cast random things
	for _,ref in ipairs(itemsForType[1]) do
		local item = game.items[ref.i]
		item.spellCast = math.random(0,53)
		-- this isn't working...
		item.castOnAttack = 1
		item.castOnItemUse = 1
		item.canUseInBattle = 1	-- this uses the spell
		item.canBeThrown = 1
		item.battlePower_defense = 0
	end
--]]

-- [[ random learn from all equipment ... and relics?
	for itemType=2,5 do
		for _,ref in ipairs(itemsForType[itemType]) do
			local item = game.items[ref.i]
			item.spellLearn.rate = math.random(1,100)
			item.spellLearn.spell.i = math.random(0,53)
		end
	end

	-- mithril knife teaches quick
	--game.items[1].spellCast = 43
--]]

--[[ espers ... gobbleygook everywhere
	for i=0,game.numEspers-1 do
		for j=0,ffi.sizeof'esper_t'-1 do
			game.espers[i].s[j] = math.random(0,255)
		end
	end
--]]
-- [[ espers: no spells and only stats
	local esperBonuses = range(0, 16)
	esperBonuses:removeObject(7)	-- not applicable
	esperBonuses:removeObject(8) 	-- nothing
	for i=0,game.numEspers-1 do
		local esper = game.espers[i]
		-- disable all spells
		for j=1,5 do
			esper['spellLearn'..j].rate = 255
		end
		-- pick a random bonus
		esper.bonus.i = table.pickRandom(esperBonuses)
	end
--]]

--[[ items
-- all the monsto death, countdown, etc is done through here it seems
-- this makes countdown often
	for i=0,game.numItems-1 do		-- is numItems 256 or 255?
		local item = game.items+i
		for j=0,ffi.sizeof'item_t'-1 do
			item.s[j] = math.random(0,255)
		end
		-- I think this is what causes glitches ... maybe ...
		item.unused_0_7 = 0
		item.unused_5_2 = 0
		item.unused_5_3 = 0
		item.unused_5_4 = 0
		item.unused_5_6 = 0
		item.unused_b_1 = 0
		item.unused_c_7 = 0
		item.unused_d_2 = 0
		item.unused_d_5 = 0
		item.unused_d_6 = 0
		item.unused_13_2 = 0
		--item.givesEffect2.countdown = 0
		--item.givesEffect2.muddle = 0
	end
--]]

-- [[ I think it's a monster_t stat that makes monsters insta-die ... like maybe negative life?
-- for that matter, maybe health can only exceed 32k if it has some extra magic flag set?
-- likewise there is some perma-confused in here
	for i=0,game.numMonsters-1 do
		--[=[ all fields
		for j=0,ffi.sizeof'monster_t'-1 do
			game.monsters[i].s[j] = math.random(0,255)
		end
		--]=]
		-- [=[ each individually
		game.monsters[i].speed = math.random(0,255)
--		game.monsters[i].battlePower = math.random(0,255)
		game.monsters[i].hitChance = math.random(0,255)
		game.monsters[i].evade = math.random(0,255)
        game.monsters[i].magicBlock = math.random(0,255)
        game.monsters[i].defense = math.random(0,255)
        game.monsters[i].magicDefense = math.random(0,255)
        game.monsters[i].magicPower = math.random(0,255)
 --       game.monsters[i].hp = math.random(0,65535)
  --      game.monsters[i].mp = math.random(0,65535)
        game.monsters[i].exp = math.random(0,65535)
        game.monsters[i].gold = math.random(0,65535)
        game.monsters[i].level = math.random(0,255)
        game.monsters[i].metamorphSet = math.random(0,31)
        game.monsters[i].metamorphResist = math.random(0,7)

		--game.monsters[i].diesIfRunOutOfMP = math.random(0,1)

		game.monsters[i].undead = math.random(0,1)
		game.monsters[i].cantSuplex = math.random(0,1)
		game.monsters[i].cantRun = math.random(0,1)
		game.monsters[i].cantControl = math.random(0,1)

		-- random per bitflag?
		game.monsters[i].immuneToEffect1.s[0] = math.random(0,255)

		game.monsters[i].elementHalfDamage.s[0] = math.random(0,255)
		game.monsters[i].elementAbsorb.s[0] = math.random(0,255)
		game.monsters[i].elementNoEffect.s[0] = math.random(0,255)
		game.monsters[i].elementWeak.s[0] = math.random(0,255)

		game.monsters[i].specialAttack = math.random(0,127)
		game.monsters[i].specialAttackDealsNoDamage = math.random(0,1)
		--]=]

		for j=0,ffi.sizeof'spellref4_t'-1 do
			game.monsterSpells[i].s[j].i = math.random(0,255)
		end
		for j=0,ffi.sizeof'spellref2_t'-1 do
			game.monsterSketches[i].s[j].i = math.random(0,255)
		end
		if i < game.numRages then
			for j=0,ffi.sizeof'spellref2_t'-1 do
				game.monsterRages[i].s[j].i = math.random(0,255)
			end
		end
	end
--]]

	-- good menu stuff:
	-- exclude Fight Item Magic Def Row
	local goodMenus = range(0,29)
	for _,i in ipairs{0,1,2,4,20,21} do
		goodMenus:removeObject(i)
	end
	-- TODO if you pick Leap then you should proly change Fight to Rage too  ... or not?

-- [[ equipping items in the wrong spot has adverse effects
	for i=0,game.numCharacters-1 do
		--[=[
		for j=0,ffi.sizeof'character_t'-1 do
			game.characters[i].s[j] = math.random(0,255)
		end
		--]=]
		-- [=[
		game.characters[i].hp = math.random(0,255)
		game.characters[i].mp = math.random(0,255)
		--game.characters[i].menu.s[0].i = math.random(0,game.numMenuNames-1)		-- fight
		--game.characters[i].menu.s[1].i = math.random(0,game.numMenuNames-1)
		--game.characters[i].menu.s[0].i = goodMenus:pickRandom()
		game.characters[i].menu.s[1].i = goodMenus:pickRandom()
		--game.characters[i].menu.s[2].i = math.random(0,game.numMenuNames-1)		-- magic
		--game.characters[i].menu.s[3].i = math.random(0,game.numMenuNames-1)		-- item
		-- [==[
		game.characters[i].vigor = math.random(0,255)
		game.characters[i].speed = math.random(0,255)
        game.characters[i].stamina = math.random(0,255)
        game.characters[i].magicPower = math.random(0,255)
        game.characters[i].battlePower = math.random(0,255)
        game.characters[i].defense = math.random(0,255)
        game.characters[i].magicDefense = math.random(0,255)
        game.characters[i].evade = math.random(0,255)
        game.characters[i].magicBlock = math.random(0,255)
		--]==]
        -- [==[
		-- TODO verify that the item is equippable
		game.characters[i].lhand.i = math.random(0,255)
		game.characters[i].rhand.i = math.random(0,255)
		game.characters[i].head.i = math.random(0,255)
		game.characters[i].body.i = math.random(0,255)
		game.characters[i].relic.s[0].i = math.random(0,255)
		game.characters[i].relic.s[1].i = math.random(0,255)
		--]==]
		--game.characters[i].level = math.random(1,99)
		--]=]
	end
--]]

	for i=0,game.numShops-1 do
		for j=0,ffi.sizeof'shopinfo_t' do
			game.shops[i].s[j] = math.random(0,255)
		end
	end

	print'writing...'
	path(outfn):write(ffi.string(data, romsize))
end
