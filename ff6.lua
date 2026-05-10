local ffi = require 'ffi'
local table = require 'ext.table'
local class = require 'ext.class'
local string = require 'ext.string'
local number = require 'ext.number'
local assert = require 'ext.assert'
local struct = require 'struct'
local createVec = require 'vec-ffi.create_vec'
local ff6struct = require 'ff6.ff6struct'
local reftype = require 'ff6.reftype'


local uint8_t = ffi.typeof'uint8_t'
local uint8_t_p = ffi.typeof'uint8_t*'
local uint16_t = ffi.typeof'uint16_t'


-- util? ext.ffi or something?
local function countof(array)
	return ffi.sizeof(array) / (ffi.cast(uint8_t_p, array+1) - ffi.cast(uint8_t_p, array+0))
end


local Game

-- default output to hex
local fieldsToHex = {
	uint8_t = function(value)
		return ('0x%02x'):format(value)
	end,
	uint16_t = function(value)
		return ('0x%04x'):format(value)
	end,
	uint32_t = function(value)
		return ('0x%08x'):format(value)
	end,
}
for _,size in ipairs{8, 16, 32} do
	for i=1,size do
		fieldsToHex['uint'..size..'_t:'..i] = function(value)
			return ('0x%0'..math.ceil(i/4)..'x'):format(value)
		end
	end
end

local romsize = 0x300000

-- you must pass this a string
-- and it saves the strong
-- all so GC doesn't free pointers while this is using them.
return function(romstr)
assert.type(romstr, 'string')
if #romstr == 0x300200 then
	romstr = romstr:sub(0x201)
end
assert.len(romstr, romsize)
-- TODO checksum


local rom = ffi.cast(uint8_t_p, romstr)

-- compstr uses game
local gameC	-- C object / ffi metatype
local game	-- Lua wrapper to provide extra Lua closure tables etc

local function findu8(ptr, ch)
	while true do
		if ptr[0] == ch then return ptr end
		ptr = ptr + 1
	end
end

-- TODO how about unicode?  no objections to fixed-size strings turning into varying-sized strings?
-- welp this seems good but itemForName :sub(2) no longer works
local gameToAscii = table{
'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
'w','x','y','z','0','1','2','3','4','5','6','7','8','9','!','?',
'/',':','”',"'",'-','.',',','…',';','#','+','(',')','%','~','*',
' ',' ','=','“','↑','→','↙','×','🔪','🗡️','⚔️','🔱','🪄','🖌️','𖣘','♦',
'🃏','🐾','🛡️','🪖','🧥','🏹','📏','💍','⚪','🔴','🔵',' ',' ',' ',' ',' ',
' ','▏','▎','▍','▌','▋','▊','▉','█','{','}',' ',' ',' ',' ',' '}
:mapi(function(v)
	if type(v) == 'number' then return string.char(v) end
	return v
end)
local function gamestr(ptr, len)
	assert(len, "did you want to use gamezstr?")
	local s = table()
	for i=0,len-1 do
		local ch = ptr[i]
		ch = bit.band(ch, 0x7f)
		local ascii = gameToAscii[ch+1]
		assert(ascii, "failed to find ascii for game char "..ptr[i])
		s:insert(ascii)
	end
	return s:concat()
end

local function gamezstr(ptr)
	local pend = findu8(ptr, 0)
	return gamestr(ptr, pend - ptr)
end

-- same as gameToAscii ... one uses single-chars the other uses []'s
local convertCompressedChar = {
"A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P",
"Q","R","S","T","U","V","W","X","Y","Z","a","b","c","d","e","f",
"g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v",
"w","x","y","z","0","1","2","3","4","5","6","7","8","9","!","?",
"/",":",'"',"'","-",".",',',"...",";","#","+","(",")","%","~","*",
"[0]","[1]","=",'"',"[up arrow]","[right arrow]","[down left arrow]","[x]", "[dirk]", "[sword]","[lance]","[knife]","[rod]","[brush]","[stars]","[special]",
"[gambler]","[claw]","[shield]","[helmet]","[armor]","[tool]","[skean]","[relic]","[white]","[grey]","[black]","[2]","[3]","[4]","[5]","[6]",
"[swd 0]","[swd 1]","[swd 2]","[swd 3]","[swd 4]","[swd 5]","[swd 6]","[swd 7]","[swd 8]","{","}","[7]","[8]","[9]","[10]","[11]",
}

local convertCompressedDoubleChar = {
'e ',' t',': ','th','t ','he','s ','er',
' a','re','in','ou','d ',' w',' s','an',
'o ',' h',' o','r ','n ','at','to',' i',
', ','ve','ng','ha',' m','Th','st','on',
'yo',' b','me','y ','en','it','ar','ll',
'ea','I ','ed',' f',' y','hi','is','es',
'or','l ',' c','ne',"'s",'nd','le','se',
' I','a ','te',' l','pe','as','ur','u ',
'al',' p','g ','om',' d','f ',' g','ow',
'rs','be','ro','us','ri','wa','we','Wh',
'et',' r','nt','m ','ma',"I'",'li','ho',
'of','Yo','h ',' n','ee','de','so','gh',
'ca','ra',"n'",'ta','ut','el','! ','fo',
'ti','We','lo','e!','ld','no','ac','ce',
'k ',' u','oo','ke','ay','w ','!!','ag',
'il','ly','co','. ','ch','go','ge','e.',
}

local compstr_displayChars = true

local function compstr(p, size)
	assert(size)
	local c = table()
	local b = 0
	for a=0,size-1 do
		if p[0] < 0x80 then
			if p[0] == 0x7f then
				c:insert' '
			elseif p[0] < 32 then
--[[
				if p[0] == 22 and p[1] == 24 and p[2] == 18 then -- pause
					if compstr_displayChars then
						c:insert'[p]\n'
					else
						c:insert'\n'
					end
					p = p + 2
				else
--]]
				if p[0] == 0 then			 -- end of message
					c:insert'[END]'
				elseif p[0] == 1 then	-- line feed mid-message
					c:insert'\n'
				elseif p[0] < 16 then	-- 2-15 = char name
					c:insert'['
--					c:insert(('%02d-'):format(p[0]))
					c:insert(tostring(gameC.characterNames[p[0]-2]))
					c:insert']'
				elseif p[0] == 16 then
					c:insert'[PAUSE]'
				--elseif p[0] == 17 then -- read until 18
				--elseif p[0] == 18 then -- terminates a 17, or begins a message.
				elseif p[0] == 19 then	-- clear and new message
					c:insert'\n[CLEAR]'
					c:insert'\n'
				--elseif p[0] == 20 then -- read 1 more char ... horizontal tab?
				elseif p[0] == 21 then
					c:insert'[PROMPT]'
				--elseif p[0] == 22 then -- read until 18
				--23, 24, 29: only used between 17 and 18, specifically in opera scene dialog
				--25, 26, 27, 28, 30, 31: never used
				else
					if compstr_displayChars then
						c:insert(('[%02d]'):format(p[0]))
					else
						c:insert'\n'
					end
				end
			else
				c:insert(convertCompressedChar[p[0]-32+1])
			end
		else
			c:insert(convertCompressedDoubleChar[p[0] - 0x80 + 1])
		end
		p = p + 1
	end
	return c:concat()
end

local function ptrType(baseType)
	return ffi.typeof('$*', baseType)
end

local function arrayType(baseType, size)
	-- neither luajit nor lua-ffi-wasm can handle decimals in array sizes.
	-- how come lua-ffi-wasm gets a decimal in array size here?
	-- aha, because lua-ffi-wasm is based on Lua 5.4, when they introduced separate decimal and integer numbers, and started serializing them differently.
	size = math.floor(size)
	return ffi.typeof('$['..size..']', baseType)
end

local function compzstr(ptr)
	local pend = findu8(ptr, 0)
	return compstr(ptr, pend - ptr)
end

local function gamestrtype(args)
	local size = assert(args.size)
	local ctype = ffi.typeof([[struct { uint8_t ptr[]]..size..[[]; }]])
	assert.eq(ffi.sizeof(ctype), size)
	ffi.metatype(ctype, {
		__tostring = function(self)
			return string.trim(gamestr(self.ptr, size))
		end,
		__concat = string.concat,
	})
	return ctype
end

local function rawtype(args)
	local size = assert(args.size)
	local ctype = ffi.typeof([[struct { uint8_t ptr[]]..size..[[]; }]])
	assert.eq(ffi.sizeof(ctype), size)
	ffi.metatype(ctype, {
		__tostring = function(self)
			local s = table()
			for i=0,size-1 do
				s:insert(('%02x'):format(self.ptr[i]))
			end
			return s:concat' '
		end,
		__concat = string.concat,
	})
	return ctype
end

--[[
args:
	options
	type (optional) default uint8_t
--]]
local function bitflagtype(args)
	local ctype = args.type or 'uint8_t'
	assert.type(ctype, 'string')
	return ff6struct{
		ctypeOnly = true,
		fields = table.mapi(assert(args.options), function(option)
			return {[assert(option)] = ctype..':1'}
		end),
	}
end

local Element = bitflagtype{
	options = {
		'fire',
		'ice',
		'thunder',
		'poison',
		'wind',
		'pearl',
		'earth',
		'water',
	},
}

local Targetting = bitflagtype{
	options = {
		'one',			-- can move cursor?
		'oneSideOnly',
		'everyone',		-- auto select all?
		'groupDefault',	-- auto select one side
		'automatic',	-- auto confirm
		'group',		-- manual party select
		'enemyDefault',
		'random',
	},
}

local Effect1 = bitflagtype{
	options = {
		'dark',
		'zombie',
		'poison',
		'magitech',
		'invisible',
		'imp',
		'petrify',
		'mortal',
	},
}

local Effect2 = bitflagtype{
	options = {
		'countdown',
		'nearFatal',
		'image',
		'mute',
		'berzerk',
		'muddle',
		'hpLeak',
		'sleep',
	},
}

local Effect3 = bitflagtype{
	options = {
		'danceFloat',
		'regen',
		'slow',
		'haste',
		'stop',
		'shell',
		'safe',
		'reflect',
	},
}

local Effect4 = bitflagtype{
	options = {
		'raging',
		'frozen',
		'reraise',
		'morphed',
		'casting',
		'removedFromBattle',
		'interceptor',
		'floating',
	},
}

local function makefixedstr(n)
	return gamestrtype{
		size = n,
	}
end

local Str6 = makefixedstr(6)
local Str7 = makefixedstr(7)
local Str8 = makefixedstr(8)
local Str9 = makefixedstr(9)
local Str10 = makefixedstr(10)
local Str12 = makefixedstr(12)
local Str13 = makefixedstr(13)

local madefixedraw = {}
local function makefixedraw(n)
	local cache = madefixedraw[n]
	if cache then return cache end
	local mt = rawtype{size = n}
	madefixedraw[n] = mt
	return mt
end

local Raw12 = makefixedraw(12)	-- Raw12 for uint8_t[12] for the blitzes

---------------- COMPRESSED/UNCOMPRESSED STRINGS ----------------

local StringList = class()

--[[
args:
	name = section name
	data = uint8_t[?] buffer in memory for string data
	addrBase = (optional) base of offsets, data by default
	offsets = uint16_t[?] buffer in memory for offsets
	compressed = boolean
--]]
function StringList:init(args)
	self.name = assert(args.name)

	self.data = assert(args.data)
	assert.type(self.data, 'cdata')

	self.offsets = assert(args.offsets)
	assert.type(self.offsets, 'cdata')

	self.numOffsets = countof(self.offsets)

	self.addrBase = args.addrBase
	self.compressed = args.compressed

	local addrBase = self.addrBase and self.addrBase or self.data
	local strf = self.compressed and compzstr or gamezstr

	for i=0,self.numOffsets-1 do
		local offset = self.offsets[i]
		if offset ~= 0xffff then
			self[i] = strf(addrBase + offset)
		end
	end
end

function StringList:__len()
	return self.numOffsets
end

function StringList:__tostring()
	local result = table()

	local addrBase = (self.addrBase and self.addrBase or self.data) - rom
	local strf = self.compressed and compzstr or gamezstr

	for i=0,self.numOffsets-1 do
		if self[i] ~= nil then
			result:insert(
				self.name..'[0x'..number.hex(i)..'] = '
				..('0x%04x'):format(self.offsets[i])
				..' "'..self[i]..'"\n')
		end
	end

	-- track memory used
	local numPtr = countof(self.data)
	assert(numPtr)
	local addrMin = self.data - rom
	local addrMax = addrMin + numPtr
	local used = {}
	for i=0,self.numOffsets-1 do
		local offset = self.offsets[i]
		if offset ~= 0xffff then
			if not (addrBase + offset >= addrMin and addrBase + offset < addrMax) then
				error("offset "..i.." was out of bound")
			end
			local ptr = rom + addrBase + offset
			local pend = findu8(ptr, 0)
			local addrEnd = pend - rom
			for j=addrBase+offset, addrEnd do
				assert.le(addrMin, j)
				assert.lt(j, addrMax)
				used[j] = true
			end
		end
	end
	local count = 0
	local show = false	-- true
	local addrSize = addrMax - addrMin
	local showWidth = math.ceil(math.sqrt(addrSize))	--64
	for j=addrMin,addrMax-1 do
		if used[j] then count = count + 1 end
		if show then
			io.write(used[j] and '#' or '.')
			if (j-addrMin) % showWidth == (showWidth-1) then
				result:insert'\n'
			end
		end
	end
	if show then
		if (addrMax-addrMin) % showWidth ~= 0 then print() end
	end
	result:insert(self.name..' % used: '..count..'/'..addrSize..' = '..('%.3f%%'):format(100*count/addrSize)..'\n')
	return result:concat()
end

function StringList.__concat(a,b)
	return tostring(a) .. tostring(b)
end

---------------- GRAPHICS ----------------

local RGBA5551 = ff6struct{
	ctypeOnly = true,
	fields = {
		{r = 'uint16_t:5'},
		{g = 'uint16_t:5'},
		{b = 'uint16_t:5'},
		{a = 'uint16_t:1'},
	},
	metatable = function(mt)
		mt.rgba = function(self)
			return
				bit.bor(
					bit.lshift(self.r, 3),
					bit.rshift(self.r, 2)
				),
				bit.bor(
					bit.lshift(self.g, 3),
					bit.rshift(self.g, 2)
				),
				bit.bor(
					bit.lshift(self.b, 3),
					bit.rshift(self.b, 2)
				),
				self.a == 0 and 0xff or 0
		end
	end,
}
assert.eq(ffi.sizeof(RGBA5551), 2)

local Palette4 = createVec{
	ctypeOnly = true,
	ctype = RGBA5551,
	dim = 4,
}
assert.eq(ffi.sizeof(Palette4), 2*4)

local Palette8 = createVec{
	ctypeOnly = true,
	ctype = RGBA5551,
	dim = 8,
}
assert.eq(ffi.sizeof(Palette8), 2*8)

local Palette16 = createVec{
	ctypeOnly = true,
	ctype = RGBA5551,
	dim = 16,
}
assert.eq(ffi.sizeof(Palette16), 2*16)

local Palette16_8 = createVec{
	ctypeOnly = true,
	ctype = Palette16,
	dim = 8,
}
assert.eq(ffi.sizeof(Palette16_8), 2*16*8)

local numMenuChars = 19

local MapColorMathProps = ff6struct{
	ctypeOnly = true,
	fields = {
		{colorMath = 'uint8_t:6'},	-- 0=layer1, 1=layer2, 2=layer3, 3=layer4, 4=sprites, 5=back area
		{half = 'uint8_t:1'},
		{subtract = 'uint8_t:1'},
		{mainScreen = uint8_t},	-- only uses 6 bits? same blending as .colorMath?
		{subScreen = uint8_t},	-- only uses 6 bits?
	},
}
assert.eq(ffi.sizeof(MapColorMathProps), 3)

---------------- AUDIO ----------------

local uint24_t = ff6struct{
	name = 'uint24_t',
	fields = {
		{lo = uint16_t},
		{hi = uint8_t},
	},
	metatable = function(mt)
		mt.value = function(self)
			return bit.bor(self.lo, bit.lshift(self.hi, 16))
		end
	end,
}
assert.eq(ffi.sizeof(uint24_t), 3)
local numBRRSamples = 63

---------------- SPELLS ----------------


-- needs 'game' as a parameter ... but can't always get it there ... so look for a global instead
local function getSpellName(i)
	i = bit.band(i, 0xff)
	-- black, grey, white
	if i >= 0 and i < 54 then return string.trim(tostring(gameC.spellNames_0to53[i])) end
	i = i - 54
	-- esper
	if i >= 0 and i < 27 then return string.trim(tostring(gameC.spellNames_54to80[i])) end
	i = i - 27
	-- rest:
	if i >= 0 and i < 175 then return string.trim(tostring(gameC.spellNames_81to255[i])) end
	error'here'
end

-- needs 'game' to correctly call 'getSpellName' with a parameter
local SpellRef = reftype{
	ctypeOnly = true,
	getter = function(i)
		if i == 0xff then return nil end
		return getSpellName(i)
	end,
	getterSkipNone = true,
}

local Spell = ff6struct{
	ctypeOnly = true,
	fields = {
		-- 00:
		{targetting = Targetting},
		-- 01:
		{elementDamage = Element},
		-- 02:
		{physical = 'uint8_t:1'},
		{isAMortalAttack = 'uint8_t:1'},	-- miss if protected from death
		{canTargetWounded = 'uint8_t:1'},
		{oppositeToUndead = 'uint8_t:1'},
		{randomizeTarget = 'uint8_t:1'},
		{undefendable = 'uint8_t:1'},
		{dontDivideDamageForMultipleTargets = 'uint8_t:1'},
		{onlyTargetEnemies = 'uint8_t:1'},
		-- 03:
		{canUseInMenu = 'uint8_t:1'},	-- rpglegion says this is use-in-battle
		{cannotReflect = 'uint8_t:1'},
		{isLore = 'uint8_t:1'},
		{enableRunic = 'uint8_t:1'},	-- ???
		{usedByWarpAndQuick = 'uint8_t:1'},	-- warp, quick
		{retargetDead = 'uint8_t:1'},	-- ???
		{killsCaster = 'uint8_t:1'},
		{damagesTargetsMP = 'uint8_t:1'},
		-- 04:
		{heals = 'uint8_t:1'},
		{drainsLife = 'uint8_t:1'},
		{removesEffects = 'uint8_t:1'},
		{invertsEffects = 'uint8_t:1'},	-- imp, vanish, imp song ... rpglegion says gives status conditions
		{evadeByStamina = 'uint8_t:1'},
		{unevadable = 'uint8_t:1'},
		{hitIfLevelDivisibleBySpellHitChance = 'uint8_t:1'},
		{damageIsPercentOfLifeTimesSpellPowerOver16 = 'uint8_t:1'},
		-- 05:
		{mp = uint8_t},
		-- 06:
		{power = uint8_t},
		-- 07:
		{maybe_noDamage = 'uint8_t:1'},
		{mabye_hitBasedOnLevel = 'uint8_t:1'},
		{unused_7_2 = 'uint8_t:6'},
		-- 08:
		{hitChance = uint8_t},
		-- 09:
		{specialEffect = uint8_t},
		-- 0x0a:
		{givesEffect1 = Effect1},
		-- 0x0b:
		{givesEffect2 = Effect2},
		-- 0x0c:
		{givesEffect3 = Effect3},
		-- 0x0d:
		{givesEffect4 = Effect4},
	},
	metatable = function(mt)
		local oldFieldToString = mt.fieldToString
		mt.fieldToString = function(self, name, ctype)
			if name == 'specialEffect' then
				if self[name] == 0xff then
					return nil
				end
			elseif name == 'unused_7_2' then
				if self[name] == 0 then
					return nil
				end
			end
			return oldFieldToString(self, name, ctype)
		end
	end,
}
assert.eq(ffi.sizeof(Spell), 0x0e)

local EsperBonusDesc = Str9
local numEsperBonuses = 17

-- another one that needs 'game'
local EsperBonus = reftype{
	ctypeOnly = true,
	getter = function(i) return gameC.esperBonusDescs[i] end,
}

-- also needs a pointer to 'game'
local function getEsperName(i) return getSpellName(i + 54) end

local SpellLearn = ff6struct{
	ctypeOnly = true,
	fields = {
		{rate = uint8_t},
		{spell = SpellRef},
	},
}
assert.eq(ffi.sizeof(SpellLearn), 2)

local Esper = ff6struct{
	ctypeOnly = true,
	fields = {
		{spellLearn1 = SpellLearn},
		{spellLearn2 = SpellLearn},
		{spellLearn3 = SpellLearn},
		{spellLearn4 = SpellLearn},
		{spellLearn5 = SpellLearn},
		{bonus = EsperBonus},
	},
}
assert.eq(ffi.sizeof(Esper), 11)
local numEspers = 27

--[[ can't do this until i convert all ff6struct to struct, then i can use anonymous fields.
local BattleAnimEffectIndex = struct{
	ctypeOnly = true,
	union = true,
	fields = {
		{name='u16', type=uint16_t, no_iter=true},
		{type=struct{
			anonymous = true,
			fields = {
				{index = 'uint16_t:15'},
				{dontRunScript = 'uint16_t:1'},
			},
		}},
	},
}
assert.eq(ffi.sizeof(BattleAnimEffectIndex), 2)
--]]

-- collections of up to 3 animation-effects to play
local BattleAnimSet = ff6struct{
	ctypeOnly = true,
	fields = {
		--[[ TODO get struct to serialize member arrays
		{effect = arrayType(uint16_t, 3)},	-- 0xffff = none, otherwise values are from 0-0x3fff, bit15 means something, idk.
		{palette = arrayType(uint8_t, 3)},
		--]]
		-- [[

		-- [[ index into 'battleAnimEffects' table
		-- 0xffff = none
		-- bit 15 means something else I think
		{effect1 = uint16_t},
		{effect2 = uint16_t},
		{effect3 = uint16_t},
		--]]
		--[[
		{effect1 = BattleAnimEffectIndex},
		{effect2 = BattleAnimEffectIndex},
		{effect3 = BattleAnimEffectIndex},
		--]]

		-- index into 'battleAnimPalettes' table
		{palette1 = uint8_t},
		{palette2 = uint8_t},
		{palette3 = uint8_t},
		--]]
		{sound = uint8_t},
		{unknown10 = uint8_t},
		{unknown11 = uint16_t},
		{wait = uint8_t},
	},
}
assert.eq(ffi.sizeof(BattleAnimSet), 0xe)

-- animations made up of frames
local BattleAnimEffect = ff6struct{
	ctypeOnly = true,
	fields = {
		{numFrames = 'uint8_t:6'},
		{graphicSetHighBit = 'uint8_t:1'},	-- is this a separate bit?
		{_2bpp = 'uint8_t:1'},	-- use 3bpp(false) vs 2bpp(true)?

		-- for effect1&2, 0x120000 + graphicSet * 0x40, len = 0xA0
		-- for effect3, 0x12C000 + graphicSet * 0x40, len = 0x80
		{graphicSet = uint8_t}, -- aka "chipset" aka "mold" (where does this point?)

		{frameIndexBase = uint16_t}, -- the index into battleAnimFrame16x16TileOffsets

		-- size in 16x16 tiles:
		{width = uint8_t},
		{height = uint8_t},
	},
}
assert.eq(ffi.sizeof(BattleAnimEffect), 6)

local BattleAnim16x16Tile = ff6struct{
	ctypeOnly = true,
	fields = {
		-- x y destination to place the tile in the frame
		{y = 'uint8_t:4'},
		{x = 'uint8_t:4'},
		-- tile = which tile index in the graphicSet
		{tile = 'uint8_t:6'},
		-- whether to hflip/vflip the 16x16 tile
		{hflip16 = 'uint8_t:1'},
		{vflip16 = 'uint8_t:1'},
	},
}
assert.eq(ffi.sizeof(BattleAnim16x16Tile), 2)

-- graphicSet is a collection of 0x40 (0x10 x 4) 8x8 tiles
-- Each 8x8 tile holds info of the tile address, hflip, and vflip
-- This is pointed to by BattleAnimEffect.graphicSet
local BattleAnim8x8Tile = ff6struct{
	ctypeOnly = true,
	fields = {
		-- * tileLen (8 * bpp) + tileAddrBase (0x187000 for 2bpp, 0x130000 for 3bpp) gives the 8x8 tile data
		-- for 3bpp, points into 0x130000 - 0x14c998, which only holds 4881
		{tile = 'uint16_t:14'},
		-- whether to hflip/vflip the 8x8 tile
		{hflip = 'uint16_t:1'},
		{vflip = 'uint16_t:1'},
	},
}
assert.eq(ffi.sizeof(BattleAnim8x8Tile), 2)

---------------- MONSTERS HEADER ----------------

local numMonsters = 0x180

local MonsterName = Str10

-- This is a uint8_t even though there are 384 monsters.
-- If I find a uint16_t then I'll make reftype more flexible and make a second monsterRef16_t type.
local MonsterRef = reftype{
	ctypeOnly = true,
	getter = function(i) return gameC.monsterNames[i] end,
}

local numFormations = 0x240

local XY4b = ff6struct{
	ctypeOnly = true,
	fields = {
		{x = 'uint8_t:4'},
		{y = 'uint8_t:4'},
	},
}
--[[
local XY4b_6 = createVec{
	ctypeOnly = true,
	ctype = XY4b,
	dim = 6,
}
--]]

local numFormationSizeOffsets = 13

local FormationSize = ff6struct{
	ctypeOnly = true,
	fields = {
		{unused_0_0 = 'uint8_t:1'},		-- 0.0
		{unused_0_1 = 'uint8_t:1'},		-- 0.1
		{unused_0_2 = 'uint8_t:1'},		-- 0.2
		{unused_0_3 = 'uint8_t:1'},		-- 0.3
		{unused_0_4 = 'uint8_t:1'},		-- 0.4
		{unused_0_5 = 'uint8_t:1'},		-- 0.5
		{unused_0_6 = 'uint8_t:1'},		-- 0.6
		{unknown_0_7 = 'uint8_t:1'},	-- 0.7
		{unknown_1_0 = 'uint8_t:1'},	-- 1.0
		{unused_1_1 = 'uint8_t:1'},		-- 1.1
		{unused_1_2 = 'uint8_t:1'},		-- 1.2
		{unknown_1_3 = 'uint8_t:1'},	-- 1.3
		{unknown_1_4 = 'uint8_t:1'},	-- 1.4
		{unused_1_5 = 'uint8_t:1'},		-- 1.5
		{unused_1_6 = 'uint8_t:1'},		-- 1.6
		{unused_1_7 = 'uint8_t:1'},		-- 1.7
		{width = uint8_t},
		{height = uint8_t},
	},
}
assert.eq(ffi.sizeof(FormationSize), 4)

local Formation = ff6struct{
	ctypeOnly = true,
	fields = {
		-- 0x00
		{unused_0_0 = 'uint8_t:1'},
		{unused_0_1 = 'uint8_t:1'},
		{unused_0_2 = 'uint8_t:1'},
		{unused_0_3 = 'uint8_t:1'},
		{formationSize = 'uint8_t:4'},	-- points to index in formationSizeOffsets
		-- 0x01
		{active1 = 'uint8_t:1'},
		{active2 = 'uint8_t:1'},
		{active3 = 'uint8_t:1'},
		{active4 = 'uint8_t:1'},
		{active5 = 'uint8_t:1'},
		{active6 = 'uint8_t:1'},
		{unused_1_6 = 'uint8_t:1'},
		{unused_1_7 = 'uint8_t:1'},

		-- 0x02
		{monster1 = uint8_t},
		-- 0x03
		{monster2 = uint8_t},
		-- 0x04
		{monster3 = uint8_t},
		-- 0x05
		{monster4 = uint8_t},
		-- 0x06
		{monster5 = uint8_t},
		-- 0x07
		{monster6 = uint8_t},

		-- 0x08 - 0x0d
		--{positions = XY4b_6},
		-- can't occlude if x,y are nested
		{pos1 = XY4b},
		{pos2 = XY4b},
		{pos3 = XY4b},
		{pos4 = XY4b},
		{pos5 = XY4b},
		{pos6 = XY4b},

		-- 0x0e
		{monster1hi = 'uint8_t:1'},
		{monster2hi = 'uint8_t:1'},
		{monster3hi = 'uint8_t:1'},
		{monster4hi = 'uint8_t:1'},
		{monster5hi = 'uint8_t:1'},
		{monster6hi = 'uint8_t:1'},
		{unused_e_6 = 'uint8_t:1'},
		{unused_e_7 = 'uint8_t:1'},
	},
	metatable = function(mt)
		local oldFieldToString = mt.fieldToString

		-- index 1-6
		mt.getMonsterIndex = function(self, i)
			return bit.bor(
				self['monster'..i],
				bit.lshift(self['monster'..i..'hi'], 8)
			)
		end

		-- index 1-6
		mt.getMonsterPos = function(self, i)
			return self['pos'..i]
		end

		-- index 1-6
		mt.getMonsterActive = function(self, i)
			return self['active'..i] ~= 0
		end

		-- index 1-6
		mt.getMonsterInfo = function(self, i)
			return {
				monster = self:getMonsterIndex(i),
				pos = self:getMonsterPos(i),
				active = self:getMonsterActive(i),
			}
		end

		-- index 1-6
		-- is it a pointer into an array of 6? or just one single value?
		mt.getFormationSize = function(self, i)
			local offset = gameC.formationSizeOffsets[self.formationSize]
			local addr = 0x020000 + offset
			local formationSize = ffi.cast(ptrType(FormationSize), rom + addr)
			return formationSize[i-1]
		end

--[=[
		mt.fieldToString = function(self, key, ctype)
			if key:match'^monster%dhi$' then return nil end
			if key:match'^active%d$' then return nil end

			local i = key:match'^monster(%d)$'
			if i then
				if self['active'..i] == 0 then return nil end
				local v = self[key]
				if self['monster'..i..'hi'] ~= 0 then v = v + 0x100 end
				return '"'..gameC.monsterNames[v]..'"'
			end

			local i = key:match'^pos(%d)$'
			if i then
				if self['active'..i] == 0 then return nil end
			end

			-- is this how this works?
			if key == 'formationSize' then
				local v = self[key]
				local offset = gameC.formationSizeOffsets[v]
				local formationSize = ffi.cast(ptrType((FormationSize), rom + 0x020000 + offset)
				return tolua(range(1,6):mapi(function(i)
					if self['active'..i] ~= 0 then
						return tostring(formationSize[i-1])
					end
				end))
			end

			return oldFieldToString(self, key, ctype)
		end
--]=]
	end,
}
assert.eq(ffi.sizeof(Formation), 0xf)

local formationIntroNames = {
	'none',	-- 0
	'smoke',
	'dropdown',
	'from left',
	'splash from below',
	'float down',
	'splash from below (sand?)',
	'from left (fast?)',
	'fade in (top-bottom)',
	'fade in (bottom-top)',
	'fade in (wavey)',
	'fade in (slicey)',
	'none',
	'blink in',
	'stay below screen',
	'slowly fall, play Dancing Mad',
}

local formationMusicNames = {
	'regular',
	'boss',
	'atmaweapon',
	'returners theme',
	'minecart',
	'dancing mad',
	'no change',
	'no change',
}

local Formation2 = ff6struct{
	ctypeOnly = true,
	fields = {
		-- 0:
		{intro = 'uint8_t:4'},
		{normal = 'uint8_t:1'},
		{back = 'uint8_t:1'},
		{pincer = 'uint8_t:1'},
		{side = 'uint8_t:1'},
		-- 1:
		{unknown_1_0 = 'uint8_t:1'},
		{continuousMusic = 'uint8_t:1'},
		{unknown_1_2 = 'uint8_t:1'},
		{unknown_1_3 = 'uint8_t:1'},
		{unknown_1_4 = 'uint8_t:1'},
		{unknown_1_5 = 'uint8_t:1'},
		{unknown_1_6 = 'uint8_t:1'},
		{hasEvent = 'uint8_t:1'},
		-- 2:
		{event = uint8_t},
		-- 3:
		{unknown_3_0 = 'uint8_t:1'},
		{unknown_3_1 = 'uint8_t:1'},
		{windows = 'uint8_t:1'},
		{music = 'uint8_t:3'},
		{unknown_3_6 = 'uint8_t:1'},
		{continuousMusic2 = 'uint8_t:1'},
	},
	metatable = function(mt)
		local oldFieldToString = mt.fieldToString
		mt.fieldToString = function(self, key, ctype)
			if key == 'intro' then
				return '"'..formationIntroNames[self[key]+1]..'"'
			end
			if key == 'music' then
				return '"'..formationMusicNames[self[key]+1]..'"'
			end
			return oldFieldToString(self, key, ctype)
		end
	end,
}
assert.eq(ffi.sizeof(Formation2), 4)

local MonsterRandomBattleEntry = ff6struct{
	ctypeOnly = true,
	fields = {
		{formation = 'uint16_t:15'},			-- only 10 bits are used, since thats all you need to index numFormations = 0x240
		{chooseFromNextFour = 'uint16_t:1'},	-- only set for random battle #0x70: Behemoth, Ninja x2, {Brainpain x2, Misfit, Apokrypohs}, Dragon
	},
}
assert.eq(ffi.sizeof(MonsterRandomBattleEntry), 2)

local MonsterRandomBattleEntry2 = createVec{
	ctypeOnly = true,
	ctype = MonsterRandomBattleEntry,
	dim = 2,
}

local MonsterRandomBattleEntry4 = createVec{
	ctypeOnly = true,
	ctype = MonsterRandomBattleEntry,
	dim = 4,
}

local terrainTypes = {'grass', 'forest', 'desert', 'dirt'}
local encounterNames = {'normal', 'low', 'high', 'none'}
local WorldSectorRandomBattlesPerTerrain = ff6struct{
	ctypeOnly = true,
	fields = {
		-- each is a ref to monsterRandomBattles[]
		{grass = uint8_t},
		{forest = uint8_t},
		{desert = uint8_t},
		{dirt = uint8_t},
	},
}

-- the first 'numMonsters' overlaps
-- then there's 32 more
-- the first 27 of those are espers
-- the last 5 are unknown/unused
-- TODO there are 0x19f of these, not 0x180 ...
-- ... same with the monster stat table?
local MonsterSprite = struct{
	ctypeOnly = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	packed = true,
	fields = {
		{name='offset', type='uint16_t:15'},	-- 0.0 - 1.6
		{name='_3bpp', type='uint16_t:1'},		-- 1.7
		{name='palHi', type='uint8_t:7'},		-- 2.0 - 2.6
		{name='tile16', type='uint8_t:1'},		-- 2.7
		{name='palLo', type=uint8_t},			-- 3.0 - 3.7
		{name='tileMaskIndex', type=uint8_t},	-- 4.0 - 4.7
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
		function mt:getPaletteIndex()
			return bit.bor(self.palLo, bit.lshift(self.palHi, 8))
		end
	end,
}
assert.eq(ffi.sizeof(MonsterSprite), 5)

---------------- ITEMS ----------------

local numItems = 0x100

local ItemRef = reftype{
	ctypeOnly = true,
	getter = function(i)
		if i == 0xff then return nil end
		return gameC.itemNames[i]
	end,
	getterSkipNone = true,
}

local itemUseAbilityNames = {
	'nothing',	-- 00
	'magicite',
	'superball',
	'smoke bomb',
	'elixer, megalixer',
	'warp stone',
	'dried meat',
}

local EquipFlags = bitflagtype{
	type = 'uint16_t',
	options = {
		'terra',
		'locke',
		'cyan',
		'shadow',
		'edgar',
		'sabin',
		'celes',
		'strago',
		'relm',
		'setzer',
		'mog',
		'gau',
		'gogo',
		'umaro',
		'impItem',
		'meritAward',
	},
}
assert.eq(ffi.sizeof(EquipFlags), 2)

local itemSpecialAbilityNames = {
	'nothing',	-- 00
	'randomally steals',
	'transforms at level up, grows as HP increases',
	'randomally dispatches an enemy',
	'double damage to humans',
	'absorbs damage as HP',
	'absorbs damage as MP',
	'uses MP for mortal blow',
	'whatever a hawk eye does ???',
	'dice - what dice do',
	'gains power as HP decreases',
	'randomally casts "wind slash"',
	'heals person',
	'randomally dices up an enemy',
	'uses MP for mortal blow, may break',
	'uses MP for mortal blow',
}

local Item = ff6struct{
	ctypeOnly = true,
	fields = {
		-- 0x00:
		{itemType = 'uint8_t:4'},		-- not the same as 'itemTypeNames'
		{canBeThrown = 'uint8_t:1'},
		{canUseInBattle = 'uint8_t:1'},
		{canUseInMenu = 'uint8_t:1'},
		{unused_0_7 = 'uint8_t:1'},		-- only here for the ptr union size calc in struct.lua
		-- 0x01:
		{equip = EquipFlags},
		-- 0x03:
		{spellLearn = SpellLearn},
		-- 0x05:
		{isCharmBangle = 'uint8_t:1'},
		{isMoogleCharm = 'uint8_t:1'},
		{unused_5_2 = 'uint8_t:1'},
		{unused_5_3 = 'uint8_t:1'},
		{unused_5_4 = 'uint8_t:1'},
		{isSprintShoes = 'uint8_t:1'},
		{unused_5_6 = 'uint8_t:1'},
		{isTintinabar = 'uint8_t:1'},
		-- 0x06:
		{immuneToEffect1 = Effect1},
		-- 0x07:
		{immuneToEffect2 = Effect2},
		-- 0x08:
		{hasEffect3 = Effect3},
		-- 0x09:
		{raiseFightDamage = 'uint8_t:1'},
		{raiseMagicDamage = 'uint8_t:1'},
		{raiseHPByQuarter = 'uint8_t:1'},
		{raiseHPByHalf = 'uint8_t:1'},
		{raiseHPByEighth = 'uint8_t:1'},
		{raiseMagDef = 'uint8_t:1'},
		{raiseMPByHalf = 'uint8_t:1'},
		{raiseMPByEighth = 'uint8_t:1'},
		-- 0x0a:
		{raisePreEmptiveAttackRate = 'uint8_t:1'},
		{preventBackAttack = 'uint8_t:1'},
		{changeFightToJump = 'uint8_t:1'},
		{changeMagicToXMagic = 'uint8_t:1'},
		{changeSketchToControl = 'uint8_t:1'},
		{changeSlotToGPRain = 'uint8_t:1'},
		{changeStealToCapture = 'uint8_t:1'},
		{changeJumpToXJump = 'uint8_t:1'},
		-- 0x0b:
		{raiseStealChance = 'uint8_t:1'},
		{unused_b_1 = 'uint8_t:1'},
		{raiseSketchChance = 'uint8_t:1'},
		{raiseControlChance = 'uint8_t:1'},
		{fightAlwaysHits = 'uint8_t:1'},
		{halfMPConsumed = 'uint8_t:1'},
		{magicCosts1 = 'uint8_t:1'},
		{raiseVigorByHalf = 'uint8_t:1'},
		-- 0x0c:
		{changeFightToXFight = 'uint8_t:1'},
		{randomlyCounterattacks = 'uint8_t:1'},
		{randomlyEvadeAttacks = 'uint8_t:1'},
		{holdOneWeaponWithTwoHands = 'uint8_t:1'},
		{holdTwoWeapons = 'uint8_t:1'},
		{equipMeritAwardItems = 'uint8_t:1'},
		{protectPartyMembersLowOnHP = 'uint8_t:1'},
		{unused_c_7 = 'uint8_t:1'},
		-- 0x0d:
		{castShellWhenHPIsLow = 'uint8_t:1'},
		{castSafeWhenHPIsLow = 'uint8_t:1'},
		{unused_d_2 = 'uint8_t:1'},
		{doubleExpGained = 'uint8_t:1'},
		{pickUpMoreGP = 'uint8_t:1'},
		{unused_d_5 = 'uint8_t:1'},
		{unused_d_6 = 'uint8_t:1'},
		{makeUndead = 'uint8_t:1'},
		-- 0x0e:
		{targetting = Targetting},
		-- 0x0f:
		-- TODO UNION
		{element_weaponDamage_equipHalfDamage = uint8_t},
		-- 0x10:
		{vigor = 'uint8_t:4'},
		{speed = 'uint8_t:4'},
		-- 0x11:
		{stamina = 'uint8_t:4'},
		{magicPower = 'uint8_t:4'},
		-- 0x12:
		{spellCast = 'uint8_t:6'},	-- should be SpellRef, but it looks like you can't use structs with bitfields
		{castOnAttack = 'uint8_t:1'},
		{castOnItemUse = 'uint8_t:1'},	-- "destroy if used"
		-- 0x13:
		{protectFromMortalBlows = 'uint8_t:1'},	-- memento ring
		{runicCompatible = 'uint8_t:1'},
		{unused_13_2 = 'uint8_t:1'},
		{healsHP = 'uint8_t:1'},
		{healsMP= 'uint8_t:1'},
		{sameDamageFromBackRow = 'uint8_t:1'},
		{canEquipTwoHands = 'uint8_t:1'},
		{swdTechCompatible = 'uint8_t:1'},
		-- 0x14:
		-- TODO UNION
		{battlePower_defense = uint8_t},
		-- 0x15:
		{hitChance_magicDefense = uint8_t},
		-- 0x16:
		{elementAbsorb = Element},
		-- 0x17:
		{elementNoEffect = Element},
		-- 0x18:
		{elementWeak = Element},
		-- 0x19:
		{givesEffect2 = Effect2},
		-- 0x1a:
		{evade = 'uint8_t:4'},
		{magicBlock = 'uint8_t:4'},	-- 0..5 => 0..5, 6..10 => -1..-5
		-- 0x1b:
		-- item type: item ability
		-- non-item type: evade:4, special ability:4
		{itemUseAbility = 'uint8_t:4'},
		{itemSpecialAbility = 'uint8_t:4'},
		-- 0x1c:
		-- sell price is half of buy price
		{buyPrice = uint16_t},
	},
	metatable = function(mt)
		local oldFieldToString = mt.fieldToString
		mt.fieldToString = function(self, name, ctype)
			if name == 'spellCast' then
				local i = self[name]
				if i == 0xff then return nil end
				return '"'..getSpellName(i)..'"'
			elseif name == 'itemUseAbility' then
				local s = itemUseAbilityNames[self[name]+1]
				return s and '"'..s..'"' or tostring(self[name])
			elseif name == 'itemSpecialAbility' then
				local s = itemSpecialAbilityNames[self[name]+1]
				return s and '"'..s..'"' or tostring(self[name])
			end
			return oldFieldToString(self, name, ctype)
		end
	end,
}
assert.eq(ffi.offsetof(Item, 'itemType'), 0)
assert.eq(ffi.offsetof(Item, 'spellLearn'), 3)
assert.eq(ffi.offsetof(Item, 'raiseStealChance'), 0x0b)
assert.eq(ffi.offsetof(Item, 'changeFightToXFight'), 0x0c)
assert.eq(ffi.sizeof(Item), 0x1e)

local ItemColosseumInfo = ff6struct{
	ctypeOnly = true,
	fields = {
		{monster = MonsterRef},
		{unknown = uint8_t},	-- always 64.  worth experimenting to see if bit 0 here is for the monster ref?
		{itemWon = ItemRef},
		{hideName = uint8_t},	-- 0 = no, 255 = yes
	},
}
assert.eq(ffi.sizeof(ItemColosseumInfo), 4)

local RareItemName = Str13
local numRareItems = 20

---------------- MONSTERS ----------------

-- Monster 0x1f
local monsterSpecialAttackNames = {
	'None',
	'Steal Item',
	'Attack increases as HP increases',
	'Kill (with X)',
	'Cause 2x damage to humans',
	'Drain HP',
	'Drain MP',
	'Attack with MP',
	'',
	'Dice',
	'Attack increases as HP decreases',
	'Wind attack',
	'Recover HP',
	'Kill',
	'Uses MP to inflict mortal blow',
	'Uses (more) MP to inflict mortal blow',
}

local monsterAttackNamesAddr = 0x0fd0d0

local Monster = ff6struct{
	ctypeOnly = true,
	fields = {
		-- 0x00:
		{speed = uint8_t},		-- rpglegion says speed
		-- 0x01:
		{battlePower = uint8_t},	-- rpglegion says battle power
		-- 0x02:
		{hitChance = uint8_t},
		-- 0x03:
		{evade = uint8_t},
		-- 0x04:
		{magicBlock = uint8_t},
		-- 0x05:
		{defense = uint8_t},
		-- 0x06:
		{magicDefense = uint8_t},
		-- 0x07:
		{magicPower = uint8_t},
		-- 0x08:
		{hp = uint16_t},
		-- 0x0a:
		{mp = uint16_t},
		-- 0x0c:
		{exp = uint16_t},
		-- 0x0e:
		{gold = uint16_t},
		-- 0x10:
		{level = uint8_t},
		-- 0x11:
		{metamorphSet = 'uint8_t:5'},		-- TODO metamorphSetRef_t ?
		{metamorphResist = 'uint8_t:3'},
		-- 0x12:
		{diesIfRunOutOfMP = 'uint8_t:1'},
		{unused_12_1 = 'uint8_t:1'},
		{hideName = 'uint8_t:1'},
		{unused_12_3 = 'uint8_t:1'},
		{human = 'uint8_t:1'},
		{unused_12_5 = 'uint8_t:1'},
		{impCritical = 'uint8_t:1'},
		{undead = 'uint8_t:1'},
		-- 0x13:
		{hardToRun = 'uint8_t:1'},
		{firstStrike = 'uint8_t:1'},
		{cantSuplex = 'uint8_t:1'},
		{cantRun = 'uint8_t:1'},
		{cantScan = 'uint8_t:1'},
		{cantSketch = 'uint8_t:1'},
		{specialEvent = 'uint8_t:1'},
		{cantControl = 'uint8_t:1'},
		-- 0x14:
		{immuneToEffect1 = Effect1},
		-- 0x15:
		{immuneToEffect2 = Effect2},
		-- 0x16:
		{elementHalfDamage = Element},
		-- 0x17:
		{elementAbsorb = Element},
		-- 0x18:
		{elementNoEffect = Element},
		-- 0x19:
		{elementWeak = Element},
		-- 0x1a:
		{fightAnimation = uint8_t},
		-- 0x1b:
		{hasEffect1 = Effect1},
		-- 0x1c:
		{hasEffect2 = Effect2},
		-- 0x1d:
		{hasEffect3 = Effect3},
		-- 0x1e:
		-- for some reason I thought this byte was Effect4 ...
		{unused_1e_0 = 'uint8_t:1'},
		{Speck = 'uint8_t:1'},
		{unused_1e_2 = 'uint8_t:1'},
		{unused_1e_3 = 'uint8_t:1'},
		{unused_1e_4 = 'uint8_t:1'},
		{unused_1e_5 = 'uint8_t:1'},
		{unused_1e_6 = 'uint8_t:1'},
		{Paranha = 'uint8_t:1'},
		-- 0x1f:
		{specialAttack = 'uint8_t:7'},
		{specialAttackDealsNoDamage = 'uint8_t:1'},
	},
	metatable = function(mt)
		local oldFieldToString = mt.fieldToString
		mt.fieldToString = function(self, name, ctype)
			if name == 'specialAttack' then
				local s = monsterSpecialAttackNames[self[name]+1]
				return s and '"'..s..'"' or tostring(self[name])
			end
			return oldFieldToString(self, name, ctype)
		end
	end,
}
assert.eq(ffi.sizeof(Monster), 0x20)

local MonsterItem = ff6struct{
	ctypeOnly = true,
	fields = {
		{rareSteal = ItemRef},
		{commonSteal = ItemRef},
		{rareDrop = ItemRef},
		{commonDrop = ItemRef},
	},
}

local SpellRef2 = createVec{
	ctypeOnly = true,
	ctype = SpellRef,
	dim = 2,
}
assert.eq(ffi.sizeof(SpellRef2), 2)

local SpellRef4 = createVec{
	ctypeOnly = true,
	ctype = SpellRef,
	dim = 4,
}
assert.eq(ffi.sizeof(SpellRef4), 4)

local ItemRef4 = createVec{
	ctypeOnly = true,
	ctype = ItemRef,
	dim = 4,
}
assert.eq(ffi.sizeof(ItemRef4), 4)

---------------- CHARACTERS ----------------

local numLevels = 98

local MenuName = Str7

local MenuNameRef = reftype{
	ctypeOnly = true,
	getter = function(i)
		return gameC.menuNames[i]
	end,
}

local CharacterName = Str6

local MenuNameRef4 = createVec{
	ctypeOnly = true,
	dim = 4,
	ctype = MenuNameRef,
}

local ItemRef2 = createVec{
	ctypeOnly = true,
	ctype = ItemRef,
	dim = 2,
}

local Character = ff6struct{
	ctypeOnly = true,
	fields = {
		{hp = uint8_t},
		{mp = uint8_t},
		{menu = MenuNameRef4},
		{vigor = uint8_t},
		{speed = uint8_t},
		{stamina = uint8_t},
		{magicPower = uint8_t},
		{battlePower = uint8_t},	-- note: equipping nothing gives +10 to battle power
		{defense = uint8_t},
		{magicDefense = uint8_t},
		{evade = uint8_t},
		{magicBlock = uint8_t},
		-- or should these all be an itemref6_t?
		{lhand = ItemRef},
		{rhand = ItemRef},
		{head = ItemRef},
		{body = ItemRef},
		{relic = ItemRef2},
		{level = uint8_t},
	},
}
assert.eq(ffi.sizeof(Character), 22)

local numCharacters = 0x40	-- allegedly...

local MogDanceName = Str12
local numMogDances = 8

local SwordTechName = Str12
local numSwordTechs = 8

local numBlitzes = 8

local numLores = 24

-- animation frames. stand, walk, etc
-- 41 frames for characters 0 through 21
-- 9 frames for characters 22 through 31
-- 1 frames for characters 63 through ...
-- then 87 and on its a different frame table ...
-- there's gotta be frame data somewhere ...
-- oh and all the palettes are messed up too
local numCharacterSpriteFrames = 41

-- number of sprited playable characters
local numCharacterSprites = 165

-- these are shared between playable and non-playable and map
local numCharacterPalettes = 0x20

local CharHiAndSize = ff6struct{
	ctypeOnly = true,
	fields = {
		{hi = uint8_t},
		{size = uint8_t},	-- in bytes
	},
}
assert.eq(ffi.sizeof(CharHiAndSize), 2)

---------------- MAP ----------------

local shopTypes = {
	'(none)',
	'weapon',
	'armor',
	'item',
	'relic',
	'vendor',
}

local shopPriceTypes = {
	'(none)',
	'2x',
	'1.5x',
	'0.5x with Sabin or Edgar',
}

local ShopInfo = ff6struct{
	ctypeOnly = true,

	fields = {
		{shopType = 'uint8_t:4'},
		{priceType = 'uint8_t:4'},
	},

	metatable = function(mt)
		local oldFieldToString = mt.fieldToString
		-- too bad C can't do bitfield-structs ...
		mt.fieldToString = function(self, name, ctype)
			if name == 'shopType' then
				local v = self[name]
				if v == 0 then return nil end
				local s = shopTypes[v+1]
				return s and '"'..s..'"' or tostring(self[name])
			elseif name == 'priceType' then
				local v = self[name]
				if v == 0 then return nil end
				local s = shopPriceTypes[v+1]
				return s and '"'..s..'"' or tostring(self[name])
			end
			return oldFieldToString(self, name, ctype)
		end
	end,
}

local ItemRef8 = createVec{
	ctypeOnly = true,
	ctype = ItemRef,
	dim = 8,
}

local Shop = ff6struct{
	ctypeOnly = true,
	fields = {
		{shopinfo = ShopInfo},
		{items = ItemRef8},
	}
}
assert.eq(ffi.sizeof(Shop), 9)

local numMapNames = 448	-- 146 entries are used, the rest are 0xffff

local numDialogs = 3328
local numBattleDialogs = 0x100
local numBattleDialog2s = 0x100
local numBattleMessages = 0x100

local numPositionedText = 5	-- might actually be lower

local XY8sb = ff6struct{
	ctypeOnly = true,
	fields = {
		{x = 'int8_t'},
		{y = 'int8_t'},
	},
}
assert.eq(ffi.sizeof(XY8sb), 2)

local XY8b = ff6struct{
	ctypeOnly = true,
	fields = {
		{x = uint8_t},
		{y = uint8_t},
	},
}
assert.eq(ffi.sizeof(XY8b), 2)

local MapNameRef = reftype{
	ctypeOnly = true,
	getter = function(i)
		return game.mapNames[i]
	end,
}

local Map = struct{
	ctypeOnly = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	packed = true,
	fields = {
		{name='name', type=MapNameRef},						-- 0
		{name='enableXZone', type='uint8_t:1'},					-- 1.0
		{name='enableWarp', type='uint8_t:1'},					-- 1.1
		{name='wavyLayer3', type='uint8_t:1'},					-- 1.2
		{name='wavyLayer2', type='uint8_t:1'},					-- 1.3
		{name='wavyLayer1', type='uint8_t:1'},					-- 1.4
		{name='enableSpotlights', type='uint8_t:1'},			-- 1.5
		{name='unknown_1_6', type='uint8_t:1'},					-- 1.6
		{name='showTimer', type='uint8_t:1'},					-- 1.7
		{name='battleBG', type='uint8_t:7'},					-- 2.0-6
		{name='layer3Priority', type='uint8_t:1'},				-- 2.7
		{name='unknown_3', type=uint8_t},						-- 3
		{name='tileProps', type=uint8_t},						-- 4		mapTilePropsOffsets[]
		{name='attacks', type='uint8_t:7'},						-- 5.0-6
		{name='enableBattles', type='uint8_t:1'},				-- 5.7
		{name='windowMask', type='uint8_t:2'},					-- 6.0-1: 0=default, 1=imperial camp, 2=ebot's rock, 3=kefka's tower spotlight
		{name='unknown_6_2', type='uint8_t:6'},					-- 6.2-7

		-- I can't just put these fields into the base struct... it says "NYI: packed bit fields"
		-- but if I nest them as an anonymous struct ... works fine
		{
			type = struct{
				anonymous = true,
				tostringFields = true,
				tostringOmitFalse = true,
				tostringOmitNil = true,
				tostringOmitEmpty = true,
				fields = {
					{name='gfx1', type='uint32_t:7'},			-- 7.0-6
					{name='gfx2', type='uint32_t:7'},			-- 7.7-13
					{name='gfx3', type='uint32_t:7'},			-- 7.14-20
					{name='gfx4', type='uint32_t:7'},			-- 7.21-27
					{name='gfxLayer3', type='uint32_t:4'},		-- 7.28-31
				},
				metatable = function(mt)
					mt.typeToString = fieldsToHex
				end,
			},
		},
		{
			type = struct{
				anonymous = true,
				tostringFields = true,
				tostringOmitFalse = true,
				tostringOmitNil = true,
				tostringOmitEmpty = true,
				fields = {
					{name='unknown_b', type='uint16_t:2'},		-- 0xb.0-1
					{name='tileset1', type='uint16_t:7'},		-- 0xb.2-8		mapTilesets[]
					{name='tileset2', type='uint16_t:7'},		-- 0xb.8-15
				},
				metatable = function(mt)
					mt.typeToString = fieldsToHex
				end,
			},
		},
		{
			type = struct{
				anonymous = true,
				tostringFields = true,
				tostringOmitFalse = true,
				tostringOmitNil = true,
				tostringOmitEmpty = true,
				fields = {
					{name='layout1', type='uint32_t:10'},		-- 0xd.0-9		mapLayouts[]
					{name='layout2', type='uint32_t:10'},		-- 0xd.10-19
					{name='layout3', type='uint32_t:10'},		-- 0xd.20-29
					{name='unknown_d_30', type='uint32_t:2'},	-- 0xd.30-31
				},
				metatable = function(mt)
					mt.typeToString = fieldsToHex
				end,
			},
		},
		{name='mapOverlayProperties', type=uint8_t},			-- 0x11
		{name='layer2Pos', type=XY8sb},						-- 0x12
		{name='layer3Pos', type=XY8sb},						-- 0x14
		{name='parallax', type=uint8_t},					-- 0x16
		{name='layer2HeightLog2Minus4', type='uint8_t:2'},		-- 0x17.2-3
		{name='layer2WidthLog2Minus4', type='uint8_t:2'},		-- 0x17.0-1
		{name='layer1HeightLog2Minus4', type='uint8_t:2'},		-- 0x17.4-5	layer1Height = 1 << (layer1HeightLog2Minus4 + 4)
		{name='layer1WidthLog2Minus4', type='uint8_t:2'},		-- 0x17.6-7	layer1Width = 1 << (layer1WidthLog2Minus4 + 4)
		{name='unknown_18_0', type='uint8_t:4'},				-- 0x18.0-3
		{name='layer3HeightLog2Minus4', type='uint8_t:2'},		-- 0x18.6-7
		{name='layer3WidthLog2Minus4', type='uint8_t:2'},		-- 0x18.4-5
		{name='palette', type=uint8_t},						-- 0x19
		{name='paletteAnimation', type=uint8_t},				-- 0x1a
		{name='animatedLayers1And2', type='uint8_t:5'},			-- 0x1b.0-4
		{name='animatedLayer3', type='uint8_t:3'},				-- 0x1b.5-7
		{name='music', type=uint8_t},							-- 0x1c
		{name='unknown_1d', type=uint8_t},					-- 0x1d
		-- map 21, size is {44,52}, tiles are defined up to {46,54} ... why is it 2 less?
		{name='size', type=XY8b},							-- 0x1e
		{name='colorMath', type=uint8_t},						-- 0x20
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
}
assert.eq(ffi.sizeof(Map), 0x21)

local MapTileProps = ff6struct{
	ctypeOnly = true,
	fields = {
		{zLevel = 'uint16_t:3'},				-- 0.0-0.2 = 0=none 1=upstairs 2=downstairs 3=upstairs & downstairs 4=bridge
		{topSpritePriority = 'uint16_t:1'},		-- 0.3
		{bottomSpritePriority = 'uint16_t:1'},	-- 0.4
		{door = 'uint16_t:1'},					-- 0.5
		{stairsUpRight = 'uint16_t:1'},			-- 0.6
		{stairsUpLeft = 'uint16_t:1'},			-- 0.7
		{passableRight = 'uint16_t:1'},			-- 1.0
		{passableLeft = 'uint16_t:1'},			-- 1.1
		{passableDown = 'uint16_t:1'},			-- 1.2
		{passableUp = 'uint16_t:1'},			-- 1.3
		{unknown_1_4 = 'uint16_t:1'},			-- 1.4
		{unknown_1_5 = 'uint16_t:1'},			-- 1.5
		{ladder = 'uint16_t:1'},				-- 1.6
		{passableNPC = 'uint16_t:1'},			-- 1.7
	},
}
assert.eq(ffi.sizeof(MapTileProps), 2)

local uint16_4_t = createVec{
	ctypeOnly = true,
	ctype = uint16_t,
	dim = 4,
}

local MapAnimProps = ff6struct{
	ctypeOnly = true,
	fields = {
		{speed = uint16_t},		-- 0-1
		{frames = uint16_4_t},	-- 2-9
	},
}
assert.eq(ffi.sizeof(MapAnimProps), 0xa)

local uint16_8_t = createVec{
	ctypeOnly = true,
	ctype = uint16_t,
	dim = 8,
}

local MapAnimPropsLayer3 = ff6struct{
	ctypeOnly = true,
	fields = {
		{speed = uint16_t},		-- 0-1
		{size = uint16_t},		-- 2-3
		{frames = uint16_8_t},	-- 4-0x13
	},
}
assert.eq(ffi.sizeof(MapAnimPropsLayer3), 0x14)

local MapPalAnim = ff6struct{
	ctypeOnly = true,
	fields = {
		{colorCounter = 'uint8_t:4'},
		{type = 'uint8_t:4'},
		{frameCounter = uint8_t},
		{firstColor = uint8_t},
		{numColors = uint8_t},
		{romColorOffset = uint16_t},
	},
}
assert.eq(ffi.sizeof(MapPalAnim), 6)
--[[
there's 20 of these in a row. 2 per 10
the 10 are:
	River
	Lava/Cyan's Dream
	Imperial Camp
	Beach
	Doma Castle
	Waterfall
	Narshe
	Kefka's Tower
	Darill's Tomb
--]]



local mapTilesetOfsAddr = 0x1fba00

local Treasure = ff6struct{
	ctypeOnly = true,
	fields = {
		{pos = XY8b},
		{switch = 'uint16_t:9'},	-- global index / bitflag? into game state data?
		{unused_2_1 = 'uint16_t:1'},	-- does this bit go in 'switch' too? like in NPC switch is 10 bits...
		{unused_2_2 = 'uint16_t:1'},
		{empty = 'uint16_t:1'},		-- set iff type == 0 i.e. empty
		{unused_2_4 = 'uint16_t:1'},
		{type = 'uint16_t:3'},	-- 0=empty, 1=monster, 2=item, 3=gp
		-- depends on 'type'
		{battleOrItemOrGP = uint8_t},	-- GP is x100
	},
}
assert.eq(ffi.sizeof(Treasure), 5)


local NPC = struct{
	ctypeOnly = true,
	packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	
		mt.getScriptAddr = function(self)
			--if self.vehicle == 0 and and self.showRider_or_specialGraphics ~= 0 then return end
			return self.script + ffi.offsetof(Game, 'eventScript')
		end
	end,
	fields = {
		{type=struct{
			union = true,
			anonymous = true,
			packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
metatable = function(mt)
	mt.typeToString = fieldsToHex
end,
			fields = {
				{type=struct{
					anonymous = true,
					packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
metatable = function(mt)
	mt.typeToString = fieldsToHex
end,
					fields = {
						-- invalid when vehicle == 0 && specialGraphics != 0
						-- offset 0xa0000 (game.eventScript)
						{name='script', type='uint32_t:18'},				-- 0.0-2.1

						{name='unknown_2_2', type='uint32_t:3'},			-- 2.2-2.4
						{name='scrollingLayer', type='uint32_t:1'},			-- 2.5 = 0=layer1, 1=layer2
						{name='switch', type='uint32_t:10'},				-- 2.6-3.7
					},
				}},

				-- invalid when vehicle value != 0 && specialGraphics == 0
				{type=struct{
					anonymous = true,
					packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
metatable = function(mt)
	mt.typeToString = fieldsToHex
end,
					fields = {
						{name='vramAddr', type='uint8_t:7'},				-- 0.0-0.6
						{name='hflip', type='uint8_t:1'},					-- 0.7
						{name='masterNPC', type='uint8_t:5'},				-- 1.0-1.4
						{name='offset', type='uint8_t:3'},					-- 1.5-1.7
						{name='offsetDir', type='uint8_t:1'},				-- 2.0 = 0=right, 1=down
						{name='slaveNPC', type='uint8_t:1'},				-- 2.1
						{name='palette', type='uint8_t:3'},					-- 2.2-2.4
						{name='unused_2_5', type='uint8_t:3'},				-- 2.5-2.7
					},
				}},
			},
		}},

		{name='x', type='uint8_t:7'},								-- 4.0-4.6

		-- "specialGraphics" if vehicle == 0 and specialGraphics != 0 then ...
		-- wait, isn't that a circular condition?
		-- otherwise "showRider"
		{name='showRider_or_specialGraphics', type='uint8_t:1'},	-- 4.7

		{name='y', type='uint8_t:6'},								-- 5.0-5.5
		{name='speed', type='uint8_t:2'},							-- 5.6-5.7 = slow to fast
		{name='graphics', type=uint8_t},							-- 6
		{name='movement', type='uint8_t:4'},						-- 7.0-7.3 = 0=none, 1=script, 2=user, 3=random
		{name='spritePriority', type='uint8_t:2'},					-- 7.4-7.5 = 0=normal 1=high 2=low 3=low

		-- "speed" when vehicle == 0
		-- "vehicle" otherwise
		-- wait, isn't that circular?
		-- having a hard time telling what fields determine what
		-- there is clearly about 2-4-8 different structs at play here
		{name='vehicle_or_speed', type='uint8_t:2'},				-- 7.6-7.7 = 0=none 1=chocobo 2=magitek 3=raft

		-- "direction" when animation == 0
		-- "type" otherwise
		{name='direction_or_type', type='uint8_t:2'},				-- 8.0-8.1 direction = {up, right, down, left}, type = {one frame, flip horz, two frames, four frames}

		-- "size" when vehicle == 0 && specialGraphics != 0
		-- otherwise "talkDoesntTurn"
		{name='size_or_talkDoesntTurn', type='uint8_t:1'},			-- 8.2.  size: 0=16x16, 1=32x32

		{name='layerPriority', type='uint8_t:2'},					-- 8.3-8.4 0=default 1=top sprite only 2=foreground 3=background

		{name='animation', type='uint8_t:3'},						-- 8.5-8.7
	},
}
assert.eq(ffi.sizeof(NPC), 9)


--[=[
-- this is in Door, maybe in BigDoor, but also in the 
local MapDest = ff6struct{
	ctypeOnly = true,
	fields = {
		-- also last 4 bytes of Door:
		{mapIndex = 'uint16_t:9'},		-- 0.0-1.0: maps[]
		{setParentMap = 'uint16_t:1'},	-- 1.1
		{zLevel = 'uint16_t:1'},		-- 1.2
		{showDestName = 'uint16_t:1'},	-- 1.3
		{destFacingDir = 'uint16_t:2'},	-- 1.4-1.5
		{unknown_3_6 = 'uint16_t:2'},	-- 1.6-1.7
		{dest = XY8b},				-- 2-3
	}
}
assert.eq(ffi.sizeof(MapDest), 4)

-- this is the event script cmd for changing maps
local ChangeMapEvent = ff6struct{
	ctypeOnly = true,
	fields = {
		{anonymous=true, type=MapDest},	-- 0.0-4.0

		{vehicle = 'uint8_t:2'},		-- 4.0-4.1
		{unknown_4_2 = 'uint8_t:3'},	-- 4.2-4.4
		{noSizeUpdate = 'uint8_t:1'},	-- 4.5
		{manualFadeIn = 'uint8_t:1'},	-- 4.6
		{enableMapEvent = 'uint8_t:1'},	-- 4.7
	},
}
assert.eq(ffi.sizeof(ChangeMapEvent), 5)
--]=]

local Door = ff6struct{
	ctypeOnly = true,
	fields = {
		{pos = XY8b},					-- 0-1

		-- TODO also MapDest ... anonymous inline?
		{mapIndex = 'uint16_t:9'},		-- 2.0-3.0: maps[]
		{setParentMap = 'uint16_t:1'},	-- 3.1
		{zLevel = 'uint16_t:1'},		-- 3.2
		{showDestName = 'uint16_t:1'},	-- 3.3
		{destFacingDir = 'uint16_t:2'},	-- 3.4-3.5
		{unknown_3_6 = 'uint16_t:2'},	-- 3.6-3.7
		{dest = XY8b},					-- 4-5
	},
}
assert.eq(ffi.sizeof(Door), 6)

local BigDoor = struct{
	ctypeOnly = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	packed = true,
	fields = {
		{name='pos', type=XY8b},					-- 0-1
		{name='length', type='uint8_t:7'},			-- 2.0-2.6
		{name='vertical', type='uint8_t:1'},		-- 2.7
	
		-- TODO everything below is also MapDest ... anonymous inline?
		{
			type = struct{
				anonymous = true,
				tostringFields = true,
				tostringOmitFalse = true,
				tostringOmitNil = true,
				tostringOmitEmpty = true,
				fields = {
					{name='mapIndex', type='uint16_t:9'},	-- 3.0-4.0: maps[]
					{name='setParentMap', type='uint16_t:1'},	-- 3.1
					{name='zLevel', type='uint16_t:1'},		-- 3.2
					{name='showDestName', type='uint16_t:1'},	-- 3.3
					{name='destFacingDir', type='uint16_t:2'},	-- 3.4-3.5
					{name='unknown_3_6', type='uint16_t:2'},	-- 3.6-3.7
				},
				metatable = function(mt)
					mt.typeToString = fieldsToHex
				end,
			},
		},
		{name='dest', type=XY8b},					-- 4-5
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
}
assert.eq(ffi.sizeof(BigDoor), 7)

local TouchTrigger = ff6struct{
	ctypeOnly = true,
	fields = {
		{pos = XY8b},
		{script = uint24_t},
	},
	metatable = function(mt)
		mt.getScriptAddr = function(self)
			return self.script:value() + ffi.offsetof(Game, 'eventScript')
		end
	end,
}
assert.eq(ffi.sizeof(TouchTrigger), 5)

local WorldTileProps = ff6struct{
	ctypeOnly = true,
	fields = {
		{blocksChocobo = 'uint16_t:1'},		-- 0.0
		{airshipCantLand = 'uint16_t:1'},	-- 0.1
		{airshipShadow = 'uint16_t:2'},		-- 0.2-3 ... aka elevation?
		{blocksWalking = 'uint16_t:1'},		-- 0.4
		{bottomHalfSemiTransparent = 'uint16_t:1'},	-- 0.5
		{enemyEncounters = 'uint16_t:1'},	-- 0.6
		{unknown_0_7 = 'uint16_t:1'},		-- 0.7
		-- now I can't tell from the ascii art, but its either 1 unused and 3 for background, or its 4 for background ...
		-- https://web.archive.org/web/20250429144337/https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:fmt:world_map_tile_properties
		-- there's 56 total
		-- https://www.spriters-resource.com/snes/ff6/asset/54685/
		-- so ... 6 bits?
		{battleBG = 'uint16_t:4'},			-- 1.0-1.3 aka 0.8-0.11
		{unknown_0_12 = 'uint16_t:1'},		-- 1.4 aka 0.12
		{veldt = 'uint16_t:1'},				-- 1.5 aka 0.13
		{phoenixCave = 'uint16_t:1'},		-- 1.6 aka 0.14
		{kefkasTower = 'uint16_t:1'},		-- 1.7 aka 0.15
	},
}
assert.eq(ffi.sizeof(WorldTileProps), 2)

local BattleBgProps = struct{
	ctypeOnly = true,
	packed = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
	fields = {
		{
			type = struct{
				anonymous = true,
				union = true,
				fields = {
					{
						name = 'graphics1combined',		-- if this is 0xff then graphics1 is invalid.
						type = uint8_t
					},
					{
						type = struct{
							anonymous = true,
							fields = {
								{name='graphics1', type='uint8_t:7'},				-- 0.0-0.6 \_ combined, 0xff = none
								{name='graphics1doubleSized', type='uint8_t:1'},	-- 0.7     /
							},
						},
					},
				},
			},
		},
		{name='graphics2', type=uint8_t},					-- 1: 0xff = none.
		{name='graphics3', type=uint8_t},					-- 2: 0xff = none.
		{name='layout1', type=uint8_t},					-- 3: battleBgLayout[]
		{name='layout2', type=uint8_t},					-- 4:
		{name='palette', type='uint8_t:7'},					-- 5.0-5.6
		{name='wavy', type='uint8_t:1'},					-- 5.7
	},
}
assert.eq(ffi.sizeof(BattleBgProps), 6)


---------------- GAME ----------------

-- TODO while putting the entire ROM into a single C struct seems clever,
-- ... doing so is very rigid and with lots of redundancies.
-- the MemoryMap system of super-metroid-randomizer is better:
--  https://github.com/thenumbernine/super-metroid-randomizer-lua
Game = struct{
	ctypeOnly = true,
	packed = true,
	notostring = true,	-- too big to serialize
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
		
	metatable = function(m)
		-- default output to hex
		m.__index.typeToString = {
			uint8_t = function(value)
				return ('0x%02x'):format(value)
			end,
			uint16_t = function(value)
				return ('0x%04x'):format(value)
			end,
			uint32_t = function(value)
				return ('0x%08x'):format(value)
			end,
		}
	end,

	fields = {
		{name = 'name', type = 'unknown_000000', type = arrayType(uint8_t, -(0x000000 - 0x0051ba))},			-- 0x000000 - 0x0051ba

		{name = 'characterMenuImageOffsets', type = arrayType(uint16_t, 16)},									-- 0x0051ba - 0x0051da
		{name = 'characterMenuImageTileLayout', type = arrayType(uint8_t, 25)},									-- 0x0051da - 0x0051f3

		{name = 'unknown_0051f3', type = arrayType(uint8_t, -(0x0051f3 - 0x0091d5))}, 							-- 0x0051f3 - 0x0091d5

		{name = 'mapAnimPropOfs', type = arrayType(uint16_t, 0x15)},											-- 0x0091d5 - 0x0091ff = map animation properties pointer table (+0x0091ff) (only 10 used)
		{name = 'mapAnimProps', type = arrayType(MapAnimProps, 144)},											-- 0x0091ff - 0x00979f = map animation properties [144] @ 10 bytes each
		{name = 'mapAnimPropsLayer3Ofs', type = arrayType(uint16_t, 7)},										-- 0x00979f - 0x0097ad = map animation properties layer 3 pointer table (+0x0097ad) (only 6 used)
		{name = 'mapAnimPropsLayer3', type = arrayType(MapAnimPropsLayer3, 6)},									-- 0x0097ad - 0x009825 = map animation properties layer 3 [6]
		{name = 'mapPalAnim', type = arrayType(MapPalAnim, 20)},												-- 0x009825 - 0x00989d = map palette animation

		{name = 'unknown_00989d', type = arrayType(uint8_t, -(0x00989d - 0x00ce3a))},							-- 0x00989d - 0x00ce3a
		-- 0x00c27f-0x00c28f = something to do with battle background? -rpglegion

		-- offset of map character sprite parts
		-- interleaved row-major, 2x3
		{name = 'characterFrameTileOffsets', type = arrayType(uint16_t, numCharacterSpriteFrames * 6)},			-- 0x00ce3a - 0x00d026

		{name = 'unknown_00d026', type = arrayType(uint8_t, -(0x00d026 - 0x00d0f2))},							-- 0x00d026 - 0x00d0f2

		-- 0x00d0f2 - ? = pointer to map character graphics (2 bytes each)
		{name = 'characterSpriteOffsetLo', type = arrayType(uint16_t, numCharacterSprites)},					-- 0x00d0f2 - 0x00d23c

		{name = 'characterSpriteOffsetHiAndSize', type = arrayType(CharHiAndSize, numCharacterSprites)},		-- 0x00d23c - 0x00d386

		-- 0x00d23c - ? = bank pointer & # bytes to copy for map char gfx (2 bytes each)
		-- 0x00dfa0 - 0x00e0a0 = 'DTE table' -rgplegion
		{name = 'unknown_00d27c', type = arrayType(uint8_t, -(0x00d386 - 0x02ce2b))},							-- 0x00d386 - 0x02ce2b

		-- battle character palette assignment (1 byte each)
		-- or actually last bit probably means something else since it's not indexable
		{name = 'characterPaletteIndexes', type = arrayType(uint8_t, numCharacterSprites)},						-- 0x02ce2b - 0x02ced0

		{name = 'unknown_02ced0', type = arrayType(uint8_t, -(0x02ced0 - 0x02d01a))},							-- 0x02ced0 - 0x02d01a

		{name = 'formationSizeOffsets', type = arrayType(uint16_t, numFormationSizeOffsets)},					-- 0x02d01a - 0x02d034 = offset by +0x020000 into formationSize[]
		{name = 'formationSizes', type = arrayType(FormationSize, 48)},											-- 0x02d034 - 0x02d0f4

		-- 0x036f00 - ? = menu portrait palette assignment (1 byte each)
		-- 0x036f1b - ? = pointer to menu portrait graphics (2 bytes each)
		{name = 'unknown_02d0f4', type = arrayType(uint8_t, -(0x02d0f4 - 0x03c00e))},							-- 0x02d0f4 - 0x03c00e

		{name = 'positionedTextOffsets', type = arrayType(uint16_t, numPositionedText)},						-- 0x03c00e - 0x03c018

		{name = 'unknown_03c018', type = arrayType(uint8_t, -(0x03c018 - 0x03c2fc))},							-- 0x03c018 - 0x03c2fc

		{name = 'positionedTextBase', type = arrayType(uint8_t, -(0x03c2fc - 0x03c406))},						-- 0x03c2fc - 0x03c406

		-- 0x03c326 - 0x03c406 = more positioned text (N items, var length) ... where are the offsets for this?
		-- "P}BUY  SELL  EXITA:}GPAr GPAz}Owned:Az Equipped:AP Bat PwrAP DefenseAl â¦Af{Hi! Can I help you?Af{Help yourself!Af{How many?Af{Whatcha got?Af{How many?Af{Bye!          Af{You need more GP!Af{Too many!       Af{One's plenty! A"
		{name = 'unknown_03c406', type = arrayType(uint8_t, -(0x03c406 - 0x040000))},							-- 0x03c406 - 0x040000

		{name = 'touchTriggerOfs', type = arrayType(uint16_t, (0x040342 - 0x040000)/2)},						-- 0x040000 - 0x040342 = offset by +0x040000
		{name = 'touchTriggers', type = arrayType(TouchTrigger, 0x48f)},										-- 0x040342 - 0x041a0d = map event triggers (5 bytes each)

		{name = 'padding_041a0d', type = arrayType(uint8_t, -(0x041a0d - 0x041a10))},							-- 0x041a0d - 0x041a10

		{name = 'npcOfs', type = arrayType(uint16_t, -(0x041a10 - 0x041d52)/2)},								-- 0x041a10 - 0x041d52 = npc offsets (+0x041a10)
		{name = 'npcs', type = arrayType(NPC, 0x891)},															-- 0x041d52 - 0x046a6b = npc data
		{name = 'unused_046a6b', type = arrayType(uint8_t, -(0x046a6b - 0x046ac0))},							-- 0x046a6b - 0x046ac0 = unused
		{name = 'spells', type = arrayType(Spell, 0x100)},														-- 0x046ac0 - 0x0478c0
		{name = 'characterNames', type = arrayType(CharacterName, numCharacters)},								-- 0x0478c0 - 0x047a40
		{name = 'blitzData', type = arrayType(Raw12, numBlitzes)},												-- 0x047a40 - 0x047aa0
		{name = 'gauInitialRages', type = arrayType(uint8_t, -(0x047aa0 - 0x047ac0))},							-- 0x047aa0 - 0x047ac0 .. TODO? index into what?
		{name = 'shops', type = arrayType(Shop, 0x80)},															-- 0x047ac0 - 0x047f40
		{name = 'metamorphSets', type = arrayType(ItemRef4, 0x1a)},												-- 0x047f40 - 0x047fa8
		{name = 'padding_047fa8', type = arrayType(uint8_t, 0x18)},												-- 0x047fa8 - 0x047fc0 = 00's.  just like font intro is.  is this just 24 more bytes of font data?
		{name = 'font', type = arrayType(uint8_t, 0x10 * 0x100)},												-- 0x047fc0 - 0x048fc0 -- font graphics (8x8x2bpp, 16 bytes each, 0x00-0xff) ... the first half is blank
		{name = 'font16_widths', type = arrayType(uint8_t, 0x80)},												-- 0x048fc0 - 0x049040 -- font character cell widths (0x00-0x7f)

		{name = 'unknown_049040', type = arrayType(uint8_t, -(0x049040 - 0x0490c0))},							-- 0x049040 - 0x0490c0.  mostly filled with '0c's.  mostly.  so is the data after it, and a bit before, so maybe it goes with this?

		{name = 'font16_20_to_7f', type = arrayType(uint8_t, 22 * (0x7f - 0x20 + 1))},							-- 0x0490c0 - 0x049900 (or 0x04a4c0) -- font graphics data (16x11x1, 22 bytes each, 0x20-0x7f)

		-- 0x049bc0-0x049bd0 = 16 bytes of something
		-- 0x04ba00-0x04c007 = Ending Font (compressed)
		-- 0x04c008-0x04f476 = Ending BG Graphics and Tile Formation (compressed)
		-- 0x04f477-0x04f6fa = Ending Sprite Graphics (compressed)
		-- 0x04f6fb-0x04ffff = Ending Sprite Graphics (compressed)
		{name = 'unknown_049900', type = arrayType(uint8_t, -(0x049900 - 0x05070e))},							-- 0x049900 - 0x05070e

		{name = 'spcMainCodeLoopLen', type = uint16_t},															-- 0x05070e - 0x050710 -- length of main SPC code loop
		{name = 'spcMainCode', type = arrayType(uint8_t, -(0x050710 - 0x051ec7))},								-- 0x050710 - 0x051ec7 -- main SPC code loop

		{name = 'unknown_051ec7', type = arrayType(uint8_t, -(0x051ec7 - 0x053c5f))},							-- 0x051ec7 - 0x053c5f

		{name = 'brrSamplePtrs', type = arrayType(uint24_t, numBRRSamples)},									-- 0x053c5f - 0x053d1c -- BRR sample pointers (x63, 3 bytes each)
		{name = 'loopStartOfs', type = arrayType(uint16_t, numBRRSamples)},										-- 0x053d1c - 0x053d9a -- loop start pointers (x63, 2 bytes each)
		{name = 'pitchMults', type = arrayType(uint16_t, numBRRSamples)},										-- 0x053d9a - 0x053e18 -- pitch multipliers (x63, 2 bytes each)
		{name = 'adsrData', type = arrayType(uint16_t, numBRRSamples)},											-- 0x053e18 - 0x053e96 -- ADSR data (x63, 2 bytes each)

		{name = 'unknown_053e96', type = arrayType(uint8_t, -(0x053e96 - 0x054a35))},							-- 0x053e96 - 0x054a35

		{name = 'brrSamples', type = arrayType(uint8_t, -(0x054a35 - 0x085c7a))},								-- 0x054a35 - 0x085c7a -- BRR samples (does divide evenly by  3195 x63...)

		{name = 'unknown_085c7a', type = arrayType(uint8_t, -(0x085c7a - 0x09fe00))},							-- 0x085c7a - 0x09fe00

		{name = 'theEndGraphics2', type = arrayType(uint8_t, -(0x09fe00 - 0x09ff00))},							-- 0x09fe00 - 0x09ff00 = 4bpp
		{name = 'theEndPalette', type = Palette16_8},															-- 0x09ff00 - 0x0a0000
		{name = 'eventScript', type = arrayType(uint8_t, -(0x0a0000 - 0x0ce600))},								-- 0x0a0000 - 0x0ce600
		{name = 'dialogOffsets', type = arrayType(uint16_t, numDialogs)},										-- 0x0ce600 - 0x0d0000.  the first dialog offset points to the dialog which needs the bank byte to increment
		{name = 'dialogBase', type = arrayType(uint8_t, -(0x0d0000 - 0x0ef100))},								-- 0x0d0000 - 0x0ef100 ... hmm, there are dangling npc-event-scripts from 0x0d200 to 0x0de302 ... in the middle of dialogBase
		{name = 'mapNameBase', type = arrayType(uint8_t, -(0x0ef100 - 0x0ef600))},								-- 0x0ef100 - 0x0ef600

		-- 0x0ef600 - 0x0ef648 looks like offsets into something
		-- 0x0ef648 - 0x0ef678 looks like arbitrary values
		-- 0x0ef678 - 0x0efb60 is mostly '06' repeated
		{name = 'unknown_0ef600', type = arrayType(uint8_t, -(0x0ef600 - 0x0efb60))},							-- 0x0ef600 - 0x0efb60

		{name = 'rareItemDescOffsets', type = arrayType(uint16_t, numRareItems)},								-- 0x0efb60 - 0x0efb88
		{name = 'padding_0efb88', type = arrayType(uint8_t, 0x18)},												-- 0x0efb88 - 0x0efba0 -- all 'ff' repeated ... enough for 12 extra offsets ... there are 20 rare items ... 20+12=32
		{name = 'rareItemNames', type = arrayType(RareItemName, numRareItems)},									-- 0x0efba0 - 0x0efca4 -- rare item names are 13 chars
		{name = 'padding_0efca4', type = arrayType(uint8_t, 0xc)},												-- 0x0efca4 - 0x0efcb0 -- all 'ff' repeated, for 12 bytes, not quite 1 more name
		{name = 'rareItemDescBase', type = arrayType(uint8_t, -(0x0efcb0 - 0x0f0000))},							-- 0x0efcb0 - 0x0f0000
		{name = 'monsters', type = arrayType(Monster, numMonsters)},											-- 0x0f0000 - 0x0f3000
		{name = 'monsterItems', type = arrayType(MonsterItem, numMonsters)},									-- 0x0f3000 - 0x0f3600

		-- 0x0f3600 - 0x0f37c0 is mostly zeroes
		-- 0x0f37c0 - 0x0f3940 is something
		{name = 'unknown_0f3600', type = arrayType(uint8_t, -(0x0f3600 - 0x0f3940))},							-- 0x0f3600 - 0x0f3940

		{name = 'esperDescBase', type = arrayType(uint8_t, -(0x0f3940 - 0x0f3c40))},							-- 0x0f3940 - 0x0f3c40
		{name = 'swordTechNames', type = arrayType(SwordTechName, numSwordTechs)},								-- 0x0f3c40 - 0x0f3ca0
		{name = 'padding_0f3ca0', type = arrayType(uint8_t, 0x60)},												-- 0x0f3ca0 - 0x0f3d00 -- all 'ff' repeated
		{name = 'monsterSpells', type = arrayType(SpellRef4, numMonsters)},										-- 0x0f3d00 - 0x0f4300
		{name = 'monsterSketches', type = arrayType(SpellRef2, numMonsters)},									-- 0x0f4300 - 0x0f4600
		{name = 'monsterRages', type = arrayType(SpellRef2, 0x100)},											-- 0x0f4600 - 0x0f4800
		{name = 'monsterRandomBattles', type = arrayType(MonsterRandomBattleEntry4, 0x100)},					-- 0x0f4800 - 0x0f5000
		{name = 'monsterEventBattles', type = arrayType(MonsterRandomBattleEntry2, 0x100)},						-- 0x0f5000 - 0x0f5400
		{name = 'worldSectorRandomBattlesPerTerrain', type = arrayType(WorldSectorRandomBattlesPerTerrain, 0x80)},-- 0x0f5400 - 0x0f5600 = [world][sectorx][sectory]  ... 64 sectors (32x32 chunks of 256x256 world map) per WoB, 64 for WoR
		{name = 'mapRandomBattleOptions', type = arrayType(uint8_t, 0x200)},									-- 0x0f5600 - 0x0f5800 = one per map, index into monsterRandomBattles
		{name = 'worldSectorRandomBattleEncounterRatesPerTerrain', type = arrayType(uint8_t, 0x80)},			-- 0x0f5800 - 0x0f5880 = 2 bits used ... 64 sectors per WoB, 64 per WoR ... 8 items per sector, 2bpp each ( https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:rom_map )
		{name = 'mapBattleProbability', type = arrayType(uint8_t, 0x80)},										-- 0x0f5880 - 0x0f5900 = 2 bits used
		{name = 'formation2s', type = arrayType(Formation2, numFormations)},									-- 0x0f5900 - 0x0f6200
		{name = 'formations', type = arrayType(Formation, numFormations)},										-- 0x0f6200 - 0x0f83c0
		{name = 'padding_0f83c0', type = arrayType(uint8_t, -(0x0f83c0 - 0x0f8400))},							-- 0x0f83c0 - 0x0f8400 - all 'ff' repeated.  probably 4 last empty formations + padding

		{name = 'unknown_0f8400', type = arrayType(uint8_t, -(0x0f8400 - 0x0f8700))},							-- 0x0f8400 - 0x0f8700

		{name = 'monsterScripts', type = arrayType(uint8_t, -(0x0f8700 - 0x0fc050))},							-- 0x0f8700 - 0x0fc050
		{name = 'monsterNames', type = arrayType(MonsterName, numMonsters)},									-- 0x0fc050 - 0x0fcf50
		{name = 'monsterNameThing', type = arrayType(uint8_t, numMonsters)},									-- 0x0fcf50 - 0x0fd0d0
		{name = 'monsterAttackNames', type = arrayType(MonsterName, numMonsters)},								-- 0x0fd0d0 - 0x0fdfd0
		{name = 'padding_0fdfd0', type = arrayType(uint8_t, 0x10)},												-- 0x0fdfd0 - 0x0fdfe0 = 'ff's
		{name = 'battleDialogOffsets', type = arrayType(uint16_t, numBattleDialogs)},							-- 0x0fdfe0 - 0x0fe1e0
		{name = 'battleDialogBase', type = arrayType(uint8_t, -(0x0fe1e0 - 0x0ff450))},							-- 0x0fe1e0 - 0x0ff450

		{name = 'unknown_0ff450', type = arrayType(uint8_t, -(0x0ff450 - 0x0ffc00))},							-- 0x0ff450 - 0x0ffc00

		{name = 'blitzDescBase', type = arrayType(uint8_t, -(0x0ffc00 - 0x0ffd00))},							-- 0x0ffc00 - 0x0ffd00
		{name = 'swordTechDescBase', type = arrayType(uint8_t, -(0x0ffd00 - 0x0ffe00))},						-- 0x0ffd00 - 0xfffe00

		{name = 'unknown_0ffe00', type = arrayType(uint8_t, -(0x0ffe00 - 0x0ffe40))},							-- 0x0ffe00 - 0x0ffe40

		{name = 'esperDescOffsets', type = arrayType(uint16_t, numEspers)},										-- 0x0ffe40 - 0x0ffe76
		{name = 'padding_0ffe76', type = arrayType(uint8_t, 0xa)},												-- 0x0ffe76 - 0x0ffe80 = 026d repeated, probably the final offset.

		{name = 'unknown_0ffe80', type = arrayType(uint8_t, -(0x0ffe80 - 0x0ffeae))},							-- 0x0ffe76 - 0x0ffeae

		{name = 'esperBonusDescs', type = arrayType(EsperBonusDesc, numEsperBonuses)},							-- 0x0ffeae - 0x0fff47
		{name = 'padding_0fff47', type = arrayType(uint8_t, 87)},												-- 0x0fff47 - 0x0fff9e = 'ff's
		{name = 'blitzDescOffsets', type = arrayType(uint16_t, numBlitzes)},									-- 0x0fff9e - 0x0fffae
		{name = 'swordTechDescOffsets', type = arrayType(uint16_t, numSwordTechs)},								-- 0x0fffae - 0x0fffbe
		{name = 'battleAnimScripts', type = arrayType(uint8_t, -(0x0fffbe - 0x107fb2))},						-- 0x0fffbe - 0x107fb2 <- indexed into with battleAnimScriptOffsets[i] + 0x100000
		{name = 'battleAnimSets', type = arrayType(BattleAnimSet, 444)},										-- 0x107fb2 - 0x1097fa
		{name = 'padding_1097fa', type = arrayType(uint8_t, -(0x1097fa - 0x109800))},							-- 0x1097fa - 0x109800 = 'ff's, just like the end of battleAnimSets

		{name = 'unknown_109800', type = arrayType(uint8_t, -(0x109800 - 0x10d000))},							-- 0x109800 - 0x10d000

		{name = 'battleDialog2Offsets', type = arrayType(uint16_t, numBattleDialog2s)},							-- 0x10d000 - 0x10d200
		{name = 'battleDialog2Base', type = arrayType(uint8_t, -(0x10d200 - 0x10fd00))},						-- 0x10d200 - 0x10fd00

		{name = 'unknown_10fd00', type = arrayType(uint8_t, -(0x10fd00 - 0x110141))},							-- 0x10fd00 - 0x110141

		{name = 'battleAnimFrame16x16Tiles', type = arrayType(BattleAnim16x16Tile, 0x74cb)},					-- 0x110141 - 0x11ead7 ... 2 bytes each ... pointers from battleAnimFrame16x16TileOffsets offset by 0x110000 but point into here
		{name = 'padding_11ead7', type = uint8_t},																-- 0x11ead7 - 0x11ead8 -- 'ff'
		{name = 'battleAnimScriptOffsets', type = arrayType(uint16_t, 660)},									-- 0x11ead8 - 0x11f000 ... uint16 offsets +0x100000 ... maybe there are only 650 of these to match with `countof(battleAnimEffects)`?
		{name = 'battleMessageBase', type = arrayType(uint8_t, -(0x11f000 - 0x11f7a0))},						-- 0x11f000 - 0x11f7a0
		{name = 'battleMessageOffsets', type = arrayType(uint16_t, numBattleMessages)},							-- 0x11f7a0 - 0x11f9a0

		{name = 'unknown_11f9a0', type = arrayType(uint8_t, -(0x11f9a0 - 0x120000))},							-- 0x11f9a0 - 0x120000

		{name = 'battleAnimGraphicsSets3bpp', type = arrayType(BattleAnim8x8Tile, 0x20 * 0x180)},				-- 0x120000 - 0x126000 - holds the 'graphicSet' uint16 offsets from BattleAnimEffect * (0x20 entries == 0x40 bytes)
		{name = 'battleAnimPalettes', type = arrayType(Palette8, 0xf0)},										-- 0x126000 - 0x126f00
		{name = 'itemTypeNames', type = arrayType(Str7, 0x20)},													-- 0x126f00 - 0x126fe0
		{name = 'padding_126fe0', type = arrayType(uint8_t, 0x20)},												-- 0x126fe0 - 0x127000 = 'ff's
		{name = 'monsterSprites', type = arrayType(MonsterSprite, 0x1a0)},										-- 0x127000 - 0x127820
		{name = 'monsterPalettes', type = arrayType(Palette8, 0x300)},											-- 0x127820 - 0x12a820
		{name = 'monsterSpriteTileMask8Ofs', type = uint16_t},													-- 0x12a820 - 0x12a822
		{name = 'monsterSpriteTileMask16Ofs', type = uint16_t},													-- 0x12a822 - 0x12a824
		{name = 'monsterSpriteTileMaskData', type = arrayType(uint8_t, 0x12b300 - 0x12a824 )},					-- 0x12a824 - 0x12b300
		{name = 'itemNames', type = arrayType(Str13, numItems)},												-- 0x12b300 - 0x12c000
		{name = 'battleAnimGraphicsSets2bpp', type = arrayType(BattleAnim8x8Tile, 0x20 * 0xb0)},				-- 0x12c000 - 0x12ec00	-- should be 2bpp battle animation 16x16-tile-info referenced by .graphicSet
		{name = 'WoBPalettes', type = Palette16_8},																-- 0x12ec00 - 0x12ed00
		{name = 'WoRPalettes', type = Palette16_8},																-- 0x12ed00 - 0x12ee00
		{name = 'setzerAirshipPalette', type = Palette16_8},													-- 0x12ee00 - 0x12ef00
		{name = 'darylAirshipPalette', type = Palette16_8},														-- 0x12ef00 - 0x12f000

		{name = 'unknown_12f000', type = arrayType(uint8_t, -(0x12f000 - 0x130000))},							-- 0x12f000 - 0x130000

		{name = 'battleAnimGraphics3bpp', type = arrayType(uint8_t, -(0x130000 - 0x14c998))},					-- 0x130000 - 0x14c998 ... 3bpp, so 4881 (= 3 x 1627 ?) tiles
		{name = 'padding_14c998', type = arrayType(uint8_t, -(0x14c998 - 0x14ca00))},							-- 0x14c998 - 0x14ca00 = 'ff's

		{name = 'unknown_14ca00', type = arrayType(uint8_t, -(0x14ca00 - 0x14d000))},							-- 0x14ca00 - 0x14d000

		{name = 'battleAnimEffects', type = arrayType(BattleAnimEffect, 650)},									-- 0x14d000 - 0x14df3c
		{name = 'battleAnimFrame16x16TileOffsets', type = arrayType(uint16_t, 4194)},							-- 0x14df3c - 0x150000	-- +0x110000 ... really just 2949 that are valid.  each is a uint16_t, add to 0x110000 to get the start of the variable-length BattleAnim16x16Tile list into battleAnimFrame16x16Tiles

		{name = 'fieldSpriteGraphics', type = arrayType(uint8_t, -(0x150000 - 0x185000))},						-- 0x150000 - 0x185000 = character images, 0x16a0 bytes each

		{name = 'items', type = arrayType(Item, numItems)},														-- 0x185000 - 0x186e00
		{name = 'espers', type = arrayType(Esper, numEspers)},													-- 0x186e00 - 0x186f29
		{name = 'padding_186f29', type = arrayType(uint8_t, -(0x186f29 - 0x187000))},							-- 0x186f29 - 0x187000 = 'ff's
		{name = 'battleAnimGraphics2bpp', type = arrayType(uint8_t, -(0x187000 - 0x18c9a0))},					-- 0x187000 - 0x18c9a0	-- 2bpp, so 1434 tiles
		{name = 'spellDescBase', type = arrayType(uint8_t, -(0x18c9a0 - 0x18cea0))},							-- 0x18c9a0 - 0x18cea0
		{name = 'menuNames', type = arrayType(MenuName, 0x20)},													-- 0x18cea0 - 0x18cf80
		{name = 'spellDescOffsets', type = arrayType(uint16_t, 54)},											-- 0x18cf80 - 0x18cfec
		{name = 'padding_18cfec', type = arrayType(uint8_t, -(0x18cfec - 0x18d000))},							-- 0x18cfec - 0x18d000 = 'ff's

		{name = 'unknown_18d000', type = arrayType(uint8_t, -(0x18d000 - 0x18e6ba))},							-- 0x18d000 - 0x18e6ba

		{name = 'SerpentTrenchPalettesCompressed', type = arrayType(uint8_t, -(0x18e6ba - 0x18e800))},			-- 0x18e6ba - 0x18e800

		-- 18e800 = something
		-- 18f000 = something
		{name = 'unknown_18e800', type = arrayType(uint8_t, -(0x18e800 - 0x19a800))},							-- 0x18e800 - 0x19a800

		{name = 'mapTilePropsCompressed', type = arrayType(uint8_t, -(0x19a800 - 0x19cd10))},					-- 0x19a800 - 0x19cd10 = map tile properties (compressed)
		{name = 'mapTilePropsOffsets', type = arrayType(uint16_t, 0x2a)},										-- 0x19cd10 - 0x19cd60 = offsets to map tile properties (+0x19a800) into mapTilePropsCompressed ... 0x40 but only 0x29 point to valid compressed data
		{name = 'unused_19cd62', type = arrayType(uint16_t, 0x16)},												-- 0x19cd60 - 0x19cd90
		{name = 'mapLayoutOffsets', type = arrayType(uint24_t, 0x160)},											-- 0x19cd90 - 0x19d1b0 = offsets to map data (352 items), (+0x19d1b0)
		{name = 'mapLayoutsCompressed', type = arrayType(uint8_t, -(0x19d1b0 - 0x1e0000))},						-- 0x19d1b0 - 0x1e0000 = map data (compressed)
		{name = 'mapTilesetsCompressed', type = arrayType(uint8_t, -(0x1e0000 - 0x1fb400))},					-- 0x1e0000 - 0x1fb400 = map tile formation (compressed)
		{name = 'formationMPs', type = arrayType(uint8_t, 0x200)},												-- 0x1fb400 - 0x1fb600
		{name = 'itemColosseumInfos', type = arrayType(ItemColosseumInfo, numItems)},							-- 0x1fb600 - 0x1fba00
		{name = 'mapTilesetOffsets', type = arrayType(uint24_t, 0x4b)},											-- 0x1fba00 - 0x1fbaff -- 24bit, offset by +0x1e0000, points into mapTilesetsCompressed ... last points to invalid data so I cut it off.
		{name = 'padding_1fbaff', type = arrayType(uint8_t, 31)},												-- 0x1fbaff - 0x1fbb00
		{name = 'doorsOfs', type = arrayType(uint16_t, 0x201)},													-- 0x1fbb00 - 0x1fbf02 -- offset by +0x1fbb00
		{name = 'doors', type = arrayType(Door, 0x469)},														-- 0x1fbf02 - 0x1fd978 = Door[] (only 415 used?)
		{name = 'padding_1fd978', type = arrayType(uint8_t, 136)},												-- 0x1fd978 - 0x1fda00 = 'ff's
		{name = 'mapTileGraphicsOffsets', type = arrayType(uint24_t, 0x52)},									-- 0x1fda00 - 0x1fdaf6 = town tile graphics pointers (+0x1fdb00), points into mapTileGraphics
		{name = 'padding_1fdaf6', type = arrayType(uint8_t, 10)},												-- 0x1fdaf6 - 0x1fdb00
		{name = 'mapTileGraphics', type = arrayType(uint8_t, -(0x1fdb00 - 0x25f400))},							-- 0x1fdb00 - 0x25f400 = map tile graphics for layers 1&2, 4bpp
		{name = 'unknown_25f400', type = arrayType(uint8_t, -(0x25f400 - 0x260000))},							-- 0x25f400 - 0x260000 -- there's one battle bg in here
		{name = 'mapAnimGraphics', type = arrayType(uint8_t, -(0x260000 - 0x268000))},							-- 0x260000 - 0x268000 = 4bpp
		{name = 'characterPalettes', type = arrayType(Palette16, numCharacterPalettes)},						-- 0x268000 - 0x268400	-- also town tile palettes?
		{name = 'mapNameOffsets', type = arrayType(uint16_t, numMapNames)},										-- 0x268400 - 0x268780
		{name = 'mapTileGraphicsLayer3', type = arrayType(uint8_t, -(0x268780 - 0x26cd60))},					-- 0x268780 - 0x26cd60  map tile garphics for layer 3, 2bpp
		{name = 'mapTileGraphicsLayer3Offsets', type = arrayType(uint24_t, 18)},								-- 0x26cd60 - 0x26cd96 = offset, +0x268780 .. there's 19, but only 18 point to valid compressed data ...
		{name = 'padding_26cd96', type = arrayType(uint8_t, 10)},												-- 0x26cd96 - 0x26cda0
		{name = 'mapAnimGraphicsLayer3Ofs', type = arrayType(uint24_t, 10)},									-- 0x26cda0 - 0x26cdbe = offset, +0x26cdc0 to mapAnimGraphicsLayer3.  has values [0]=0 thru [6]=0x23d8 (which is the end of mapAnimGraphicsLayer3), so there's 6 entries
		{name = 'padding_26cdbe', type = arrayType(uint8_t, 2)},												-- 0x26cdbe - 0x26cdc0
		{name = 'mapAnimGraphicsLayer3', type = arrayType(uint8_t, -(0x26cdc0 - 0x26f198))},					-- 0x26cdc0 - 0x26f198 = 2bpp, compressed
		{name = 'padding_26f198', type = arrayType(uint8_t, -(0x26f198 - 0x26f200))},							-- 0x26f198 - 0x26f200 = FF's, probably tail filler of mapAnimGraphicsLayer3

		{name = 'unknown_26f200', type = arrayType(uint8_t, -(0x26f200 - 0x26f4a0))},							-- 0x26f200 - 0x26f4a0.  filled from 0x26f200 - 0x26f440, then FF's from 0x0026f440 - 0x26f4a0

		{name = 'hpIncPerLevelUp', type = arrayType(uint8_t, numLevels)},										-- 0x26f4a0 - 0x26f502
		{name = 'mpIncPerLevelUp', type = arrayType(uint8_t, numLevels)},										-- 0x26f502 - 0x26f564
		{name = 'stragoInitialLores', type = arrayType(uint8_t, 3)},											-- 0x26f564 - 0x26f567 ... hmm TODO?
		{name = 'spellNames_0to53', type = arrayType(Str7, 54)}, 												-- 0x26f567 - 0x26f6e1
		{name = 'spellNames_54to80', type = arrayType(Str8, 27)},                             					-- 0x26f6e1 - 0x26f7b9
		{name = 'spellNames_81to255', type = arrayType(Str10, 175)},											-- 0x26f7b9 - 0x26fe8f
		{name = 'esperAttackNames', type = arrayType(Str10, numEspers)},										-- 0x26fe8f - 0x26ff9d
		{name = 'mogDanceNames', type = arrayType(MogDanceName, numMogDances)},									-- 0x26ff9d - 0x26fffd
		{name = 'padding_26fffd', type = arrayType(uint8_t, 3)},												-- 0x26fffd - 0x270000
		{name = 'battleBgProperties', type = arrayType(BattleBgProps, 56)},										-- 0x270000 - 0x270150 = 56*6
		{name = 'battleBgPalettes', type = arrayType(RGBA5551, 0xa80)},											-- 0x270150 - 0x271650 ... everything's says 56 or 96? max index is 0x34 = 52
		{name = 'battleBgGfxAddrs', type = arrayType(uint24_t, 0xa8)},											-- 0x271650 - 0x271848 = 75 used, the rest are 0's, most points into battleBgGfxCompressed
		{name = 'battleBgLayoutOffsets', type = arrayType(uint16_t, 0x70)},										-- 0x271848 - 0x271928 = +0x270000 .  49 are valid. invalid contain 0x1928.  points into battleBgLayoutCompressed
		{name = 'battleBgLayoutCompressed', type = arrayType(uint8_t, -(0x271928-0x27a9e7))},					-- 0x271928 - 0x27a9e7 = 32x32x4bpp
		{name = 'battleBgGfxCompressed', type = arrayType(uint8_t, -(0x27a9e7-0x296300))},						-- 0x27a9e7 - 0x296300 = 4bpp
		{name = 'theEndGraphics1', type = arrayType(uint8_t, -(0x296300 - 0x297000))},							-- 0x296300 - 0x297000 = 4bpp
		{name = 'monsterSpriteData', type = arrayType(uint8_t, -(0x297000 - 0x2d0000))},						-- 0x297000 - 0x2d0000 = monster graphics
		{name = 'menuImages', type = arrayType(uint8_t, -(0x2d0000 - 0x2d0e00))},								-- 0x2d0000 - 0x2d0e00 = menu images 0x200 = bg pattern, 0x180 = borders, so 0x380 total ... x8 per menu scheme

		{name = 'unknown_2d0e00', type = arrayType(uint8_t, -(0x2d0e00 - 0x2d1c00))},							-- 0x2d0e00 - 0x2d1c00

		{name = 'menuWindowPalettes', type = Palette16_8},														-- 0x2d1c00 - 0x2d1d00 = menu window palettes, x8, 16 colors each, 2 bytes per color
		-- TODO struct multi dim arrays ...
		--{name = 'characterMenuImages', type = 'uint8_t['..numMenuChars..'][5][5][8][4]'},						-- 0x2d1d00 - 0x2d5860 = character menu images [char][tx][ty][col][row] @ 4bpp
		{name = 'characterMenuImages', type = arrayType(uint8_t, numMenuChars * 5 * 5 * 8 * 4)},				-- 0x2d1d00 - 0x2d5860 = character menu images [char][tx][ty][col][row] @ 4bpp
		{name = 'menuPortraitPalette', type = arrayType(Palette16, numMenuChars)},								-- 0x2d5860 - 0x2d5ac0 = menu portrait palettes (16 colors each)
		{name = 'handCursorGraphics', type = arrayType(uint8_t, -(0x2d5ac0 - 0x2d62c0))},						-- 0x2d5ac0 - 0x2d62c0 ? = hand cursor graphics
		{name = 'battleWhitePalette', type = Palette4},															-- 0x2d62c0 - 0x2d62c8 = battle standard (white) text palette, 4 colors
		{name = 'battleGrayPalette', type = Palette4},															-- 0x2d62c8 - 0x2d62d0 = battle disabled (grey) text palette, 4 colors
		{name = 'battleYellowPalette', type = Palette4},														-- 0x2d62d0 - 0x2d62d8 = battle active (yellow) text palette, 4 colors
		{name = 'battleBluePalette', type = Palette4},															-- 0x2d62d8 - 0x2d62e0 = battle blue text palette, 4 colors
		{name = 'battleEmptyPalette', type = Palette4},															-- 0x2d62e0 - 0x2d62e8 = empty color palette, 4 colors
		{name = 'battleGrayPalette', type = Palette4},															-- 0x2d62e8 - 0x2d62f0 = battle gauge (grey) text palette, 4 colors
		{name = 'battleGreenPalette', type = Palette4},															-- 0x2d62f0 - 0x2d62f8 = battle green text palette, 4 colors
		{name = 'battleRedPalette', type = Palette4},															-- 0x2d62f8 - 0x2d6300 = battle red text palette, 4 colors
		{name = 'battleMenuPalettes', type = Palette16_8},														-- 0x2d6300 - 0x2d6400 = battle/menu character sprite palettes, 8 palettes, 16 colors each
		{name = 'itemDescBase', type = arrayType(uint8_t, -(0x2d6400 - 0x2d77a0))},								-- 0x2d6400 - 0x2d77a0
		{name = 'loreDescBase', type = arrayType(uint8_t, -(0x2d77a0 - 0x2d7a70))},								-- 0x2d77a0 - 0x2d7a70
		{name = 'loreDescOffsets', type = arrayType(uint16_t, numLores)},										-- 0x2d7a70 - 0x2d7aa0
		{name = 'itemDescOffsets', type = arrayType(uint16_t, numItems)},										-- 0x2d7aa0 - 0x2d7ca0
		{name = 'characters', type = arrayType(Character, numCharacters)},										-- 0x2d7ca0 - 0x2d8220
		{name = 'expForLevelUp', type = arrayType(uint16_t, 106)},												-- 0x2d8220 - 0x2d82f4
		{name = 'treasureOfs', type = arrayType(uint16_t, 0x1a0)},												-- 0x2d82f4 - 0x2d8634 	-- offset +0x2d8634 into treasures
		{name = 'treasures', type = arrayType(Treasure, 0x11e)},												-- 0x2d8634 - 0x2d8bca
		{name = 'padding_2d8bca', type = arrayType(uint8_t, -(0x2d8bca - 0x2d8e5b))},							-- 0x2d8bca - 0x2d8e5b = 'ff's
		{name = 'battleBgDance', type = arrayType(uint8_t, 0x40)},												-- 0x2d8e5b - 0x2d8e9b
		{name = 'padding_2d8e9b', type = arrayType(uint8_t, -(0x2d8e9b - 0x2d8f00))},							-- 0x2d8e9b - 0x2d8f00 = 'ff's
		{name = 'maps', type = arrayType(Map, 0x19f)},															-- 0x2d8f00 - 0x2dc47f
		{name = 'padding_2dc47f', type = arrayType(uint8_t, 1)},												-- 0x2dc47f - 0x2df480
		{name = 'mapPalettes', type = arrayType(Palette16_8, 48)},												-- 0x2dc480 - 0x2df480 = map palettes (48 elements, 16x8 colors each)
		{name = 'bigDoorsOfs', type = arrayType(uint16_t, 0x201)},												-- 0x2df480 - 0x2df882
		{name = 'bigDoors', type = arrayType(BigDoor, 0x98)},													-- 0x2df882 - 0x2dfcaa
		{name = 'padding_2dfcaa', type = arrayType(uint8_t, -(0x2dfcaa - 0x2dfe00))},							-- 0x2dfcaa - 0x2dfe00 = 'ff's
		{name = 'longEsperBonusDescBase', type = arrayType(uint8_t, -(0x2dfe00 - 0x2dffd0))},					-- 0x2dfe00 - 0x2dffd0
		{name = 'longEsperBonusDescOffsets', type = arrayType(uint16_t, numEsperBonuses)},						-- 0x2dffd0 - 0x2dfff2

		-- 0x2e4842 - 0x2e4851     Sprites used for various positions of map character
		{name = 'unknown_2dfff2', type = arrayType(uint8_t, -(0x2dfff2 - 0x2e9b14))},

		{name = 'WoBTileProps', type = arrayType(WorldTileProps, 0x100)},										-- 0x2e9b14 - 0x2e9d14
		{name = 'WoRTileProps', type = arrayType(WorldTileProps, 0x100)},										-- 0x2e9d14 - 0x2e9f14

		{name = 'unknown_2e9f14', type = arrayType(uint8_t, -(0x2e9f14 - 0x2ed434))},							-- 0x2e9f14 - 0x2ed434 = looks like more world tile props.

		{name = 'WoBLayoutCompressed', type = arrayType(uint8_t, -(0x2ed434 - 0x2f114f))},						-- 0x2ed434 - 0x2f114f     World of Balance Map Data (compressed)
		{name = 'WoBGfxDataCompressed', type = arrayType(uint8_t, -(0x2f114f - 0x2f3250))},						-- 0x2f114f - 0x2f3250     World of Balance Tile Graphics (compressed)

		{name = 'unknown_2f3250', type = arrayType(uint8_t, -(0x2f3250 - 0x2f4a46))},							-- 0x2f3250 - 0x2f4a46 ... around 0x1800 bytes of *something* ...

		{name = 'WoRGfxDataCompressed', type = arrayType(uint8_t, -(0x2f4a46 - 0x2f6a56))},						-- 0x2f4a46 - 0x2f6a56
		{name = 'WoRLayoutCompressed', type = arrayType(uint8_t, -(0x2f6a56 - 0x2f9d17))},						-- 0x2f6a56 - 0x2f9d17
		{name = 'SerpentTrenchLayoutCompressed', type = arrayType(uint8_t, -(0x2f9d17 - 0x2fb631))},			-- 0x2f9d17 - 0x2fb631
		{name = 'SerpentTrenchGfxDataCompressed', type = arrayType(uint8_t, -(0x2fb631 - 0x2fc624))},			-- 0x2fb631 - 0x2fc624

		-- still something else at the end
		-- 0x2fce77 - 0x2fce97 = vector approach palette

		-- - 0x300000
	},
}
local function assertOffset(name, addr)
	assert.eq(ffi.offsetof(Game, name), addr, name)
end

assertOffset('characterFrameTileOffsets', 0x00ce3a)
assertOffset('characterSpriteOffsetLo', 0x00d0f2)
assertOffset('characterSpriteOffsetHiAndSize', 0x00d23c)
assertOffset('characterPaletteIndexes', 0x02ce2b)
assertOffset('formationSizeOffsets', 0x02d01a)
assertOffset('formationSizes', 0x02d034)
assertOffset('positionedTextOffsets', 0x03c00e)
assertOffset('positionedTextBase', 0x03c2fc)
assertOffset('spells', 0x046ac0)
assertOffset('characterNames', 0x0478c0)
assertOffset('shops', 0x047ac0)
assertOffset('metamorphSets', 0x047f40)
assertOffset('font', 0x047fc0)
assertOffset('font16_widths', 0x048fc0)
assertOffset('font16_20_to_7f', 0x0490c0)
assertOffset('spcMainCodeLoopLen', 0x05070e)
assertOffset('spcMainCode', 0x050710)
assertOffset('brrSamplePtrs', 0x053c5f)
assertOffset('loopStartOfs', 0x053d1c)
assertOffset('pitchMults', 0x053d9a)
assertOffset('adsrData', 0x053e18)
assertOffset('brrSamples', 0x054a35)
assertOffset('dialogOffsets', 0x0ce600)
assertOffset('dialogBase', 0x0d0000)
assertOffset('mapNameBase', 0x0ef100)
assertOffset('rareItemDescOffsets', 0x0efb60)
assertOffset('rareItemNames', 0x0efba0)
assertOffset('rareItemDescBase', 0x0efcb0)
assertOffset('monsters', 0x0f0000)
assertOffset('monsterItems', 0x0f3000)
assertOffset('esperDescBase', 0x0f3940)
assertOffset('swordTechNames', 0x0f3c40)
assertOffset('monsterSpells', 0x0f3d00)
assertOffset('monsterSketches', 0x0f4300)
assertOffset('monsterRages', 0x0f4600)
assertOffset('monsterNames', 0x0fc050)
assertOffset('monsterAttackNames', monsterAttackNamesAddr)
assertOffset('blitzDescBase', 0x0ffc00)
assertOffset('swordTechDescBase', 0x0ffd00)
assertOffset('esperDescOffsets', 0x0ffe40)
assertOffset('esperBonusDescs', 0x0ffeae)
assertOffset('blitzDescOffsets', 0x0fff9e)
assertOffset('swordTechDescOffsets', 0x0fffae)
assertOffset('battleAnimScripts', 0x0fffbe)
assertOffset('battleAnimSets', 0x107fb2)
assertOffset('battleDialog2Offsets', 0x10d000)
assertOffset('battleAnimFrame16x16Tiles', 0x110141)
assertOffset('battleMessageOffsets', 0x11f7a0)
assertOffset('battleAnimGraphicsSets3bpp', 0x120000)
assertOffset('battleAnimPalettes', 0x126000)
assertOffset('itemTypeNames', 0x126f00)
assertOffset('monsterSprites', 0x127000)
assertOffset('monsterPalettes', 0x127820)
assertOffset('itemNames', 0x12b300)
assertOffset('WoBPalettes', 0x12ec00)
assertOffset('WoRPalettes', 0x12ed00)
assertOffset('setzerAirshipPalette', 0x12ee00)
assertOffset('darylAirshipPalette', 0x12ef00)
assertOffset('items', 0x185000)
assertOffset('espers', 0x186e00)
assertOffset('spellDescBase', 0x18c9a0)
assertOffset('menuNames', 0x018cea0)
assertOffset('spellDescOffsets', 0x18cf80)
assertOffset('formationMPs', 0x1fb400)
assertOffset('itemColosseumInfos', 0x1fb600)
assertOffset('mapTilesetOffsets', mapTilesetOfsAddr)
assertOffset('characterPalettes', 0x268000)
assertOffset('mapNameOffsets', 0x268400)
assertOffset('mapAnimGraphicsLayer3Ofs', 0x26cda0)
assertOffset('mapAnimGraphicsLayer3', 0x26cdc0)
assertOffset('hpIncPerLevelUp', 0x26f4a0)
assertOffset('mpIncPerLevelUp', 0x26f502)
assertOffset('spellNames_0to53', 0x26f567)
assertOffset('esperAttackNames', 0x26fe8f)
assertOffset('mogDanceNames', 0x26ff9d)
assertOffset('monsterSpriteData', 0x297000)
assertOffset('menuImages', 0x2d0000)
assertOffset('menuWindowPalettes', 0x2d1c00)
assertOffset('characterMenuImages', 0x2d1d00)
assertOffset('itemDescBase', 0x2d6400)
assertOffset('loreDescBase', 0x2d77a0)
assertOffset('loreDescOffsets', 0x2d7a70)
assertOffset('itemDescOffsets', 0x2d7aa0)
assertOffset('characters', 0x2d7ca0)
--assertOffset('expForLevelUp', expForLevelUpAddr)
assertOffset('longEsperBonusDescBase', 0x2dfe00)
assertOffset('longEsperBonusDescOffsets', 0x2dffd0)

gameC = ffi.cast(ptrType(Game), rom)

game = setmetatable({}, {
	__index = gameC,
})

game.rom = rom
game.romstr = romstr
game.romsize = romsize

game.numEsperBonuses = numEsperBonuses
game.numEspers = numEspers
game.numMonsters = numMonsters
game.numItems = numItems
game.numRareItems = numRareItems
game.numLevels = numLevels
game.numCharacters = numCharacters
game.numCharacterSpriteFrames = numCharacterSpriteFrames
game.numCharacterSprites = numCharacterSprites
game.numCharacterPalettes = numCharacterPalettes
game.numMogDances = numMogDances
game.numSwordTechs = numSwordTechs
game.numBlitzes = numBlitzes
game.numLores = numLores
game.numMapNames = numMapNames
game.numDialogs = numDialogs
game.numBattleDialogs = numBattleDialogs
game.numBattleDialog2s = numBattleDialog2s
game.numBattleMessages = numBattleMessages
game.numFormations = numFormations
game.numFormationSizeOffsets = numFormationSizeOffsets
game.numPositionedText = numPositionedText
game.numBRRSamples = numBRRSamples
game.numMenuChars = numMenuChars

game.findu8 = findu8
game.gamezstr = gamezstr
game.compzstr = compzstr
game.gamestr = gamestr
game.compstr = compstr
game.getSpellName = getSpellName
game.getEsperName = getEsperName

game.countof = countof

game.decompress = require 'ff6.decompress'

game.itemForName = {}
for i=0,numItems-1 do
	local name = tostring(gameC.itemNames[i])
	--if i < 231 then name = name:sub(2) end
	game.itemForName[name] = i
end

game.spellForName = {}
for i=0,numItems-1 do
	local name = getSpellName(i)
	--if i < 54 then name = name:sub(2) end
	game.spellForName[name] = i
end

game.mapNames = StringList{
	name = 'map names',
	data = gameC.mapNameBase,
	offsets = gameC.mapNameOffsets,
	compressed = true,
}



game.dialog = StringList{
	name = 'dialog',
	data = gameC.dialogBase,
	offsets = gameC.dialogOffsets,
	compressed = true,
}

game.battleDialog = StringList{
	name = 'battle dialog',
	data = gameC.battleDialogBase,
	offsets = gameC.battleDialogOffsets,
	addrBase = rom + 0x0f0000,
}

game.battleDialog2 = StringList{
	name = 'battle dialog2',
	data = gameC.battleDialog2Base,
	offsets = gameC.battleDialog2Offsets,
	addrBase = rom + 0x100000,
}

game.battleMessages = StringList{
	name = 'battle message',
	data = gameC.battleMessageBase,
	offsets = gameC.battleMessageOffsets,
	addrBase = rom + 0x110000,
}

game.positionedText = StringList{
	name = 'positioned text',
	data = gameC.positionedTextBase,
	offsets = gameC.positionedTextOffsets,
	addrBase = rom + 0x030000,
}


game.uint24_t = uint24_t
game.RGBA5551 = RGBA5551
game.Palette4 = Palette4
game.Palette8 = Palette8
game.Palette16 = Palette16
game.Palette16_8 = Palette16_8
game.Spell = Spell
game.SpellLearn = SpellLearn
game.SpellRef = SpellRef
game.SpellRef2 = SpellRef2
game.SpellRef4 = SpellRef4
game.Esper = Esper
game.EsperBonusDesc = EsperBonusDesc
game.EsperBonus = EsperBonus
game.FormationSize = FormationSize
game.MonsterSprite = MonsterSprite
game.Item = Item
game.ItemRef = ItemRef
game.ItemRef2 = ItemRef2
game.ItemRef4 = ItemRef4
game.ItemColosseumInfo = ItemColosseumInfo
game.RareItemName = RareItemName
game.MonsterName = MonsterName
game.Monster = Monster
game.MonsterItem = MonsterItem
game.Formation = Formation
game.Formation2 = Formation2
game.MenuName = MenuName
game.MenuNameRef4 = MenuNameRef4
game.CharacterName = CharacterName
game.Character = Character
game.SwordTechName = SwordTechName
game.CharHiAndSize = CharHiAndSize
game.ShopInfo = ShopInfo
game.Shop = Shop
game.Map = Map
game.MapTileProps = MapTileProps
game.MapAnimProps = MapAnimProps
game.MapAnimPropsLayer3 = MapAnimPropsLayer3
game.MapPalAnim = MapPalAnim
game.NPC = NPC
game.Door = Door 
game.BigDoor = BigDoor
game.TouchTrigger = TouchTrigger
game.WorldTileProps = WorldTileProps
game.BattleBgProps = BattleBgProps
game.Treasure = Treasure
game.BattleAnimSet = BattleAnimSet
game.BattleAnimEffect = BattleAnimEffect
game.BattleAnim16x16Tile = BattleAnim16x16Tile 
game.BattleAnim8x8Tile = BattleAnim8x8Tile
game.terrainTypes = terrainTypes
game.encounterNames = encounterNames
game.WorldSectorRandomBattlesPerTerrain = WorldSectorRandomBattlesPerTerrain
game.Game = Game

require 'ff6.maps'(game)

require 'ff6.script'(game)


--[[ 0xd1600
					-- Game Genie code:
rom[0xd1614] = 0x97	-- B5FF-8F79
rom[0xd1618] = 0xc1	-- AFFF-8479
rom[0xd1619] = 0x8f -- 6EFF-8459
					-- wait I used +0xC00000 ... didn't seem to work ... whats the correct offset for ROM GG codes?
print(compzstr(ffi.cast(uint8_t_p, rom+0xd1600)))
--[=[
test = table{0x2a, 0x24, 0x25, 0x2a, 0x20, 0x61, 0x7f, 0x20, 0x85, 0x46, 0x67, 0x83, 0x87, 0x3e, 0x63, 0x86, 0x32, 0x20, 0x2d, 0x23,
	0x97,--0x92,	-- 14: " o"
	0x94, 			-- 15: "n "
	0x46, 			-- 16: "m"
	0x52, 			-- 17: "y"
	0xc1,--0xa1,	-- 18: " b"
	0x8f,--0xf2,	-- 19: "oo"
	0x4d,			-- 1a: "t"
	0x4c,			-- 1b: "s"
	0x5e			-- 1c: "!"
}:mapi(function(ch) return string.char(ch) end):concat()
print(#test)
for i=0,#test-1 do
	print(number.hex(i), ('%q'):format(compstr(ffi.cast(uint8_t_p, test)+i, 1)))
end
print()
--]=]
os.exit()
--]]

return game

end
