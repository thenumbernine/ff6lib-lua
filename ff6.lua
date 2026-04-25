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


local rom = ffi.cast('uint8_t*', romstr)

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

local function compzstr(ptr)
	local pend = findu8(ptr, 0)
	return compstr(ptr, pend - ptr)
end

local function gamestrtype(args)
	local name = assert(args.name)
	local size = assert(args.size)
	ffi.cdef([[
typedef struct ]]..name..[[ {
	uint8_t ptr[]]..size..[[];
} ]]..name..[[;
]])
	assert.eq(ffi.sizeof(name), size)
	local metatype = ffi.metatype(name, {
		__tostring = function(self)
			return string.trim(gamestr(self.ptr, size))
		end,
		__concat = function(a,b) return tostring(a) .. tostring(b) end,
	})
	return metatype
end

local function rawtype(args)
	local name = assert(args.name)
	local size = assert(args.size)
	ffi.cdef([[
typedef struct ]]..name..[[ {
	uint8_t ptr[]]..size..[[];
} ]]..name..[[;
]])
	assert.eq(ffi.sizeof(name), size)
	local metatype = ffi.metatype(name, {
		__tostring = function(self)
			local s = table()
			for i=0,size-1 do
				s:insert(('%02x'):format(self.ptr[i]))
			end
			return s:concat' '
		end,
		__concat = function(a,b) return tostring(a) .. tostring(b) end,
	})
	return metatype
end

--[[
args:
	name
	options
	type (optional) default uint8_t
--]]
local function bitflagtype(args)
	local ctype = args.type or 'uint8_t'
	return ff6struct{
		name = assert(args.name),
		fields = table.mapi(assert(args.options), function(option)
			return {[assert(option)] = ctype..':1'}
		end),
	}
end

local element_t = bitflagtype{
	name = 'element_t',
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

local targetting_t = bitflagtype{
	name = 'targetting_t',
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

local effect1_t = bitflagtype{
	name = 'effect1_t',
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

local effect2_t = bitflagtype{
	name = 'effect2_t',
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

local effect3_t = bitflagtype{
	name = 'effect3_t',
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

local effect4_t = bitflagtype{
	name = 'effect4_t',
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
		name = 'str'..n..'_t',
		size = n,
	}
end

makefixedstr(6)
makefixedstr(7)
makefixedstr(8)
makefixedstr(9)
makefixedstr(10)
makefixedstr(12)
makefixedstr(13)

local madefixedraw = {}
local function makefixedraw(n)
	local cache = madefixedraw[n]
	if cache then return table.unpack(cache) end
	local name = 'raw'..n..'_t'
	local mt = rawtype{
		name = name,
		size = n,
	}
	madefixedraw[n] = {mt, name}
	return mt, name
end

makefixedraw(12)	-- raw12_t for uint8_t[12] for the blitzes

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

	self.numOffsets = tostring(ffi.typeof(self.offsets)):match'ctype<unsigned short %(&%)%[(%d+)%]>'

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
	local numPtr = tostring(ffi.typeof(self.data)):match'ctype<unsigned char %(&%)%[(%d+)%]>'
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

local color_t = ff6struct{
	name = 'color_t',
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
assert.eq(ffi.sizeof'color_t', 2)

local palette4_t = createVec{
	dim = 4,
	ctype = 'color_t',
	vectype = 'palette4_t',
}
assert.eq(ffi.sizeof'palette4_t', 2*4)

local palette8_t = createVec{
	dim = 8,
	ctype = 'color_t',
	vectype = 'palette8_t',
}
assert.eq(ffi.sizeof'palette8_t', 2*8)

local palette16_t = createVec{
	dim = 16,
	ctype = 'color_t',
	vectype = 'palette16_t',
}
assert.eq(ffi.sizeof'palette16_t', 2*16)

local palette16_8_t = createVec{
	dim = 8,
	ctype = 'palette16_t',
	vectype = 'palette16_8_t',
}
assert.eq(ffi.sizeof'palette16_8_t', 2*16*8)

local numMenuChars = 19

---------------- AUDIO ----------------

local uint24_t = ff6struct{
	name = 'uint24_t',
	fields = {
		{lo = 'uint16_t'},
		{hi = 'uint8_t'},
	},
	metatable = function(mt)
		mt.value = function(self)
			return bit.bor(self.lo, bit.lshift(self.hi, 16))
		end
	end,
}
assert.eq(ffi.sizeof'uint24_t', 3)
local numBRRSamples = 63

---------------- SPELLS ----------------

local numSpells = 0x100


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
local spellref_t = reftype{
	name = 'spellref_t',
	getter = function(i)
		if i == 0xff then return nil end
		return getSpellName(i)
	end,
	getterSkipNone = true,
}

local spell_t = ff6struct{
	name = 'spell_t',
	fields = {
		-- 00:
		{targetting = 'targetting_t'},
		-- 01:
		{elementDamage = 'element_t'},
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
		{mp = 'uint8_t'},
		-- 06:
		{power = 'uint8_t'},
		-- 07:
		{maybe_noDamage = 'uint8_t:1'},
		{mabye_hitBasedOnLevel = 'uint8_t:1'},
		{unused_7_2 = 'uint8_t:6'},
		-- 08:
		{hitChance = 'uint8_t'},
		-- 09:
		{specialEffect = 'uint8_t'},
		-- 0x0a:
		{givesEffect1 = 'effect1_t'},
		-- 0x0b:
		{givesEffect2 = 'effect2_t'},
		-- 0x0c:
		{givesEffect3 = 'effect3_t'},
		-- 0x0d:
		{givesEffect4 = 'effect4_t'},
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
assert.eq(ffi.sizeof'spell_t', 0x0e)

local spellsAddr = 0x046ac0
local spellDescOffsetsAddr = 0x18cf80
local spellDescBaseAddr = 0x18c9a0	-- spells 0-53

ffi.cdef[[typedef str9_t esperBonusDesc_t;]]
local numEsperBonuses = 17
local esperBonusDescsAddr = 0x0ffeae

local longEsperBonusDescOffsetsAddr = 0x2dffd0
local longEsperBonusDescBaseAddr = 0x2dfe00

-- another one that needs 'game'
local esperBonus_t = reftype{
	name = 'esperBonus_t',
	getter = function(i) return gameC.esperBonusDescs[i] end,
}

local esperAttackNamesAddr = 0x26fe8f
-- also needs a pointer to 'game'
local function getEsperName(i) return getSpellName(i + 54) end

local spellLearn_t = ff6struct{
	name = 'spellLearn_t',
	fields = {
		{rate = 'uint8_t'},
		{spell = 'spellref_t'},
	},
}

local esper_t = ff6struct{
	name = 'esper_t',
	fields = {
		{spellLearn1 = 'spellLearn_t'},
		{spellLearn2 = 'spellLearn_t'},
		{spellLearn3 = 'spellLearn_t'},
		{spellLearn4 = 'spellLearn_t'},
		{spellLearn5 = 'spellLearn_t'},
		{bonus = 'esperBonus_t'},
	},
}
assert.eq(ffi.sizeof'esper_t', 11)
local numEspers = 27
local espersAddr = 0x186e00

-- 0x0f3940 - 0x0f3c40
local esperDescBaseAddr = 0x0f3940

-- 0x0ffe40 - 0x0ffe76 = esper desc offsets
local esperDescOffsetsAddr = 0x0ffe40

--[[ can't do this until i convert all ff6struct to struct, then i can use anonymous fields.
local battleAnimEffectIndex_t = struct{
	name = 'battleAnimEffectIndex_t',
	union = true,
	fields = {
		{name='u16', type='uint16_t', no_iter=true},
		{type=struct{
			anonymous = true,
			fields = {
				{index = 'uint16_t:15'},
				{dontRunScript = 'uint16_t:1'},
			},
		}},
	},
}
assert.eq(ffi.sizeof(battleAnimEffectIndex_t), 2)
--]]

-- collections of up to 3 animation-effects to play
local battleAnimSet_t = ff6struct{
	name = 'battleAnimSet_t',
	fields = {
		--[[ TODO get struct to serialize member arrays
		{effect = 'uint16_t[3]'},	-- 0xffff = none, otherwise values are from 0-0x3fff, bit15 means something, idk.
		{palette = 'uint8_t[3]'},
		--]]
		-- [[

		-- [[ index into 'battleAnimEffects' table
		-- 0xffff = none
		-- bit 15 means something else I think
		{effect1 = 'uint16_t'},
		{effect2 = 'uint16_t'},
		{effect3 = 'uint16_t'},
		--]]
		--[[
		{effect1 = 'battleAnimEffectIndex_t'},
		{effect2 = 'battleAnimEffectIndex_t'},
		{effect3 = 'battleAnimEffectIndex_t'},
		--]]

		-- index into 'battleAnimPalettes' table
		{palette1 = 'uint8_t'},
		{palette2 = 'uint8_t'},
		{palette3 = 'uint8_t'},
		--]]
		{sound = 'uint8_t'},
		{unknown10 = 'uint8_t'},
		{unknown11 = 'uint16_t'},
		{wait = 'uint8_t'},
	},
}
assert.eq(ffi.sizeof'battleAnimSet_t', 0xe)
local numBattleAnimSets = 444

local numBattleAnimPalettes = 0xf0

-- animations made up of frames
local battleAnimEffect_t = ff6struct{
	name = 'battleAnimEffect_t',
	fields = {
		{numFrames = 'uint8_t:6'},
		{graphicSetHighBit = 'uint8_t:1'},	-- is this a separate bit?
		{_2bpp = 'uint8_t:1'},	-- use 3bpp(false) vs 2bpp(true)?

		-- for effect1&2, 0x120000 + graphicSet * 0x40, len = 0xA0
		-- for effect3, 0x12C000 + graphicSet * 0x40, len = 0x80
		{graphicSet = 'uint8_t'}, -- aka "chipset" aka "mold" (where does this point?)

		{frameIndexBase = 'uint16_t'}, -- the index into battleAnimFrame16x16TileOffsets

		-- size in 16x16 tiles:
		{width = 'uint8_t'},
		{height = 'uint8_t'},
	},
}
assert.eq(ffi.sizeof'battleAnimEffect_t', 6)
local numBattleAnimEffects = 650

local battleAnim16x16Tile_t = ff6struct{
	name = 'battleAnim16x16Tile_t',
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
assert.eq(ffi.sizeof'battleAnim16x16Tile_t', 2)

-- graphicSet is a collection of 0x40 (0x10 x 4) 8x8 tiles
-- Each 8x8 tile holds info of the tile address, hflip, and vflip
-- This is pointed to by battleAnimEffect_t.graphicSet
local battleAnim8x8Tile_t = ff6struct{
	name = 'battleAnim8x8Tile_t',
	fields = {
		-- * tileLen (8 * bpp) + tileAddrBase (0x187000 for 2bpp, 0x130000 for 3bpp) gives the 8x8 tile data
		-- for 3bpp, points into 0x130000 - 0x14c998, which only holds 4881
		{tile = 'uint16_t:14'},
		-- whether to hflip/vflip the 8x8 tile
		{hflip = 'uint16_t:1'},
		{vflip = 'uint16_t:1'},
	},
}
assert.eq(ffi.sizeof'battleAnim8x8Tile_t', 2)

---------------- MONSTERS HEADER ----------------

local numMonsters = 0x180

ffi.cdef[[typedef str10_t monsterName_t;]]
local monsterNamesAddr = 0x0fc050

-- This is a uint8_t even though there are 384 monsters.
-- If I find a uint16_t then I'll make reftype more flexible and make a second monsterRef16_t type.
local monsterRef_t = reftype{
	name = 'monsterRef_t',
	getter = function(i) return gameC.monsterNames[i] end,
}

local numFormations = 0x240
local numFormationMPs = 0x200

local formationAddr = 0xf6200		-- 576 in size
local formationMPAddr = 0x1fb400	-- 512 in size

local xy4b_t = ff6struct{
	name = 'xy4b_t',
	fields = {
		{x = 'uint8_t:4'},
		{y = 'uint8_t:4'},
	},
}
local xy4b_6_t = createVec{
	dim = 6,
	ctype = 'xy4b_t',
	vectype = 'xy4b_6_t',
}

local formation_t = ff6struct{
	name = 'formation_t',
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
		{monster1 = 'uint8_t'},
		-- 0x03
		{monster2 = 'uint8_t'},
		-- 0x04
		{monster3 = 'uint8_t'},
		-- 0x05
		{monster4 = 'uint8_t'},
		-- 0x06
		{monster5 = 'uint8_t'},
		-- 0x07
		{monster6 = 'uint8_t'},

		-- 0x08 - 0x0d
		--{positions = 'xy4b_6_t'},
		-- can't occlude if x,y are nested
		{pos1 = 'xy4b_t'},
		{pos2 = 'xy4b_t'},
		{pos3 = 'xy4b_t'},
		{pos4 = 'xy4b_t'},
		{pos5 = 'xy4b_t'},
		{pos6 = 'xy4b_t'},

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
			local formationSize = ffi.cast('formationSize_t*', rom + addr)
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
				local formationSize = ffi.cast('formationSize_t*', rom + 0x020000 + offset)
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
assert.eq(ffi.sizeof'formation_t', 0xf)

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

local formation2_t = ff6struct{
	name = 'formation2_t',
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
		{event = 'uint8_t'},
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
assert.eq(ffi.sizeof'formation2_t', 4)

local numFormationSizeOffsets = 13

-- this is an arbitrary number, just like null-term string Base field sizes,
-- because it is referenced by offsets
local numFormationSizes = 48

local formationSize_t = ff6struct{
	name = 'formationSize_t',
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
		{width = 'uint8_t'},
		{height = 'uint8_t'},
	},
}
assert.eq(ffi.sizeof'formationSize_t', 4)

ff6struct{
	name = 'monsterRandomBattleEntry_t',
	fields = {
		{formation = 'uint16_t:15'},			-- only 10 bits are used, since thats all you need to index numFormations = 0x240
		{chooseFromNextFour = 'uint16_t:1'},	-- only set for random battle #0x70: Behemoth, Ninja x2, {Brainpain x2, Misfit, Apokrypohs}, Dragon
	},
}
assert.eq(ffi.sizeof'monsterRandomBattleEntry_t', 2)

local monsterRandomBattleEntry2_t = createVec{
	dim = 2,
	ctype = 'monsterRandomBattleEntry_t',
	vectype = 'monsterRandomBattleEntry2_t',
}

local monsterRandomBattleEntry4_t = createVec{
	dim = 4,
	ctype = 'monsterRandomBattleEntry_t',
	vectype = 'monsterRandomBattleEntry4_t',
}

ff6struct{
	name = 'RandomBattlesPerTerrain_t',
	fields = {
		-- each is a lookup into monsterRandomBattles[]
		{grass = 'uint8_t'},
		{forest = 'uint8_t'},
		{desert = 'uint8_t'},
		{dirt = 'uint8_t'},
	},
}

local monsterPalettesAddr = 0x127820
local numMonsterPalettes = 0x300
-- ... of type palette8_t

-- the first 'numMonsters' overlaps
-- then there's 32 more
-- the first 27 of those are espers
-- the last 5 are unknown/unused
local numMonsterSprites = 0x1a0

-- TODO there are 0x19f of these, not 0x180 ...
-- ... same with the monster stat table?
local monsterSpritesAddr = 0x127000
local monsterSpriteDataAddr = 0x297000

local monsterSprite_t = struct{
	name = 'monsterSprite_t',
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	packed = true,
	fields = {
		{name='offset', type='uint16_t:15'},
		{name='_3bpp', type='uint16_t:1'},
		{name='palHi', type='uint8_t:7'},
		{name='tile16', type='uint8_t:1'},
		{name='palLo', type='uint8_t'},
		{name='tileMaskIndex', type='uint8_t'},
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
}
assert.eq(ffi.sizeof'monsterSprite_t', 5)

---------------- ITEMS ----------------

local numItems = 0x100
local numItemTypes = 0x20

local itemNamesAddr = 0x12b300

local itemref_t = reftype{
	name = 'itemref_t',
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

local equipFlags_t, code = bitflagtype{
	name = 'equipFlags_t',
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
assert.eq(ffi.sizeof'equipFlags_t', 2)

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

local item_t = ff6struct{
	name = 'item_t',
	fields = {
		-- 0x00:
		{itemType = 'uint8_t:4'},		-- not the same as 'itemTypeNames'
		{canBeThrown = 'uint8_t:1'},
		{canUseInBattle = 'uint8_t:1'},
		{canUseInMenu = 'uint8_t:1'},
		{unused_0_7 = 'uint8_t:1'},		-- only here for the ptr union size calc in struct.lua
		-- 0x01:
		{equip = 'equipFlags_t'},
		-- 0x03:
		{spellLearn = 'spellLearn_t'},
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
		{immuneToEffect1 = 'effect1_t'},
		-- 0x07:
		{immuneToEffect2 = 'effect2_t'},
		-- 0x08:
		{hasEffect3 = 'effect3_t'},
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
		{targetting = 'targetting_t'},
		-- 0x0f:
		-- TODO UNION
		{element_weaponDamage_equipHalfDamage = 'uint8_t'},
		-- 0x10:
		{vigor = 'uint8_t:4'},
		{speed = 'uint8_t:4'},
		-- 0x11:
		{stamina = 'uint8_t:4'},
		{magicPower = 'uint8_t:4'},
		-- 0x12:
		{spellCast = 'uint8_t:6'},	-- should be spellref_t, but it looks like you can't use structs with bitfields
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
		{battlePower_defense = 'uint8_t'},
		-- 0x15:
		{hitChance_magicDefense = 'uint8_t'},
		-- 0x16:
		{elementAbsorb = 'element_t'},
		-- 0x17:
		{elementNoEffect = 'element_t'},
		-- 0x18:
		{elementWeak = 'element_t'},
		-- 0x19:
		{givesEffect2 = 'effect2_t'},
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
		{buyPrice = 'uint16_t'},
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
assert.eq(ffi.offsetof('item_t', 'itemType'), 0)
assert.eq(ffi.offsetof('item_t', 'spellLearn'), 3)
assert.eq(ffi.offsetof('item_t', 'raiseStealChance'), 0x0b)
assert.eq(ffi.offsetof('item_t', 'changeFightToXFight'), 0x0c)
assert.eq(ffi.sizeof'item_t', 0x1e)

local itemColosseumInfo_t = ff6struct{
	name = 'itemColosseumInfo_t',
	fields = {
		{monster = 'monsterRef_t'},
		{unknown = 'uint8_t'},	-- always 64.  worth experimenting to see if bit 0 here is for the monster ref?
		{itemWon = 'itemref_t'},
		{hideName = 'uint8_t'},	-- 0 = no, 255 = yes
	},
}
assert.eq(ffi.sizeof'itemColosseumInfo_t', 4)

local itemsAddr = 0x185000

local itemColosseumInfosAddr = 0x1fb600
local itemDescOffsetsAddr = 0x2d7aa0
local itemDescBaseAddr = 0x2d6400

ffi.cdef[[typedef str13_t rareItemName_t;]]
local numRareItems = 20
local rareItemDescOffsetAddr = 0x0efb60
local rareItemNamesAddr = 0x0efba0
local rareItemDescBaseAddr = 0x0efcb0

---------------- MONSTERS ----------------

-- monster_t 0x1f
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

local monster_t = ff6struct{
	name = 'monster_t',
	fields = {
		-- 0x00:
		{speed = 'uint8_t'},		-- rpglegion says speed
		-- 0x01:
		{battlePower = 'uint8_t'},	-- rpglegion says battle power
		-- 0x02:
		{hitChance = 'uint8_t'},
		-- 0x03:
		{evade = 'uint8_t'},
		-- 0x04:
		{magicBlock = 'uint8_t'},
		-- 0x05:
		{defense = 'uint8_t'},
		-- 0x06:
		{magicDefense = 'uint8_t'},
		-- 0x07:
		{magicPower = 'uint8_t'},
		-- 0x08:
		{hp = 'uint16_t'},
		-- 0x0a:
		{mp = 'uint16_t'},
		-- 0x0c:
		{exp = 'uint16_t'},
		-- 0x0e:
		{gold = 'uint16_t'},
		-- 0x10:
		{level = 'uint8_t'},
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
		{immuneToEffect1 = 'effect1_t'},
		-- 0x15:
		{immuneToEffect2 = 'effect2_t'},
		-- 0x16:
		{elementHalfDamage = 'element_t'},
		-- 0x17:
		{elementAbsorb = 'element_t'},
		-- 0x18:
		{elementNoEffect = 'element_t'},
		-- 0x19:
		{elementWeak = 'element_t'},
		-- 0x1a:
		{fightAnimation = 'uint8_t'},
		-- 0x1b:
		{hasEffect1 = 'effect1_t'},
		-- 0x1c:
		{hasEffect2 = 'effect2_t'},
		-- 0x1d:
		{hasEffect3 = 'effect3_t'},
		-- 0x1e:
		-- for some reason I thought this byte was effect4_t ...
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
assert.eq(ffi.sizeof'monster_t', 0x20)
local monstersAddr = 0x0f0000

local monsterItem_t = ff6struct{
	name = 'monsterItem_t',
	fields = {
		{rareSteal = 'itemref_t'},
		{commonSteal = 'itemref_t'},
		{rareDrop = 'itemref_t'},
		{commonDrop = 'itemref_t'},
	},
}
local monsterItemsAddr = 0x0f3000

local monsterSketchesAddr = 0x0f4300

local spellref2_t = createVec{
	dim = 2,
	ctype = 'spellref_t',
	vectype = 'spellref2_t',
}
assert.eq(ffi.sizeof'spellref2_t', 2)
local monsterRagesAddr = 0x0f4600
local numRages = 0x100

local spellref4_t = createVec{
	dim = 4,
	ctype = 'spellref_t',
	vectype = 'spellref4_t',
}
assert.eq(ffi.sizeof'spellref4_t', 4)
local monsterSpellsAddr = 0x0f3d00

local itemref4_t = createVec{
	dim = 4,
	ctype = 'itemref_t',
	vectype = 'itemref4_t',
}
assert.eq(ffi.sizeof'itemref4_t', 4)

local metamorphSetsAddr = 0x047f40
local numMetamorphSets = 0x1a

---------------- CHARACTERS ----------------

local numExpLevelUps = 106
local numLevels = 98

ffi.cdef[[typedef str7_t menuName_t;]]
local numMenuNames = 0x20
local menuNamesAddr = 0x018cea0

local menuref_t = reftype{
	name = 'menuref_t',
	getter = function(i)
		return gameC.menuNames[i]
	end,
}

ffi.cdef[[typedef str6_t characterName_t;]]
local characterNamesAddr = 0x0478c0

local menuref4_t = createVec{
	dim = 4,
	ctype = 'menuref_t',
	vectype = 'menuref4_t',
}

local itemref2_t = createVec{
	dim = 2,
	ctype = 'itemref_t',
	vectype = 'itemref2_t',
}

local character_t = ff6struct{
	name = 'character_t',
	fields = {
		{hp = 'uint8_t'},
		{mp = 'uint8_t'},
		{menu = 'menuref4_t'},
		{vigor = 'uint8_t'},
		{speed = 'uint8_t'},
		{stamina = 'uint8_t'},
		{magicPower = 'uint8_t'},
		{battlePower = 'uint8_t'},	-- note: equipping nothing gives +10 to battle power
		{defense = 'uint8_t'},
		{magicDefense = 'uint8_t'},
		{evade = 'uint8_t'},
		{magicBlock = 'uint8_t'},
		-- or should these all be an itemref6_t?
		{lhand = 'itemref_t'},
		{rhand = 'itemref_t'},
		{head = 'itemref_t'},
		{body = 'itemref_t'},
		{relic = 'itemref2_t'},
		{level = 'uint8_t'},
	},
}
assert.eq(ffi.sizeof'character_t', 22)

local numCharacters = 0x40	-- allegedly...
local charactersAddr = 0x2d7ca0

ffi.cdef[[typedef str12_t mogDanceName_t;]]
local numMogDances = 8
local mogDanceNamesAddr = 0x26ff9d

ffi.cdef[[typedef str12_t swordTechName_t;]]
local numSwordTechs = 8
local swordTechNamesAddr = 0x0f3c40
local swordTechDescBaseAddr = 0x0ffd00
local swordTechDescOffsetsAddr = 0x0fffae

local numBlitzes = 8
local blitzDescBaseAddr = 0x0ffc00
local blitzDescOffsetsAddr = 0x0fff9e

local numLores = 24
local loreDescBaseAddr = 0x2d77a0
local loreDescOffsetsAddr = 0x2d7a70

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

local charHiAndSize_t = ff6struct{
	name = 'charHiAndSize_t',
	fields = {
		{hi = 'uint8_t'},
		{size = 'uint8_t'},	-- in bytes
	},
}
assert.eq(ffi.sizeof'charHiAndSize_t', 2)

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

local shopinfo_t = ff6struct{
	name = 'shopinfo_t',
	fields = {
		{shopType = 'uint8_t:4'},
		{priceType = 'uint8_t:4'},
	},
	metatable = function(mt)
		local oldFieldToString = mt.fieldToString
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

local itemref8_t = createVec{
	dim = 8,
	ctype = 'itemref_t',
	vectype = 'itemref8_t',
}
local numShops = 0x80
local shop_t = ff6struct{
	name = 'shop_t',
	fields = {
		{shopinfo = 'shopinfo_t'},
		{items = 'itemref8_t'},
	}
}
assert.eq(ffi.sizeof'shop_t', 9)

local shopsAddr = 0x047ac0

local numMapNames = 448	-- 146 entries are used, the rest are 0xffff

local numDialogs = 3328
local numBattleDialogs = 0x100
local numBattleDialog2s = 0x100
local numBattleMessages = 0x100

local numPositionedText = 5	-- might actually be lower

local xy8sb_t = ff6struct{
	name = 'xy8sb_t',
	fields = {
		{x = 'int8_t'},
		{y = 'int8_t'},
	},
}
assert.eq(ffi.sizeof'xy8sb_t', 2)

local xy8b_t = ff6struct{
	name = 'xy8b_t',
	fields = {
		{x = 'uint8_t'},
		{y = 'uint8_t'},
	},
}
assert.eq(ffi.sizeof'xy8b_t', 2)

local mapNameRef_t = reftype{
	name = 'mapNameRef_t',
	getter = function(i)
		return game.mapNames[i]
	end,
}

local map_t = struct{
	name = 'map_t',
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	packed = true,
	fields = {
		{name='name', type='mapNameRef_t'},						-- 0
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
		{name='unknown_3', type='uint8_t'},						-- 3
		{name='tileProps', type='uint8_t'},						-- 4		mapTilePropsOffsets[]
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
		{name='mapOverlayProperties', type='uint8_t'},			-- 0x11
		{name='layer2Pos', type='xy8sb_t'},						-- 0x12
		{name='layer3Pos', type='xy8sb_t'},						-- 0x14
		{name='parallax', type='uint8_t'},					-- 0x16
		{name='layer2HeightLog2Minus4', type='uint8_t:2'},		-- 0x17.2-3
		{name='layer2WidthLog2Minus4', type='uint8_t:2'},		-- 0x17.0-1
		{name='layer1HeightLog2Minus4', type='uint8_t:2'},		-- 0x17.4-5	layer1Height = 1 << (layer1HeightLog2Minus4 + 4)
		{name='layer1WidthLog2Minus4', type='uint8_t:2'},		-- 0x17.6-7	layer1Width = 1 << (layer1WidthLog2Minus4 + 4)
		{name='unknown_18_0', type='uint8_t:4'},				-- 0x18.0-3
		{name='layer3HeightLog2Minus4', type='uint8_t:2'},		-- 0x18.6-7
		{name='layer3WidthLog2Minus4', type='uint8_t:2'},		-- 0x18.4-5
		{name='palette', type='uint8_t'},						-- 0x19
		{name='paletteAnimation', type='uint8_t'},				-- 0x1a
		{name='animatedLayers1And2', type='uint8_t:5'},			-- 0x1b.0-4
		{name='animatedLayer3', type='uint8_t:3'},				-- 0x1b.5-7
		{name='music', type='uint8_t'},							-- 0x1c
		{name='unknown_1d', type='uint8_t'},					-- 0x1d
		-- map 21, size is {44,52}, tiles are defined up to {46,54} ... why is it 2 less?
		{name='size', type='xy8b_t'},							-- 0x1e
		{name='colorMath', type='uint8_t'},						-- 0x20
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
}
assert.eq(ffi.sizeof'map_t', 0x21)

local mapTileProps_t = ff6struct{
	name = 'mapTileProps_t',
	fields = {
		{zLevel = 'uint16_t:3'},				-- 0.0-0.2 = 0=none 1=upstairs 2=downstairs 3=upstairs & downstairs 4=bridge
		{topSpritePriority = 'uint16_t:1'},		-- 0.3
		{bottomSpritePriority = 'uint16_t:1'},	-- 0.4
		{door = 'uint16_t:1'},					-- 0.5
		{stairsUpRight = 'uint16_t:1'},			-- 0.6
		{stairsUpLeft = 'uint16_t:1'},			-- 0.7
		{passableRight = 'uint16_t:1'},			-- 1.0
		{passableLeft = 'uint16_t:1'},			-- 1.1
		{passableBottom = 'uint16_t:1'},		-- 1.2
		{passableTop = 'uint16_t:1'},			-- 1.3
		{unknown_1_4 = 'uint16_t:1'},			-- 1.4
		{unknown_1_5 = 'uint16_t:1'},			-- 1.5
		{ladder = 'uint16_t:1'},				-- 1.6
		{passableNPC = 'uint16_t:1'},			-- 1.7
	},
}
assert.eq(ffi.sizeof'mapTileProps_t', 2)

local uint16_4_t = createVec{
	dim = 4,
	ctype = 'uint16_t',
	vectype = 'uint16_4_t',
}

local mapAnimProps_t = ff6struct{
	name = 'mapAnimProps_t',
	fields = {
		{speed = 'uint16_t'},		-- 0-1
		{frames = 'uint16_4_t'},	-- 2-9
	},
}
assert.eq(ffi.sizeof'mapAnimProps_t', 0xa)

local uint16_8_t = createVec{
	dim = 8,
	ctype = 'uint16_t',
	vectype = 'uint16_8_t',
}

local mapAnimPropsLayer3_t = ff6struct{
	name = 'mapAnimPropsLayer3_t',
	fields = {
		{speed = 'uint16_t'},		-- 0-1
		{size = 'uint16_t'},		-- 2-3
		{frames = 'uint16_8_t'},	-- 4-0x13
	},
}
assert.eq(ffi.sizeof'mapAnimPropsLayer3_t', 0x14)

local mapPalAnim_t = ff6struct{
	name = 'mapPalAnim_t',
	fields = {
		{colorCounter = 'uint8_t:4'},
		{type = 'uint8_t:4'},
		{frameCounter = 'uint8_t'},
		{firstColor = 'uint8_t'},
		{numColors = 'uint8_t'},
		{romColorOffset = 'uint16_t'},
	},
}
assert.eq(ffi.sizeof'mapPalAnim_t', 6)
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

local treasure_t = ff6struct{
	name = 'treasure_t',
	-- can't be anonymous because game_t is inserting its serialized name into its C code definition...
	--anonymous = true,

	fields = {
		{pos = 'xy8b_t'},
		{switch = 'uint16_t:9'},	-- global index / bitflag? into game state data?
		{unused_2_1 = 'uint16_t:1'},	-- does this bit go in 'switch' too? like in npc_t switch is 10 bits...
		{unused_2_2 = 'uint16_t:1'},
		{empty = 'uint16_t:1'},		-- set iff type == 0 i.e. empty
		{unused_2_4 = 'uint16_t:1'},
		{type = 'uint16_t:3'},	-- 0=empty, 1=monster, 2=item, 3=gp
		-- depends on 'type'
		{battleOrItemOrGP = 'uint8_t'},	-- GP is x100
	},
}
assert.eq(ffi.sizeof(treasure_t), 5)


local npc_t = struct{
	name = 'npc_t',
	packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
	fields = {
		{type=struct{
			union = true,
			anonymous = true,
			packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
			fields = {
				{type=struct{
					anonymous = true,
					packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
					fields = {
						-- invalid when vehicle == 0 && special npc != 0
						{name='script', type='uint32_t:18'},				-- 0.0-2.1

						{name='unknown_2_2', type='uint32_t:3'},			-- 2.2-2.4
						{name='scrollingLayer', type='uint32_t:1'},			-- 2.5 = 0=layer1, 1=layer2
						{name='switch', type='uint32_t:10'},				-- 2.6-3.7
					},
				}},

				-- invalid when vehicle value != 0 && special npc == 0
				{type=struct{
					anonymous = true,
					packed = true,
tostringFields = true,
tostringOmitFalse = true,
tostringOmitNil = true,
tostringOmitEmpty = true,
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

		-- "specialGraphics" if vehicle == 0 and special npc != 0 then ...
		-- otherwise "showRider "
		{name='showRider_or_specialGraphics', type='uint8_t:1'},	-- 4.7

		{name='y', type='uint8_t:6'},								-- 5.0-5.5
		{name='speed', type='uint8_t:2'},							-- 5.6-5.7 = slow to fast
		{name='graphics', type='uint8_t'},							-- 6
		{name='movement', type='uint8_t:4'},						-- 7.0-7.3 = 0=none, 1=script, 2=user, 3=random
		{name='spritePriority', type='uint8_t:2'},					-- 7.4-7.5 = 0=normal 1=high 2=low 3=low

		-- "speed" when vehicle == 0
		-- "vehicle" otherwise
		{name='vehicle_or_speed', type='uint8_t:2'},				-- 7.6-7.7 = 0=none 1=chocobo 2=magitek 3=raft

		-- "direction" when animation == 0
		-- "type" otherwise
		{name='direction_or_type', type='uint8_t:2'},				-- 8.0-8.1 direction = {up, right, down, left}, type = {one frame, flip horz, two frames, four frames}

		-- "size" when vehicle == 0 && special npc != 0
		-- otherwise "talkDoesntTurn"
		{name='size_or_talkDoesntTurn', type='uint8_t:1'},			-- 8.2.  size: 0=16x16, 1=32x32

		{name='layerPriority', type='uint8_t:2'},					-- 8.3-8.4 0=default 1=top sprite only 2=foreground 3=background

		{name='animation', type='uint8_t:3'},						-- 8.5-8.7
	},
}
assert.eq(ffi.sizeof'npc_t', 9)

local numEntranceTriggerOfs = 513
local entranceTrigger_t = ff6struct{
	name = 'entranceTrigger_t',
	fields = {
		{pos = 'xy8b_t'},				-- 0-1
		{mapIndex = 'uint16_t:9'},		-- 2.0-3.0: maps[]
		{setParentMap = 'uint16_t:1'},	-- 3.1
		{zLevel = 'uint16_t:1'},		-- 3.2
		{showDestName = 'uint16_t:1'},	-- 3.3
		{destFacingDir = 'uint16_t:2'},	-- 3.4-3.5
		{unknown_3_6 = 'uint16_t:2'},	-- 3.6-3.7
		{dest = 'xy8b_t'},				-- 4-5
	},
}
assert.eq(ffi.sizeof'entranceTrigger_t', 6)

local entranceAreaTrigger_t = struct{
	name = 'entranceAreaTrigger_t',
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	packed = true,
	fields = {
		{name='pos', type='xy8b_t'},				-- 0-1
		{name='length', type='uint8_t:7'},			-- 2.0-2.6
		{name='vertical', type='uint8_t:1'},		-- 2.7
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
		{name='dest', type='xy8b_t'},				-- 4-5
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
}
assert.eq(ffi.sizeof'entranceAreaTrigger_t', 7)

local mapEventTrigger_t = ff6struct{
	name = 'mapEventTrigger_t',
	fields = {
		{pos = 'xy8b_t'},
		{eventCode = 'uint24_t'},
	},
}
assert.eq(ffi.sizeof'mapEventTrigger_t', 5)

local WorldTileProps_t = ff6struct{
	name = 'WorldTileProps_t',
	fields = {
		{blocksChocobo = 'uint16_t:1'},		-- 0.0
		{airshipCantLand = 'uint16_t:1'},	-- 0.1
		{airshipShadow = 'uint16_t:2'},		-- 0.2-3 ... aka elevation?
		{blocksWalking = 'uint16_t:1'},		-- 0.4
		{forest = 'uint16_t:1'},			-- 0.5
		{enemyEncounters = 'uint16_t:1'},	-- 0.6
		{unknown_0_7 = 'uint16_t:1'},		-- 0.7
		-- now I can't tell from the ascii art, but its either 1 unused and 3 for background, or its 4 for background ...
		-- https://web.archive.org/web/20250429144337/https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:fmt:world_map_tile_properties
		-- there's 56 total
		-- https://www.spriters-resource.com/snes/ff6/asset/54685/
		-- so ... 6 bits?
		{battleBG = 'uint16_t:4'},			-- 1.0-1.3 aka 0.8-0.11
		{unknown0_12 = 'uint16_t:1'},		-- 1.4 aka 0.12
		{veldt = 'uint16_t:1'},				-- 1.5 aka 0.13
		{phoenixCave = 'uint16_t:1'},		-- 1.6 aka 0.14
		{kefkasTower = 'uint16_t:1'},		-- 1.7 aka 0.15
	},
}
assert.eq(ffi.sizeof'WorldTileProps_t', 2)

local battleBgProps_t = struct{
	name = 'battleBgProps_t',
	packed = true,
	tostringFields = true,
	tostringOmitFalse = true,
	tostringOmitNil = true,
	tostringOmitEmpty = true,
	fields = {
		{
			type = struct{
				anonymous = true,
				union = true,
				fields = {
					{
						name = 'graphics1combined',		-- if this is 0xff then graphics1 is invalid.
						type = 'uint8_t'
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
		{name='graphics2', type='uint8_t'},					-- 1: 0xff = none.
		{name='graphics3', type='uint8_t'},					-- 2: 0xff = none.
		{name='layout1', type='uint8_t'},					-- 3: battleBgLayout[]
		{name='layout2', type='uint8_t'},					-- 4:
		{name='palette', type='uint8_t:7'},					-- 5.0-5.6
		{name='wavy', type='uint8_t:1'},					-- 5.7
	},
	metatable = function(mt)
		mt.typeToString = fieldsToHex
	end,
}
assert.eq(ffi.sizeof(battleBgProps_t), 6)


---------------- GAME ----------------

-- TODO this is clever but ... rigid and with lots of redundancies
-- the memorymap system of the super metroid randomizer is better.
local game_t = struct{
	anonymous = true,
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
		{name = 'name', type = 'unknown_000000', type = 'uint8_t['..(-(0x000000 - 0x0051ba))..']'},				-- 0x000000 - 0x0051ba

		{name = 'characterMenuImageOffsets', type = 'uint16_t[16]'},											-- 0x0051ba - 0x0051da
		{name = 'characterMenuImageTileLayout', type = 'uint8_t[25]'},											-- 0x0051da - 0x0051f3

		{name = 'unknown_0051f3', type = 'uint8_t['..(-(0x0051f3 - 0x0091d5))..']'}, 							-- 0x0051f3 - 0x0091d5

		{name = 'mapAnimPropOfs', type = 'uint16_t[0x15]'},														-- 0x0091d5 - 0x0091ff = map animation properties pointer table (+0x0091ff) (only 10 used)
		{name = 'mapAnimProps', type = 'mapAnimProps_t[144]'},													-- 0x0091ff - 0x00979f = map animation properties [144] @ 10 bytes each
		{name = 'mapAnimPropsLayer3Ofs', type = 'uint16_t[7]'},													-- 0x00979f - 0x0097ad = map animation properties layer 3 pointer table (+0x0097ad) (only 6 used)
		{name = 'mapAnimPropsLayer3', type = 'mapAnimPropsLayer3_t[6]'},										-- 0x0097ad - 0x009825 = map animation properties layer 3 [6]
		{name = 'mapPalAnim', type = 'mapPalAnim_t[20]'},														-- 0x009825 - 0x00989d = map palette animation

		{name = 'unknown_00989d', type = 'uint8_t['..(-(0x00989d - 0x00ce3a))..']'},							-- 0x00989d - 0x00ce3a
		-- 0x00c27f-0x00c28f = something to do with battle background? -rpglegion

		-- offset of map character sprite parts
		-- interleaved row-major, 2x3
		{name = 'characterFrameTileOffsets', type = 'uint16_t['..(numCharacterSpriteFrames * 6)..']'},			-- 0x00ce3a - 0x00d026

		{name = 'unknown_00d026', type = 'uint8_t['..(-(0x00d026 - 0x00d0f2))..']'},							-- 0x00d026 - 0x00d0f2

		-- 0x00d0f2 - ? = pointer to map character graphics (2 bytes each)
		{name = 'characterSpriteOffsetLo', type = 'uint16_t['..numCharacterSprites..']'},						-- 0x00d0f2 - 0x00d23c

		{name = 'characterSpriteOffsetHiAndSize', type = 'charHiAndSize_t['..numCharacterSprites..']'},			-- 0x00d23c - 0x00d386

		-- 0x00d23c - ? = bank pointer & # bytes to copy for map char gfx (2 bytes each)
		-- 0x00dfa0 - 0x00e0a0 = 'DTE table' -rgplegion
		{name = 'unknown_00d27c', type = 'uint8_t['..(-(0x00d386 - 0x02ce2b))..']'},							-- 0x00d386 - 0x02ce2b

		-- battle character palette assignment (1 byte each)
		{name = 'characterPaletteIndexes', type = 'uint8_t['..numCharacterSprites..']'},						-- 0x02ce2b - 0x02ced0

		{name = 'unknown_02ced0', type = 'uint8_t['..(-(0x02ced0 - 0x02d01a))..']'},							-- 0x02ced0 - 0x02d01a

		{name = 'formationSizeOffsets', type = 'uint16_t['..numFormationSizeOffsets..']'},						-- 0x02d01a - 0x02d034 = offset by +0x020000 into formationSize[]
		{name = 'formationSizes', type = 'formationSize_t['..numFormationSizes..']'},							-- 0x02d034 - 0x02d0f4

		-- 0x036f00 - ? = menu portrait palette assignment (1 byte each)
		-- 0x036f1b - ? = pointer to menu portrait graphics (2 bytes each)
		{name = 'unknown_02d0f4', type = 'uint8_t['..(-(0x02d0f4 - 0x03c00e))..']'},							-- 0x02d0f4 - 0x03c00e

		{name = 'positionedTextOffsets', type = 'uint16_t['..numPositionedText..']'},							-- 0x03c00e - 0x03c018

		{name = 'unknown_03c018', type = 'uint8_t['..(-(0x03c018 - 0x03c2fc))..']'},							-- 0x03c018 - 0x03c2fc

		{name = 'positionedTextBase', type = 'uint8_t['..(-(0x03c2fc - 0x03c406))..']'},						-- 0x03c2fc - 0x03c406

		-- 0x03c326 - 0x03c406 = more positioned text (N items, var length) ... where are the offsets for this?
		-- "P}BUY  SELL  EXITA:}GPAr GPAz}Owned:Az Equipped:AP Bat PwrAP DefenseAl â¦Af{Hi! Can I help you?Af{Help yourself!Af{How many?Af{Whatcha got?Af{How many?Af{Bye!          Af{You need more GP!Af{Too many!       Af{One's plenty! A"
		{name = 'unknown_03c406', type = 'uint8_t['..(-(0x03c406 - 0x040000))..']'},							-- 0x03c406 - 0x040000

		{name = 'mapEventTriggerOfs', type = 'uint16_t['..((0x040342 - 0x040000)/2)..']'},						-- 0x040000 - 0x040342 = offset by +0x040000
		{name = 'mapEventTriggers', type = 'mapEventTrigger_t[0x48f]'},											-- 0x040342 - 0x041a0d = map event triggers (5 bytes each)

		{name = 'padding_041a0d', type = 'uint8_t['..(-(0x041a0d - 0x041a10))..']'},							-- 0x041a0d - 0x041a10

		{name = 'npcOfs', type = 'uint16_t['..(-(0x041a10 - 0x041d52)/2)..']'},									-- 0x041a10 - 0x041d52 = npc offsets (+0x041a10)
		{name = 'npcs', type = 'npc_t[0x891]'},																	-- 0x041d52 - 0x046a6b = npc data
		{name = 'unused_046a6b', type = 'uint8_t['..(-(0x046a6b - 0x046ac0))..']'},								-- 0x046a6b - 0x046ac0 = unused
		{name = 'spells', type = 'spell_t['..numSpells..']'},													-- 0x046ac0 - 0x0478c0
		{name = 'characterNames', type = 'characterName_t['..numCharacters..']'},								-- 0x0478c0 - 0x047a40
		{name = 'blitzData', type = 'raw12_t['..numBlitzes..']'},												-- 0x047a40 - 0x047aa0

		{name = 'unknown_047aa0', type = 'uint8_t['..(-(0x047aa0 - 0x047ac0))..']'},							-- 0x047aa0 - 0x047ac0.  something here, but mostly zeroes at the end.  maybe there's just 1 last blitz at the end?

		{name = 'shops', type = 'shop_t['..numShops..']'},														-- 0x047ac0 - 0x047f40
		{name = 'metamorphSets', type = 'itemref4_t['..numMetamorphSets..']'},									-- 0x047f40 - 0x047fa8
		{name = 'padding_047fa8', type = 'uint8_t[0x18]'},														-- 0x047fa8 - 0x047fc0 = 00's.  just like font intro is.  is this just 24 more bytes of font data?
		{name = 'font', type = 'uint8_t['..(0x10 * 0x100)..']'},												-- 0x047fc0 - 0x048fc0 -- font graphics (8x8x2bpp, 16 bytes each, 0x00-0xff) ... the first half is blank
		{name = 'font16_widths', type = 'uint8_t['.. 0x80 ..']'},												-- 0x048fc0 - 0x049040 -- font character cell widths (0x00-0x7f)

		{name = 'unknown_049040', type = 'uint8_t['..(-(0x049040 - 0x0490c0))..']'},							-- 0x049040 - 0x0490c0.  mostly filled with '0c's.  mostly.  so is the data after it, and a bit before, so maybe it goes with this?

		{name = 'font16_20_to_7f', type = 'uint8_t['..(22 * (0x7f - 0x20 + 1))..']'},							-- 0x0490c0 - 0x049900 (or 0x04a4c0) -- font graphics data (16x11x1, 22 bytes each, 0x20-0x7f)

		-- 0x049bc0-0x049bd0 = 16 bytes of something
		-- 0x04ba00-0x04c007 = Ending Font (compressed)
		-- 0x04c008-0x04f476 = Ending BG Graphics and Tile Formation (compressed)
		-- 0x04f477-0x04f6fa = Ending Sprite Graphics (compressed)
		-- 0x04f6fb-0x04ffff = Ending Sprite Graphics (compressed)
		{name = 'unknown_049900', type = 'uint8_t['..(-(0x049900 - 0x05070e))..']'},							-- 0x049900 - 0x05070e

		{name = 'spcMainCodeLoopLen', type = 'uint16_t'},														-- 0x05070e - 0x050710 -- length of main SPC code loop
		{name = 'spcMainCode', type = 'uint8_t['..(-(0x050710 - 0x051ec7))..']'},								-- 0x050710 - 0x051ec7 -- main SPC code loop

		{name = 'unknown_051ec7', type = 'uint8_t['..(-(0x051ec7 - 0x053c5f))..']'},							-- 0x051ec7 - 0x053c5f

		{name = 'brrSamplePtrs', type = 'uint24_t['..numBRRSamples..']'},										-- 0x053c5f - 0x053d1c -- BRR sample pointers (x63, 3 bytes each)
		{name = 'loopStartOfs', type = 'uint16_t['..numBRRSamples..']'},										-- 0x053d1c - 0x053d9a -- loop start pointers (x63, 2 bytes each)
		{name = 'pitchMults', type = 'uint16_t['..numBRRSamples..']'},											-- 0x053d9a - 0x053e18 -- pitch multipliers (x63, 2 bytes each)
		{name = 'adsrData', type = 'uint16_t['..numBRRSamples..']'},											-- 0x053e18 - 0x053e96 -- ADSR data (x63, 2 bytes each)

		{name = 'unknown_053e96', type = 'uint8_t['..(-(0x053e96 - 0x054a35))..']'},							-- 0x053e96 - 0x054a35

		{name = 'brrSamples', type = 'uint8_t['..(-(0x054a35 - 0x085c7a))..']'},								-- 0x054a35 - 0x085c7a -- BRR samples (does divide evenly by  3195 x63...)

		{name = 'unknown_085c7a', type = 'uint8_t['..(-(0x085c7a - 0x09fe00))..']'},							-- 0x085c7a - 0x09fe00

		{name = 'theEndGraphics2', type = 'uint8_t['..(-(0x09fe00 - 0x09ff00))..']'},							-- 0x09fe00 - 0x09ff00 = 4bpp
		{name = 'theEndPalette', type = 'palette16_8_t'},														-- 0x09ff00 - 0x0a0000
		{name = 'eventCode', type = 'uint8_t['..(-(0x0a0000 - 0x0ce600))..']'},									-- 0x0a0000 - 0x0ce600
		{name = 'dialogOffsets', type = 'uint16_t['..numDialogs..']'},											-- 0x0ce600 - 0x0d0000.  the first dialog offset points to the dialog which needs the bank byte to increment
		{name = 'dialogBase', type = 'uint8_t['..(-(0x0d0000 - 0x0ef100))..']'},								-- 0x0d0000 - 0x0ef100
		{name = 'mapNameBase', type = 'uint8_t['..(-(0x0ef100 - 0x0ef600))..']'},								-- 0x0ef100 - 0x0ef600

		-- 0x0ef600 - 0x0ef648 looks like offsets into something
		-- 0x0ef648 - 0x0ef678 looks like arbitrary values
		-- 0x0ef678 - 0x0efb60 is mostly '06' repeated
		{name = 'unknown_0ef600', type = 'uint8_t['..(-(0x0ef600 - 0x0efb60))..']'},							-- 0x0ef600 - 0x0efb60

		{name = 'rareItemDescOffsets', type = 'uint16_t['..numRareItems..']'},									-- 0x0efb60 - 0x0efb88
		{name = 'padding_0efb88', type = 'uint8_t[0x18]'},														-- 0x0efb88 - 0x0efba0 -- all 'ff' repeated ... enough for 12 extra offsets ... there are 20 rare items ... 20+12=32
		{name = 'rareItemNames', type = 'rareItemName_t['..numRareItems..']'},									-- 0x0efba0 - 0x0efca4 -- rare item names are 13 chars
		{name = 'padding_0efca4', type = 'uint8_t[0xc]'},														-- 0x0efca4 - 0x0efcb0 -- all 'ff' repeated, for 12 bytes, not quite 1 more name
		{name = 'rareItemDescBase', type = 'uint8_t['..(-(0x0efcb0 - 0x0f0000))..']'},							-- 0x0efcb0 - 0x0f0000
		{name = 'monsters', type = 'monster_t['..numMonsters..']'},												-- 0x0f0000 - 0x0f3000
		{name = 'monsterItems', type = 'monsterItem_t['..numMonsters..']'},										-- 0x0f3000 - 0x0f3600

		-- 0x0f3600 - 0x0f37c0 is mostly zeroes
		-- 0x0f37c0 - 0x0f3940 is something
		{name = 'unknown_0f3600', type = 'uint8_t['..(-(0x0f3600 - 0x0f3940))..']'},							-- 0x0f3600 - 0x0f3940

		{name = 'esperDescBase', type = 'uint8_t['..(-(0x0f3940 - 0x0f3c40))..']'},								-- 0x0f3940 - 0x0f3c40
		{name = 'swordTechNames', type = 'swordTechName_t['..numSwordTechs..']'},								-- 0x0f3c40 - 0x0f3ca0
		{name = 'padding_0f3ca0', type = 'uint8_t[0x60]'},														-- 0x0f3ca0 - 0x0f3d00 -- all 'ff' repeated
		{name = 'monsterSpells', type = 'spellref4_t['..numMonsters..']'},										-- 0x0f3d00 - 0x0f4300
		{name = 'monsterSketches', type = 'spellref2_t['..numMonsters..']'},									-- 0x0f4300 - 0x0f4600
		{name = 'monsterRages', type = 'spellref2_t['..numRages..']'},											-- 0x0f4600 - 0x0f4800
		{name = 'monsterRandomBattles', type = 'monsterRandomBattleEntry4_t[0x100]'},							-- 0x0f4800 - 0x0f5000
		{name = 'monsterEventBattles', type = 'monsterRandomBattleEntry2_t[0x100]'},							-- 0x0f5000 - 0x0f5400
		{name = 'worldSectorRandomBattlesPerTerrain', type = 'RandomBattlesPerTerrain_t[0x80]'},				-- 0x0f5400 - 0x0f5600 = [world][sectorx][sectory]  ... 64 sectors (32x32 chunks of 256x256 world map) per WoB, 64 for WoR
		{name = 'mapBattleGroups', type = 'RandomBattlesPerTerrain_t[0x80]'},									-- 0x0f5600 - 0x0f5800 ... not sure if this struct is correct
		{name = 'worldSectorRandomBattleEncounterRatesPerTerrain', type = 'uint8_t[0x80]'},						-- 0x0f5800 - 0x0f5880 = 2 bits used ... 64 sectors per WoB, 64 per WoR ... 8 items per sector, 2bpp each ( https://www.ff6hacking.com/wiki/doku.php?id=ff3:ff3us:doc:asm:rom_map )
		{name = 'mapBattleProbability', type = 'uint8_t[0x80]'},												-- 0x0f5880 - 0x0f5900 = 2 bits used
		{name = 'formation2s', type = 'formation2_t['..numFormations..']'},										-- 0x0f5900 - 0x0f6200
		{name = 'formations', type = 'formation_t['..numFormations..']'},										-- 0x0f6200 - 0x0f83c0
		{name = 'padding_0f83c0', type = 'uint8_t['..(-(0x0f83c0 - 0x0f8400))..']'},							-- 0x0f83c0 - 0x0f8400 - all 'ff' repeated.  probably 4 last empty formations + padding

		{name = 'unknown_0f8400', type = 'uint8_t['..(-(0x0f8400 - 0x0f8700))..']'},							-- 0x0f8400 - 0x0f8700

		{name = 'monsterScripts', type = 'uint8_t['..(-(0x0f8700 - 0x0fc050))..']'},							-- 0x0f8700 - 0x0fc050
		{name = 'monsterNames', type = 'monsterName_t['..numMonsters..']'},										-- 0x0fc050 - 0x0fcf50
		{name = 'monsterNameThing', type = 'uint8_t['..numMonsters..']'},										-- 0x0fcf50 - 0x0fd0d0
		{name = 'monsterAttackNames', type = 'monsterName_t['..numMonsters..']'},								-- 0x0fd0d0 - 0x0fdfd0
		{name = 'padding_0fdfd0', type = 'uint8_t[0x10]'},														-- 0x0fdfd0 - 0x0fdfe0 = 'ff's
		{name = 'battleDialogOffsets', type = 'uint16_t['..numBattleDialogs..']'},								-- 0x0fdfe0 - 0x0fe1e0
		{name = 'battleDialogBase', type = 'uint8_t['..(-(0x0fe1e0 - 0x0ff450))..']'},							-- 0x0fe1e0 - 0x0ff450

		{name = 'unknown_0ff450', type = 'uint8_t['..(-(0x0ff450 - 0x0ffc00))..']'},							-- 0x0ff450 - 0x0ffc00

		{name = 'blitzDescBase', type = 'uint8_t['..(-(0x0ffc00 - 0x0ffd00))..']'},								-- 0x0ffc00 - 0x0ffd00
		{name = 'swordTechDescBase', type = 'uint8_t['..(-(0x0ffd00 - 0x0ffe00))..']'},							-- 0x0ffd00 - 0xfffe00

		{name = 'unknown_0ffe00', type = 'uint8_t['..(-(0x0ffe00 - 0x0ffe40))..']'},							-- 0x0ffe00 - 0x0ffe40

		{name = 'esperDescOffsets', type = 'uint16_t['..numEspers..']'},										-- 0x0ffe40 - 0x0ffe76
		{name = 'padding_0ffe76', type = 'uint8_t[0xa]'},														-- 0x0ffe76 - 0x0ffe80 = 026d repeated, probably the final offset.

		{name = 'unknown_0ffe80', type = 'uint8_t['..(-(0x0ffe80 - 0x0ffeae))..']'},							-- 0x0ffe76 - 0x0ffeae

		{name = 'esperBonusDescs', type = 'esperBonusDesc_t['..numEsperBonuses..']'},							-- 0x0ffeae - 0x0fff47
		{name = 'padding_0fff47', type = 'uint8_t[87]'},														-- 0x0fff47 - 0x0fff9e = 'ff's
		{name = 'blitzDescOffsets', type = 'uint16_t['..numBlitzes..']'},										-- 0x0fff9e - 0x0fffae
		{name = 'swordTechDescOffsets', type = 'uint16_t['..numSwordTechs..']'},								-- 0x0fffae - 0x0fffbe
		{name = 'battleAnimScripts', type = 'uint8_t['..(-(0x0fffbe - 0x107fb2))..']'},							-- 0x0fffbe - 0x107fb2 <- indexed into with battleAnimScriptOffsets[i] + 0x100000
		{name = 'battleAnimSets', type = 'battleAnimSet_t['..numBattleAnimSets..']'},							-- 0x107fb2 - 0x1097fa
		{name = 'padding_1097fa', type = 'uint8_t['..(-(0x1097fa - 0x109800))..']'},							-- 0x1097fa - 0x109800 = 'ff's, just like the end of battleAnimSets

		{name = 'unknown_109800', type = 'uint8_t['..(-(0x109800 - 0x10d000))..']'},							-- 0x109800 - 0x10d000

		{name = 'battleDialog2Offsets', type = 'uint16_t['..numBattleDialog2s..']'},							-- 0x10d000 - 0x10d200
		{name = 'battleDialog2Base', type = 'uint8_t['..(-(0x10d200 - 0x10fd00))..']'},							-- 0x10d200 - 0x10fd00

		{name = 'unknown_10fd00', type = 'uint8_t['..(-(0x10fd00 - 0x110141))..']'},							-- 0x10fd00 - 0x110141

		{name = 'battleAnimFrame16x16Tiles', type = 'battleAnim16x16Tile_t[0x74cb]'},							-- 0x110141 - 0x11ead7 ... 2 bytes each ... pointers from battleAnimFrame16x16TileOffsets offset by 0x110000 but point into here
		{name = 'padding_11ead7', type = 'uint8_t'},															-- 0x11ead7 - 0x11ead8 -- 'ff'
		{name = 'battleAnimScriptOffsets', type = 'uint16_t[660]'},												-- 0x11ead8 - 0x11f000 ... uint16 offsets +0x100000 ... maybe there are only 650 of these to match with `numBattleAnimEffects`?
		{name = 'battleMessageBase', type = 'uint8_t['..(-(0x11f000 - 0x11f7a0))..']'},							-- 0x11f000 - 0x11f7a0
		{name = 'battleMessageOffsets', type = 'uint16_t['..numBattleMessages..']'},							-- 0x11f7a0 - 0x11f9a0

		{name = 'unknown_11f9a0', type = 'uint8_t['..(-(0x11f9a0 - 0x120000))..']'},							-- 0x11f9a0 - 0x120000

		{name = 'battleAnimGraphicsSets3bpp', type = 'battleAnim8x8Tile_t['..(0x20 * 0x180)..']'},				-- 0x120000 - 0x126000 - holds the 'graphicSet' uint16 offsets from battleAnimEffect_t * (0x20 entries == 0x40 bytes)
		{name = 'battleAnimPalettes', type = 'palette8_t['..numBattleAnimPalettes..']'},						-- 0x126000 - 0x126f00
		{name = 'itemTypeNames', type = 'str7_t['..numItemTypes..']'},											-- 0x126f00 - 0x126fe0
		{name = 'padding_126fe0', type = 'uint8_t[0x20]'},														-- 0x126fe0 - 0x127000 = 'ff's
		{name = 'monsterSprites', type = 'monsterSprite_t['..numMonsterSprites..']'},							-- 0x127000 - 0x127820
		{name = 'monsterPalettes', type = 'palette8_t['..numMonsterPalettes..']'},								-- 0x127820 - 0x12a820
		{name = 'monsterSpriteTileMask8Ofs', type = 'uint16_t'},												-- 0x12a820 - 0x12a822
		{name = 'monsterSpriteTileMask16Ofs', type = 'uint16_t'},												-- 0x12a822 - 0x12a824
		{name = 'monsterSpriteTileMaskData', type = 'uint8_t['..(0x12b300 - 0x12a824 )..']'},					-- 0x12a824 - 0x12b300
		{name = 'itemNames', type = 'str13_t['..numItems..']'},													-- 0x12b300 - 0x12c000
		{name = 'battleAnimGraphicsSets2bpp', type = 'battleAnim8x8Tile_t['..(0x20 * 0xb0)..']'},				-- 0x12c000 - 0x12ec00	-- should be 2bpp battle animation 16x16-tile-info referenced by .graphicSet
		{name = 'WoBPalettes', type = 'palette16_8_t'},															-- 0x12ec00 - 0x12ed00
		{name = 'WoRPalettes', type = 'palette16_8_t'},															-- 0x12ed00 - 0x12ee00
		{name = 'setzerAirshipPalette', type = 'palette16_8_t'},												-- 0x12ee00 - 0x12ef00
		{name = 'darylAirshipPalette', type = 'palette16_8_t'},													-- 0x12ef00 - 0x12f000

		{name = 'unknown_12f000', type = 'uint8_t['..(-(0x12f000 - 0x130000))..']'},							-- 0x12f000 - 0x130000

		{name = 'battleAnimGraphics3bpp', type = 'uint8_t['..(-(0x130000 - 0x14c998))..']'},					-- 0x130000 - 0x14c998 ... 3bpp, so 4881 (= 3 x 1627 ?) tiles
		{name = 'padding_14c998', type = 'uint8_t['..(-(0x14c998 - 0x14ca00))..']'},							-- 0x14c998 - 0x14ca00 = 'ff's

		{name = 'unknown_14ca00', type = 'uint8_t['..(-(0x14ca00 - 0x14d000))..']'},							-- 0x14ca00 - 0x14d000

		{name = 'battleAnimEffects', type = 'battleAnimEffect_t['..numBattleAnimEffects..']'},					-- 0x14d000 - 0x14df3c
		{name = 'battleAnimFrame16x16TileOffsets', type = 'uint16_t[4194]'},									-- 0x14df3c - 0x150000	-- +0x110000 ... really just 2949 that are valid.  each is a uint16_t, add to 0x110000 to get the start of the variable-length battleAnim16x16Tile_t list into battleAnimFrame16x16Tiles

		{name = 'fieldSpriteGraphics', type = 'uint8_t['..(-(0x150000 - 0x185000))..']'},						-- 0x150000 - 0x185000 = character images, 0x16a0 bytes each

		{name = 'items', type = 'item_t['..numItems..']'},														-- 0x185000 - 0x186e00
		{name = 'espers', type = 'esper_t['..numEspers..']'},													-- 0x186e00 - 0x186f29
		{name = 'padding_186f29', type = 'uint8_t['..(-(0x186f29 - 0x187000))..']'},							-- 0x186f29 - 0x187000 = 'ff's
		{name = 'battleAnimGraphics2bpp', type = 'uint8_t['..(-(0x187000 - 0x18c9a0))..']'},					-- 0x187000 - 0x18c9a0	-- 2bpp, so 1434 tiles
		{name = 'spellDescBase', type = 'uint8_t['..(-(0x18c9a0 - 0x18cea0))..']'},								-- 0x18c9a0 - 0x18cea0
		{name = 'menuNames', type = 'menuName_t['..numMenuNames..']'},											-- 0x18cea0 - 0x18cf80
		{name = 'spellDescOffsets', type = 'uint16_t[54]'},														-- 0x18cf80 - 0x18cfec
		{name = 'padding_18cfec', type = 'uint8_t['..(-(0x18cfec - 0x18d000))..']'},							-- 0x18cfec - 0x18d000 = 'ff's

		{name = 'unknown_18d000', type = 'uint8_t['..(-(0x18d000 - 0x18e6ba))..']'},							-- 0x18d000 - 0x18e6ba

		{name = 'SerpentTrenchPalettesCompressed', type = 'uint8_t['..(-(0x18e6ba - 0x18e800))..']'},			-- 0x18e6ba - 0x18e800

		-- 18e800 = something
		-- 18f000 = something
		{name = 'unknown_18e800', type = 'uint8_t['..(-(0x18e800 - 0x19a800))..']'},							-- 0x18e800 - 0x19a800

		{name = 'mapTilePropsCompressed', type = 'uint8_t['..(-(0x19a800 - 0x19cd10))..']'},					-- 0x19a800 - 0x19cd10 = map tile properties (compressed)
		{name = 'mapTilePropsOffsets', type = 'uint16_t[0x2a]'},												-- 0x19cd10 - 0x19cd60 = offsets to map tile properties (+0x19a800) into mapTilePropsCompressed ... 0x40 but only 0x29 point to valid compressed data
		{name = 'unused_19cd62', type = 'uint16_t[0x16]'},														-- 0x19cd60 - 0x19cd90
		{name = 'mapLayoutOffsets', type = 'uint24_t[0x160]'},													-- 0x19cd90 - 0x19d1b0 = offsets to map data (352 items), (+0x19d1b0)
		{name = 'mapLayoutsCompressed', type = 'uint8_t['..(-(0x19d1b0 - 0x1e0000))..']'},						-- 0x19d1b0 - 0x1e0000 = map data (compressed)
		{name = 'mapTilesetsCompressed', type = 'uint8_t['..(-(0x1e0000 - 0x1fb400))..']'},						-- 0x1e0000 - 0x1fb400 = map tile formation (compressed)
		{name = 'formationMPs', type = 'uint8_t['..numFormationMPs..']'},										-- 0x1fb400 - 0x1fb600
		{name = 'itemColosseumInfos', type = 'itemColosseumInfo_t['..numItems..']'},							-- 0x1fb600 - 0x1fba00
		{name = 'mapTilesetOffsets', type = 'uint24_t[0x4b]'},													-- 0x1fba00 - 0x1fbaff -- 24bit, offset by +0x1e0000, points into mapTilesetsCompressed ... last points to invalid data so I cut it off.
		{name = 'padding_1fbaff', type = 'uint8_t[31]'},														-- 0x1fbaff - 0x1fbb00
		{name = 'entranceTriggerOfs', type = 'uint16_t['..numEntranceTriggerOfs..']'},							-- 0x1fbb00 - 0x1fbf02 -- offset by +0x1fbb00
		{name = 'entranceTriggers', type = 'entranceTrigger_t[0x469]'},											-- 0x1fbf02 - 0x1fd978 = entranceTrigger_t[] (only 415 used?)
		{name = 'padding_1fd978', type = 'uint8_t[136]'},														-- 0x1fd978 - 0x1fda00 = 'ff's
		{name = 'mapTileGraphicsOffsets', type = 'uint24_t[0x52]'},												-- 0x1fda00 - 0x1fdaf6 = town tile graphics pointers (+0x1fdb00), points into mapTileGraphics
		{name = 'padding_1fdaf6', type = 'uint8_t[10]'},														-- 0x1fdaf6 - 0x1fdb00
		{name = 'mapTileGraphics', type = 'uint8_t['..(-(0x1fdb00 - 0x25f400))..']'},							-- 0x1fdb00 - 0x25f400 = map tile graphics for layers 1&2, 4bpp
		{name = 'unknown_25f400', type = 'uint8_t['..(-(0x25f400 - 0x260000))..']'},							-- 0x25f400 - 0x260000 -- there's one battle bg in here
		{name = 'mapAnimGraphics', type = 'uint8_t['..(-(0x260000 - 0x268000))..']'},							-- 0x260000 - 0x268000 = 4bpp
		{name = 'characterPalettes', type = 'palette16_t['..numCharacterPalettes..']'},							-- 0x268000 - 0x268400	-- also town tile palettes?
		{name = 'mapNameOffsets', type = 'uint16_t['..numMapNames..']'},										-- 0x268400 - 0x268780
		{name = 'mapTileGraphicsLayer3', type = 'uint8_t['..(-(0x268780 - 0x26cd60))..']'},						-- 0x268780 - 0x26cd60  map tile garphics for layer 3, 2bpp
		{name = 'mapTileGraphicsLayer3Offsets', type = 'uint24_t[18]'},											-- 0x26cd60 - 0x26cd96 = offset, +0x268780 .. there's 19, but only 18 point to valid compressed data ...
		{name = 'padding_26cd96', type = 'uint8_t[10]'},														-- 0x26cd96 - 0x26cda0
		{name = 'mapAnimGraphicsLayer3Ofs', type = 'uint24_t[10]'},												-- 0x26cda0 - 0x26cdbe = offset, +0x26cdc0 to mapAnimGraphicsLayer3.  has values [0]=0 thru [6]=0x23d8 (which is the end of mapAnimGraphicsLayer3), so there's 6 entries
		{name = 'padding_26cdbe', type = 'uint8_t[2]'},															-- 0x26cdbe - 0x26cdc0
		{name = 'mapAnimGraphicsLayer3', type = 'uint8_t['..(-(0x26cdc0 - 0x26f198))..']'},						-- 0x26cdc0 - 0x26f198 = 2bpp, compressed
		{name = 'padding_26f198', type = 'uint8_t['..(-(0x26f198 - 0x26f200))..']'},							-- 0x26f198 - 0x26f200 = FF's, probably tail filler of mapAnimGraphicsLayer3

		{name = 'unknown_26f200', type = 'uint8_t['..(-(0x26f200 - 0x26f4a0))..']'},							-- 0x26f200 - 0x26f4a0.  filled from 0x26f200 - 0x26f440, then FF's from 0x0026f440 - 0x26f4a0

		{name = 'hpIncPerLevelUp', type = 'uint8_t['..numLevels..']'},											-- 0x26f4a0 - 0x26f502
		{name = 'mpIncPerLevelUp', type = 'uint8_t['..numLevels..']'},											-- 0x26f502 - 0x26f564
		{name = 'padding_26f564', type = 'uint8_t[3]'},															-- 0x26f564 - 0x26f567
		{name = 'spellNames_0to53', type = 'str7_t[54]'}, 														-- 0x26f567 - 0x26f6e1
		{name = 'spellNames_54to80', type = 'str8_t[27]'},                             							-- 0x26f6e1 - 0x26f7b9
		{name = 'spellNames_81to255', type = 'str10_t[175]'},													-- 0x26f7b9 - 0x26fe8f
		{name = 'esperAttackNames', type = 'str10_t['..numEspers..']'},											-- 0x26fe8f - 0x26ff9d
		{name = 'mogDanceNames', type = 'str12_t['..numMogDances..']'},											-- 0x26ff9d - 0x26fffd
		{name = 'padding_26fffd', type = 'uint8_t[3]'},															-- 0x26fffd - 0x270000
		{name = 'battleBgProperties', type = 'battleBgProps_t[56]'},											-- 0x270000 - 0x270150 = 56*6
		{name = 'battleBgPalettes', type = 'color_t[0xa80]'},													-- 0x270150 - 0x271650 ... everything's says 56 or 96? max index is 0x34 = 52
		{name = 'battleBgGfxAddrs', type = 'uint24_t[0xa8]'},													-- 0x271650 - 0x271848 = 75 used, the rest are 0's, most points into battleBgGfxCompressed
		{name = 'battleBgLayoutOffsets', type = 'uint16_t[0x70]'},												-- 0x271848 - 0x271928 = +0x270000 .  49 are valid. invalid contain 0x1928.  points into battleBgLayoutCompressed
		{name = 'battleBgLayoutCompressed', type = 'uint8_t['..(-(0x271928-0x27a9e7))..']'},					-- 0x271928 - 0x27a9e7 = 32x32x4bpp
		{name = 'battleBgGfxCompressed', type = 'uint8_t['..(-(0x27a9e7-0x296300))..']'},						-- 0x27a9e7 - 0x296300 = 4bpp
		{name = 'theEndGraphics1', type = 'uint8_t['..(-(0x296300 - 0x297000))..']'},							-- 0x296300 - 0x297000 = 4bpp
		{name = 'monsterSpriteData', type = 'uint8_t['..(-(0x297000 - 0x2d0000))..']'},							-- 0x297000 - 0x2d0000 = monster graphics
		{name = 'menuImages', type = 'uint8_t['..(-(0x2d0000 - 0x2d0e00))..']'},								-- 0x2d0000 - 0x2d0e00 = menu images 0x200 = bg pattern, 0x180 = borders, so 0x380 total ... x8 per menu scheme

		{name = 'unknown_2d0e00', type = 'uint8_t['..(-(0x2d0e00 - 0x2d1c00))..']'},							-- 0x2d0e00 - 0x2d1c00

		{name = 'menuWindowPalettes', type = 'palette16_8_t'},													-- 0x2d1c00 - 0x2d1d00 = menu window palettes, x8, 16 colors each, 2 bytes per color
		-- TODO struct multi dim arrays ...
		--{name = 'characterMenuImages', type = 'uint8_t['..numMenuChars..'][5][5][8][4]'},						-- 0x2d1d00 - 0x2d5860 = character menu images [char][tx][ty][col][row] @ 4bpp
		{name = 'characterMenuImages', type = 'uint8_t['..(numMenuChars * 5 * 5 * 8 * 4)..']'},					-- 0x2d1d00 - 0x2d5860 = character menu images [char][tx][ty][col][row] @ 4bpp
		{name = 'menuPortraitPalette', type = 'palette16_t['..numMenuChars..']'},								-- 0x2d5860 - 0x2d5ac0 = menu portrait palettes (16 colors each)
		{name = 'handCursorGraphics', type = 'uint8_t['..(-(0x2d5ac0 - 0x2d62c0))..']'},						-- 0x2d5ac0 - 0x2d62c0 ? = hand cursor graphics
		{name = 'battleWhitePalette', type = 'palette4_t'},														-- 0x2d62c0 - 0x2d62c8 = battle standard (white) text palette, 4 colors
		{name = 'battleGrayPalette', type = 'palette4_t'},														-- 0x2d62c8 - 0x2d62d0 = battle disabled (grey) text palette, 4 colors
		{name = 'battleYellowPalette', type = 'palette4_t'},													-- 0x2d62d0 - 0x2d62d8 = battle active (yellow) text palette, 4 colors
		{name = 'battleBluePalette', type = 'palette4_t'},														-- 0x2d62d8 - 0x2d62e0 = battle blue text palette, 4 colors
		{name = 'battleEmptyPalette', type = 'palette4_t'},														-- 0x2d62e0 - 0x2d62e8 = empty color palette, 4 colors
		{name = 'battleGrayPalette', type = 'palette4_t'},														-- 0x2d62e8 - 0x2d62f0 = battle gauge (grey) text palette, 4 colors
		{name = 'battleGreenPalette', type = 'palette4_t'},														-- 0x2d62f0 - 0x2d62f8 = battle green text palette, 4 colors
		{name = 'battleRedPalette', type = 'palette4_t'},														-- 0x2d62f8 - 0x2d6300 = battle red text palette, 4 colors
		{name = 'battleMenuPalettes', type = 'palette16_8_t'},													-- 0x2d6300 - 0x2d6400 = battle/menu character sprite palettes, 8 palettes, 16 colors each
		{name = 'itemDescBase', type = 'uint8_t['..(-(0x2d6400 - 0x2d77a0))..']'},								-- 0x2d6400 - 0x2d77a0
		{name = 'loreDescBase', type = 'uint8_t['..(-(0x2d77a0 - 0x2d7a70))..']'},								-- 0x2d77a0 - 0x2d7a70
		{name = 'loreDescOffsets', type = 'uint16_t['..numLores..']'},											-- 0x2d7a70 - 0x2d7aa0
		{name = 'itemDescOffsets', type = 'uint16_t['..numItems..']'},											-- 0x2d7aa0 - 0x2d7ca0
		{name = 'characters', type = 'character_t['..numCharacters..']'},										-- 0x2d7ca0 - 0x2d8220
		{name = 'expForLevelUp', type = 'uint16_t['..numExpLevelUps..']'},										-- 0x2d8220 - 0x2d82f4
		{name = 'treasureOfs', type = 'uint16_t[0x1a0]'},														-- 0x2d82f4 - 0x2d8634 	-- offset +0x2d8634 into treasures
		{name = 'treasures', type = ffi.typeof('$[0x11e]', treasure_t)},														-- 0x2d8634 - 0x2d8bca
		{name = 'padding_2d8bca', type = 'uint8_t['..(-(0x2d8bca - 0x2d8e5b))..']'},							-- 0x2d8bca - 0x2d8e5b = 'ff's
		{name = 'battleBgDance', type = 'uint8_t[0x40]'},														-- 0x2d8e5b - 0x2d8e9b
		{name = 'padding_2d8e9b', type = 'uint8_t['..(-(0x2d8e9b - 0x2d8f00))..']'},							-- 0x2d8e9b - 0x2d8f00 = 'ff's
		{name = 'maps', type = 'map_t[0x19f]'},																	-- 0x2d8f00 - 0x2dc47f
		{name = 'padding_2dc47f', type = 'uint8_t[1]'},															-- 0x2dc47f - 0x2df480
		{name = 'mapPalettes', type = 'palette16_8_t[48]'},														-- 0x2dc480 - 0x2df480 = map palettes (48 elements, 16x8 colors each)
		{name = 'entranceAreaTriggerOfs', type = 'uint16_t['..numEntranceTriggerOfs..']'},						-- 0x2df480 - 0x2df882
		{name = 'entranceAreaTriggers', type = 'entranceAreaTrigger_t[0x98]'},									-- 0x2df882 - 0x2dfcaa
		{name = 'padding_2dfcaa', type = 'uint8_t['..(-(0x2dfcaa - 0x2dfe00))..']'},							-- 0x2dfcaa - 0x2dfe00 = 'ff's
		{name = 'longEsperBonusDescBase', type = 'uint8_t['..(-(0x2dfe00 - 0x2dffd0))..']'},					-- 0x2dfe00 - 0x2dffd0
		{name = 'longEsperBonusDescOffsets', type = 'uint16_t['..numEsperBonuses..']'},							-- 0x2dffd0 - 0x2dfff2

		-- 0x2e4842 - 0x2e4851     Sprites used for various positions of map character
		{name = 'unknown_2dfff2', type = 'uint8_t['..(-(0x2dfff2 - 0x2e9b14))..']'},

		{name = 'WoBTileProps', type = 'WorldTileProps_t[0x100]'},												-- 0x2e9b14 - 0x2e9d14
		{name = 'WoRTileProps', type = 'WorldTileProps_t[0x100]'},												-- 0x2e9d14 - 0x2e9f14

		{name = 'unknown_2e9f14', type = 'uint8_t['..(-(0x2e9f14 - 0x2ed434))..']'},							-- 0x2e9f14 - 0x2ed434 = looks like more world tile props.

		{name = 'WoBLayoutCompressed', type = 'uint8_t['..(-(0x2ed434 - 0x2f114f))..']'},						-- 0x2ed434 - 0x2f114f     World of Balance Map Data (compressed)
		{name = 'WoBGfxDataCompressed', type = 'uint8_t['..(-(0x2f114f - 0x2f3250))..']'},						-- 0x2f114f - 0x2f3250     World of Balance Tile Graphics (compressed)

		{name = 'unknown_2f3250', type = 'uint8_t['..(-(0x2f3250 - 0x2f4a46))..']'},							-- 0x2f3250 - 0x2f4a46 ... around 0x1800 bytes of *something* ...

		{name = 'WoRGfxDataCompressed', type = 'uint8_t['..(-(0x2f4a46 - 0x2f6a56))..']'},						-- 0x2f4a46 - 0x2f6a56
		{name = 'WoRLayoutCompressed', type = 'uint8_t['..(-(0x2f6a56 - 0x2f9d17))..']'},						-- 0x2f6a56 - 0x2f9d17
		{name = 'SerpentTrenchLayoutCompressed', type = 'uint8_t['..(-(0x2f9d17 - 0x2fb631))..']'},				-- 0x2f9d17 - 0x2fb631
		{name = 'SerpentTrenchGfxDataCompressed', type = 'uint8_t['..(-(0x2fb631 - 0x2fc624))..']'},			-- 0x2fb631 - 0x2fc624

		-- still something else at the end
		-- 0x2fce77 - 0x2fce97 = vector approach palette

		-- - 0x300000
	},
}
local function assertOffset(name, addr)
	assert.eq(ffi.offsetof(game_t, name), addr, name)
end

assertOffset('characterFrameTileOffsets', 0x00ce3a)
assertOffset('characterSpriteOffsetLo', 0x00d0f2)
assertOffset('characterSpriteOffsetHiAndSize', 0x00d23c)
assertOffset('characterPaletteIndexes', 0x02ce2b)
assertOffset('formationSizeOffsets', 0x02d01a)
assertOffset('formationSizes', 0x02d034)
assertOffset('positionedTextOffsets', 0x03c00e)
assertOffset('positionedTextBase', 0x03c2fc)
assertOffset('spells', spellsAddr)
assertOffset('characterNames', characterNamesAddr)
assertOffset('shops', shopsAddr)
assertOffset('metamorphSets', metamorphSetsAddr)
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
assertOffset('rareItemDescOffsets', rareItemDescOffsetAddr)
assertOffset('rareItemNames', rareItemNamesAddr)
assertOffset('rareItemDescBase', rareItemDescBaseAddr)
assertOffset('monsters', monstersAddr)
assertOffset('monsterItems', monsterItemsAddr)
assertOffset('esperDescBase', esperDescBaseAddr)
assertOffset('swordTechNames', swordTechNamesAddr)
assertOffset('monsterSpells', monsterSpellsAddr)
assertOffset('monsterSketches', monsterSketchesAddr)
assertOffset('monsterRages', monsterRagesAddr)
assertOffset('monsterNames', monsterNamesAddr)
assertOffset('monsterAttackNames', monsterAttackNamesAddr)
assertOffset('blitzDescBase', blitzDescBaseAddr)
assertOffset('swordTechDescBase', swordTechDescBaseAddr)
assertOffset('esperDescOffsets', esperDescOffsetsAddr)
assertOffset('esperBonusDescs', esperBonusDescsAddr)
assertOffset('blitzDescOffsets', blitzDescOffsetsAddr)
assertOffset('swordTechDescOffsets', swordTechDescOffsetsAddr)
assertOffset('battleAnimScripts', 0x0fffbe)
assertOffset('battleAnimSets', 0x107fb2)
assertOffset('battleDialog2Offsets', 0x10d000)
assertOffset('battleAnimFrame16x16Tiles', 0x110141)
assertOffset('battleMessageOffsets', 0x11f7a0)
assertOffset('battleAnimGraphicsSets3bpp', 0x120000)
assertOffset('battleAnimPalettes', 0x126000)
assertOffset('itemTypeNames', 0x126f00)
assertOffset('monsterSprites', monsterSpritesAddr)
assertOffset('monsterPalettes', monsterPalettesAddr)
assertOffset('itemNames', itemNamesAddr)
assertOffset('WoBPalettes', 0x12ec00)
assertOffset('WoRPalettes', 0x12ed00)
assertOffset('setzerAirshipPalette', 0x12ee00)
assertOffset('darylAirshipPalette', 0x12ef00)
assertOffset('items', itemsAddr)
assertOffset('espers', espersAddr)
assertOffset('spellDescBase', spellDescBaseAddr)
assertOffset('menuNames', menuNamesAddr)
assertOffset('spellDescOffsets', spellDescOffsetsAddr)
assertOffset('formationMPs', 0x1fb400)
assertOffset('itemColosseumInfos', itemColosseumInfosAddr)
assertOffset('mapTilesetOffsets', mapTilesetOfsAddr)
assertOffset('characterPalettes', 0x268000)
assertOffset('mapNameOffsets', 0x268400)
assertOffset('mapAnimGraphicsLayer3Ofs', 0x26cda0)
assertOffset('mapAnimGraphicsLayer3', 0x26cdc0)
assertOffset('hpIncPerLevelUp', 0x26f4a0)
assertOffset('mpIncPerLevelUp', 0x26f502)
assertOffset('spellNames_0to53', 0x26f567)
assertOffset('esperAttackNames', esperAttackNamesAddr)
assertOffset('mogDanceNames', mogDanceNamesAddr)
assertOffset('monsterSpriteData', monsterSpriteDataAddr)
assertOffset('menuImages', 0x2d0000)
assertOffset('menuWindowPalettes', 0x2d1c00)
assertOffset('characterMenuImages', 0x2d1d00)
assertOffset('itemDescBase', itemDescBaseAddr)
assertOffset('loreDescBase', loreDescBaseAddr)
assertOffset('loreDescOffsets', loreDescOffsetsAddr)
assertOffset('itemDescOffsets', itemDescOffsetsAddr)
assertOffset('characters', charactersAddr)
--assertOffset('expForLevelUp', expForLevelUpAddr)
assertOffset('longEsperBonusDescBase', longEsperBonusDescBaseAddr)
assertOffset('longEsperBonusDescOffsets', longEsperBonusDescOffsetsAddr)

gameC = ffi.cast(ffi.typeof('$*', game_t), rom)

game = setmetatable({}, {
	__index = gameC,
})

game.rom = rom
game.romstr = romstr
game.romsize = romsize

game.numSpells = numSpells
game.numBattleAnimSets = numBattleAnimSets
game.numBattleAnimEffects = numBattleAnimEffects
game.numBattleAnimPalettes = numBattleAnimPalettes
game.numEsperBonuses = numEsperBonuses
game.numEspers = numEspers
game.numMonsters = numMonsters
game.numMonsterSprites = numMonsterSprites
game.numMonsterPalettes = numMonsterPalettes
game.numItems = numItems
game.numItemTypes = numItemTypes
game.numRareItems = numRareItems
game.numRages = numRages
game.numMetamorphSets = numMetamorphSets
game.numExpLevelUps = numExpLevelUps
game.numLevels = numLevels
game.numMenuNames = numMenuNames
game.numCharacters = numCharacters
game.numCharacterSpriteFrames = numCharacterSpriteFrames
game.numCharacterSprites = numCharacterSprites
game.numCharacterPalettes = numCharacterPalettes
game.numMogDances = numMogDances
game.numSwordTechs = numSwordTechs
game.numBlitzes = numBlitzes
game.numLores = numLores
game.numShops = numShops
game.numMapNames = numMapNames
game.numEntranceTriggerOfs = numEntranceTriggerOfs
game.numDialogs = numDialogs
game.numBattleDialogs = numBattleDialogs
game.numBattleDialog2s = numBattleDialog2s
game.numBattleMessages = numBattleMessages
game.numFormations = numFormations
game.numFormationMPs = numFormationMPs
game.numFormationSizeOffsets = numFormationSizeOffsets
game.numFormationSizes = numFormationSizes
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

-- util? ext.ffi or something?
game.countof = function(array)
	return ffi.sizeof(array) / (ffi.cast('uint8_t*', array+1) - ffi.cast('uint8_t*', array+0))
end

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

game.character_t = character_t

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

game.game_t = game_t
game.treasure_t = treasure_t


require 'ff6.maps'(game)


--[[ 0xd1600
					-- Game Genie code:
rom[0xd1614] = 0x97	-- B5FF-8F79
rom[0xd1618] = 0xc1	-- AFFF-8479
rom[0xd1619] = 0x8f -- 6EFF-8459
					-- wait I used +0xC00000 ... didn't seem to work ... whats the correct offset for ROM GG codes?
print(compzstr(ffi.cast('uint8_t*', rom+0xd1600)))
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
	print(number.hex(i), ('%q'):format(compstr(ffi.cast('uint8_t*', test)+i, 1)))
end
print()
--]=]
os.exit()
--]]

return game

end
