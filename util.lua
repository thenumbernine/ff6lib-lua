-- util is stuff used by ff6.lua and by anything else that doesn't need the ROM to be loaded to work.
local ffi = require 'ffi'
local assert = require 'ext.assert'
local string = require 'ext.string'
local table = require 'ext.table'
local struct = require 'struct'
local ff6struct = require 'ff6.ff6struct'


local uint8_t = ffi.typeof'uint8_t'
local uint8_t_p = ffi.typeof'uint8_t*'
local uint16_t = ffi.typeof'uint16_t'


local function countof(array)
	return ffi.sizeof(array) / (ffi.cast(uint8_t_p, array+1) - ffi.cast(uint8_t_p, array+0))
end

local function arrayType(baseType, size)
	size = math.floor(size)
	return ffi.typeof('$['..size..']', baseType)
end

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
		mt.setValue = function(self, x)
			self.lo = bit.band(x, 0xffff)
			self.hi = bit.band(bit.rshift(x, 16), 0xff)
		end
	end,
}
assert.eq(ffi.sizeof(uint24_t), 3)

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
		metatable = function(mt)
			-- me being lazy in how to do bitflag-detect.
			-- because this is a ctype you'll have to do op.safeindex which is an xpcall which is expensive every frame, so ... try to get rid of imgui
			mt.isBitflags = true
			mt.options = args.options
			mt.baseType = ctype
		end,
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

--[[
local Targeting = bitflagtype{
	options = {
		'chooseTarget',			-- can move cursor?
		'cantChangeParty',

		'everyone',		-- auto select all?
		'groupDefault',	-- auto select one side

		'autoConfirm',
		'canToggleMultiple',
		'startOnEnemy',
		'roulette',
	},
}
--]]
-- [[
local Targeting = ff6struct{
	ctypeOnly = true,
	fields = {
		{chooseTarget = 'uint8_t:1'},
		{cantChangeParty = 'uint8_t:1'},
		{targetMultiple = 'uint8_t:2'},
		{autoConfirm = 'uint8_t:1'},
		{canToggleMultiple = 'uint8_t:1'},
		{startOnEnemy = 'uint8_t:1'},
		{roulette = 'uint8_t:1'},
	},
	metatable = function(mt)
		mt.__tostring = function(self)
			local s = '{'
			local sep = ''
			for fieldname in self:fielditer() do
				local value = self[fieldname]
				if fieldname == 'targetMultiple' then
					if value == 0 then
						s = s .. sep .. 'targetUnit=true'
						sep = ', '
					elseif value == 1 then
						s = s .. sep .. 'targetAll=true'
						sep = ', '
					elseif value == 2 then
						s = s .. sep .. 'targetGroup=true'
						sep = ', '
					elseif value == 3 then
						s = s .. sep .. 'targetParty=true'
						sep = ', '
					end
				else
					if value ~= 0 then
						s = s .. sep .. fieldname .. '=true'
						sep = ', '
					end
				end
			end
			s = s .. '}'
			return s
		end
	end
}
--]]

local Effect1 = bitflagtype{
	options = {
		'Dark',
		'Zombie',
		'Poison',
		'Magitek',
		'Clear',
		'Imp',
		'Petrify',
		'Mortal',
	},
}

local Effect2 = bitflagtype{
	options = {
		'Countdown',
		'NearFatal',
		'Image',
		'Mute',
		'Berzerk',
		'Confuse',
		'Seizure',
		'Sleep',
	},
}

local Effect3 = bitflagtype{
	options = {
		'Float',
		'Regen',
		'Slow',
		'Haste',
		'Stop',
		'Shell',
		'Safe',
		'Reflect',
	},
}

local Effect4 = bitflagtype{
	options = {
		'Rage',
		'Frozen',
		'Reraise',
		'Morphed',
		'Casting',
		'RemovedFromBattle',
		'Interceptor',
		'Float',
	},
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

local function gamestrtype(args)
	local size = assert(args.size)
	local typeobj = struct{
		ctypeOnly = true,
		fields = {
			{name='ptr', type=arrayType(uint8_t, size)},
		},
		metatable = function(mt)
			mt.__tostring = function(self)
				return string.trim(gamestr(self.ptr, size))
			end
			mt.__concat = string.concat
		end,
	}
	assert.eq(ffi.sizeof(typeobj), size)
	return typeobj
end

local function makefixedstr(n)
	return gamestrtype{
		size = n,
	}
end

local Str6 = makefixedstr(6)
local Str12 = makefixedstr(12)

local CharacterName = Str6
local SwordTechName = Str12
local MogDanceName = Str12

return {
	countof = countof,
	arrayType = arrayType,
	uint24_t = uint24_t,
	EquipFlags = EquipFlags,
	Element = Element,
	Targeting = Targeting,
	Effect1 = Effect1,
	Effect2 = Effect2,
	Effect3 = Effect3,
	Effect4 = Effect4,
	gamestr = gamestr,
	makefixedstr = makefixedstr,
	Str6 = Str6,
	Str12 = Str12,
	CharacterName = CharacterName,
	SwordTechName = SwordTechName,
	MogDanceName = MogDanceName,
}
