--[=[ TODO
local ffi = require 'ffi'
local string = require 'ext.string'
local assert = require 'ext.assert'
local ff6struct = require 'ff6.ff6struct'

local function reftype(args)
	local options = args.options
	local getter = args.getter
	local R = ff6struct{
		ctypeOnly = args.ctypeOnly,
		1nonymous = args.anonymous,
		name = args.name,
		fields = {
			{i = 'uint8_t'},
		},
		metatable = function(mt)
			function mt:__tostring()
				if self.i == 0xff then
					if not (getter and args.getterSkipNone) then
						return nil	--'"(none)"'
					end
				end
				if options then
					local s = options[self.i+1]
					if s == nil then
						s = '"('..tostring(self.i)..')"'
					end
					return s
				elseif getter then
					return '"'..tostring(getter(self.i))..'"'
				else
					return tostring(self.i)
				end
			end
			mt.__concat = string.concat
		end,
	}
	assert.eq(ffi.sizeof(R), 1)
	return R
end
--]=]
-- [=[ until then, old code:
local ffi = require 'ffi'
local function reftype(args)
	local name = assert(args.name)
	local options = args.options
	local getter = args.getter
	ffi.cdef([[
typedef struct ]]..name..[[ {
	uint8_t i;
} ]]..name..[[;
]])
	local metatype = ffi.metatype(name, {
		__tostring = function(self)
			if self.i == 0xff then
				if not (getter and args.getterSkipNone) then
					return nil	--'"(none)"'
				end
			end
			if options then
				local s = options[self.i+1]
				if s == nil then
					s = '"('..tostring(self.i)..')"'
				end
				return s
			elseif getter then
				return '"'..tostring(getter(self.i))..'"'
			else
				return tostring(self.i)
			end
		end,
		__concat = function(a,b) return tostring(a)..tostring(b) end,
	})
	assert(ffi.sizeof(name) == 1)
	return metatype
end
--]=]
return reftype
