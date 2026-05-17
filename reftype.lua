local ffi = require 'ffi'
local string = require 'ext.string'
local assert = require 'ext.assert'
local ff6struct = require 'ff6.ff6struct'

local function reftype(args)
	local options = args.options
	local getter = args.getter
	local R = ff6struct{
		ctypeOnly = args.ctypeOnly,
		anonymous = args.anonymous,
		name = args.name,
		fields = {
			{i = 'uint8_t'},
		},
		metatable = function(mt)
			function mt:__tostring()
				if self.i == 0xff then
					if not (getter and args.getterSkipNone) then
						--return nil
						return '"(none)"'
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
return reftype
