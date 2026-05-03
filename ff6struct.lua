local table = require 'ext.table'
local struct = require 'struct'

return function(args)
	local s = struct{
		notostring = args.notostring,
		tostringFields = true,
		tostringOmitFalse = true,
		tostringOmitNil = true,
		tostringOmitEmpty = true,
		name = args.name,

		anonymous = args.anonymous,
		ctypeOnly = args.ctypeOnly,

		packed = true,
		union = true,
		fields = {
			{name='s', type='uint8_t[1]', no_iter=true},
			{type=struct{
				notostring = args.notostring,
				tostringFields = true,
				tostringOmitFalse = true,
				tostringOmitNil = true,
				tostringOmitEmpty = true,

				-- fields with ctypes need names
				-- that means anonymous-inner classes must not be ctypeOnly
				anonymous = true,
				-- but ctypeOnly i.e. using ffi.typeof is contagious as well (right? wrong?)

				packed = true,
				fields = table.mapi(args.fields, function(kv)
					local name, ctype = next(kv)
					return {
						name = name,
						type = ctype,
					}
				end),
			}},
		},
		metatable = function(m, ...)
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
			if args.metatable then
				args.metatable(m, ...)
			end
		end,
	}
	return s
end
