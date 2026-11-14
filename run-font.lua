#!/usr/bin/env luajit

local path = require 'ext.path'
local range = require 'ext.range'
local Image = require 'image'

local function run(game, cmdline)
	cmdline = cmdline or {}
	local rom = game.rom

	do
		local img = Image(8*16, 8*16, 1, 'uint8_t'):clear()
		for j=0,15 do
			for i=0,15 do
				local index = i + 16 * j
				for x=0,7 do
					for y=0,7 do
						img.buffer[(x + 8 * i) + img.width * (y + 8 * j)] =
							((game.font[2*y + 0x10 * index] >> (7-x)) & 1)
							| (((game.font[2*y+1 + 0x10 * index] >> (7-x)) & 1) << 1)
					end
				end
			end
		end
		img.palette = range(0,3):mapi(function(i)
			local l = math.floor(i/3*255)
			return {l,l,l}
		end)
		img:save'font.png'

		-- TODO output font ... font16_20_to_7f
		print('font16 widths: '..range(0,0x7f):mapi(function(i)
			return ('%02x'):format(game.font16_widths[i])
		end):concat' ')
	end

	do
		local w, h = 16, 11
		local img = Image(16*w, 6*(h+1), 1, 'uint8_t'):clear()
		for j=0,5 do
			for i=0,15 do
				local ch = ((j+2) << 4) | i
				for y=0,h-1 do
					for x=0,w-1 do
						local bi = x + w * (y + h * (i + 16 * j))
						local by = bi >> 3
						bi = bi & 7
						img.buffer[
							((w-1-x) + w * i) + img.width * (y + (h+1) * j)
						] = 1 & (game.font16_20_to_7f[by] >> bi)
					end
				end
			end
		end
		img.palette = {
			{0,0,0},
			{255,255,255},
		}
		img:save'font16.png'
	end
end

if select('#', ...) > 0 then
	local cmdline = require 'ext.cmdline'(...)
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	run(game, cmdline)
end

return run
