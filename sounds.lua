local ffi = require 'ffi'
local AudioWAV = require 'audio.io.wav'
local path = require 'ext.path'
local table = require 'ext.table'
local range = require 'ext.range'
local assert = require 'ext.assert'

return function(rom, game, romsize)

local brrAddrs = table()
local brrLengths = table()
print'brr info:'
for i=0,game.numBRRSamples-1 do
	-- addrs are in ascending order
	local brrAddr = bit.bor(
		game.brrSamplePtrs[i].lo,
		bit.lshift(game.brrSamplePtrs[i].hi, 16)
	)
	assert.ne(bit.band(0xc00000, brrAddr), 0)
	brrAddr = brrAddr - 0xc00000
	brrAddrs[i] = brrAddr
	io.write(('#%02d: '):format(i))
	io.write(' samplePtr: '..('0x%06x'):format(brrAddr))

	-- first two bytes fo the samplePtr is the length-in-bytes of the brr sample
	brrLengths[i] = ffi.cast('uint16_t*', rom+brrAddr)[0]
	assert.eq(brrLengths[i] % 9, 0, "why isn't the brr length aligned to brr frames?")
	io.write(' length: '..('0x%04x'):format(brrLengths[i]))

	-- if loopStartPtr is only 16bit then it can't span the full range of the brrSample data, which covers 0x31245 bytes
	-- so it must be an offset into the structure
	assert.eq(game.brrLoopStartOfs[i] % 9, 0, "why isn't the brr loop aligned to brr frames?")
	io.write(' loopStartPtr: '..('0x%04x'):format(tonumber(game.brrLoopStartOfs[i])))
	io.write(' brrPitchMults: '..('0x%04x'):format(tonumber(game.brrPitchMults[i])))
	io.write(' adsrData: '..('0x%04x'):format(tonumber(game.adsrData[i])))

	print()
	-- then the brr data should decode until it gets to a loop frame, and ideally that'll be right before the next brr's address
end
local brrpath = path'brr'
brrpath:mkdir()
local wavpath = path'wav'
wavpath:mkdir()
print'brr data:'
for i=0,game.numBRRSamples-1 do
	local startAddr = brrAddrs[i] + 2			-- skip past the length info
	local len = brrLengths[i]
	local numFrames = len / 9
	local endAddr = startAddr + len
	local calcdEndAddr
	if i < game.numBRRSamples-1 then
		calcdEndAddr = brrAddrs[i+1]
	else
		calcdEndAddr = (ffi.cast('uint8_t*', game.brrSamples) + ffi.sizeof(game.brrSamples) - rom)
	end
	assert.eq(endAddr, calcdEndAddr)	-- perfectly fits
	print(('#%02d: '):format(i)
		..('$%06x-$%06x: '):format(startAddr, endAddr)
		..('(%4d brr frames) '):format(numFrames)
		--[[ this is a lot
		..range(0, len-1):mapi(function(i)
			local s = ('%02x'):format(rom[startAddr + i])
			if i % 9 == 0 then s = '['..s end
			if i % 9 == 8 then s = s..']' end
			return s
		end):concat' '
		--]]
	)

	-- write out the brr
	-- should I put pitch, adsr, loop info at the start of the brr sample?
	brrpath:write(i..'.brr', ffi.string(rom + startAddr, len))
	-- write out the wav too
	-- that means converting it from brr to wav
	-- that means ... 16bpp samples, x16 samples per brr-frame
	local numSamples = 16 * numFrames
	local wavData = ffi.new('int16_t[?]', numSamples)
	local brrptr = rom + startAddr
	local wavptr = wavData + 0
	local lastSample = ffi.new('int16_t[2]', {0,0})	-- for filters
	local function clampbits(x, b)
		return math.clamp(x, bit.lshift(-1, b-1), bit.lshift(1, b-1)-1)
	end
	for j=0,numFrames-1 do
		local endflag = bit.band(brrptr[0], 1) ~= 0
		local loopflag = bit.band(brrptr[0], 2) ~= 0
		local decodeFilter = bit.band(bit.rshift(brrptr[0], 2), 3)	-- 0-3 = decode filter = combine nibble with previous nibbles ...
		local shift = bit.band(bit.rshift(brrptr[0], 4), 0xf)
		-- https://wiki.superfamicom.org/bit-rate-reduction-(brr)
		-- https://github.com/Optiroc/BRRtools/blob/master/src/brr.c
		-- https://github.com/boldowa/snesbrr/blob/master/src/brr/BrrCodec.cpp
		for k=0,15 do
			local sample
			if bit.band(k,1) == 0 then
				sample = bit.band(bit.rshift(brrptr[1+bit.rshift(k,1)], 4), 0xf)
			else
				sample = bit.band(brrptr[1+bit.rshift(k,1)], 0xf)
			end

			-- sample is now 0 to 15 , representing a 4-bit-signed -8 to +7
			--if sample >= 8 then sample = sample - 16 end
			sample = bit.bxor(sample, 8) - 8
			-- sample is now -8 to +7

			-- [[ invalid shift
			if shift > 0xc then
				sample = bit.band(sample, bit.bnot(0x7ff))
			else
				sample = bit.lshift(sample, shift)
				-- why is this? maybe to do with the filter using the post-sampled value for previous frame values?
				sample = bit.arshift(sample, 1)
			end
			--]]

			local sampleBeforeFilter = sample
			-- [[ https://github.com/boldowa/snesbrr/blob/master/src/brr/BrrCodec.cpp
			if decodeFilter == 0 then
			elseif decodeFilter == 1 then
				sample = sample
					+ lastSample[0]
					- bit.arshift(lastSample[0], 4)
			elseif decodeFilter == 2 then
				sample = sample
					+ bit.lshift(lastSample[0], 1)
					+ bit.arshift(-(lastSample[0] + bit.lshift(lastSample[0], 1)), 5)
					- lastSample[1]
					+ bit.arshift(lastSample[1], 4)
				sample = clampbits(sample, 16)
			elseif decodeFilter == 3 then
				sample = sample +
					  bit.lshift(lastSample[0], 1)
					+ bit.arshift(-(lastSample[0] + bit.lshift(lastSample[0], 2) + bit.lshift(lastSample[0], 3)), 6)
					- lastSample[1]
					+ bit.arshift(lastSample[1] + bit.lshift(lastSample[1], 1), 4)
				sample = clampbits(sample, 16)
			else
				error'here'
			end
			--]]

			-- [[ snesbrr: "wrap to 15 bits, sign-extend to 16 bits"
			sample = bit.arshift(bit.lshift(sample, 1), 1)
			sample = ffi.cast('int16_t', sample)
			--]]

			lastSample[1] = lastSample[0]
			lastSample[0] = sample

			--sample = bit.arshift(sample * 0x7f, 7)	-- volume ... ?
			wavptr[0] = bit.lshift(sample, 1)

			--wavptr[0] = bit.lshift(sample, 1)
			--lastSample[0], lastSample[1] = sampleBeforeFilter, lastSample[0]
			wavptr = wavptr + 1
		end
		brrptr = brrptr + 9
	end
	assert.eq(wavptr, wavData + numSamples)
	assert.eq(brrptr, rom + endAddr)
	-- [[ now gaussian filter
	do
		local prev = (372 + 1304) * wavData[0] + 372 * wavData[1]
		for i=1,numSamples-2 do
			local k0 = 372 * (wavData[i-1] + wavData[i+1])
			local k = 1304 * wavData[i]
			wavData[i-1] = bit.arshift(prev, 11)
			prev = k0 + k
		end
		local last = 372 * wavData[numSamples-2] + (1304 + 372) * wavData[numSamples-1]
		wavData[numSamples-2] = bit.arshift(prev, 11)
		wavData[numSamples-1] = bit.arshift(last, 11)
	end
	--]]
	-- now save the wav
	local basename = ('%02X'):format(i+1)
	local freq = 32000
	AudioWAV():save{
		filename = wavpath(basename..'.wav').path,
		ctype = 'int16_t',
		channels = 1,
		data = wavData,
		size = numSamples * ffi.sizeof'int16_t',
		freq = freq,
	}
	-- and its associated info
	wavpath(basename..'.txt'):write(table{
		('adsr=0x%04X'):format(tonumber(game.adsrData[i])),
		('pitch=0x%04X'):format(tonumber(game.brrPitchMults[i])),
		('loopOffset=0x%04X/9*32'):format(tonumber(game.brrLoopStartOfs[i])),
	}:concat'\n'..'\n')
	--[[ debug plot it so i can see the waveform.
	require'gnuplot'{
		terminal = 'svg size '..math.floor(4*numSamples)..',512',
		output = wavpath(basename..'.svg').path,
		--samples = numSamples,
		style = 'data linespoints',
		unset = {'colorbox'},
		range = {numSamples/freq, 1},
		cbrange = {0,1},
		data = {
			range(0,numSamples-1):mapi(function(j) return j/freq end),
			range(0,numSamples-1):mapi(function(j) return tonumber(wavData[j])/32768 end),
			range(0,numSamples-1):mapi(function(j)
				local brraddr = j/16*9
				return brraddr >= game.brrLoopStartOfs[i] and .5 or 0
			end)
		},
		{using='1:2:3', notitle=true, palette=true},
	}
	--]]
end


end
