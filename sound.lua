local ffi = require 'ffi'
local class = require 'ext.class'
local assert = require 'ext.assert'
local math = require 'ext.math'

local int16_t = ffi.typeof'int16_t'
local int16_t_2 = ffi.typeof'int16_t[2]'
local int16_t_ar = ffi.typeof'int16_t[?]'
local uint16_t_p = ffi.typeof'uint16_t*'


local function clampbits(x, b)
	return math.clamp(x, bit.lshift(-1, b-1), bit.lshift(1, b-1)-1)
end


return function(game)
	local rom = game.rom

	local BRR = class()

	game.BRR = BRR

	--[[
	get info associated with a single BRR
	--]]
	function game.getBRR(i)
		if i < 0 or i >= game.numBRRSamples then return end

		-- addrs are in ascending order
		local addr = game.brrSampleOfs[i]:value()
		assert.ne(bit.band(0xc00000, addr), 0)
		addr = bit.band(bit.bnot(0xc00000), addr)

		-- first two bytes fo the sampleAddr is the length-in-bytes of the brr sample
		local brrLen = ffi.cast(uint16_t_p, rom+addr)[0]
		assert.eq(brrLen % 9, 0, "why isn't the brr length aligned to brr frames?")

		local endAddr = addr + brrLen + 2	-- length excludes the 2 bytes of length info

		-- if loopStartPtr is only 16bit then it can't span the full range of the brrSample data, which covers 0x31245 bytes
		-- so it must be an offset into the structure
		assert.eq(game.brrLoopStartOfs[i] % 9, 0, "why isn't the brr loop aligned to brr frames?")

		-- then the brr data should decode until it gets to a loop frame, and ideally that'll be right before the next brr's address

		return setmetatable({
			addr = addr,			-- includes the u16 length at the beginning
			endAddr = endAddr,	-- endAddr - addr - 2 is divisible by 9 (which is the BRR sample size)

			-- maybe redundant
			ptr = rom + addr,
			len = brrLen,

			loopStartOfs = tonumber(game.brrLoopStartOfs[i]),	-- offset into the brr sample, or offset to more info relative to some base address?
			pitchMult = tonumber(game.brrPitchMults[i]),
			adsrDataAddr = tonumber(game.adsrData[i]),	-- offset relative to something?
		}, BRR)
	end


	-- return args for require 'audio.io.wav'():save(...) - except .filename
	function BRR:getWAV()

		local numFrames = self.len / 9
		-- write out the wav too
		-- that means converting it from brr to wav
		-- that means ... 16bpp samples, x16 samples per brr-frame
		local numSamples = 16 * numFrames
		local wavData = int16_t_ar(numSamples)
		local brrptr = self.ptr + 2
		local wavptr = wavData + 0
		local lastSample = int16_t_2({0,0})	-- for filters
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
				sample = ffi.cast(int16_t, sample)
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
		assert.eq(brrptr, rom + self.endAddr)
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

		return {
			ctype = int16_t,
			channels = 1,
			data = wavData,
			size = numSamples * ffi.sizeof(int16_t),
			freq = 32000,
			-- not part of audio.io.wav:
			numSamples = numSamples,
			numFrames = numFrames,
		}
	end
end
