local ffi = require 'ffi'
local assert = require 'ext.assert'


local uint16_t_p = ffi.typeof'uint16_t*'

return function(game)
	local rom = game.rom

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

		return {
			addr = addr,			-- includes the u16 length at the beginning
			endAddr = endAddr,	-- endAddr - addr - 2 is divisible by 9 (which is the BRR sample size)

			-- maybe redundant
			ptr = rom + addr,
			len = brrLen,

			loopStartOfs = tonumber(game.brrLoopStartOfs[i]),	-- offset into the brr sample, or offset to more info relative to some base address?
			pitchMult = tonumber(game.brrPitchMults[i]),
			adsrDataAddr = tonumber(game.adsrData[i]),	-- offset relative to something?
		}
	end
end
