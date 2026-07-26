#!/usr/bin/env luajit
local ffi = require 'ffi'
local AudioWAV = require 'audio.io.wav'
local path = require 'ext.path'
local table = require 'ext.table'
local string = require 'ext.string'
local math = require 'ext.math'
local range = require 'ext.range'
local assert = require 'ext.assert'

local function runSound(game, cmdline)
	cmdline = cmdline or {}
	local rom = game.rom
	local romsize = game.romsize

	--[[
	TODO
	output font ...
		font16_20_to_7f

	output audio ...
		spcMainCodeLoopLen
		spcMainCode
		spcMainCode
		brrSampleOfs
		brrLoopStartOfs
		brrPitchMults
		adsrData
		brrSampleBase	... this also has instrument data etc in it?
	--]]

	print('spcMainCodeLoopLen = '..game.spcMainCodeLoopLen)
	--[[
	print('spcMainCode = '..
		range(0,math.min(game.spcMainCodeLoopLen, ffi.sizeof(game.spcMainCode))-1)
		:mapi(function(i) return (' %02x'):format(game.spcMainCode[i]) end):concat()
	)
	--]]

	local brrs = table()
	print'brr info:'
	for i=0,game.numBRRSamples-1 do
		-- addrs are in ascending order
		local brr = game.getBRR(i)
		brrs[i+1] = brr

		io.write(('#%02d: '):format(i))

		if i > 0 then
			-- brr data is contiguous
			assert.eq(brr.addr, brrs[i].endAddr, "failed for brr sample "..i)
		end

		io.write(' sampleAddr: '..('0x%06x'):format(brr.addr))
		io.write(' - '..('0x%06x'):format(brr.endAddr))
		io.write(' length: '..('0x%04x'):format(brr.len))

		-- if loopStartPtr is only 16bit then it can't span the full range of the brrSample data, which covers 0x31245 bytes
		-- so it must be an offset into the structure
		assert.eq(game.brrLoopStartOfs[i] % 9, 0, "why isn't the brr loop aligned to brr frames?")
		io.write(' loopStartOfs: '..('0x%04x'):format(brr.loopStartOfs))
		io.write(' pitchMult: '..('0x%04x'):format(brr.pitchMult))
		io.write(' adsrData: '..('0x%04x'):format(brr.adsrDataAddr))

		print()
		-- then the brr data should decode until it gets to a loop frame, and ideally that'll be right before the next brr's address
	end

	local brrpath = path'brr'
	brrpath:mkdir()
	local wavpath = path'wav'
	wavpath:mkdir()
	local wavSamples = table()
	print'brr data:'
	for i=0,game.numBRRSamples-1 do
		local brr = brrs[i+1]

		local startAddr = brr.addr + 2			-- skip past the length info
		local len = brr.len
		local numFrames = len / 9
		local endAddr = startAddr + len
		local calcdEndAddr
		if i < game.numBRRSamples-1 then
			calcdEndAddr = brrs[i+2].addr
		else
			calcdEndAddr = (ffi.cast('uint8_t*', game.brrSampleBase) + ffi.sizeof(game.brrSampleBase) - rom)
		end
		assert.eq(endAddr, calcdEndAddr)	-- perfectly fits
		print(('#%02d: '):format(i)
			..('0x%06x - 0x%06x: '):format(startAddr - 2, endAddr)
			..('(%4d brr frames)'):format(numFrames)
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
		brrpath(('%02X'):format(i+1)..'.brr'):write(ffi.string(rom + startAddr, len))

		-- now save the wav
		local wavArgs = brr:getWAV()
		local basename = ('%02X'):format(i+1)
		wavArgs.filename = wavpath(basename..'.wav').path
		AudioWAV():save(wavArgs)
		wavSamples[i+1] = wavArgs

		-- and its associated info
		wavpath(basename..'.txt'):write(table{
			('adsr=0x%04X'):format(brr.adsrDataAddr),
			('pitch=0x%04X'):format(brr.pitchMult),
			('loopOffset=0x%04X/9*32'):format(brr.loopStartOfs),
		}:concat'\n'..'\n')
		--[[ debug plot it so i can see the waveform.
		require'gnuplot'{
			terminal = 'svg size '..math.floor(4*numSamples)..',512',
			output = wavpath(basename..'.svg').path,
			--samples = numSamples,
			style = 'data linespoints',
			unset = {'colorbox'},
			range = {numSamples/wavArgs.freq, 1},
			cbrange = {0,1},
			data = {
				range(0,numSamples-1):mapi(function(j) return j/wavArgs.freq end),
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


	--[[
	now to hack out the songs
	this is from http://www.rpglegion.com/ff6/hack/ff3info.txt
	TODO find the table in the ROM
	ex: https://www.ff6hacking.com/forums/thread-493.html
	battle intruments are at 0x054614, battle song is at 0x90043 (is this on v1.0?)
	in v1.1 instruments are at 0x54414 and song is at 0x8fe43
	aha but he's including header
	ff3info says the instruments are 36 bytes each

	found it at: 0x053e96
	--]]
--[[ if you want song names ...
	local songAddrs = table{
		0x85c7a,	-- Song 0 data (Silence)
		0x85ca0,	-- Song 1 data (Prelude)
		0x983db,	-- Song 2 data (Opening Theme #1)
		0x8b49d,	-- Song 3 data (Opening Theme #2)
		0x8c882,	-- Song 4 data (Opening Theme #3)
		0x8641e,	-- Song 5 data (Awakening)
		0x86733,	-- Song 6 data (Terra)
		0x86d69,	-- Song 7 data (Shadow)
		0x870c5,	-- Song 8 data (Strago)
		0x874bf,	-- Song 9 data (Gau)
		0x878f8,	-- Song 10 data (Edgar and Sabin)
		0x87caf,	-- Song 11 data (Coin Song)
		0x88028,	-- Song 12 data (Cyan)
		0x88438,	-- Song 13 data (Locke)
		0x8889a,	-- Song 14 data (Forever Rachel)
		0x88bed,	-- Song 15 data (Relm)
		0x88f56,	-- Song 16 data (Setzer)
		0x8956f,	-- Song 17 data (Epitaph)
		0x89829,	-- Song 18 data (Celes)
		0x89b62,	-- Song 19 data (Techno de Chocobo)
		0x8a5d4,	-- Song 20 data (The Decisive Battle)
		0x8ad36,	-- Song 21 data (Johnny C. Bad)
		0x8b7b8,	-- Song 22 data (Kefka)
		0x8bfe8,	-- Song 23 data (The Mines of Narshe)
		0x8c24c,	-- Song 24 data (Cave Theme)
		0x8cec1,	-- Song 25 data (Wild West)
		0x8d330,	-- Song 26 data (Save Them!)
		0x8da56,	-- Song 27 data (The Empire Gestahl)
		0x8ddbf,	-- Song 28 data (Troops March On)
		0x8e16b,	-- Song 29 data (Under Martial Law)
		0x8e357,	-- Song 30 data (Waterfall)
		0x8e3e2,	-- Song 31 data (Metamorphosis)
		0x8ea48,	-- Song 32 data (The Phantom Train #1)
		0x8efa6,	-- Song 33 data (Another World of Beasts)
		0x8f472,	-- Song 34 data (Grand Finale #2)
		0x8fa15,	-- Song 35 data (Mt. Koltz)
		0x8fe43,	-- Song 36 data (Battle Theme)
		0x9054b,	-- Song 37 data (Fanfare, slow)
		0x905e9,	-- Song 38 data (The Wedding Waltz #1)
		0x90a66,	-- Song 39 data (Aria de Mezzo Caraterre)
		0x990b6,	-- Song 40 data (The Serpent Trench)
		0x993a2,	-- Song 41 data (Slam Shuffle)
		0x9149c,	-- Song 42 data (Kids Run Through the City Corner)
		0x98e14,	-- Song 43 data (???, Crazy Old Man's House)
		0x9975f,	-- Song 44 data (Grand Finale #1)
		0x91a4c,	-- Song 45 data (Gogo)
		0x91edd,	-- Song 46 data (Returners)
		0x9268f,	-- Song 47 data (Battle Fanfare)
		0x92997,	-- Song 48 data (Umaro)
		0x92e0b,	-- Song 49 data (Mog)
		0x93258,	-- Song 50 data (The Unforgiven)
		0x937ff,	-- Song 51 data (The Fierce Battle)
		0x93fae,	-- Song 52 data (The Day After)
		0x94465,	-- Song 53 data (Blackjack)
		0x94ab3,	-- Song 54 data (Catastrophe)
		0x94d6f,	-- Song 55 data (The Magic House)
		0x95316,	-- Song 56 data (Nighty Night)
		0x953db,	-- Song 57 data (Wind)
		0x954c5,	-- Song 58 data (Windy Shores)
		0x95557,	-- Song 59 data (Dancing Mad #1)
		0x962c9,	-- Song 60 data (The Raft and the Flowing River)
		0x963cd,	-- Song 61 data (Spinach Rag)
		0x96903,	-- Song 62 data (Rest in Peace)
		0x96a6e,	-- Song 63 data (Train Running)
		0x96b19,	-- Song 64 data (The Dream of a Train)
		0x96bc2,	-- Song 65 data (Overture #1)
		0x970da,	-- Song 66 data (Overture #2)
		0x971c9,	-- Song 67 data (Overture #3)
		0x97a06,	-- Song 68 data (The Wedding Waltz #2)
		0x97ceb,	-- Song 69 data (The Wedding Waltz #3)
		0x97f7c,	-- Song 70 data (The Wedding Waltz #4)
		0x98842,	-- Song 71 data (Devil's Lab)
		0x98c99,	-- Song 72 data (Fire!)
		0x98ce8,	-- Song 73 data (Machine Running)
		0x98d85,	-- Song 74 data (Inside the Burning House)
		0x997df,	-- Song 75 data (New Continent)
		0x99dbf,	-- Song 76 data (Searching for Friends)
		0x9a24f,	-- Song 77 data (Fanatics)
		0x9a3d8,	-- Song 78 data (Last Dungeon)
		0x9ac51,	-- Song 79 data (Dark World)
		0x9ae9f,	-- Song 80 data (Dancing Mad #4.2)
		0x85c7a,	-- Song 81 data (Silence)
		0x9b9b9,	-- Song 82 data
		0x9baf9,	-- Song 83 data (Ending Theme #1)
		0x9df3f,	-- Song 84 data (Ending Theme #2)
	}
--]]

	local songAddrs = table()
	for i=0,game.numSongs-1 do
		local addr = game.songDataOfs[i]:value()
		assert.ne(0, bit.band(addr, 0xc00000))
		addr = addr - 0xc00000
--print('song #'..i..' addr '..('0x%06x'):format(addr))
--assert.eq(addr, songAddrs[i+1], "song "..i)
		songAddrs:insert(addr)
	end

	local sortedSongAddrs = table(songAddrs):mapi(function(addr) return true, addr end):keys():sort()
	local musicPath = path'music'
	musicPath:mkdir(true)
	for i=0,#songAddrs-1 do
		local lenAddr = songAddrs[i+1]
		-- length of track minus 2 bytes (for the length of the track)
		local len = ffi.cast('uint16_t*', rom + lenAddr)[0]

		--[[ this matches endAddr except last track...
		local sortedIndex = sortedSongAddrs:find(startAddr)
		local endAddr = sortedIndex
			and sortedSongAddrs[sortedIndex+1]
			or (ffi.offsetof(game.Game, 'songData') + ffi.sizeof(game.songData))
		-- now ...... what's the format?
		local len2 = endAddr - startAddr
		print('len', len, 'len2', len2)
		--]]
		-- [[
		local startAddr = lenAddr + 2
		local ptr = rom + startAddr
		local endAddr = startAddr + len
		--]]
print('song #'..i
	..' addr '..('0x%06x'):format(lenAddr)..' - '..('0x%06x'):format(endAddr)
	..' len', '0x'..bit.tohex(endAddr - lenAddr))
		-- there should be no more than 11 instruments per song
		local maxInstruments = 11
		local instrumentsPtr = ffi.cast('uint16_t*', game.songInstruments + i)
		local instruments = table()
		for i=0,15 do
			local value = instrumentsPtr[i]
			if i >= maxInstruments then
				assert.eq(value, 0)
			else
				instruments:insert(value)
			end
		end
		assert.len(instruments, maxInstruments)
print('instruments: '..instruments:mapi(function(value) return ('%04x'):format(value) end):concat' ')

		local d = ffi.string(ptr-2, len+2)
		musicPath('song'..i..'.spc'):write(d)
		musicPath('song'..i..'.hex'):write(string.hexdump(d))

		--[[
SPC song data:

00-0A       Play note, frequency 0
10-1A       Play note, frequency 1
20-2A       Play note, frequency 2
30-3A       Play note, frequency 3
40-4A       Play note, frequency 4
50-5A       Play note, frequency 5
60-6A       Play note, frequency 6
70-7A       Play note, frequency 7
80-8A       Play note, frequency 8
90-9A       Play note, frequency 9
A0-AA       Play note, frequency 10
	-- "play note at frequency" but notes are frequencies.  they are a frequency of `f0 * 2^(i/12)`
	-- does 'note' in this case mean 'instrument'?
B0-BA       Play silence
C4 xx       Set volume of channel to xx (00-7F), higher values == greater volume.
	-- how to change channel? or can you?
C5 ss xx    Fade volume to xx at speed ss
C6 xx       Pan song left->right as xx (00-7F) increases
C7 xx       Pan song right->left as xx (00-7F) increases
//C8            Created an odd ascending portamento effect.
CD xx       Pan repeatedly back and forth at speed xx (00-FF), higher values == slower panning.
CE          Disable pan loop
D7          Raise octave by 1
D8          Lower octave by 1
DE          Disables DF.
//DF            Created an odd volume "cut" very shortly into the note...
//E4            Creates a portamento effect linking all following notes with pitch slides.
E5          Disables E4.
E6          Enable vibrato.
E7          Disable vibrato.
F0 xx       Set tempo to xx (00-FF), higher values == faster tempo.
F1 ss xx    Fade tempo to xx at speed ss
F2 xx       Set echo to xx
F3 ss xx    Fade echo to xx at speed ss

TODO what is MIDI's format?  how to convert?
or should I bake it into WAV? then convert to mp3?
and how about exporting to numo9's music format?
		--]]

		local cmds = table()
		local endptr = rom + endAddr
		local ptr = rom + startAddr
		while ptr < endptr do
			local cmd = ptr[0]
			local lo = bit.band(0xf, cmd)
			local hi = bit.band(0xf, bit.rshift(cmd, 4))
			if lo < 11 and hi < 11 then
				cmds:insert('play instrument '..lo..' at note '..hi)	-- 
			elseif cmd >= 0xb0 and cmd <= 0xba then
				cmds:insert('silence '..cmd)
			elseif cmd == 0xc4 then
				cmds:insert('set volume to '..ptr[1])	-- last bit means something
				ptr = ptr + 1
			elseif cmd == 0xc5 then
				cmds:insert('fade volume to '..ptr[2]..' at speed '..ptr[1])
				ptr = ptr + 2
			elseif cmd == 0xc6 then
				cmds:insert('pan song left->right as '..ptr[1]..' increases')	-- last bit means something
				ptr = ptr + 1
			elseif cmd == 0xc7 then
				cmds:insert('pan song right->left as '..ptr[1]..' increases')	-- last bit means something
				ptr = ptr + 1
			elseif cmd == 0xcd then
				cmds:insert('pan repeatedly back and forth at speed '..ptr[1])
				ptr = ptr + 1
			elseif cmd == 0xce then
				cmds:insert('disable pan loop')
			elseif cmd == 0xd7 then
				cmds:insert('raise octave by 1')
			elseif cmd == 0xd8 then
				cmds:insert('lower octave by 1')
			elseif cmd == 0xe6 then
				cmds:insert('enable vibrato')
			elseif cmd == 0xe7 then
				cmds:insert('disable vibrato')
			elseif cmd == 0xf0 then
				cmds:insert('set tempo to '..ptr[1])
				ptr = ptr + 1
			elseif cmd == 0xf1 then
				cmds:insert('set tempo to '..ptr[2]..' at speed '..ptr[1])
				ptr = ptr + 2
			elseif cmd == 0xf2 then
				cmds:insert('set echo to '..ptr[1])
				ptr = ptr + 1
			elseif cmd == 0xf1 then
				cmds:insert('fade echo to '..ptr[2]..' at speed '..ptr[1])
				ptr = ptr + 2
			else
				cmds:insert('unknown '..('$%02X'):format(cmd))
			end
			ptr = ptr + 1
		end
		musicPath('song'..i..'-cmds.txt'):write(cmds:concat'\n'..'\n')

		-- to port this over to numo9 all I gotta do is remap the instruments, then convert note freq to freq time scalar
	end

-- [=[ testing
	local samplesPerSec = 32000	-- hz
	local secondsPerNote = .5
	local samplesPerNote = math.floor(samplesPerSec * secondsPerNote)
	local lengthInNotes = 8
	local lengthInSeconds = math.floor(secondsPerNote * lengthInNotes)
	local numSamples = math.floor(samplesPerSec * lengthInSeconds)	-- 1 second worth
	local wavData = ffi.new('int16_t[?]', numSamples)
	local maxAmpl = 32767
	local notes = {0, 2, 4, 5, 7, 9, 11, 12}
	for i=0,numSamples-1 do
		local t = i / samplesPerSec
		local noteIndex = math.floor(t / secondsPerNote)
		local sampleInNote = i % samplesPerNote
		local freqNote = notes[1+noteIndex]
		local freq = 2^(freqNote/12)
		local vol = 1
		--[[ pure sine wave
		wavData[i] = math.floor(
			vol
			* maxAmpl
			* math.sin(
				2 * math.pi
				* t
				* freq
				* 440
			)
		)
		--]]
		-- [[ one of our samples
		local sample = wavSamples[12]
		local srci = math.floor(sampleInNote * freq)
		wavData[i] = srci < sample.numSamples and sample.data[srci] or 0
		--]]
	end
	AudioWAV():save{
		filename = 'test.wav',
		ctype = 'int16_t',
		channels = 1,
		data = wavData,
		size = numSamples * ffi.sizeof'int16_t',
		freq = samplesPerSec,
	}
--]=]
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local cmdline = require 'ext.cmdline'(...)
	-- hmm if luajit does get ... upon require then ff6 will get passed a bad file
	-- maybe xpcall and bailout on fail?
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	runSound(game, cmdline)
end

return runSound
