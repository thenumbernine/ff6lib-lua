#!/usr/bin/env luajit
--[[
source is everything8215 ff6/notes/battle-lists.txt:952 Battle Animation Commands
--]]
local ffi = require 'ffi'
local path = require 'ext.path'
local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local number = require 'ext.number'

local function outputBattleAnimScripts(game)
	local rom = game.rom
	local romsize = game.romsize
	local countof = game.countof

	-- space for 660 entries
	-- but if they are 1:1 with battleAnimEffects
	--  then just 650 entries
	local battleScriptAddrs = table()
	print()
	--for i=0,660-1 do
	for i=0,countof(game.battleAnimEffects)-1 do
		local offset = game.battleAnimScriptOffsets[i]
		local addr = offset + 0x100000
		--print('battleAnimScript['..i..']: offset=0x'..number.hex(offset)..', addr=0x'..number.hex(addr))
		battleScriptAddrs[addr] = battleScriptAddrs[addr] or table()
		battleScriptAddrs[addr]:insert(i)
	end
	print()

	-- of 660, 566 unique entries
	-- of 650, 565 as well
	local addrsInOrder = battleScriptAddrs:keys():sort()
	for i,addr in ipairs(addrsInOrder) do
		local addrend = addrsInOrder[i+1] or 0x107fb2
		print('battleAnimScript addr=0x'..number.hex(addr)..':')
		print(' used by effect #s: '..battleScriptAddrs[addr]:mapi(function(effectIndex)
				-- I forgot already, is effectIndex 1:1 with spell index?
				return ('0x%02x'):format(effectIndex)
			end):concat', ')
		print('-- '..battleScriptAddrs[addr]:mapi(function(effectIndex)
			return ('%q'):format(tostring(game.getSpellName(effectIndex)))
		end):concat', ')
		local pc = addr
		local linepc
		local lhs
		local ifblockstack = table()
		local tab = 0
		local rhsprint
		local function startline()
			lhs = ''
			linepc = pc
			repeat
				if #ifblockstack == 0 then break end
				local last = ifblockstack:last()
				if bit.band(0xffff, pc) < last then break end
				ifblockstack:remove()
				tab = tab - 1
				rhsprint'end if'
			until false
		end
		local function read()
--print(debug.traceback())
			assert.le(0, pc)
			assert.lt(pc, romsize)
			local cmd = ffi.cast('uint8_t*', rom + pc)[0]
			--local cmd = rom[pc] --ffi.cast('uint8_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%02x'):format(cmd)	-- number.tostring arg is max # decimal digits ... i should do args for # lhs padding as well ...
			pc = pc + 1
assert.type(cmd, 'number')
			return cmd
		end
		local function reads8()
--print(debug.traceback())
			assert.le(0, pc)
			assert.lt(pc, romsize)
			local cmd = ffi.cast('int8_t*', rom + pc)[0]
			--local cmd = rom[pc] --ffi.cast('int8_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%02x'):format(cmd)	-- number.tostring arg is max # decimal digits ... i should do args for # lhs padding as well ...
			pc = pc + 1
assert.type(cmd, 'number')
			return cmd
		end
		local function readu16()
--print(debug.traceback())
			assert.le(0, pc)
			assert.lt(pc, romsize-2)
			local cmd = ffi.cast('uint16_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%04x'):format(cmd)
			pc = pc + 2
assert.type(cmd, 'number')
			return cmd
		end
		local function readu32()
			assert.le(0, pc)
			assert.lt(pc, romsize-4)
--print(debug.traceback())
			local cmd = ffi.cast('uint32_t*', rom + pc)[0]
			lhs = lhs .. ' ' .. ('%08x'):format(cmd)
			pc = pc + 4
assert.type(cmd, 'number')
			return cmd
		end
		function rhsprint(...)
			assert.eq(select('#', ...), 1)
			local s = ...
--print(debug.traceback())
			print(
				('$%04x: '):format(bit.band(0xffff, linepc))
				--('$%06x: '):format(linepc)
				..lhs
				..(' '):rep(20 - #lhs), ('  '):rep(tab)..s)
			startline()
		end
		startline()

		local movedirs = {
			[0] = 'down/forward',
			'down',
			'down/back',
			'forward',
			'back',
			'up/forward',
			'up',
			'up/back'
		}

		local function u8(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('uint8_t', b))
			return ('0x%02x'):format(b)
		end
		local function s8(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('int8_t', b))
			if b < 0 then return '-0x'..number.hex(math.abs(b)) end
			return '0x'..number.hex(b)
			--return ('0x%02x'):format(b)
		end
		local function u16(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('uint16_t', b))
			return ('0x%04x'):format(b)
		end
		local function u32(b)
assert.type(b, 'number')
			b = tonumber(ffi.cast('uint32_t', b))
			return ('0x%08x'):format(b)
		end
		local function addrtostr(x)
			return ('$%04x'):format(bit.band(x, 0xffff))
		end

		while pc < addrend do
			local cmd = read()
			-- is this always and only for the first two bytes?
			if pc == addr+1 then
				local subcmd = read()
				local speed = 1 + bit.rshift(cmd, 4)
				local x = bit.band(cmd, 0xf)
				local align = bit.rshift(subcmd, 4)
				-- 0 = bottom of character
				-- 2 = center of character
				-- 4 = top of character
				-- 6 = unused
				-- 8 = initial position
				-- 0xa = center of screen
				-- 0xc = front of character
				-- 0xe = "align to character" (which side?)
				local y = bit.band(subcmd, 4)
				assert.eq(x, 0)
				assert.eq(y, 0)
				rhsprint('setSpeedAndAlign('..speed..', '..align..')')
			elseif cmd >= 0 and cmd < 0x20 then
				rhsprint('showFrame('..u8(cmd)..')')
			elseif cmd >= 0x20 and cmd < 0x80 then
				rhsprint('-')	-- that's all it has in the notes ... '-'
			elseif cmd == 0x80 then
				local subcmd = read()
				if subcmd == 0x00 then
					rhsprint('quadraSlamSlice()')
				elseif subcmd == 0x01 then
					rhsprint('')
				elseif subcmd == 0x02 then
					rhsprint('')
				elseif subcmd == 0x03 then
					rhsprint('')
				elseif subcmd == 0x04 then
					rhsprint('randomizeVectorAngleAndPosInitFireDanceSprites()')
				elseif subcmd == 0x05 then
					rhsprint('bumRush()')
				elseif subcmd == 0x06 then
					rhsprint('initTornado()')
				elseif subcmd == 0x07 then
					rhsprint('moveTornadoToThreadPos()')
				elseif subcmd == 0x08 then
					rhsprint('moveThreadToVectorPosition()')
				elseif subcmd == 0x09 then
					rhsprint('updateObjSpriteTilePriorityForTornado()')
				elseif subcmd == 0x0A then
					rhsprint('whiteEffectMagicIntro()')
				elseif subcmd == 0x0B then
					rhsprint('updateEsperPreAnimationBallsPos()')
				elseif subcmd == 0x0C then
					rhsprint('')
				elseif subcmd == 0x0D then
					rhsprint('')
				elseif subcmd == 0x0E then
					rhsprint('')
				elseif subcmd == 0x0F then
					rhsprint('')
				elseif subcmd == 0x10 then
					rhsprint('moveToTargetPos()')
				elseif subcmd == 0x11 then
					rhsprint('randomizeVectorAngle()')
				elseif subcmd == 0x12 then
					rhsprint('')
				elseif subcmd == 0x13 then
					rhsprint('makeTargetImp()')
				elseif subcmd == 0x14 then
					rhsprint('makeTargetVanish()')
				elseif subcmd == 0x15 then
					rhsprint('moveCircleToThreadPos()')
				elseif subcmd == 0x16 then
					rhsprint('')
				elseif subcmd == 0x17 then
					rhsprint('updateSpriteLayerPriorityBasedOnTarget()')
				elseif subcmd == 0x18 then
					rhsprint('loadSketchedMonsterPalette()')
				elseif subcmd == 0x19 then
					rhsprint('sketch()')
				elseif subcmd == 0x1A then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x1B then
					rhsprint('transformIntoMagicite()')
				elseif subcmd == 0x1C then
					rhsprint('decrScreenBrightness()')
				elseif subcmd == 0x1D then
					rhsprint('transformIntoMagicite()')
				elseif subcmd == 0x1E then
					rhsprint('')
				elseif subcmd == 0x1F then
					rhsprint('')
				elseif subcmd == 0x20 then
					rhsprint('')
				elseif subcmd == 0x21 then
					rhsprint('updateRotatingSpriteLayerPriority()')
				elseif subcmd == 0x22 then
					rhsprint('pearlWind()')
				elseif subcmd == 0x23 then
					rhsprint('pearlWind()')
				elseif subcmd == 0x24 then
					rhsprint('clear_BG3_HDMA_scrolldata()')
				elseif subcmd == 0x25 then
					rhsprint('clear_BG1_HDMA_scrolldata()')
				elseif subcmd == 0x26 then
					local arg = read()
					rhsprint('setCharColorPalUpdates('..tostring(arg ~= 0)..')')
				elseif subcmd == 0x27 then
					local arg = read()
					rhsprint('showCharsForEsperAttack('..tostring(arg ~= 0)..')')
				elseif subcmd == 0x28 then
					local arg = read()
					rhsprint('setAllCharsSpritePriority('
						..bit.band(3, bit.rshift(arg, 4))
						..')')
				elseif subcmd == 0x29 then
					local arg = read()
					rhsprint('showEsperAttackCursorSprite('..tostring(arg ~= 0)..')')
				elseif subcmd == 0x2A then
					local arg = read()
					rhsprint('loadSpriteAndAnimPal('..u8(arg)..')')
				elseif subcmd == 0x2B then
					local arg = read()
					rhsprint('loadBG1AnimPal('..u8(arg)..')')
				elseif subcmd == 0x2C then
					local arg = read()
					rhsprint('loadBG3AnimPal('..u8(arg)..')')
				elseif subcmd == 0x2D then
					local x = readu16()
					local y = readu16()
					local z = readu16()
					rhsprint('jumpBasedOnAttackType{'
						..'normal='..addrtostr(x)
						..', backOrSide34='..addrtostr(y)
						..', pincerOrSide12='..addrtostr(z)
						..'}')
				elseif subcmd == 0x2E then
					local x = read()
					local y = read()
					rhsprint('moveSpriteTo('
						..u8(x)..', '
						..u8(y)..')')
				elseif subcmd == 0x2F then
					rhsprint('')
				elseif subcmd == 0x30 then
					local arg = read()
					rhsprint('loadChar1AnimPal('..u8(arg)..')')
				elseif subcmd == 0x31 then
					local speed = read()
					rhsprint('moveInWideSineWave('..u8(speed)..')')
				elseif subcmd == 0x32 then
					local x = readu16()
					local y = readu16()
					if x == y then
						rhsprint('jumpTo('..addrtostr(x)..')')
					else
						rhsprint('jumpIfFacing('
							..addrtostr(x)..', '
							..addrtostr(y)..')')
					end
				elseif subcmd == 0x33 then
					local arg = read()
					rhsprint('updateRainbowGradientLines()')
				elseif subcmd == 0x34 then
					rhsprint('copyMonsterToCharPalettes()')
				elseif subcmd == 0x35 then
					rhsprint('useCharPalsForMonsterSpriteData()')
				elseif subcmd == 0x36 then
					rhsprint('restoreMonsterSpritePals()')
				elseif subcmd == 0x37 then
					rhsprint('clearFixedColorValueHDMAData()')
				elseif subcmd == 0x38 then
					rhsprint('enableHighPriorityBG3()')
				elseif subcmd == 0x39 then
					local arg = read()
					rhsprint('updateBlueGradientLines()')
				elseif subcmd == 0x3A then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x3B then
					rhsprint('setTargetColorPalToAnimPal()')
				elseif subcmd == 0x3C then
					rhsprint('setTargetColorPaletteToNormal()')
				elseif subcmd == 0x3D then
					rhsprint('quadraSlamSlice()')
				elseif subcmd == 0x3E then
					local arg = read()
					rhsprint('setMainScreenDesignation()')
				elseif subcmd == 0x3F then
					rhsprint('sonicDive()')
				elseif subcmd == 0x40 then
					local arg = read()
					rhsprint('setScreenMode('..u8(arg)..')')
				elseif subcmd == 0x41 then
					local cx, cy, dx, dy = reads8(), reads8(), reads8(), reads8()
					rhsprint('shrinkBG1AndMove('
						..s8(cx)..','..s8(cy)..', '	-- shrink-x shrink-y
						..s8(dx)..','..s8(dy)..')')	-- move-x move-y
				elseif subcmd == 0x42 then
					local vh = read()
					rhsprint('set_MODE7_register('
						..tostring(0 ~= bit.band(2, vh))..', '		-- vflip
						..tostring(0 ~= bit.band(1, vh))..')')		-- hflip
				elseif subcmd == 0x43 then
					rhsprint('moonSongCharm()')
				elseif subcmd == 0x44 then
					rhsprint('fireBoltIceBeam()')
				elseif subcmd == 0x45 then
					local arg = read()
					rhsprint('set_BG12_mask_register()')
				elseif subcmd == 0x46 then
					rhsprint('')
				elseif subcmd == 0x47 then
					rhsprint('')
				elseif subcmd == 0x48 then
					rhsprint('clear')
				elseif subcmd == 0x49 then
					rhsprint('inkHitVirite()')
				elseif subcmd == 0x4A then
					rhsprint('')
				elseif subcmd == 0x4B then
					rhsprint('updateRedYellowGradientLines()')
				elseif subcmd == 0x4C then
					rhsprint('moveTriangleToThreadPosition()')
				elseif subcmd == 0x4D then
					rhsprint('setVectorFromTriangleToTarget()')
				elseif subcmd == 0x4E then
					rhsprint('')
				elseif subcmd == 0x4F then
					rhsprint('')
				elseif subcmd == 0x50 then
					rhsprint('')
				elseif subcmd == 0x51 then
					rhsprint('rippler()')
				elseif subcmd == 0x52 then
					rhsprint('stone()')
				elseif subcmd == 0x53 then
					rhsprint('rpolarity()')
				elseif subcmd == 0x54 then
					rhsprint('rpolarity()')
				elseif subcmd == 0x55 then
					rhsprint('quasar()')
				elseif subcmd == 0x56 then
					rhsprint('goner()')
				elseif subcmd == 0x57 then
					local arg = read()
					rhsprint('set_BG34_window_mask('..u8(arg)..')')
				elseif subcmd == 0x58 then
					local arg = read()
					rhsprint('setCircleShape('..u8(arg)..')')
				elseif subcmd == 0x59 then
					rhsprint('gonerFlareStar()')
				elseif subcmd == 0x5A then
					rhsprint('mindBlast()')
				elseif subcmd == 0x5B then
					rhsprint('mindBlast()')
				elseif subcmd == 0x5C then
					rhsprint('mindBlast()')
				elseif subcmd == 0x5D then
					rhsprint('')
				elseif subcmd == 0x5E then
					rhsprint('overcast()')
				elseif subcmd == 0x5F then
					local arg = reads8()
					rhsprint('incBlueBgGrad('..s8(arg)..')')
				elseif subcmd == 0x60 then
					local flags = readu32()	-- TODO is aabbccdd 4 sets of 2 bits or 4 bytes?
					rhsprint('toggleAttackerStatus('..u32(flags)..')')
				elseif subcmd == 0x61 then
					local xx, yy, zz = read(), read(), read()
					rhsprint('')
				elseif subcmd == 0x62 then
					rhsprint('evilTootFader()')
				elseif subcmd == 0x63 then
					local arg = read()
					rhsprint('moveInNarrowVertSineWave('..u8(arg)..')')	-- speed
				elseif subcmd == 0x64 then
					rhsprint('purifierInvizEdge()')
				elseif subcmd == 0x65 then
					rhsprint('')
				elseif subcmd == 0x66 then
					rhsprint('shockWave()')
				elseif subcmd == 0x67 then
					rhsprint('loadExtraEsperPalette()')
				elseif subcmd == 0x68 then
					rhsprint('purifier()')
				elseif subcmd == 0x69 then
					rhsprint('updateSpriteLayerPriorityBasedOnAttacker(')
				elseif subcmd == 0x6A then
					rhsprint('alignBottomOfThreadWithBottomOfTarget()')
				elseif subcmd == 0x6B then
					rhsprint('lqpearl()')
				elseif subcmd == 0x6C then
					rhsprint('overcast()')
				elseif subcmd == 0x6D then
					rhsprint('disableBattleMenu()')
				elseif subcmd == 0x6E then
					rhsprint('')
				elseif subcmd == 0x6F then
					rhsprint('')
				elseif subcmd == 0x70 then
					rhsprint('')
				elseif subcmd == 0x71 then
					rhsprint('restoreCharPals()')
				elseif subcmd == 0x72 then
					local arg = read()
					rhsprint('branchIfAttackHit('..addrtostr(pc + arg)..')')
				elseif subcmd == 0x73 then
					local arg = read()
					rhsprint('setDiceGraphics('..u8(arg)..')')
				elseif subcmd == 0x74 then
					rhsprint('')
				elseif subcmd == 0x75 then
					rhsprint('superball()')
				elseif subcmd == 0x76 then
					local arg = read()
					rhsprint('seize')
				elseif subcmd == 0x77 then
					rhsprint('seize')
				elseif subcmd == 0x78 then
					rhsprint('discard')
				elseif subcmd == 0x79 then
					rhsprint('characters run to left side of screen (takes 56 loops to reach other side)')
				elseif subcmd == 0x7A then
					rhsprint('characters run to right side of screen (takes 56 loops to reach other side)')
				elseif subcmd == 0x7B then
					rhsprint('flip all characters (after running to opposite side of screen)')
				elseif subcmd == 0x7C then
					rhsprint('swap target and attacker')
				elseif subcmd == 0x7D then
					local arg = read()
					rhsprint('if dragonHornEffectIsActive() then goto _'..addrtostr(pc + arg)..' end')
				elseif subcmd == 0x7E then
					rhsprint('flip target character vertically')
				elseif subcmd == 0x7F then
					rhsprint('hide all monsters')
				elseif subcmd == 0x80 then
					rhsprint('boss death')
				elseif subcmd == 0x81 then
					rhsprint('')
				elseif subcmd == 0x82 then
					rhsprint('boss death')
				elseif subcmd == 0x83 then
					rhsprint('')
				elseif subcmd == 0x84 then
					rhsprint('chadarnook exit')
				elseif subcmd == 0x85 then
					rhsprint('chadarnook exit')
				elseif subcmd == 0x86 then
					local arg = read()
					rhsprint('play sound effect '
						..u8(arg)..', pan based on sprite X position')
				elseif subcmd == 0x87 then
					local arg = read()
					rhsprint('play sound effect '
						..u8(arg)..', pan based on sprite Y position')
				elseif subcmd == 0x88 then
					rhsprint('')
				elseif subcmd == 0x89 then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x8A then
					rhsprint('set target monster sprite priority to 0')
				elseif subcmd == 0x8B then
					rhsprint('play ching sound effect')
				elseif subcmd == 0x8C then
					local arg = read()
					rhsprint('play sound effect '
						..u8(arg)..', pan center')
				else
					rhsprint('!!! uncharted subcmd')
				end
			elseif cmd == 0x81 then
				local xx, yy = read(), read()
				if xx == yy then
					rhsprint('set attacking character graphic to '..u8(xx))
				else
					rhsprint('set attacking character graphic to '..u8(xx)..' if facing left, '..u8(yy)..' if facing right')
				end
			elseif cmd == 0x82 then
				local xx, yy = read(), read()
				if xx == yy then
					rhsprint('set targetted character graphic to '..u8(xx))
				else
					rhsprint('set targetted character graphic to '..u8(xx)..' if facing left, '..u8(yy)..' if facing right')
				end
			elseif cmd == 0x83 then
				local arg = read()
				rhsprint('set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x84 then
				local xx = read()
				rhsprint('set animation speed to '..u8(xx))
			elseif cmd == 0x85 then
				rhsprint('move thread to attacker position')
			elseif cmd == 0x86 then
				local arg = read()
				rhsprint('for attacker, set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x87 then
				local arg = read()
				rhsprint('for target, set dir='
					..movedirs[bit.rshift(arg, 5)]
					..' and move '..u8(bit.band(arg, 0x1f)+1))
			elseif cmd == 0x88 then
				local arg = read()
				rhsprint([[$F71D fight: set frame to ]]
					..u8(arg)..[[ and jump forward with weapon]])
			elseif cmd == 0x89 then
				local arg = read()
				rhsprint('for i=0,'..u8(arg-1)..' do')
				tab = tab + 1
			elseif cmd == 0x8A then
				tab = tab - 1
				rhsprint('end--for')
			elseif cmd == 0x8B then
				local arg = read()
				rhsprint('animated loop frame offset from +0 to +'..u8(arg-1))
				tab = tab + 1
			elseif cmd == 0x8C then
				tab = tab - 1
				rhsprint('end animated loop')
			elseif cmd == 0x8D then
				local arg = read()
				rhsprint('if animationIsHflipped() then setDirAndMove('
					..movedirs[bit.rshift(arg, 5)]
					..', '..u8(bit.band(arg, 0x1f)+1)..') end')
			elseif cmd == 0x8E then
				local arg = read()
				rhsprint('show thread'
					..(0 ~= bit.band(0x80, arg) and ' below' or ' above')
					..(0 ~= bit.band(0x40, arg) and ' front' or ' back')
					..' other sprites (sprite priority) with'
					..(0 ~= bit.band(1, arg) and '' or ' opposite')..' weapon hand')
			elseif cmd == 0x8F then
				-- TODO either 0x8D or 0x8F should probably be 'vflipped'
				local arg = read()
				rhsprint('if animationIsHflipped() then setDirAndMove('
					..movedirs[bit.rshift(arg, 5)]
					..' an, '..u8(bit.band(arg, 0x1f)+1)..') end')
			elseif cmd == 0x90 then
				local arg = read()
				rhsprint('set thread sprite tile priority to '
					..bit.band(3, bit.rshift(arg, 4))
					..' (tile priority)')
			elseif cmd == 0x91 then
				rhsprint('move this thread to attacker thread position')
			elseif cmd == 0x92 then
				local speed, branch = read(), read()
				rhsprint('move thread along vector (speed '
					..u8(speed)..', code branch '
					..u8(branch)..')')
			elseif cmd == 0x93 then
				local arg = read()
				rhsprint('set position on vector to '..u8(arg))
			elseif cmd == 0x94 then
				rhsprint('set vector from attacker to a random location on the target (GP Rain, AutoCrossbow)')
			elseif cmd == 0x95 then
				rhsprint('set vector from attacker to target')
			elseif cmd == 0x96 then
				local xx, yy = read(), read()
				rhsprint('if idk then jump backwards '..u8(xx))
			elseif cmd == 0x97 then
				rhsprint('boomerang/wing edge/full moon/rising sun')
			elseif cmd == 0x98 then
				local arg, arg2 = read(), read()
				rhsprint('increment graphic index offset every '
					..u8(arg)
					..' frame(s), '..u8(arg2))
			elseif cmd == 0x99 then
				local arg = read()
				rhsprint('set thread palette to '..u8(bit.band(7, bit.rshift(arg, 1))))
			elseif cmd == 0x9A then
				rhsprint('set thread facing direction to match attacker')
			elseif cmd == 0x9B then
				rhsprint('')
			elseif cmd == 0x9C then
				local xx = read()
				rhsprint('')
			elseif cmd == 0x9D then
				local xx = read()
				rhsprint('')
			elseif cmd == 0x9E then
				rhsprint('')
			elseif cmd == 0x9F then
				local arg = read()
				rhsprint('animated loop start (loop count equal to the number of active threads, '
					..u8(arg)..' = 0) (autocrossbow)')
				tab = tab + 1
			elseif cmd == 0xA0 then
				local arg, arg2 = read(), read()
				rhsprint('jump forward along vector (speed '
					..u8(arg)..', code branch '..u8(arg)..')')
			elseif cmd == 0xA1 then
				local arg, arg2 = read(), read()
				rhsprint('jump backward along vector (speed '
					..u8(arg)..', code branch '..u8(arg2)..')')
			elseif cmd == 0xA2 then
				rhsprint('drill')
			elseif cmd == 0xA3 then
				local xxxx = readu16()
				rhsprint('shift color palette left')
			elseif cmd == 0xA4 then
				local arg, arg2 = read(), read()
				rhsprint('shift color palette right'
					..' numcolors='..bit.band(0xf, arg)
					..' offset='..bit.band(0xf, bit.rshift(arg, 4))
					..' speed='..bit.band(0xf, arg2)
					..' paletteIndex='..bit.band(0xf, bit.rshift(arg, 4)))
			elseif cmd == 0xA5 then
				local aa, bb, cc, xx, yyyy, zz = read(), read(), read(), read(), readu16(), read()
				rhsprint('circle origin ('
					..u8(aa+0x80)..','..u8(bb+0x80)..')'
					..' growspeed?='..u8(cc)
					..' maxsize='..u16(yyyy))
			elseif cmd == 0xA6 then
				local dx, dy, dr = read(), read(), read()
				rhsprint('move circle ('..s8(dx)..','..s8(dy)..'), circle size += '..s8(dr))
			elseif cmd == 0xA7 then
				rhsprint('update circle?')
			elseif cmd == 0xA8 then
				rhsprint('move circle to attacker')
			elseif cmd == 0xA9 then
				local dx, dy = read(), read()
				rhsprint('move circle ('..s8(dx)..','..s8(dy)..') (based on character facing direction)')
			elseif cmd == 0xAA then
				local arg = read()
				rhsprint('set sprite palette 3 color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xAB then
				local arg = read()
				rhsprint('set sprite palette 3 color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xAC then
				local xx, yy = read(), read()
				rhsprint([[$EE9C set background scroll HDMA data 123fffff vhaaaaaa 1: affect BG1 2: affect BG2 3: affect BG3 f: frequency v: vertical h: horizontal a: amplitude (max 14, must be even ???)]])
			elseif cmd == 0xAD then
				local arg = read()
				rhsprint('set BG scroll HDMA index: BG='..bit.rshift(arg, 6)
					..' index='..u8(bit.band(arg, 0x3f)))
			elseif cmd == 0xAE then
				local vh___123 = read()
				rhsprint('vh---123    $ED86 Update Scroll HDMA data v: vertical h: horizontal 1: affect BG1 2: affect BG2 3: affect BG3')
			elseif cmd == 0xAF then
				local arg = read()
				rhsprint('set background palette color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xB0 then
				local arg = read()
				rhsprint('set background palette color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xB1 then
				local arg = read()
				rhsprint('set sprite palette 1 color subtraction (absolute)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB2 then
				local arg = read()
				rhsprint('Set sprite palette 1 color addition (absolute)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB3 then
				local arg = read()
				rhsprint('add color to sprite palette 3 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB4 then
				local arg = read()
				rhsprint('subtract color from sprite palette 3 palette (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB5 then
				local arg = read()
				rhsprint('add color to background palette (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB6 then
				local arg = read()
				rhsprint('subtract color from background palette (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB7 then
				local arg = read()
				rhsprint('add color to sprite palette 1 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB8 then
				local arg = read()
				rhsprint('subtract color from sprite palette 1 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xB9 then
				local arg = read()
				rhsprint('set monster palettes color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xBA then
				local arg = read()
				rhsprint('set monster palettes color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xBB then
				local arg = read()
				rhsprint('add color to monster palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xBC then
				local arg = read()
				rhsprint('subtract color from monster palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xBD then
				local arg = read()
				rhsprint(
					(0 ~= bit.band(0x80, arg) and
						(
							(0 ~= bit.band(0x20, arg) and 'hide' or 'show')
							..' BG1'
						)
						or ''
					)
					..(0 ~= bit.band(0x40, arg) and
						(
							(0 ~= bit.band(0x10, arg) and 'hide' or 'show')
							..' BG3'
						)
						or ''
					)
					..' animation thread graphics')
			elseif cmd == 0xBE then
				local arg = read()
				rhsprint('set screen mosaic to '..u8(arg)..' ($2106)')
			elseif cmd == 0xBF then
				local arg = readu16()
				rhsprint('call '..addrtostr(arg))
			elseif cmd == 0xC0 then
				rhsprint('return'..(
					#ifblockstack == 0 and '\n' or ''
				))
			elseif cmd == 0xC1 then
				local xx, yy = read(), read()
				rhsprint('vector movement speed? = '..u8(xx)..', branch to '..addrtostr(pc - yy)..' (-'..u8(yy)..')')
			elseif cmd == 0xC2 then
				local abc_____ = read()
				rhsprint('unpause animation a: unpause BG1 b: unpause BG3 c: unpause sprites')
			elseif cmd == 0xC3 then
				rhsprint('moveCircleToTarget()')
			elseif cmd == 0xC4 then
				local arg = read()
				rhsprint('moveThreadToThisThreadPos('
					..(0 ~= bit.band(arg, 0x80) and '1' or '')	-- BG1
					..(0 ~= bit.band(arg, 0x40) and '3' or '')	-- BG3
					..')')
			elseif cmd == 0xC5 then
				local a, b, c, d = readu16(), readu16(), readu16(), readu16()
				rhsprint('jumpBasedOnSwdtechHit('
					..table{addrtostr(a), addrtostr(b), addrtostr(c), addrtostr(d)}:concat', '..')'
					)
			elseif cmd == 0xC6 then
				local xx, yy = read(), read()
				rhsprint('quadraSlamSlice()')
			elseif cmd == 0xC7 then
				local subcmd = read()
				if subcmd == 0x00 then
					local arg = read()
					rhsprint('setAttackingCharacterDirectionToFace()'
						..(arg == 0 and 'left' or 'right'))
				elseif subcmd == 0x01 then
					rhsprint('resetPositionOffsetsForAttackingCharacter()')
				elseif subcmd == 0x02 then
					rhsprint('saveAttackingCharacterPosition()')
				elseif subcmd == 0x03 then
					rhsprint('restoreAttackingCharacterPositionAndResetOffsets()')
				elseif subcmd == 0x04 then
					rhsprint('restoreAttackingCharacterPosition()')
				elseif subcmd == 0x05 then
					local arg = read()
					error'here'
					rhsprint('(unused)')
				elseif subcmd == 0x06 then
					local arg, arg2 = read(), read()
					rhsprint('')
				elseif subcmd == 0x07 then
					rhsprint('updateCharacterActionBasedOnVectorDirectionWalking()')
				elseif subcmd == 0x08 then
					local x, y = read(), read()
					rhsprint('setVectorTargetFromAttacker('..u8(x)..','..u8(y)..')')
				elseif subcmd == 0x09 then
					rhsprint('updateCharacterActionBasedOnVectorDirectionArmsUp()')
				elseif subcmd == 0x0A then
					local xx = read()
					error'here'
					rhsprint('(unused)')
				elseif subcmd == 0x0B then
					local x, y, z = read(), read(), read()
					rhsprint('spc('
						..table{u8(x), u8(y), u8(z)}:concat', '..')'
					)
				elseif subcmd == 0x0C then
					local actor, graphicsIndex = read(), read()
					rhsprint('setActorGraphicIndex('..u8(actor)..', '..u8(graphicsIndex)..')')
				elseif subcmd == 0x0D then
					local arg = read()
					rhsprint('')
				elseif subcmd == 0x0E then
					local arg = read()
					rhsprint('screenShaking( '..u8(arg)..')')	-- $6285
				elseif subcmd == 0x0F then
					error'here'
					rhsprint('(unused)')
				elseif subcmd == 0x10 then
					local xx = read()
					rhsprint('')
				elseif subcmd == 0x11 then
					rhsprint('disable run from battle')
				else
					rhsprint('!!! unknown subcmd')
				end
			elseif cmd == 0xC8 then
				local arg = read()
				rhsprint('set attacker graphic = '..u8(arg))
			elseif cmd == 0xC9 then
				local arg = read()
				rhsprint(''..(arg == 0
					and ('play animation default sound effect')
					or 'play sound effect '..u8(arg)
				))
			elseif cmd == 0xCA then
				rhsprint('')
			elseif cmd == 0xCB then
				local eddddddd = read()
				rhsprint('enable/disable echo sprites (4 copies of character sprite) e: 1 = enable, 0 = disable d: frame delay between echo sprites (bitmask)')
			elseif cmd == 0xCC then
				local arg = read()
				rhsprint('set sprite palette 2 color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xCD then
				local arg = read()
				rhsprint('set sprite palette 2 color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xCE then
				local arg = read()
				rhsprint('add color to sprite palette 2 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xCF then
				local arg = read()
				rhsprint('subtract color from sprite palette 2 (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xD0 then
				local vhftpppm = read()
				rhsprint('set sprite data for all character/monster sprites')
			elseif cmd == 0xD1 then
				local arg = read()
				rhsprint(
					(arg == 0 and 'validate' or 'invalidate')
					..'CharacterMonsterOrderPriority()')
			elseif cmd == 0xD2 then
				local xx, yy = read(), read()
				rhsprint('setTargetPositionWithoutMoving('..u8(xx)..', '..u8(yy)..')')
			elseif cmd == 0xD3 then
				rhsprint('moveCircleToAttackingCharacter()')
			elseif cmd == 0xD4 then
				local xxxx, yy = readu16(), read()
				-- hmm cant tell bit/byte order from desc...
				rhsprint('set color addition/subtraction data')
			elseif cmd == 0xD5 then
				local arg = read()
				rhsprint('set monster'
					..(0 ~= bit.band(1, arg) and ' hflip' or '')
					..(0 ~= bit.band(2, arg) and ' vflip' or ''))
			elseif cmd == 0xD6 then
				rhsprint('')
			elseif cmd == 0xD7 then
				local xx = read()
				rhsprint('move fire dance sprites')
			elseif cmd == 0xD8 then
				local xx, yy, zz = read(), read(), read()
				rhsprint('x speed='..u8(xx)..' y speed='..u8(yy)..' ??? = '..u8(zz))
			elseif cmd == 0xD9 then
				local xx = read()
				rhsprint('(bum rush)')
			elseif cmd == 0xDA then
				local xxxx = readu16()
				rhsprint('update tornado (w wind/spiraler)')
			elseif cmd == 0xDB then
				local arg = read()
				rhsprint('if characterAlreadySteppedForwardToAttack() then goto _'..addrtostr(pc - arg)..' end')
			elseif cmd == 0xDC then
				rhsprint('rotate triangle 2D')
			elseif cmd == 0xDD then
				local xx, yy, dd, rr = read(), read(), read(), read()
				rhsprint('init triangle')
			elseif cmd == 0xDE then
				rhsprint('move triangle to attacker position')
			elseif cmd == 0xDF then
				rhsprint('move triangle to target position')
			elseif cmd == 0xE0 then
				local xx, yy, dd, rr = read(), read(), read(), read()
				rhsprint('modify triangle')
			elseif cmd == 0xE1 then
				local xx = read()
				rhsprint('show/hide attacker sprite')
			elseif cmd == 0xE2 then
				rhsprint('')
			elseif cmd == 0xE3 then
				rhsprint('')
			elseif cmd == 0xE4 then
				rhsprint('')
			elseif cmd == 0xE5 then
				local xx, yy, zz = read(), read(), read()
				rhsprint('branch to '..addrtostr(pc - yy)..' (-'..u8(yy)..')')
			elseif cmd == 0xE6 then
				local xx, yy, zz = read(), read(), read()
				rhsprint('branch to '..addrtostr(pc - yy)..' (-'..u8(yy)..')')
			elseif cmd == 0xE7 then
				rhsprint('calculate vector from attacking character to target')
			elseif cmd == 0xE8 then
				local rr, tt = read(), read()
				rhsprint('move to polar coordinates r='..u8(rr)..' theta='..u8(tt))
			elseif cmd == 0xE9 then
				local dx, dy = read(), read()
				-- TODO should this be 0..dx or 0..(dx-1) ?
				rhsprint('move randomly (0..'..u8(dx)..', 0..'..u8(dy)..')')
			elseif cmd == 0xEA then
				local _13__xxxx = read()
				rhsprint('set BG tile data quadrants 1 = affect bg1 3 = affect bg1 x = quadrant')
			elseif cmd == 0xEB then
				-- TODO count as many as threads ... how to determine # of threads?
				local addrs = range(3):mapi(function() return readu16() end)
				rhsprint('jump based on thread to {'
					..addrs:mapi(function(addr) return addrtostr(addr) end):concat', '
					..'}')
			elseif cmd == 0xEC then
				local xx = read()
				rhsprint('set thread layer (0 = sprite, 1 = BG1, 2 = BG3)')
			elseif cmd == 0xED then
				rhsprint('')
			elseif cmd == 0xEE then
				local __oo____ = read()
				rhsprint('set target sprite tile priority')
			elseif cmd == 0xEF then
				local rr, tt = read(), read()
				rhsprint('move to polar coordinates r='..u8(rr)..' theta='..u8(tt)..' (similar to $E8)')
			elseif cmd == 0xF0 then
				local a,b,c,d,e = readu16(), readu16(), readu16(), readu16(), readu16()
				rhsprint('jump based on current target'
					..' char1='..addrtostr(a)
					..' char2='..addrtostr(b)
					..' char3='..addrtostr(c)
					..' char4='..addrtostr(d)
					..' monster='..addrtostr(e))
			elseif cmd == 0xF1 then
				local xx = read()
				rhsprint('')
			elseif cmd == 0xF2 then
				rhsprint('set a trajectory from target center to attacker')
			elseif cmd == 0xF3 then
				local a,b,c,d,e = readu16(), readu16(), readu16(), readu16(), readu16()
				rhsprint('jump based on current attacker'
					..' char1='..addrtostr(a)
					..' char2='..addrtostr(b)
					..' char3='..addrtostr(c)
					..' char4='..addrtostr(d)
					..' monster='..addrtostr(e))
			elseif cmd == 0xF4 then
				local _______t = read()
				rhsprint('set sprite layer priority')
			elseif cmd == 0xF5 then
				rhsprint('until no threads are active')
			elseif cmd == 0xF6 then
				rhsprint('rotate triangle 3D')
			elseif cmd == 0xF7 then
				local arg = read()
				rhsprint('wait until vertical scanline position '..u8(arg))
			elseif cmd == 0xF8 then
				--[[
				local arg, arg2 = readu16(), readu16()
				rhsprint('if magitek mode is enabled then jump to '
					..addrtostr(arg)
					..' else '..addrtostr(arg2))
				--]]
				-- [[
				local arg, arg2 = readu16(), readu16()
				-- funny thing, arg2 always matches script PC
				assert.eq(arg2, bit.band(0xffff, pc))
				ifblockstack:insert(arg)
				-- this is always a command "if magitek mode is enabled then jump to" (with no else)
				-- and that means this is really an if-block: "if magitek mode is not enabled"
				--  that we can pop once we reach that address
				rhsprint'if not magitekMode() then'
				tab = tab + 1
				--]]
			elseif cmd == 0xF9 then
				local xx,yy,zz = read(), read(), read()
				rhsprint('')
			elseif cmd == 0xFA then
				local arg = readu16()
				rhsprint('jump to '..addrtostr(arg))
			elseif cmd == 0xFB then
				local arg = read()
				rhsprint('set character palettes color subtraction (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xFC then
				local arg = read()
				rhsprint('set character palettes color addition (absolute)'
					..' amount='..u8(bit.band(0x1f, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg)))
			elseif cmd == 0xFD then
				local arg = read()
				rhsprint('add color to character palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xFE then
				local arg = read()
				rhsprint('subtract color from character palettes (relative)'
					..' amount='..u8(bit.band(0xf, arg))
					..' red='..tostring(0 ~= bit.band(0x80, arg))
					..' green='..tostring(0 ~= bit.band(0x40, arg))
					..' blue='..tostring(0 ~= bit.band(0x20, arg))
					..(0 ~= bit.band(0x10, arg) and ' sub' or ' add'))
			elseif cmd == 0xFF then
				rhsprint('end animation'..(
					#ifblockstack == 0 and '\n' or ''
				))
			end
		end
	end
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local cmdline = require 'ext.cmdline'(...)
	-- hmm if luajit does get ... upon require then ff6 will get passed a bad file
	-- maybe xpcall and bailout on fail?
	print'--[['
	local game = require 'ff6'((
		assert(path((...)):read())
	))
	print'--]]'
	outputBattleAnimScripts(game, cmdline)
end

return outputBattleAnimScripts
