#!/usr/bin/env luajit
--[[
source is
everything8215 ff6/notes/battle-lists.txt
everything8215 ff6tool/ff6/ff3u-sfc.json /scriptEncoding/monster

cmdline:
	hideAddrs
--]]
local ffi = require 'ffi'
local path = require 'ext.path'
local assert = require 'ext.assert'
local table = require 'ext.table'
local string = require 'ext.string'
local range = require 'ext.range'
local tolua = require 'ext.tolua'

local function outputMonsterScripts(game, cmdline)
	cmdline = cmdline or {}
	local rom = game.rom
	local Game = game.Game
	local countof = game.countof

	local monsterScriptsAddr = ffi.offsetof(Game, 'monsterScripts')	-- 0x0f8700

	local monstersForAddr = {}
	local scriptAddrs = {}
	for i=0,game.numMonsters-1 do
		local addr = monsterScriptsAddr + game.monsterScriptOfs[i]
		scriptAddrs[addr] = true
		monstersForAddr[addr] = monstersForAddr[addr] or table()
		monstersForAddr[addr]:insert(i)
	end
	scriptAddrs = table.keys(scriptAddrs):sort()

	print[[
----------------------------------------------------------------
-- THE GENERATED GAME SCRIPT                                  --
----------------------------------------------------------------
setfenv(1, require 'ff6.monster-battle-script-env')
return {
]]
	for i=1,#scriptAddrs do
		local startAddr = scriptAddrs[i]
		local nextAddr = scriptAddrs[i+1]
			or (monsterScriptsAddr + ffi.sizeof(game.monsterScripts))

		assert.eq(#monstersForAddr[startAddr], 1)
		local monsterIndex = monstersForAddr[startAddr][1]
		local name = string.trim(tostring(game.monsterNames[monsterIndex]))
		if name ~= '' then name = ' '..name end
		print('\t['..monsterIndex..'] = {\t-- '
			..(' 0x%06x'):format(startAddr)
			..'-'..('0x%06x'):format(nextAddr)
			..name
		)

		local function getItemName(i)
			return tostring(game.itemNames[i])
		end
		local function getSpellName(i)
			if i == 0xfe then return 'nothing' end
			return tostring(game.getSpellName(i))
		end
		local function getActionName(i)
			if i < countof(game.menuNames) then
				return tostring(game.menuNames[i])
			end
			return 'action???'..i
		end
		local function getTarget(i)
			if i < 16 then
				-- absolute character-based target
				-- notice, 14 == Banon ... is that always where he is?
				return 'characters[1+'..i..']'	-- 1+ cuz it's a 1-based table...
			end
			if i >= 48 and i < 54 then
				return 'enemyFormationSlot['..(i-48)..']'	-- 0-5 for battle formation index
			end
			if i >= 72 and i < 76 then
				return 'characterFormationSlot['..(i-72)..']'
			end
			return ({
				[54] = 'self',
				[55] = '"all other enemies"',
				[56] = '"all enemies"',
				[57] = '"random other enemy"',
				[58] = '"random enemy"',
				[59] = '"all dead characters"',
				[60] = '"random dead character"',
				[61] = '"all dead enemies"',
				[62] = '"random dead enemy"',
				[63] = '"all characters with reflect"',
				[64] = '"random character with reflect"',
				[65] = '"all enemies with reflect"',
				[66] = '"random enemy with reflect"',
				[67] = '"all characters"',
				[68] = '"random character"',
				[69] = '"attacker"',
				[70] = '"all targets"',
				[71] = '"nothing"',
				[77] = '"targeting target"',	-- hmm... that's a weird one
			})[i] or i
		end
		-- honestly I should just keep the args and translate them in the glue layer...
		-- btw who uses this?
		local function getTargetMask(i)
			if i == 0 then return 'self' end
			if i == 0xff then return '"all enemies"' end
			return range(0,5)
			:filteri(function(j)
				return 0 ~= bit.band(i, bit.lshift(1, j))
			end)
			:mapi(function(j)
				return 'enemyFormationSlot['..j..']'
			end):concat', '
		end

		local returnCount = 0
		local funcNames = {'act', 'react'}
		local funcs = table()
		funcs[1+returnCount] = table()

		local startptr = rom + startAddr
		local len = nextAddr - startAddr
		local data = ffi.string(startptr, len)
		local p = ffi.cast('uint8_t*', startptr)
		local pend = p + len
		local inCond
		local lastWasCond

		while p < pend do
			local thisIsCond
			local cmd = p[0]
			local cmdAddr = p - rom
			local out
			if cmd < 0xf0 then
				out = 'doAttack('..cmd..')\t-- '..getSpellName(cmd)
				p=p+1
			elseif cmd == 0xf0 then
				local is = table{p[1], p[2], p[3]}
				out = 'pickAttack('..is:concat', '..')'
					..'\t-- options: '..is:mapi(function(i) return tostring(getSpellName(i)) end):concat', '
				p=p+4
			elseif cmd == 0xf1 then
				out = 'setTargets('..getTarget(p[1])..')'
				p=p+2
			elseif cmd == 0xf2 then
				local x1 = p[1]
				local exitEffect = bit.band(0x7f, x1)
				local scrollBG = 0 ~= bit.band(0x80, x1)
				local x23 = ffi.cast('uint16_t*', p+2)[0]
				local battleIndex = bit.band(0x7fff, x23)
				local restoreMonsters = 0 ~= bit.band(0x8000, x23)
				out = 'changeBattle{exitEffect='..exitEffect
					..', scrollBG='..tostring(scrollBG)
					..', battleIndex='..battleIndex
					..', restoreMonsters='..tostring(restoreMonsters)
					..'}'
				p=p+4
			elseif cmd == 0xf3 then
				-- numBattleDialogs = 0x100
				-- numBattleDialog2s = 0x100
				-- numBattleMessages = 0x100
				local i = ffi.cast('uint16_t*', p+1)[0]
				-- i is from 0 to 140
				out = 'dialog('..tolua(game.battleDialog[i])..')\t-- '..i
				p=p+3
			elseif cmd == 0xf4 then
				out = 'cmd('..table{p[1], p[2], p[3]}:concat', '..')'
				p=p+4
			elseif cmd == 0xf5 then
				local anim = p[1]
				local op = ({
					[0] = 'setVis{visible=true, restoreHP=true',
					-- does "disable targeting" mean also "don't consider for win-condition?"
					[1] = 'setVis{visible=false, allowTargeting=false',
					[2] = 'setVis{visible=true',
					[3] = 'setVis{visible=false, allowTargeting=true',
					[4] = 'setVis{visible=false, dontEndBattle=true',
					[5] = 'setVis{visible=false, debugMode=true',	-- not used
				})[p[2]] or error'here'
				local enemyIndex = p[3]
				out = op
					..', anim='..anim
					..', targets={'..getTargetMask(enemyIndex)..'}'
					..'}'
				p=p+4
			elseif cmd == 0xf6 then
				local useVsThrow = p[1]
				local item12 = getItemName(p[2])
				local item3 = getItemName(p[3])
				if useVsThrow == 0 then
					out = ('useItem(%q, %q)'):format(item12, item3)
				elseif useVsThrow == 1 then
					out = ('throwItem(%q, %q)'):format(item12, item3)
				else
					error'here'
				end
				p=p+4
			elseif cmd == 0xf7 then
				local x = p[1]
				-- this must be what does the battleDialog2[] or battleMessages[]....
				out = 'doBattleEvent('..x..')'
				p=p+2
			elseif cmd == 0xf8 then
				local var = p[1]
				local op = bit.band(3, bit.rshift(p[2], 6))
				local value = bit.band(0x3f, p[2])
				if op == 0 then
					out = 'battleVarSet('..var..', '..value..')'
				elseif op == 1 then
					error'here'
				elseif op == 2 then
					out = 'battleVarInc('..var..', '..value..')'
				elseif op == 3 then
					out = 'battleVarDec('..var..', '..value..')'
				end
				p=p+3
			elseif cmd == 0xf9 then
				local flagIndex = bit.bor(
					bit.lshift(p[2], 3),
					bit.band(7, p[3])
				)
				local rest = bit.rshift(p[3], 3)
				assert.eq(rest, 0)	-- why even only use 3 lower bits? weird.
				if p[1] == 0 then
					out = 'battleFlagToggle('..flagIndex..')'
				elseif p[1] == 1 then
					out = 'battleFlagSet('..flagIndex..')'
				elseif p[1] == 2 then
					out = 'battleFlagClear('..flagIndex..')'
				else
					error'here'
				end
				p=p+4
			elseif cmd == 0xfa then
				local x1 = p[1]
				if x1 == 0x09 then
					out = 'playSound('..p[2]..', '..p[3]..')'
				else
					-- is fa09 special?
					local x2 = p[2]
					local x3 = p[3]
					out = 'anim('..table{x1, x2, x3}:concat', '..')'
				end
				p=p+4
			elseif cmd == 0xfb then
				if p[1] == 0 then
					assert.eq(p[2], 0)
					out = 'resetMonsterTimer(self)'
				elseif p[1] == 1 then
					out = 'makeInvisible('..p[2]..')'
				elseif p[1] == 2 then
					assert.eq(p[2], 0)
					out = 'endBattle()'
				elseif p[1] == 3 then
					assert.eq(p[2], 0)
					out = 'addGauToParty()'
				elseif p[1] == 4 then
					assert.eq(p[2], 0)
					out = 'resetBattleTimer()'
				elseif p[1] == 5 then
					out = 'removeInvisible('..p[2]..')'
				elseif p[1] == 6 then
					out = 'makeTargetable('..p[2]..')'
				elseif p[1] == 7 then
					out = 'makeUntargetable('..p[2]..')'
				elseif p[1] == 8 then
					out = 'setATBToMax('..p[2]..')'
				elseif p[1] == 9 then
					assert.eq(p[2], 0)
					out = 'sendGauToVeldt()'
				elseif p[1] == 11 then
					out = 'setStatus(self, '..p[2]..')'	-- on who?
				elseif p[1] == 12 then		-- not used?
					out = 'removeStatus(self, '..p[2]..')'
				elseif p[1] == 13 then
					assert.eq(p[2], 0)
					out = 'hideMonster(self)'
				else
					error'here'
				end
				p=p+3
			elseif cmd == 0xfc then
				local ifStmt = lastWasCond and 'and' or 'if'
				thisIsCond = true
				local condIndex = p[1]
				if condIndex == 1 then
					-- p[2] means command-2 which means menu-item-2 which is magic
					local action1 = ('%q'):format(getActionName(p[2]))
					local action2 = ('%q'):format(getActionName(p[3]))
					local actions = action1 == action2 and action1 or action1..', '..action2
					-- p[3] == 2 is targeting <-> this monster?
					-- or p[2] == 2, ip[3] == 2 <-> action == magic, spell #2 == bolt?
					out = ifStmt..' wasTargetedWithAction('..actions..')'
				elseif condIndex == 2 then
					out = ifStmt..' wasTargetedWithFight('..p[2]..', '..p[3]..')'
				elseif condIndex == 3 then
					local item1 = ('%q'):format(getItemName(p[2]))
					local item2 = ('%q'):format(getItemName(p[3]))
					local items = item1 == item2 and item1 or item1..', '..item2
					out = ifStmt..' wasTargetedWithItem('..items..')'
				elseif condIndex == 4 then
					assert.eq(p[3], 0)
					out = ifStmt..' wasHitByElement('..p[2]..')'
				elseif condIndex == 5 then
					assert.eq(p[2], 0)
					assert.eq(p[3], 0)
					out = ifStmt..' wasTargeted()'	-- what's this mean? if any action targeted this monster?
				elseif condIndex == 6 then
					-- is it always < ?
					out = ifStmt..' '..getTarget(p[2])..'.hp < '..(p[3] * 128)
				elseif condIndex == 7 then
					-- is it always < ?
					out = ifStmt..' '..getTarget(p[2])..'.mp < '..p[3]
				elseif condIndex == 8 then
					out = ifStmt..' 0 ~= targetStatus('..getTarget(p[2])..') & (1 << '..('0x02%x'):format(p[3])..')'
				elseif condIndex == 9 then
					out = ifStmt..' 0 == targetStatus('..getTarget(p[2])..') & (1 << '..('0x%02x'):format(p[3])..')'
				elseif condIndex == 11 then
					assert.eq(p[3], 0)
					out = ifStmt..' monsterTimer(self) > '..(p[2] * 2)	-- seconds?
				elseif condIndex == 12 then
					out = ifStmt..' battleVarGet('..p[2]..') < '..p[3]
				elseif condIndex == 13 then
					out = ifStmt..' battleVarGet('..p[2]..') >= '..p[3]
				elseif condIndex == 14 then
					out = ifStmt..' '..getTarget(p[2])..'.level <'..p[3]
				elseif condIndex == 15 then
					local target
					-- for all target conds or just this one?
					if p[2] == 68 then
						out = ifStmt..' randomTargetsLevelGt('..p[3]..')'
					elseif p[2] < 16 then
						-- target 0-15 is character 0-15 is object 0-15
						target = 'getObj('..p[2]..')'
						out = ifStmt..' '..target..'.level > '..p[3]
					else
						target = 'getTarget('..p[2]..')'
						out = ifStmt..' '..target..'.level > '..p[3]
					end
				elseif condIndex == 16 then
					assert.eq(p[2], 0)
					assert.eq(p[3], 0)
					out = ifStmt..' onlyOneTypeOfEnemyAlive()'
				elseif condIndex == 17 then
					assert.eq(p[3], 0)
					out = ifStmt..' isAlive('..getTargetMask(p[2])..')'
				elseif condIndex == 18 then
					assert.eq(p[3], 0)
					out = ifStmt..' isDead('..getTargetMask(p[2])..')'
				elseif condIndex == 19 then
					local func, cmp
					if p[2] == 0 then
						func = 'numPartyAlive()'
						cmp = '>='
					elseif p[2] == 1 then
						func = 'numEnemiesAlive()'
						cmp = '<='
					else
						func = '???'..p[2]
						cmp = '???'
					end
					out = ifStmt..' '..func..' '..cmp..' '..p[3]
				elseif condIndex == 20 then
					-- {p[2], p[3]}:
					-- {9, 1} = 0x61
					-- {6, 1} = 0x49
					local flagIndex = bit.bor(
						bit.lshift(p[2], 3),
						bit.band(7, p[3])
					)
					local rest = bit.rshift(p[3], 3)
					assert.eq(rest, 0)	-- why even only use 3 lower bits? weird.
					out = ifStmt..' battleFlagGet('..flagIndex..')'
				elseif condIndex == 21 then
					local flagIndex = bit.bor(
						bit.lshift(p[2], 3),
						bit.band(7, p[3])
					)
					local rest = bit.rshift(p[3], 3)
					assert.eq(rest, 0)	-- why even only use 3 lower bits? weird.
					out = ifStmt..' not battleFlagGet('..flagIndex..')'
				elseif condIndex == 22 then
					assert.eq(p[3], 0)
					out = ifStmt..' battleTimer() < '..p[2]
				elseif condIndex == 23 then
					assert.eq(p[3], 0)
					out = ifStmt..' targetIsValid('..p[2]..')'
				elseif condIndex == 24 then
					assert.eq(p[2], 0)
					assert.eq(p[3], 0)
					out = ifStmt..' gauIsPresent()'
				elseif condIndex == 25 then
					assert.eq(p[3], 0)
					local monsterName = p[2] == 0 and 'self'
						or p[2] == 0xff and '"all enemies"'
						or tostring(p[2]-1)
					out = ifStmt..' battlePlaceAvailable('..monsterName..')'	-- what does 'this monster slot' or 'all monster slot' mean?
				elseif condIndex == 26 then
					out = ifStmt..' wasHitWithElement('..p[2]..', '..p[3]..')'
				elseif condIndex == 27 then
					out = ifStmt..' battleIndex['..ffi.cast('uint16_t*', p+1)[0]..']'
				else
					error'here'
				end
				p=p+4
			elseif cmd == 0xfd then
				out = 'passTurn()'
				p=p+1
			elseif cmd == 0xfe then
				if not inCond then
--print(('!!! found end-if when not in-cond at %06x'):format(cmdAddr))
				end
				-- i need to distinguish between mid-if and after-if ...
				if inCond or lastWasCond then
					out = 'end--if'
				else
					out = '--end--if'
				end
				inCond = false
				p=p+1
			-- there should always only be 2 of these
			-- 1st block is for the main routine
			-- 2nd block is reaction/counter-attack
			elseif cmd == 0xff then
				out = 'return'
				p=p+1
			else
				error'here'
			end

			-- I thought I could quickly implement this as a state-machine, but I was wrong...
			-- I really need to buffer the commands as objects - esp if I want to add them to the visualizer.

			assert(out, ("failed to write out on cmd 0x%02x"):format(cmd))
			if lastWasCond
			and not thisIsCond
			--and cmd ~= 0xfe
			then
				if inCond then
--print(('!!! lastWasCond, not thisIsCond, but inCond at %06x'):format(cmdAddr))
				end
				if cmd ~= 0xfe then
					inCond = true
				end
				local indent = cmdline.hideAddrs and 3 or 5
				--if cmd == 0xfe then indent = indent - 1 end
				funcs[1+returnCount]:insert(('\t'):rep(indent)..'then')
			end
			if inCond and (cmd == 0xff or cmd == 0xfc) then
				inCond = false
				local tab = cmdline.hideAddrs and '\t\t\t' or '\t\t\t\t\t'
				funcs[1+returnCount]:insert(tab..'end--if')
			end
			if not cmdline.hideAddrs then
				io.write(('%06x\t'):format(cmdAddr))
			end
			if out == 'return' then
				-- last monster has like 9 of these
				returnCount = returnCount + 1
				funcs[1+returnCount] = funcs[1+returnCount] or table()
			else
				local indent = inCond and cmd ~= 0xfc and cmd ~= 0xfe and 4 or 3
				funcs[1+returnCount]:insert(('\t'):rep(indent)..out)
			end
			lastWasCond = thisIsCond

			-- reset state on return
			if cmd == 0xff then
				inCond = nil
				lastWasCond = nil
			end
		end
		if returnCount ~= 2 then
			-- mag roader #243's script at 0x0f9448 only has 1 return ...
			-- monster #383 (empty?) has 9... and is really just a 'Battle' ...
			funcs[1+returnCount]:insert(('-- !!! battle script at %06x only has %d returns, expected 2'):format(startAddr, returnCount))
		end

		for i,func in ipairs(funcs) do
			if #func > 0
			and i <= 2	-- TODO... not need this condition
			then
				print('\t\t'..(funcNames[i] or '['..i..']')..'=|:|do')
				for _,line in ipairs(func) do
					print(line)
				end
				print'\t\tend,'
			end
		end

		print'\t},'
	end
	print'}'
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
	outputMonsterScripts(game, cmdline)
end

return outputMonsterScripts
