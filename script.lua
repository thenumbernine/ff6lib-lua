local ffi = require 'ffi'
local table = require 'ext.table'
local number = require 'ext.number'
local class = require 'ext.class'

-- handle the event script

return function(game)
	local game_t = game.game_t
	local rom = game.rom
	local romsize = game.romsize
	local countof = game.countof
	
	-- event code, pointed into by NPCs and maybe other things

	game.eventScriptAddrs = {}
	-- collect script addrs from npcs
	-- hmm should I use the master npc list or just which ones are in maps?
	for mapIndex=0,countof(game.maps)-1 do
		local mapInfo = game.getMap(mapIndex)
		if mapInfo then
			for _,n in ipairs(mapInfo.npcs) do
				local scriptAddr = n:getScriptAddr()
				if scriptAddr then
					game.eventScriptAddrs[scriptAddr] = true
				end
			end
		end
	end

	local startaddr = ffi.offsetof(game_t, 'eventCode')	-- 0xa0000
	local endaddr = ffi.offsetof(game_t, 'eventCode') + ffi.sizeof(game.eventCode)

	-- how to generate this in a modular way that both outputs and is reusable later
	-- for now I will insert in-order and provide an address lookup table to the index in this table
	-- but later I could do something like a table from call-address to list-of-instructions that stops at 'return' ... ? or does the script retain a well-defined structure to respect that rule?
	-- one per cmd
	-- TODO tempting to make this just a bunch of structs...
	local class = require 'ext.class'
	local ScriptCmds = {}
	game.ScriptCmds = ScriptCmds

	local Cmd = class()	-- parent abstract class
	function Cmd:digest(read)
		local args = table()
		if self.argtypes then
			for _,ctype in ipairs(self.argtypes) do
				args:insert(read(ctype))
			end
		end
		self:getargs(args:unpack())
		return addr
	end
	function Cmd:getargs(...)
		self.args = table{...}
	end
	function Cmd:__tostring()
		return self.desc..self.args:mapi(tostring):concat' '
	end
	ScriptCmds.Cmd = Cmd
	
	local ObjectCmd = Cmd:subclass{
		argtypes = {'uint8_t'},
		getargs = function(self, objectIndex)
			self.objectIndex = tonumber(objectIndex)
		end,
		__tostring = function(self)
			return self.desc..self.objectIndex
		end,
	}
	ScriptCmds.ObjectCmd = ObjectCmd

	ScriptCmds.RunObject = Cmd:subclass{
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, arg, arg2)
			arg = tonumber(arg)
			arg2 = tonumber(arg2)
			self.objectIndex = bit.band(0x3f, arg)
			self.length = bit.band(0x7f, arg2)
			self.waitUntilComplete = 0 ~= bit.band(0x80, arg2)
		end,
		__tostring = function(self)
			return "run object #"..self.objectIndex.." script, length="..self.length
				..(self.waitUntilComplete and ", wait until complete" or '')
		end,
	}
	for i=0,0x34 do
		ScriptCmds['RunObject '..i] = ScriptCmds.RunObject:subclass{cmd=i}
	end

	ScriptCmds.WaitForObject = ObjectCmd:subclass{
		cmd = 0x35,
		desc = "wait for object #",
	}

	ScriptCmds.EnableObjectPassability = ObjectCmd:subclass{
		cmd = 0x36,
		desc = "enable passability of object #",
	}

	ScriptCmds.ChangeObjectSprite = Cmd:subclass{
		cmd = 0x37,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, objectIndex, spriteIndex)
			self.objectIndex = tonumber(objectIndex)
			self.spriteIndex = tonumber(spriteIndex)
		end,
		__tostring = function(self)
			return 'change object #'..self.objectIndex..' sprite to #'..self.spriteIndex
		end,
	}

	ScriptCmds.EnableUserControl = Cmd:subclass{
		cmd = 0x3a,
		__tostring = function() return "enable user control" end,
	}
	ScriptCmds.DisableUserControl = Cmd:subclass{
		cmd = 0x3b,
		__tostring = function() return "disable user control" end,
	}

	ScriptCmds.CreateObject = ObjectCmd:subclass{
		cmd = 0x3d,
		desc = "create object #",
	}

	ScriptCmds.DeleteObject = ObjectCmd:subclass{
		cmd = 0x3e,
		desc = "delete object #",
	}

	ScriptCmds.AddCharacterToParty = Cmd:subclass{
		cmd = 0x3f,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, charIndex, partyIndex)
			self.charIndex = tonumber(charIndex)
			self.partyIndex = tonumber(partyIndex)
		end,
		__tostring = function(self)
			if self.charIndex == 0 then
				return "remove character #"..self.charIndex.." from party"
			else
				return "change character #"..self.charIndex.."'s party to #"..self.partyIndex
			end	
		end,
	}

	ScriptCmds.ChangeCharacterProperties = Cmd:subclass{
		cmd = 0x40,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, charIndex, propIndex)
			self.charIndex = tonumber(charIndex)
			self.propIndex = tonumber(propIndex)
		end,
		__tostring = function(self)
			return "change character #"..self.charIndex.."'s property #"..self.propIndex
		end,
	}

	ScriptCmds.ShowObject = ObjectCmd:subclass{
		cmd = 0x41,
		desc = 'show object #',
	}

	ScriptCmds.HideObject = ObjectCmd:subclass{
		cmd = 0x42,
		desc = "hide object #",
	}

	ScriptCmds.ChangeObjectPalette = Cmd:subclass{
		cmd = 0x43,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, objectIndex, paletteIndex)
			self.objectIndex = tonumber(objectIndex)
			self.paletteIndex = tonumber(paletteIndex)
		end,
		__tostring = function(self)
			return 'change object #'..self.objectIndex..' palette to #'..self.paletteIndex
		end,
	}

	ScriptCmds.ChangeObjectVehicle = Cmd:subclass{
		cmd = 0x44,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, objectIndex, arg)
			self.objectIndex = tonumber(objectIndex)
			arg = tonumber(arg)
			-- TODO why to use struct bitfields......
			self.vehicleIndex = bit.band(3, bit.rshift(arg, 5))
			self.showRider = 0 ~= bit.band(0x80, arg)
		end,
		__tostring = function(self)
			return "change object #"..self.objectIndex
				.." to vehicle #"..self.vehicleIndex
				..(self.showRider and ", show rider" or "")
		end,
	}

	ScriptCmds.UpdateCharacterObjects = Cmd:subclass{
		cmd = 0x45,
		desc = "update character objects",
	}

	ScriptCmds.CreatePartyObject = Cmd:subclass{
		cmd = 0x47,
		desc = "create the party object",
	}

	ScriptCmds.Dialog = Cmd:subclass{
		cmd = 0x4b,
		argtypes = {'uint16_t'},
		getargs = function(self, arg)
			arg = tonumber(arg)
			self.dialogIndex = bit.band(arg, 0x3fff)
			self.showTextOnly = 0 ~= bit.band(0x4000, arg)
			self.bottomOfScreen = 0 ~= bit.band(0x8000, arg)
		end,
		__tostring = function(self)
			-- wait if it's +1 then how do you index dialog 0?
			return 'show dialog[0x'..number.hex(self.dialogIndex+1)..']:'
				..('%q'):format(game.dialog[self.dialogIndex+1])
				..', '..(self.dontWait and "don't " or '')..' wait for button press'
				..(self.showTextOnly and ", show text only" or "")
				..(self.bottomOfScreen and ", bottom of screen")
		end,
	}

	ScriptCmds.DialogDontWait = ScriptCmds.Dialog:subclass{
		cmd = 0x48,
		dontWait = true,
	}

	ScriptCmds.WaitForDialogWindow = Cmd:subclass{
		cmd = 0x49,
		desc = "wait for dialog window",
	}

	ScriptCmds.WaitForDialogButtonPress = Cmd:subclass{
		cmd = 0x4a,
		desc = "wait for dialog button press",
	}

	ScriptCmds.Battle = Cmd:subclass{
		cmd = 0x4d,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, group, arg)
			self.group = tonumber(group)
			arg = tonumber(arg)
			self.bg = bit.band(0x3f, arg)
			self.noSound = 0 ~= bit.band(0x40, arg)
			self.noBlur = 0 ~= bit.band(0x80, arg)
		end,
		__tostring = function(self)
			return (self.collisionBattle and 'collision battle' or 'normal battle')
				..' group='..self.group
				..' background='..bit.band(0x3f, self.bg)
				..(self.noSound and ' no sound' or '')
				..(self.noBlur and ' no blur' or '')
		end,
	}

	ScriptCmds.CollisionBattle = ScriptCmds.Battle:subclass{
		cmd = 0x4c,
		collisionBattle = true,
	}

	ScriptCmds.RandomBattle = Cmd:subclass{
		cmd = 0x4e,
		desc = "random battle",
	}

	ScriptCmds.RestoreSavedGame = Cmd:subclass{
		cmd = 0x4f,
		desc = "restore saved game",
	}

	ScriptCmds.ChangeRangeOfColors = Cmd:subclass{
		cmd = 0x51,
		argtypes = {'uint8_t', 'uint8_t', 'uint8_t'},
		desc = "change range of color palettes ",
	}

	ScriptCmds.ChangeRangeOfColors2 = ScriptCmds.ChangeRangeOfColors:subclass{
		cmd = 0x53,
	}

	ScriptCmds.FlashScreenColor = Cmd:subclass{
		cmd = 0x55,
		argtypes = {'uint8_t'},
		desc = "flash screen color ",
	}

	ScriptCmds.ChangeMapPalette = Cmd:subclass{
		cmd = 0x60,
		argtypes = {'uint8_t', 'uint8_t'},
		desc = "change map palette ",
	}

	ScriptCmds.SetMapAnimCounter = Cmd:subclass{
		cmd = 0x64,
		argtypes = {'uint8_t', 'uint8_t'},
		desc = 'set map animation counter ',
	}

	ScriptCmds.SetMapAnimCounter = Cmd:subclass{
		cmd = 0x65,
		argtypes = {'uint8_t', 'uint8_t'},
		desc = 'set map animation speed ',
	}

	ScriptCmds.ChangeMap = Cmd:subclass{
		cmd = 0x6a,
		argtypes = {'uint16_t', 'uint8_t', 'uint8_t', 'uint8_t'},
		getargs = function(self, arg, x, y, flags)
			self.x = tonumber(x)
			self.y = tonumber(y)
			self.flags = tonumber(flags)
			arg = tonumber(arg)
			self.arg = arg
			self.destmap = bit.band(arg, 0x1ff)
		end,
		__tostring = function(self)
			return "change to map #"..self.destmap
				..(0 ~= bit.band(0x0200, self.arg) and "set parent map" or "")
				..(self.destmap >= 3 and 0 ~= bit.band(0x0400, self.arg) and "z-level=1" or "")
				..(self.destmap >= 3 and 0 ~= bit.band(0x0800, self.arg) and "show map title")
				..("facing direction="..bit.band(3, bit.rshift(self.arg, 12)))
				..("{x="..('0x%02x'):format(self.x)..", y="..('0x%02x'):format(self.y).."}")
				..("vehicle="..bit.band(3, self.flags))
				..(self.destmap >= 3 and 0 ~= bit.band(0x20, self.flags) and "no size update" or '')
				..(self.destmap >= 3 and 0 ~= bit.band(0x40, self.flags) and "manual fade-in" or '')
				..(self.destmap >= 3 and 0 ~= bit.band(0x80, self.flags) and "enable map event")
		end,
	}

	ScriptCmds.SetParentMap = Cmd:subclass{
		cmd = 0x6c,
		argtypes = {'uint8_t', 'uint8_t', 'uint8_t'},
		desc = 'set parent map ',
	}

	ScriptCmds.ChangeMapLayer = Cmd:subclass{
		cmd = 0x73,
		argtypes = {'uint8_t', 'uint8_t', 'uint8_t', 'uint8_t'},
		desc = 'change map layer ',
	}

	ScriptCmds.ChangeMapLayer2 = ScriptCmds.ChangeMapLayer:subclass{
		cmd = 0x74,
	}

	ScriptCmds.UpdateMapLayers = Cmd:subclass{
		cmd = 0x75,
		desc = 'update map layers after changes',
	}

	ScriptCmds.RestoreCharacterToFullHPMP = Cmd:subclass{
		cmd = 0x77,
		argtypes = {'uint8_t'},
		desc = 'restore character # to full hp/mp ',
	}
	
	ScriptCmds.DisablePassabilityOfObject = ObjectCmd:subclass{
		cmd = 0x78,
		desc = 'disable passability of object #',
	}

	ScriptCmds.ChangeObjectEvent = Cmd:subclass{
		cmd = 0x7a,
		argtypes = {'uint8_t', 'uint24_t'},
		getargs = function(self, objectIndex, newScriptAddr)
			self.objectIndex = tonumber(objectIndex)
			self.newScriptAddr = newScriptAddr:value()
		end,
		__tostring = function(self)
			return 'change object #'..self.objectIndex
				..' event script to '..('$%06x'):format(self.newScriptAddr)
		end,
	}

	ScriptCmds.EnableCollisionEvent = Cmd:subclass{
		cmd = 0x7c,
		argtypes = {'uint8_t'},
		desc = 'enable collision event #',
	}

	ScriptCmds.DisableCollisionEvent = Cmd:subclass{
		cmd = 0x7d,
		argtypes = {'uint8_t'},
		desc = 'disable collision event #',
	}

	ScriptCmds.ChangeCharacterName = Cmd:subclass{
		cmd = 0x7f,
		argtypes = {'uint8_t', 'uint8_t'},
		desc = "change character #'s name to names #",
	}

	ScriptCmds.GiveItem = Cmd:subclass{
		cmd = 0x80,
		argtypes = {'uint8_t'},
		desc = 'give item #',
	}
	
	ScriptCmds.TakeItem = Cmd:subclass{
		cmd = 0x81,
		argtypes = {'uint8_t'},
		desc = 'take item #',
	}
	
	ScriptCmds.GiveGP = Cmd:subclass{
		cmd = 0x84,
		argtypes = {'uint16_t'},
		desc = 'give GP ',
	}

	ScriptCmds.TakeGP = Cmd:subclass{
		cmd = 0x85,
		argtypes = {'uint16_t'},
		desc = 'take GP ',
	}

	ScriptCmds.GiveEsper = Cmd:subclass{
		cmd = 0x86,
		argtypes = {'uint8_t'},
		desc = 'give esper #',
	}
	
	ScriptCmds.TakeEsper = Cmd:subclass{
		cmd = 0x87,
		argtypes = {'uint8_t'},
		desc = 'take esper #',
	}

	ScriptCmds.RemoveCharacterStatus = Cmd:subclass{
		cmd = 0x88,
		argtypes = {'uint8_t', 'uint16_t'},
		desc = "remove character #'s status: ",
	}

	ScriptCmds.ToggleCharacterStatus = Cmd:subclass{
		cmd = 0x89,
		argtypes = {'uint8_t', 'uint16_t'},
		desc = "toggle character #'s status: ",
	}
	
	ScriptCmds.SetCharacterStatus = Cmd:subclass{
		cmd = 0x8a,
		argtypes = {'uint8_t', 'uint16_t'},
		desc = "set character #'s status: ",
	}

	ScriptCmds.GiveCharacterHP = Cmd:subclass{
		cmd = 0x8b,
		argtypes = {'uint8_t', 'uint8_t'},
		getargs = function(self, charIndex, signedAmount)
			self.charIndex = tonumber(charIndex)
			signedAmount = tonumber(signedAmount)
			self.amount = bit.lshift(1, bit.band(0x7f, signedAmount))
			self.take = 0 ~= bit.band(0x80, signedAmount)
		end,
		__tostring = function(self)
			return
				"character #"..self.charIndex.."'s "
				..(self.mp and 'MP' or 'HP')
				..(self.amount == bit.lshift(1,0x7f) and 'max' or 
					((self.take and '-' or '+')..self.amount)
				)
		end,
	}
	
	ScriptCmds.GiveCharacterMP = ScriptCmds.GiveCharacterHP:subclass{
		cmd = 0x8c,
		mp = true,
	}

	ScriptCmds.RemoveCharactersEquipment = Cmd:subclass{
		cmd = 0x8d,
		argtypes = {'uint8_t'},
		desc = 'remove equipment of character #',
	}
	
	ScriptCmds.MonsterInABox = Cmd:subclass{
		cmd = 0x8e,
		desc = 'monster-in-a-box battle',
	}

	ScriptCmds.UnlockAllSwdTechs = Cmd:subclass{
		cmd = 0x8f,
		desc = 'unlock all swdtechs',
	}

	ScriptCmds.UnlockBumRush = Cmd:subclass{
		cmd = 0x90,
		desc = 'unlock bum rush',
	}

	ScriptCmds.OpenCharacterNameChangeMenu = Cmd:subclass{
		cmd = 0x98,
		argtypes = {'uint8_t'},
		desc = 'open name change menu for character #',
	}

	ScriptCmds.OpenPartySelectMenu = Cmd:subclass{
		cmd = 0x99,
		argtypes = {'uint8_t','uint8_t', 'uint8_t'},
		desc = 'open party select menu ',
	}

	ScriptCmds.OpenColosseumMenu  = Cmd:subclass{
		cmd = 0x9a,
		desc = 'open colosseum menu',
	}
	
	ScriptCmds.OpenShopMenu  = Cmd:subclass{
		cmd = 0x9b,
		argtypes = {'uint8_t'},
		desc = 'open shop menu #',
	}

	ScriptCmds.OptimizeCharacterEquipment = Cmd:subclass{
		cmd = 0x9c,
		argtypes = {'uint8_t'},
		desc = 'optimize equipment of character #',
	}
	
	ScriptCmds.OpenFinalBattleMenu  = Cmd:subclass{
		cmd = 0x9d,
		desc = 'open final battle menu',
	}	

	ScriptCmds.ShowPyramidObject = Cmd:subclass{
		cmd = 0xa7,
		argtypes = {'uint8_t'},
		desc = 'show pyramid object #',
	}	

	ScriptCmds.ShowFloatingIslandCutscene = Cmd:subclass{
		cmd = 0xa8,
		desc = 'show floating island cutscene',
	}	

	ScriptCmds.ShowTitleScreen = Cmd:subclass{
		cmd = 0xa9,
		desc = 'show title screen',
	}

	ScriptCmds.ShowIntro = Cmd:subclass{
		cmd = 0xaa,
		desc = 'show intro',
	}

	ScriptCmds.OpenGameLoadMenu = Cmd:subclass{
		cmd = 0xab,
		desc = 'open game load menu',
	}

	ScriptCmds.LoadSavedCharacterObjectData = Cmd:subclass{
		cmd = 0xac,
		desc = 'load saved character object data',
	}

	ScriptCmds.ShowWoRCustscene = Cmd:subclass{
		cmd = 0xad,
		desc = 'show WoR cutscene',
	}

	ScriptCmds.ShowMagitechFactoryCutscene = Cmd:subclass{
		cmd = 0xae,
		desc = 'show magitech factory cutscene',
	}

	ScriptCmds.BeginRepeat = Cmd:subclass{
		cmd = 0xb0,
		argtypes = {'uint8_t'},
		desc = 'repeat x',
	}

	ScriptCmds.EndRepeat = Cmd:subclass{
		cmd = 0xb1,
		desc = 'end repeat',
	}

	ScriptCmds.Call = Cmd:subclass{
		cmd = 0xb2,
		argtypes = {'uint24_t'},
		desc = 'call ',
	}

	ScriptCmds.CallRepeat = Cmd:subclass{
		cmd = 0xb3,
		argtypes = {'uint24_t', 'uint8_t'},
		desc = 'call x ',
	}

	ScriptCmds.CallBasedOnDialogChoice = Cmd:subclass{
		cmd = 0xb6,
		digest = function(self, read)
			self.addrs = table()
			local choices = 2	-- TODO depends on previous dialog text prompt count
			for i=1,choices do
				self.addrs:insert(startaddr + read'uint24_t':value())
			end
		end,
		__tostring = function(self)
			-- TODO is it 'jump' or is it 'call'?
			-- cuz if it's jump then the the next instruction after "want to learn about espers?" shouldn't be a 'return', because each jump option there has its own return ...
			return 'call based on dialog choice:'
				..self.addrs:mapi(function(addr)
					return (' $%06x'):format(addr)
				end):concat' '
		end,
	}

	ScriptCmds.JumpBasedOnBattleSwitch = Cmd:subclass{
		cmd = 0xb7,
		argtypes = {'uint8_t', 'uint24_t'},
		desc = 'jump based on battle switch # to script 0x0a0000+ ... ',
	}

	ScriptCmds.ShowEndingCharacterCutscene = Cmd:subclass{
		cmd = 0xba,
		argtypes = {'uint8_t'},
		desc = 'show ending character cutscene ',
	}

	ScriptCmds.ShowEndCutscene = Cmd:subclass{
		cmd = 0xbb,
		desc = 'show end cutscene',
	}

	ScriptCmds.Jump5050 = Cmd:subclass{
		cmd = 0xbd,
		argtypes = {'uint24_t'},
		desc = '50% change to jump ',
	}

	ScriptCmds.JumpBasedOnCharacterSwitch = Cmd:subclass{
		cmd = 0xbe,
		digest = function(self, read)
			local count = tonumber(bit.band(0xf, read'uint8_t'))
			self.addrs = table()
			for i=1,count do
				self.addrs:insert(read'uint24_t':value())
			end
		end,
		__tostring = function(self)
			return "jump based on character switch, count="..#self.addrs 
				.." ??? = "..self.addrs:mapi(function(addr)
					return (' $%06x'):format(addr)
				end):concat' '
		end,
	}

	ScriptCmds.ShowAirshipEndingCutscene = Cmd:subclass{
		cmd = 0xbf,
		desc = 'show airship ending cutscene',
	}

	local Switch = Cmd:subclass{
		digest = function(self, read)
			local count = 1 + bit.band(self.cmd, 7)
			self._and = 0 ~= bit.band(self.cmd, 8)
			self.conds = table()
			for i=1,count do
				local arg = tonumber(read'uint16_t')
				self.conds:insert{
					switchIndex = tonumber(bit.band(0x3fff, arg)),
					value = tonumber(bit.rshift(arg, 15)),
				}
			end
			self.addr = startaddr + read'uint24_t':value()
		end,
		__tostring = function(self)
			return "if "
				..self.conds:mapi(function(cond)
					return 'switch[#'..cond.switchIndex..'] == '..cond.value
				end):concat(self._and and ' and ' or ' or ')
				..' then goto '..('$%06x'):format(self.addr)
		end,
	}
	for cmd=0xc0,0xcf do
		ScriptCmds['Switch '..cmd] = Switch:subclass{cmd = cmd}
	end
	
	ScriptCmds.ShowCharacterPortrait = Cmd:subclass{
		cmd = 0xe7,
		argtypes = {'uint8_t', 'uint8_t'},
		desc = "show character #'s portrait # ",
	}

	ScriptCmds.PlaySound = Cmd:subclass{
		cmd = 0xf4,
		argtypes = {'uint8_t'},
		desc = "play sound #",
	}

	ScriptCmds.Return = Cmd:subclass{
		cmd = 0xfe,
		desc = 'return',
	}
	
	ScriptCmds.EndScript = Cmd:subclass{
		cmd = 0xff,
		desc = 'end script',
	}
	
	-- ScriptCmds key by cmd (number) or by name (string)
	for _,k in ipairs(table.keys(ScriptCmds)) do
		local cl = ScriptCmds[k]
		if cl.cmd then	-- some abstract classes are in ScriptCmds but don't have a .cmd
			assert.type(cl.cmd, 'number')
			ScriptCmds[cl.cmd] = cl
		end
	end

	for i=0,255 do
		if not ScriptCmds[i] then
			ScriptCmds[i] = Cmd:subclass{
				cmd = i,
				desc = '??? '..('0x%02x'):format(i),
			}
		end
	end

	game.eventScriptCmds = table()
	game.eventScriptCmdIndexForAddr = {}

	local addrsInOrder = table.keys(game.eventScriptAddrs):sort()
		:filteri(function(addr)
			if addr >= startaddr and addr < endaddr then return true end
			print('TODO addr', number.hex(addr), 'oob!')
		end)

	for i,addr in ipairs(addrsInOrder) do
		local nextaddr = addrsInOrder[i+1] or endaddr
		while addr < nextaddr do	
			local cmdaddr = addr
			local cmd = rom[addr]
			addr = addr + 1

			local function read(ctype)
				ctype = ffi.typeof(ctype)
				local ptrtype = ffi.typeof('$*', ctype)
				local o = ctype( ffi.cast(ptrtype, rom + addr)[0] )
				addr = addr + ffi.sizeof(ctype)
				return o
			end

assert.index(ScriptCmds, cmd, "failed to find class for script command")
			local cl = ScriptCmds[cmd]
assert.eq(cl.class, cl, "class is not a class for command 0x"..number.hex(cmd))
assert.is(cl, Cmd, "somehow class of command 0x"..number.hex(cmd).." is not of Cmd")
			local cmdobj = cl()
			cmdobj:digest(read)

			cmdobj.addr = cmdaddr
			game.eventScriptCmds:insert(cmdobj)
			game.eventScriptCmdIndexForAddr[cmdaddr] = #game.eventScriptCmds
		end
	end
end
