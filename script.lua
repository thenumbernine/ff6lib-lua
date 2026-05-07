local ffi = require 'ffi'
local assert = require 'ext.assert'
local table = require 'ext.table'
local number = require 'ext.number'
local class = require 'ext.class'
local template = require 'template'


local int8_t = ffi.typeof'int8_t'
local uint8_t = ffi.typeof'uint8_t'
local int16_t = ffi.typeof'int16_t'
local uint16_t = ffi.typeof'uint16_t'
local int32_t = ffi.typeof'int32_t'
local uint32_t = ffi.typeof'uint32_t'
local float = ffi.typeof'float'
local double = ffi.typeof'double'


return function(game)
	local Game = game.Game
	local rom = game.rom
	local romsize = game.romsize
	local countof = game.countof

	local uint24_t = game.uint24_t 

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
			for _,e in ipairs(mapInfo.touchTriggers) do
				local scriptAddr = e:getScriptAddr()
				if scriptAddr then
					game.eventScriptAddrs[scriptAddr] = true
				end
			end
		end
	end

	local startaddr = ffi.offsetof(Game, 'eventScript')	-- 0xa0000
	local endaddr = ffi.offsetof(Game, 'eventScript') + ffi.sizeof(game.eventScript)

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
	end
	function Cmd:getargs(...)
		if self.argnames then
			for i=1,math.min(#self.argnames, (select('#', ...))) do
				self[self.argnames[i]] = select(i, ...)
			end
		else
			self.args = table{...}
		end
	end
	function Cmd:__tostring()
		--[[
		return self.desc..self.args:mapi(function(x)
			-- 'cdata' since I used an explicit constructor instead of a cast
			-- so how about converting to Lua numbers here if possible?
			return tostring(x)
		end):concat' '
		--]]
		-- [[
		return template(self.desc, self)
		--]]
	end
	ScriptCmds.Cmd = Cmd

	ScriptCmds.RunObject = Cmd:subclass{
		argtypes = {uint8_t, uint8_t},
		argnames = {'objectIndex', 'length'},
		desc = "runObjectScript{object=<?=objectIndex?>, length=<?=bit.band(0x7f, length)?><?=0~=bit.band(0x80, length) and ', block=true' or ''?>}",
	}
	for i=0x00,0x34 do
		ScriptCmds['RunObject '..i] = ScriptCmds.RunObject:subclass{cmd=i}
	end

	ScriptCmds.WaitForObject = Cmd:subclass{
		cmd = 0x35,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "waitFor(objs[<?=objectIndex?>])",
	}

	ScriptCmds.EnableObjectPassability = Cmd:subclass{
		cmd = 0x36,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "objs[<?=objectIndex?>].solid = true",
	}

	ScriptCmds.ChangeObjectSprite = Cmd:subclass{
		cmd = 0x37,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objectIndex', 'spriteIndex'},
		desc = 'objs[<?=objectIndex?>].sprite = <?=spriteIndex?>',
	}

	ScriptCmds.LockScreen = Cmd:subclass{
		cmd = 0x38,
		desc = 'screenLocked = true',
	}

	ScriptCmds.UnlockScreen = Cmd:subclass{
		cmd = 0x39,
		desc = 'screenLocked = false',
	}


	ScriptCmds.EnableUserControl = Cmd:subclass{
		cmd = 0x3a,
		desc = "userControl = true",
	}

	ScriptCmds.DisableUserControl = Cmd:subclass{
		cmd = 0x3b,
		desc = "userControl = false",
	}

	ScriptCmds.SetPartyCharacters = Cmd:subclass{
		cmd = 0x3c,
		argtypes = {uint8_t, uint8_t, uint8_t, uint8_t},
		desc = 'setPartyCharacters(<?=args:concat", "?>)',
	}

	ScriptCmds.CreateObject = Cmd:subclass{
		cmd = 0x3d,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "createObject(<?=objectIndex?>)",
	}

	ScriptCmds.DeleteObject = Cmd:subclass{
		cmd = 0x3e,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "deleteObject(<?=objectIndex?>)",
	}

	ScriptCmds.SetCharacterParty = Cmd:subclass{
		cmd = 0x3f,
		argtypes = {uint8_t, uint8_t},
		argnames = {'charIndex', 'partyIndex'},
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
		argtypes = {uint8_t, uint8_t},
		argnames = {'charIndex', 'propIndex'},
		__tostring = function(self)
			return "change character #"..self.charIndex.."'s property #"..self.propIndex
		end,
	}

	ScriptCmds.ShowObject = Cmd:subclass{
		cmd = 0x41,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = 'objs[<?=objectIndex?>].visible = true',
	}

	ScriptCmds.HideObject = Cmd:subclass{
		cmd = 0x42,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = 'objs[<?=objectIndex?>].visible = false',
	}

	ScriptCmds.ChangeObjectPalette = Cmd:subclass{
		cmd = 0x43,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objectIndex', 'paletteIndex'},
		__tostring = function(self)
			return 'change object #'..self.objectIndex..' palette to #'..self.paletteIndex
		end,
	}

	ScriptCmds.ChangeObjectVehicle = Cmd:subclass{
		cmd = 0x44,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, objectIndex, arg)
			self.objectIndex = objectIndex
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

	ScriptCmds.SetActiveParty = Cmd:subclass{
		cmd = 0x46,
		argtypes = {uint8_t},
		argnames = {'partyIndex'},
		desc = 'setActiveParty(<?=partyIndex?>)',
	}

	ScriptCmds.CreatePartyObject = Cmd:subclass{
		cmd = 0x47,
		desc = "createPartyObject()",
	}

	ScriptCmds.Dialog = Cmd:subclass{
		cmd = 0x4b,
		argtypes = {uint16_t},
		getargs = function(self, arg)
			self.dialogIndex = bit.band(arg, 0x3fff)
			self.showTextOnly = 0 ~= bit.band(0x4000, arg)
			self.bottomOfScreen = 0 ~= bit.band(0x8000, arg)
		end,
		__tostring = function(self)
			-- wait if it's +1 then how do you index dialog 0?
			local str = ('%q'):format(game.dialog[self.dialogIndex+1])
			if self.dontWait
			or self.showTextOnly
			or self.bottomOfScreen then
				return --'show dialog[0x'..number.hex(self.dialogIndex+1)..']:'
					'dialog('..('%q'):format(str)
					..', {'
					..table()
					:append{self.dontWait and 'dontWait=true' or nil}
					:append{self.showTextOnly and 'showTextOnly=true' or nil}
					:append{self.bottomOfScreen and 'bottomOfScreen=true' or nil}
					:concat','
				..'})'
			else
				return 'dialog'..('%q'):format(game.dialog[self.dialogIndex+1])
			end
		end,
	}

	ScriptCmds.DialogDontWait = ScriptCmds.Dialog:subclass{
		cmd = 0x48,
		dontWait = true,
	}

	ScriptCmds.WaitForDialogWindow = Cmd:subclass{
		cmd = 0x49,
		desc = "waitForDialogWindow()",
	}

	ScriptCmds.WaitForDialogButtonPress = Cmd:subclass{
		cmd = 0x4a,
		desc = "waitForDialogButtonPress()",
	}

	ScriptCmds.Battle = Cmd:subclass{
		cmd = 0x4d,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, group, arg)
			self.group = group
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
		desc = "randomBattle()",
	}

	ScriptCmds.RestoreSavedGame = Cmd:subclass{
		cmd = 0x4f,
		desc = "reloadSavedGame()",
	}

	ScriptCmds.ChangeBackgroundPalette = Cmd:subclass{
		cmd = 0x50,
		argtypes = {uint8_t},
		argnames = {'arg'},
		what = 'Background',
		desc = 'change<?=what?>Palette{'
			..'func=<?=bit.band(3, bit.rshift(arg, 6))?>'
			..', color=<?=bit.band(7, bit.rshift(arg, 2))?>'
			..', intensity=<?=bit.band(3, arg)?>'
			..'<?=firstColor and ", "..firstColor or ""?>'
			..'<?=lastColor and ", "..lastColor or ""?>'
		..'}',
	}

	ScriptCmds.ChangeBackgroundPaletteRange = ScriptCmds.ChangeBackgroundPalette:subclass{
		cmd = 0x51,
		argtypes = {uint8_t, uint8_t, uint8_t},
		argnames = {'arg', 'firstColor', 'lastColor'},
	}

	ScriptCmds.ChangeSpritePalette = ScriptCmds.ChangeBackgroundPalette:subclass{
		cmd = 0x52,
		what = 'Sprite',
	}

	ScriptCmds.ChangeBackgroundPaletteRange = ScriptCmds.ChangeBackgroundPaletteRange:subclass{
		cmd = 0x53,
		what = 'Sprite',
	}

	ScriptCmds.DisableFixedColorMath = Cmd:subclass{
		cmd = 0x54,
		desc = 'disableFixedColorMath()',
	}

	ScriptCmds.FlashScreenColor = Cmd:subclass{
		cmd = 0x55,
		argtypes = {uint8_t},
		desc = "flashScreenColor(<?=args[1]?>)",
	}

	ScriptCmds.SetFixedColorMathAdd = Cmd:subclass{
		cmd = 0x56,
		argtypes = {uint8_t},
		argnames = {'arg'},
		desc = 'setFixedColorMathAdd(<?=bit.band(7, arg)?>, <?=bit.band(3, bit.rshift(arg, 3))?>, <?=bit.band(3, bit.rshift(arg, 6))?>)',
	}

	ScriptCmds.SetFixedColorMathSub = Cmd:subclass{
		cmd = 0x57,
		argtypes = {uint8_t},
		argnames = {'arg'},
		desc = 'setFixedColorMathSub(<?=bit.band(0x7, arg)?>, <?=bit.band(0x3, bit.rshift(arg, 3))?>, <?=bit.band(0x3, bit.rshift(arg, 6))?>)',
	}

	ScriptCmds.ShakeScreen = Cmd:subclass{
		cmd = 0x58,
		argtypes = {uint8_t},
		desc = 'shakeScreen(<?=args[1]?>)',
	}

	ScriptCmds.FadeInRate = Cmd:subclass{
		cmd = 0x59,
		argtypes = {uint8_t},
		desc = 'fadeIn(<?=args[1]?>)',
	}

	ScriptCmds.FadeOutRate = Cmd:subclass{
		cmd = 0x5a,
		argtypes = {uint8_t},
		desc = 'fadeOut(<?=args[1]?>)'
	}

	ScriptCmds.StopFade = Cmd:subclass{
		cmd = 0x5b,
		desc = 'stopFade()',
	}

	ScriptCmds.WaitForFade = Cmd:subclass{
		cmd = 0x5c,
		desc = 'waitForFade()',
	}

	for _1scomp,cmds in ipairs{
		{0x5d, 0x5e, 0x5f},
		{0x70, 0x71, 0x72}
	} do
		for layer,cmd in ipairs(cmds) do
			ScriptCmds['ScrollBackground'..('0x%02x'):format(cmd)] = Cmd:subclass{
				cmd = cmd,
				args = {uint8_t, uint8_t},
				-- 1's comp is negative scrolling?
				desc = 'scrollBackgroundLayer{layer='..layer..(_1scomp==1 and ', 1sCompliment=true' or '')..'}',
			}
		end
	end

	ScriptCmds.ChangeMapPalette = Cmd:subclass{
		cmd = 0x60,
		argtypes = {uint8_t, uint8_t},
		argnames = {'dst', 'src'},
		desc = "changeMapPalette(<?=dst?>, <?=src?>)",
	}

	ScriptCmds.TintColors = Cmd:subclass{
		cmd = 0x61,
		argtypes = {uint8_t, uint8_t, uint8_t},
		argnames = {'fixed', 'first', 'last'},
		desc = 'tintColors{fixed=<?=fixed?>, first=<?=first?>, last=<?=last?>}',
	}

	ScriptCmds.PixelateScreen = Cmd:subclass{
		cmd = 0x62,
		argtypes = {uint8_t},
		desc = 'pixelateScreen(<?=args[1]?>)',
	}

	ScriptCmds.ShowLamp = Cmd:subclass{
		cmd = 0x63,
		argtypes = {uint8_t},
		argnames = {'radius'},
		desc = 'showLamp(<?=radius?>)',
	}

	ScriptCmds.SetMapAnimCounter = Cmd:subclass{
		cmd = 0x64,
		argtypes = {uint8_t, uint8_t},
		argnames = {'tile', 'frame'},
		desc = 'setMapAnimationCounter{tile=<?=tile?>, frame=<?=frame?>}',
	}

	ScriptCmds.SetMapAnimCounter = Cmd:subclass{
		cmd = 0x65,
		argtypes = {uint8_t, uint8_t},
		argnames = {'tile', 'speed'},
		desc = 'setMapAnimationSpeed{tile=<?=tile?>, speed=<?=speed?>}',
	}

	ScriptCmds.ChangeMap = Cmd:subclass{
		cmd = 0x6a,
		argtypes = {uint16_t, uint8_t, uint8_t, uint8_t},
		getargs = function(self, arg, x, y, flags)
			self.x = x
			self.y = y
			self.flags = flags
			self.arg = arg
			self.mapIndex = bit.band(arg, 0x1ff)
		end,
		__tostring = function(self)
			return "setMap{"
				.."mapIndex="..self.mapIndex
				..(0 ~= bit.band(0x0200, self.arg) and ", setParentMap=true" or "")
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x0400, self.arg) and ", zLevel=1" or "")
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x0800, self.arg) and ", showMapTitle=true" or '')
				..(", dir="..bit.band(3, bit.rshift(self.arg, 12)))
				..(", pos={"..('0x%02x'):format(self.x)..", "..('0x%02x'):format(self.y).."}")
				..(", vehicle="..bit.band(3, self.flags))
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x20, self.flags) and ", noSizeUpdate=true" or '')
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x40, self.flags) and ", manualFadeIn=true" or '')
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x80, self.flags) and ", enableEapEvent=true" or '')
			..'}'
		end,
	}

	ScriptCmds.ChangeMap2 = ScriptCmds.ChangeMap:subclass{
		cmd = 0x6b,
	}

	ScriptCmds.SetParentMap = Cmd:subclass{
		cmd = 0x6c,
		argtypes = {uint16_t, uint8_t, uint8_t, uint8_t},
		argnames = {'map', 'dir', 'x', 'y'},
		desc = 'setParentMap{map=<?=map?>, dir=<?=dir?>, x=<?=x?>, y=<?=y?>}',
	}

	ScriptCmds.ChangeMapLayer = Cmd:subclass{
		cmd = 0x73,
		argtypes = {uint8_t, uint8_t, uint8_t, uint8_t, uint8_t},
		argnames = {'x', 'layer', 'y', 'w', 'h'},
		extra = ', updateImmediately=true',
		desc = 'changeMapLayer{layer=<?=layer?>, x=<?=x?>, y=<?=y?>, w=<?=w?>, h=<?=h?><?=extra?>}',
	}

	ScriptCmds.ChangeMapLayer2 = ScriptCmds.ChangeMapLayer:subclass{
		cmd = 0x74,
		extra = '',
	}

	ScriptCmds.RefreshMapLayerChanges = Cmd:subclass{
		cmd = 0x75,
		desc = 'refreshMapLayerChanges()',
	}

	ScriptCmds.RestoreCharacterToFullHPMP = Cmd:subclass{
		cmd = 0x77,
		argtypes = {uint8_t},
		argnames = {'charIndex'},
		desc = 'characters[<?=charIndex?>]:setFullHPMP()',
	}

	ScriptCmds.DisablePassabilityOfObject = Cmd:subclass{
		cmd = 0x78,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = 'objs[<?=objectIndex?>].solid = false',
	}

	ScriptCmds.MovePartyToMap = Cmd:subclass{
		cmd = 0x79,
		argtypes = {uint8_t, uint16_t},
		argnames = {'partyIndex', 'mapIndex'},
		desc = 'party[<?=partyIndex?>]:setMap(<?=mapIndex?>)',
	}

	ScriptCmds.ChangeObjectEvent = Cmd:subclass{
		cmd = 0x7a,
		argtypes = {uint8_t, uint24_t},
		argnames = {'objectIndex', 'newScriptAddr'},
		desc = "objs[<?=objectIndex?>].script = <?=('$%06x'):format(0xa0000 + newScriptAddr:value())?>",
	}

	ScriptCmds.RestorePreviousParty = Cmd:subclass{
		cmd = 0x7b,
		desc = 'restorePreviousParty()',
	}

	ScriptCmds.EnableCollisionEvent = Cmd:subclass{
		cmd = 0x7c,
		argtypes = {uint8_t},
		argnames = {'index'},
		desc = 'collisionEvents[<?=index?>].enabled = true',
	}

	ScriptCmds.DisableCollisionEvent = Cmd:subclass{
		cmd = 0x7d,
		argtypes = {uint8_t},
		argnames = {'index'},
		desc = 'collisionEvents[<?=index?>].enabled = false',
	}

	ScriptCmds.ChangeCurrentPartyPosition = Cmd:subclass{
		cmd = 0x7e,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = 'currentParty.pos = {<?=x?>, <?=y?>}',
	}

	ScriptCmds.ChangeCharacterName = Cmd:subclass{
		cmd = 0x7f,
		argtypes = {uint8_t, uint8_t},
		argnames = {'characterIndex', 'nameIndex'},
		desc = "characters[<?=characterIndex?>].name = names[<?=nameIndex?>]",
	}

	ScriptCmds.GiveItem = Cmd:subclass{
		cmd = 0x80,
		argtypes = {uint8_t},
		desc = 'giveItem(<?=args[1]?>)',
	}

	ScriptCmds.TakeItem = Cmd:subclass{
		cmd = 0x81,
		argtypes = {uint8_t},
		desc = 'takeItem(<?=args[1]?>)',
	}

	ScriptCmds.ResetPreviousParty = Cmd:subclass {
		cmd = 0x82,
		desc = 'resetPreviousParty()',
	}

	ScriptCmds.GiveGP = Cmd:subclass{
		cmd = 0x84,
		argtypes = {uint16_t},
		desc = 'giveGP(<?=args[1]?>)',
	}

	ScriptCmds.TakeGP = Cmd:subclass{
		cmd = 0x85,
		argtypes = {uint16_t},
		desc = 'takeGP(<?=args[1]?>)',
	}

	ScriptCmds.GiveEsper = Cmd:subclass{
		cmd = 0x86,
		argtypes = {uint8_t},
		desc = 'giveEsper(<?=args[1]?>)',
	}

	ScriptCmds.TakeEsper = Cmd:subclass{
		cmd = 0x87,
		argtypes = {uint8_t},
		desc = 'takeEsper(<?=args[1]?>)',
	}

	ScriptCmds.RemoveCharacterStatus = Cmd:subclass{
		cmd = 0x88,
		argtypes = {uint8_t, uint16_t},
		argnames = {'characterIndex', 'status'},
		desc = "characters[<?=characterIndex?>].status &= ~0x<?=bit.tohex(4, status)?>",
	}

	ScriptCmds.ToggleCharacterStatus = Cmd:subclass{
		cmd = 0x89,
		argtypes = {uint8_t, uint16_t},
		argnames = {'characterIndex', 'status'},
		desc = "characters[<?=characterIndex?>].status ~~= 0x<?=bit.tohex(4, status)?>",
	}

	ScriptCmds.SetCharacterStatus = Cmd:subclass{
		cmd = 0x8a,
		argtypes = {uint8_t, uint16_t},
		argnames = {'characterIndex', 'status'},
		desc = "characters[<?=characterIndex?>].status |= 0x<?=bit.tohex(4, status)?>",
	}

	ScriptCmds.GiveCharacterHP = Cmd:subclass{
		cmd = 0x8b,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, charIndex, signedAmount)
			self.charIndex = charIndex
			self.amount = bit.lshift(1, bit.band(0x7f, signedAmount))
			self.take = 0 ~= bit.band(0x80, signedAmount)
		end,
		__tostring = function(self)
			return
				"characters["..self.charIndex.."]."
				..(self.mp and 'mp' or 'hp')
				..' '
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
		argtypes = {uint8_t},
		desc = 'characters[<?=args[1]?>]:removeEquipment()',
	}

	ScriptCmds.MonsterInABox = Cmd:subclass{
		cmd = 0x8e,
		desc = 'monsterInABoxBattle()',
	}

	ScriptCmds.UnlockAllSwdTechs = Cmd:subclass{
		cmd = 0x8f,
		desc = 'unlockAllSwdTechs()',
	}

	ScriptCmds.UnlockBumRush = Cmd:subclass{
		cmd = 0x90,
		desc = 'unlockBumRush()',
	}

	ScriptCmds.Sleep15 = Cmd:subclass{
		cmd = 0x91,
		desc = 'sleep(.25)',
	}

	ScriptCmds.Sleep30 = Cmd:subclass{
		cmd = 0x92,
		desc = 'sleep(.5)',
	}

	ScriptCmds.Sleep45 = Cmd:subclass{
		cmd = 0x93,
		desc = 'sleep(.75)',
	}

	ScriptCmds.Sleep60 = Cmd:subclass{
		cmd = 0x94,
		desc = 'sleep(1)',
	}

	ScriptCmds.Sleep120 = Cmd:subclass{
		cmd = 0x95,
		desc = 'sleep(2)',
	}

	ScriptCmds.FadeIn = Cmd:subclass{
		cmd = 0x96,
		desc = 'fadeIn()',
	}

	ScriptCmds.FadeOut = Cmd:subclass{
		cmd = 0x97,
		desc = 'fadeOut()',
	}

	ScriptCmds.OpenCharacterNameChangeMenu = Cmd:subclass{
		cmd = 0x98,
		argtypes = {uint8_t},
		desc = 'characterNameChange(<?=args[1]?>)',
	}

	ScriptCmds.OpenSelectPartyMenu = Cmd:subclass{
		cmd = 0x99,
		argtypes = {uint8_t,uint8_t, uint8_t},
		desc = 'openSelectPartyMenu(<?=args:concat", "?>)',
	}

	ScriptCmds.OpenColosseumMenu  = Cmd:subclass{
		cmd = 0x9a,
		desc = 'openColosseumMenu()',
	}

	ScriptCmds.OpenShopMenu  = Cmd:subclass{
		cmd = 0x9b,
		argtypes = {uint8_t},
		desc = 'openShopMenu(<?=args[1]?>)',
	}

	ScriptCmds.OptimizeCharacterEquipment = Cmd:subclass{
		cmd = 0x9c,
		argtypes = {uint8_t},
		desc = 'characters[<?=args[1]?>]:optimizeEquipment()',
	}

	ScriptCmds.StartTimer = Cmd:subclass{
		cmd = 0xa0,
		argtypes = {uint16_t, uint24_t},
		argnames = {'duration', 'arg'},
		desc = 'startTimer{'
			..'duration=<?=duration?>'
			..', addr=<?=0xa0000 + bit.band(0x3ffff, arg:value())?>'
			..', flags=<?=bit.rshift(arg:value(), 20)?>'
		..'}',
	}

	ScriptCmds.StopTimer = Cmd:subclass{
		cmd = 0xa1,
		argtypes = {uint8_t},
		desc = 'stopTimer(<?=args[1]?>)',
	}

	ScriptCmds.OpenFinalBattleMenu  = Cmd:subclass{
		cmd = 0x9d,
		desc = 'openFinalBattleMenu()',
	}

	ScriptCmds.ShowPyramidObject = Cmd:subclass{
		cmd = 0xa7,
		argtypes = {uint8_t},
		desc = 'showPyramidObject(<?=args[1]?>)',
	}

	ScriptCmds.ShowFloatingIslandCutscene = Cmd:subclass{
		cmd = 0xa8,
		desc = 'cutscene"floating island"',
	}

	ScriptCmds.ShowTitleScreen = Cmd:subclass{
		cmd = 0xa9,
		desc = 'showTitle()',
	}

	ScriptCmds.ShowIntro = Cmd:subclass{
		cmd = 0xaa,
		desc = 'cutscene"intro"',
	}

	ScriptCmds.OpenGameLoadMenu = Cmd:subclass{
		cmd = 0xab,
		desc = 'loadGameMenu()',
	}

	ScriptCmds.LoadSavedCharacterObjectData = Cmd:subclass{
		cmd = 0xac,
		desc = 'loadSavedCharacterObjectData()',
	}

	ScriptCmds.ShowWoRCustscene = Cmd:subclass{
		cmd = 0xad,
		desc = 'cutscene"world of ruin"',
	}

	ScriptCmds.ShowMagitechFactoryCutscene = Cmd:subclass{
		cmd = 0xae,
		desc = 'cutscene"magitech factory"',
	}

	ScriptCmds.BeginRepeat = Cmd:subclass{
		cmd = 0xb0,
		argtypes = {uint8_t},
		desc = 'for i=1,<?=args[1]?>',
	}

	ScriptCmds.EndRepeat = Cmd:subclass{
		cmd = 0xb1,
		desc = 'end--for',
	}

	ScriptCmds.Call = Cmd:subclass{
		cmd = 0xb2,
		argtypes = {uint24_t},
		getargs = function(self, destAddr)
			self.destAddr = startaddr + destAddr:value()
		end,
		__tostring = function(self)
			return ('call $%06x'):format(self.destAddr)
		end,
	}

	ScriptCmds.CallRepeat = Cmd:subclass{
		cmd = 0xb3,
		argtypes = {uint24_t, uint8_t},
		getargs = function(self, destAddr, count)
			self.destAddr = startaddr + destAddr:value()
			self.count = count
		end,
		__tostring = function(self)
			return ('for i=1,%d do call $%06x end'):format(self.count, self.destAddr)
		end,
	}

	ScriptCmds.Sleep = Cmd:subclass{
		cmd = 0xb4,
		argtypes = {uint8_t},
		getargs = function(self, dt)
			self.dt = dt / 60
		end,
		desc = 'sleep(<?=dt?>)'
	}

	ScriptCmds.SleepSeconds = Cmd:subclass{
		cmd = 0xb5,
		argtypes = {uint8_t},
		argnames = {'dt'},
		desc = 'sleep(<?=dt?>)'
	}

	ScriptCmds.CallBasedOnDialogChoice = Cmd:subclass{
		cmd = 0xb6,
		digest = function(self, read)
			self.addrs = table()
			local choices = 2	-- TODO depends on previous dialog text prompt count
			for i=1,choices do
				self.addrs:insert(startaddr + read(uint24_t):value())
			end
		end,
		__tostring = function(self)
			-- TODO is it 'jump' or is it 'call'?
			-- cuz if it's jump then the the next instruction after "want to learn about espers?" shouldn't be a 'return', because each jump option there has its own return ...
			return 'callForDialogChoice('
				..self.addrs:mapi(function(addr)
					return (' $%06x'):format(addr)
				end):concat' '
				..')'
		end,
	}

	ScriptCmds.JumpBasedOnBattleSwitch = Cmd:subclass{
		cmd = 0xb7,
		argtypes = {uint8_t, uint24_t},
		argnames = {'flagIndex', 'destAddrOfs'},
		desc = 'if gameState.battleFlag<?=flagIndex?> then goto <?=("$%06x"):format(0x0a0000 + destAddrOfs:value())?>',
	}

	local SetFlagCmd = Cmd:subclass{
		argtypes = {uint8_t},
		argnames = {'flagIndex'},
		-- value =
		-- flagName =
		desc = 'gameState.<?=flagName?><?=flagIndex?> = <?=value?>',
	}

	ScriptCmds.SetBattleFlag = SetFlagCmd:subclass{
		cmd = 0xb8,
		flagName = 'battleFlag',
		value = true,
	}

	ScriptCmds.ClearBattleFlag = SetFlagCmd:subclass{
		cmd = 0xb9,
		flagName = 'battleFlag',
		value = false,
	}

	ScriptCmds.ShowEndingCharacterCutscene = Cmd:subclass{
		cmd = 0xba,
		argtypes = {uint8_t},
		desc = 'cutscene("ending character <?=args[1]?>")',
	}

	ScriptCmds.ShowEndCutscene = Cmd:subclass{
		cmd = 0xbb,
		desc = 'cutscene"end"',
	}

	ScriptCmds.EndRepeatSwitch = Cmd:subclass{
		cmd = 0xbc,
		argtypes = {uint8_t},
		desc = 'end--for switch',
	}

	ScriptCmds.Jump5050 = Cmd:subclass{
		cmd = 0xbd,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = 'if math.random() < .5 then goto <?=("$%06x"):format(0xa0000 + destAddrOfs:value())?>',
	}

	ScriptCmds.JumpBasedOnCharacterSwitch = Cmd:subclass{
		cmd = 0xbe,
		digest = function(self, read)
			local count = bit.band(0xf, read(uint8_t))
			self.addrs = table()
			for i=1,count do
				self.addrs:insert(read(uint24_t):value())
			end
		end,
		__tostring = function(self)
			return "jump based on characterFlag, count="..#self.addrs
				.." ??? = "..self.addrs:mapi(function(addr)
					return (' $%06x'):format(addr)
				end):concat' '
		end,
	}

	ScriptCmds.ShowAirshipEndingCutscene = Cmd:subclass{
		cmd = 0xbf,
		desc = 'cutscene"airship ending"',
	}

	local Switch = Cmd:subclass{
		digest = function(self, read)
			local count = 1 + bit.band(self.cmd, 7)
			self._and = 0 ~= bit.band(self.cmd, 8)
			self.conds = table()
			for i=1,count do
				local arg = read(uint16_t)
				self.conds:insert{
					gameFlagIndex = bit.band(0x3fff, arg),
					value = bit.rshift(arg, 15),
				}
			end
			self.destAddr = startaddr + read(uint24_t):value()
		end,
		__tostring = function(self)
			return "if "
				..self.conds:mapi(function(cond)
					return 'gameState.flag'..cond.gameFlagIndex..' == '..cond.value
				end):concat(self._and and ' and ' or ' or ')
				..' then goto '..('$%06x'):format(self.destAddr)
		end,
	}
	for cmd=0xc0,0xcf do
		ScriptCmds['Switch '..cmd] = Switch:subclass{cmd = cmd}
	end

	for cmd=0xd0,0xdd do
		ScriptCmds['ChangeFlag'..('0x%02x'):format(cmd)] = Cmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			desc = 'changeFlag <?=args[1]?>',
		}
	end

	for cmd=0xde,0xe4 do
		ScriptCmds['SetControlFlag'..('0x%02x'):format(cmd)] = Cmd:subclass{
			cmd = cmd,
			desc = 'setControlFlag '..('0x%02x'):format(cmd),
		}
	end

	ScriptCmds.ShowCharacterPortrait = Cmd:subclass{
		cmd = 0xe7,
		argtypes = {uint8_t, uint8_t},
		argnames = {'characterIndex', 'portraitIndex'},
		desc = "show character[<?=characterIndex?>] portrait[<?=portraitIndex?>] ",
	}

	ScriptCmds.VarSet = Cmd:subclass{
		cmd = 0xe8,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = <?=value?>',
	}

	ScriptCmds.VarAdd = Cmd:subclass{
		cmd = 0xe9,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] + <?=value?>',
	}

	ScriptCmds.VarSub = Cmd:subclass{
		cmd = 0xea,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] - <?=value?>',
	}

	ScriptCmds.VarCmp = Cmd:subclass{
		cmd = 0xeb,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] < <?=value?> and 1 or 0',	-- idk what this is really
	}

	ScriptCmds.PlaySongVol = Cmd:subclass{
		cmd = 0xef,
		argtypes = {uint8_t, uint8_t},
		desc = 'playSong{'
			..'<?=bit.band(0x7f, args[1])?>'
			..'<?=0~=bit.band(0x80,args[1]) and "altStart=true, " or ""?>'
			..', volume=<?=args[2]?>'
		..'}',
	}

	ScriptCmds.PlaySong = Cmd:subclass{
		cmd = 0xf0,
		argtypes = {uint8_t},
		desc = 'playSong(<?=args[1]?>)',
	}

	ScriptCmds.PlaySongFadeIn = Cmd:subclass{
		cmd = 0xf1,
		argtypes = {uint8_t, uint8_t},
		desc = 'fadeInSong{<?=args[1]?>, speed=<?=args[2]?>}',
	}

	ScriptCmds.PlaySongFadeOut = Cmd:subclass{
		cmd = 0xf2,
		argtypes = {uint8_t, uint8_t},
		desc = 'fadeOutSong{speed=<?=args[1]?>}',
	}

	ScriptCmds.FadeInPrevSong = Cmd:subclass{
		cm = 0xf3,
		argtypes = {uint8_t},
		desc = 'fadeInPrevSong{speed=<?=args[1]?>}',
	}

	ScriptCmds.PlaySound = Cmd:subclass{
		cmd = 0xf4,
		argtypes = {uint8_t},
		argnames = {'sfx'},
		desc = "playSound(<?=sfx?>)",
	}

	ScriptCmds.PlaySoundPan = Cmd:subclass{
		cmd = 0xf5,
		argtypes = {uint8_t, uint8_t, uint8_t},
		argnames = {'sfx', 'pan', 'envelope'},
		desc = 'playSound{sfx=<?=sfx?>, pan=<?=pan?>, envelope=<?=envelope?>}',
	}

	ScriptCmds.SPCInterrupt = Cmd:subclass{
		cmd = 0xf6,
		argtypes = {uint24_t},
		argnames = {'destAddr'},
		desc = 'spcInterrupt <?=destAddr:value()?>',
	}

	ScriptCmds.WaitForSPC = Cmd:subclass{
		cmd = 0xf8,
		desc = 'waitForSPC{port=2}',
	}

	ScriptCmds.SyncSPC = Cmd:subclass{
		cmd = 0xf9,
		argtypes = {uint8_t},
		argnames = {'pos'},
		desc = 'syncSPC{pos=<?=pos?>}',
	}

	ScriptCmds.WaitForSPC = Cmd:subclass{
		cmd = 0xfa,
		desc = 'waitForSPC{port=3}',
	}

	ScriptCmds.Return = Cmd:subclass{
		cmd = 0xfe,
		desc = 'return',
	}

	ScriptCmds.EndScript = Cmd:subclass{
		cmd = 0xff,
		desc = 'endScript()',
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

game.oobEventScriptAddrs = table()	-- TODO handle these too
	local addrsInOrder = table.keys(game.eventScriptAddrs):sort()
		:filteri(function(addr)
			if addr >= startaddr and addr < endaddr then return true end
			game.oobEventScriptAddrs:insert(addr)
			--print('TODO addr', number.hex(addr), 'oob!')
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
				local o = ffi.cast(ptrtype, rom + addr)[0]
				addr = addr + ffi.sizeof(ctype)
				-- if it's a primitive and bitness <= 32
				if ctype == uint8_t
				or ctype == int8_t
				or ctype == uint16_t
				or ctype == int16_t
				or ctype == uint32_t
				or ctype == int32_t
				or ctype == float
				or ctype == double
				then
					o = tonumber(o)
				else
					o = ctype(o)
				end
				return o
			end

--DEBUG:assert.index(ScriptCmds, cmd, "failed to find class for script command")
			local cl = ScriptCmds[cmd]
--DEBUG:assert.eq(cl.class, cl, "class is not a class for command 0x"..number.hex(cmd))
--DEBUG:assert.is(cl, Cmd, "somehow class of command 0x"..number.hex(cmd).." is not of Cmd")
			local cmdobj = cl()
			cmdobj:digest(read)

			cmdobj.addr = cmdaddr
			game.eventScriptCmds:insert(cmdobj)
			game.eventScriptCmdIndexForAddr[cmdaddr] = #game.eventScriptCmds
		end
	end
end
