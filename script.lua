--[[
if battleanim.lua has battle-animation-scripts
then should i call this events.lua for event-scripts?
--]]
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


-- this is used to go to for 'return' so often ...
local commonReturnAddr = 0x0a5eb3


return function(game)
	local Game = game.Game
	local rom = game.rom
	local romsize = game.romsize
	local countof = game.countof

	local uint24_t = game.uint24_t

	-- should I even bounds check?
	local startaddr = ffi.offsetof(Game, 'eventScript')	-- 0xa0000
	local endaddr = ffi.offsetof(Game, 'eventScript') + ffi.sizeof(game.eventScript)

	local startaddr2 = ffi.offsetof(Game, 'dialogBase')
	local endaddr2 = ffi.offsetof(Game, 'dialogBase') + ffi.sizeof(game.dialogBase)

--DEBUG:print('event script ranges:')
--DEBUG:print(('$%06x-$%06x'):format(startaddr, endaddr))
--DEBUG:print(('$%06x-$%06x'):format(startaddr2, endaddr2))

	-- how to generate this in a modular way that both outputs and is reusable later
	-- for now I will insert in-order and provide an address lookup table to the index in this table
	-- but later I could do something like a table from call-address to list-of-instructions that stops at 'return' ... ? or does the script retain a well-defined structure to respect that rule?
	-- one per cmd
	-- TODO tempting to make this just a bunch of structs...
	-- but some cmds are variable-length so ... I can have their args structs but can't have the object as a whole ... unless I make one struct per possible cmd-size ...
	local class = require 'ext.class'

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
	game.Cmd = Cmd	-- parent-class of all script cmds
	-- only for template env
	Cmd.game = game
	Cmd.startaddr = startaddr


	-- here's some interpretation state-variables
	-- that I should probably move into an interpretation-context-object
	local startCmdSet
	local cmdset
	local objectScriptCmd

	-- because we are using it in EventCmds:
	local ObjectCmds = {}


	-- event-commands:

	-- this will hold cmds 0-255 for event-scripts
	local EventCmds = {}
	game.EventCmds = EventCmds
	local EventCmd = Cmd:subclass()
	game.EventCmd = EventCmd

	-- and now during this duration (.length arg), we are using 'object-script' opcodes instead of 'event-script' opcodes...
	EventCmds.ObjectScript = EventCmd:subclass{
		argtypes = {uint8_t},
		getargs = function(self, arg)
			--self.objectIndex = self.cmd
			self.length = bit.band(arg, 0x7f)	-- in bytes, and the last byte is usually always 0xff <->> end-object-script
			self.blocking = 0 ~= bit.band(arg, 0x80)
		end,
		digest = function(self, ...)
			-- if it's an event-script command to start an object-script, then switch our opcodes...
			-- maybe todo push?  or will exiting object-scripts always revert to event-scripts?
			cmdset = ObjectCmds
			assert.eq(objectScriptCmd, nil, "got two object-scripts before the first one ended...")
			objectScriptCmd = self
			return EventCmd.digest(self, ...)	-- call super
		end,
		desc = "fork(||do local obj=objs[<?=cmd?>] -- length=<?=length?><?= blocking and ', block=true' or ''?>"
		-- then upon end, "end)" and if blocking then "joinAll()" on all previous object-script forks
	}
	for i=0x00,0x34 do
		EventCmds['ObjectScript '..i] = EventCmds.ObjectScript:subclass{cmd=i}
	end

	EventCmds.WaitForObject = EventCmd:subclass{
		cmd = 0x35,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "waitFor(objs[<?=objectIndex?>])",
	}

	EventCmds.EnableObjectPassability = EventCmd:subclass{
		cmd = 0x36,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "objs[<?=objectIndex?>].solid = true",
	}

	EventCmds.ChangeObjectSprite = EventCmd:subclass{
		cmd = 0x37,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objectIndex', 'spriteIndex'},
		desc = 'objs[<?=objectIndex?>].sprite = <?=spriteIndex?>',
	}

	EventCmds.LockScreen = EventCmd:subclass{
		cmd = 0x38,
		desc = 'screenLocked = true',
	}

	EventCmds.UnlockScreen = EventCmd:subclass{
		cmd = 0x39,
		desc = 'screenLocked = false',
	}


	EventCmds.EnableUserControl = EventCmd:subclass{
		cmd = 0x3a,
		desc = "userControl = true",
	}

	EventCmds.DisableUserControl = EventCmd:subclass{
		cmd = 0x3b,
		desc = "userControl = false",
	}

	EventCmds.SetPartyCharacters = EventCmd:subclass{
		cmd = 0x3c,
		argtypes = {uint8_t, uint8_t, uint8_t, uint8_t},
		desc = 'setPartyCharacters(<?=args:concat", "?>)',
	}

	EventCmds.CreateObject = EventCmd:subclass{
		cmd = 0x3d,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "createObject(<?=objectIndex?>)",
	}

	EventCmds.DeleteObject = EventCmd:subclass{
		cmd = 0x3e,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = "deleteObject(<?=objectIndex?>)",
	}

	EventCmds.SetCharacterParty = EventCmd:subclass{
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

	EventCmds.ChangeCharacterProperties = EventCmd:subclass{
		cmd = 0x40,
		argtypes = {uint8_t, uint8_t},
		argnames = {'charIndex', 'propIndex'},
		__tostring = function(self)
			return "change character #"..self.charIndex.."'s property #"..self.propIndex
		end,
	}

	EventCmds.ShowObject = EventCmd:subclass{
		cmd = 0x41,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = 'objs[<?=objectIndex?>].visible = true',
	}

	EventCmds.HideObject = EventCmd:subclass{
		cmd = 0x42,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = 'objs[<?=objectIndex?>].visible = false',
	}

	EventCmds.ChangeObjectPalette = EventCmd:subclass{
		cmd = 0x43,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objectIndex', 'paletteIndex'},
		__tostring = function(self)
			return 'change object #'..self.objectIndex..' palette to #'..self.paletteIndex
		end,
	}

	EventCmds.ChangeObjectVehicle = EventCmd:subclass{
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

	EventCmds.UpdateCharacterObjects = EventCmd:subclass{
		cmd = 0x45,
		desc = "update character objects",
	}

	EventCmds.SetActiveParty = EventCmd:subclass{
		cmd = 0x46,
		argtypes = {uint8_t},
		argnames = {'partyIndex'},
		desc = 'setActiveParty(<?=partyIndex?>)',
	}

	EventCmds.CreatePartyObject = EventCmd:subclass{
		cmd = 0x47,
		desc = "createPartyObject()",
	}

	EventCmds.Dialog = EventCmd:subclass{
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

	EventCmds.DialogDontWait = EventCmds.Dialog:subclass{
		cmd = 0x48,
		dontWait = true,
	}

	EventCmds.WaitForDialogWindow = EventCmd:subclass{
		cmd = 0x49,
		desc = "waitForDialogWindow()",
	}

	EventCmds.WaitForDialogButtonPress = EventCmd:subclass{
		cmd = 0x4a,
		desc = "waitForDialogButtonPress()",
	}

	EventCmds.Battle = EventCmd:subclass{
		cmd = 0x4d,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, eventBattleOptionsIndex, arg)
			self.eventBattleOptionsIndex = eventBattleOptionsIndex	-- index into game.monsterEventBattles[]
			self.battleBG = bit.band(0x3f, arg)
			self.noSound = 0 ~= bit.band(0x40, arg)
			self.noBlur = 0 ~= bit.band(0x80, arg)
		end,
		__tostring = function(self)
			return (self.touchBattle and 'touchBattle' or 'battle')
				..' eventBattleOptionsIndex='..self.eventBattleOptionsIndex
				..' background='..bit.band(0x3f, self.battleBG)
				..(self.noSound and ' no sound' or '')
				..(self.noBlur and ' no blur' or '')
		end,
	}

	EventCmds.TouchBattle = EventCmds.Battle:subclass{
		cmd = 0x4c,
		touchBattle = true,
	}

	EventCmds.RandomBattle = EventCmd:subclass{
		cmd = 0x4e,
		desc = "randomBattle()",
	}

	EventCmds.RestoreSavedGame = EventCmd:subclass{
		cmd = 0x4f,
		desc = "reloadSavedGame()",
	}

	EventCmds.ChangeBackgroundPalette = EventCmd:subclass{
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

	EventCmds.ChangeBackgroundPaletteRange = EventCmds.ChangeBackgroundPalette:subclass{
		cmd = 0x51,
		argtypes = {uint8_t, uint8_t, uint8_t},
		argnames = {'arg', 'firstColor', 'lastColor'},
	}

	EventCmds.ChangeSpritePalette = EventCmds.ChangeBackgroundPalette:subclass{
		cmd = 0x52,
		what = 'Sprite',
	}

	EventCmds.ChangeBackgroundPaletteRange = EventCmds.ChangeBackgroundPaletteRange:subclass{
		cmd = 0x53,
		what = 'Sprite',
	}

	EventCmds.DisableFixedColorMath = EventCmd:subclass{
		cmd = 0x54,
		desc = 'disableFixedColorMath()',
	}

	EventCmds.FlashScreenColor = EventCmd:subclass{
		cmd = 0x55,
		argtypes = {uint8_t},
		desc = "flashScreenColor(<?=args[1]?>)",
	}

	EventCmds.SetFixedColorMathAdd = EventCmd:subclass{
		cmd = 0x56,
		argtypes = {uint8_t},
		argnames = {'arg'},
		desc = 'setFixedColorMathAdd(<?=bit.band(7, arg)?>, <?=bit.band(3, bit.rshift(arg, 3))?>, <?=bit.band(3, bit.rshift(arg, 6))?>)',
	}

	EventCmds.SetFixedColorMathSub = EventCmd:subclass{
		cmd = 0x57,
		argtypes = {uint8_t},
		argnames = {'arg'},
		desc = 'setFixedColorMathSub(<?=bit.band(0x7, arg)?>, <?=bit.band(0x3, bit.rshift(arg, 3))?>, <?=bit.band(0x3, bit.rshift(arg, 6))?>)',
	}

	EventCmds.ShakeScreen = EventCmd:subclass{
		cmd = 0x58,
		argtypes = {uint8_t},
		desc = 'shakeScreen(<?=args[1]?>)',
	}

	EventCmds.FadeInRate = EventCmd:subclass{
		cmd = 0x59,
		argtypes = {uint8_t},
		desc = 'fadeIn(<?=args[1]?>)',
	}

	EventCmds.FadeOutRate = EventCmd:subclass{
		cmd = 0x5a,
		argtypes = {uint8_t},
		desc = 'fadeOut(<?=args[1]?>)'
	}

	EventCmds.StopFade = EventCmd:subclass{
		cmd = 0x5b,
		desc = 'stopFade()',
	}

	EventCmds.WaitForFade = EventCmd:subclass{
		cmd = 0x5c,
		desc = 'waitForFade()',
	}

	for _1scomp,cmds in ipairs{
		{0x5d, 0x5e, 0x5f},
		{0x70, 0x71, 0x72}
	} do
		for layer,cmd in ipairs(cmds) do
			EventCmds['ScrollBackground'..('0x%02x'):format(cmd)] = EventCmd:subclass{
				cmd = cmd,
				args = {uint8_t, uint8_t},
				-- 1's comp is negative scrolling?
				desc = 'scrollBackgroundLayer{layer='..layer..(_1scomp==1 and ', 1sCompliment=true' or '')..'}',
			}
		end
	end

	EventCmds.ChangeMapPalette = EventCmd:subclass{
		cmd = 0x60,
		argtypes = {uint8_t, uint8_t},
		argnames = {'dst', 'src'},
		desc = "changeMapPalette(<?=dst?>, <?=src?>)",
	}

	EventCmds.TintColors = EventCmd:subclass{
		cmd = 0x61,
		argtypes = {uint8_t, uint8_t, uint8_t},
		argnames = {'fixed', 'first', 'last'},
		desc = 'tintColors{fixed=<?=fixed?>, first=<?=first?>, last=<?=last?>}',
	}

	EventCmds.PixelateScreen = EventCmd:subclass{
		cmd = 0x62,
		argtypes = {uint8_t},
		desc = 'pixelateScreen(<?=args[1]?>)',
	}

	EventCmds.ShowLamp = EventCmd:subclass{
		cmd = 0x63,
		argtypes = {uint8_t},
		argnames = {'radius'},
		desc = 'showLamp(<?=radius?>)',
	}

	EventCmds.SetMapAnimCounter = EventCmd:subclass{
		cmd = 0x64,
		argtypes = {uint8_t, uint8_t},
		argnames = {'tile', 'frame'},
		desc = 'setMapAnimationCounter{tile=<?=tile?>, frame=<?=frame?>}',
	}

	EventCmds.SetMapAnimCounter = EventCmd:subclass{
		cmd = 0x65,
		argtypes = {uint8_t, uint8_t},
		argnames = {'tile', 'speed'},
		desc = 'setMapAnimationSpeed{tile=<?=tile?>, speed=<?=speed?>}',
	}

	-- used in EventCmds and WorldCmds
	local SetMap = Cmd:subclass{
		argtypes = {uint16_t, uint8_t, uint8_t, uint8_t},
		getargs = function(self, arg, x, y, flags)
			self.x = x
			self.y = y
			self.flags = flags
			self.arg = arg
			-- hmm why is the map index off by 1? and not always too?
			-- off by 1 in the event entering Mt Kolts.
			-- not off by 1 in the event when entering the figaro caves from the figaro castle basement.
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

	EventCmds.SetMap = EventCmd:subclass(SetMap, {cmd = 0x6a})
	EventCmds.SetMap2 = EventCmd:subclass(SetMap, {cmd = 0x6b})

	EventCmds.SetParentMap = EventCmd:subclass{
		cmd = 0x6c,
		argtypes = {uint16_t, uint8_t, uint8_t, uint8_t},
		argnames = {'map', 'dir', 'x', 'y'},
		desc = 'setParentMap{map=<?=map?>, dir=<?=dir?>, x=<?=x?>, y=<?=y?>}',
	}

	EventCmds.ChangeMapLayer = EventCmd:subclass{
		cmd = 0x73,
		argtypes = {uint8_t, uint8_t, uint8_t, uint8_t, uint8_t},
		digest = function(self, read)
			self.x = read(uint8_t)
			local y = read(uint8_t)
			self.y = bit.band(y, 0x3f)
			self.layer = bit.rshift(y, 6)
			self.w = read(uint8_t)
			self.h = read(uint8_t)
			self.data = table()
			for i=0,self.w*self.h-1 do
				self.data:insert(read(uint8_t))
			end
		end,
		extra = ', refresh=true',
		desc = "setMapLayer{layer=<?=layer+1?>, x=<?=x?>, y=<?=y?>, w=<?=w?>, h=<?=h?>, data={<?=data:concat', '?>}<?=extra?>}",
	}

	EventCmds.ChangeMapLayerNoRefresh = EventCmds.ChangeMapLayer:subclass{
		cmd = 0x74,
		extra = '',	-- no refresh=true
	}

	EventCmds.RefreshMapLayerChanges = EventCmd:subclass{
		cmd = 0x75,
		desc = 'refreshMapLayerChanges()',
	}

	EventCmds.RestoreCharacterToFullHPMP = EventCmd:subclass{
		cmd = 0x77,
		argtypes = {uint8_t},
		argnames = {'charIndex'},
		desc = 'characters[<?=charIndex?>]:setFullHPMP()',
	}

	EventCmds.DisablePassabilityOfObject = EventCmd:subclass{
		cmd = 0x78,
		argtypes = {uint8_t},
		argnames = {'objectIndex'},
		desc = 'objs[<?=objectIndex?>].solid = false',
	}

	EventCmds.MovePartyToMap = EventCmd:subclass{
		cmd = 0x79,
		argtypes = {uint8_t, uint16_t},
		argnames = {'partyIndex', 'mapIndex'},
		desc = 'party[<?=partyIndex?>]:setMap(<?=mapIndex?>)',
	}

	EventCmds.ChangeObjectEvent = EventCmd:subclass{
		cmd = 0x7a,
		argtypes = {uint8_t, uint24_t},
		argnames = {'objectIndex', 'newScriptAddrOfs'},
		desc = "objs[<?=objectIndex?>].script = <?=('$%06x'):format(startaddr + newScriptAddrOfs)?>",
	}

	EventCmds.RestorePreviousParty = EventCmd:subclass{
		cmd = 0x7b,
		desc = 'restorePreviousParty()',
	}

	EventCmds.EnableCollisionEvent = EventCmd:subclass{
		cmd = 0x7c,
		argtypes = {uint8_t},
		argnames = {'index'},
		desc = 'collisionEvents[<?=index?>].enabled = true',
	}

	EventCmds.DisableCollisionEvent = EventCmd:subclass{
		cmd = 0x7d,
		argtypes = {uint8_t},
		argnames = {'index'},
		desc = 'collisionEvents[<?=index?>].enabled = false',
	}

	EventCmds.ChangeCurrentPartyPosition = EventCmd:subclass{
		cmd = 0x7e,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = 'currentParty.pos = {<?=x?>, <?=y?>}',
	}

	EventCmds.ChangeCharacterName = EventCmd:subclass{
		cmd = 0x7f,
		argtypes = {uint8_t, uint8_t},
		argnames = {'characterIndex', 'nameIndex'},
		desc = "characters[<?=characterIndex?>].name = names[<?=nameIndex?>]",
	}

	EventCmds.GiveItem = EventCmd:subclass{
		cmd = 0x80,
		argtypes = {uint8_t},
		argnames = {'itemIndex'},
		desc = 'giveItem(<?=("%q"):format(tostring(game.itemNames[itemIndex]))?>)',
	}

	EventCmds.TakeItem = EventCmd:subclass{
		cmd = 0x81,
		argtypes = {uint8_t},
		argnames = {'itemIndex'},
		desc = 'takeItem(<?=("%q"):format(tostring(game.itemNames[itemIndex]))?>)',
	}

	EventCmds.ResetPreviousParty = EventCmd:subclass {
		cmd = 0x82,
		desc = 'resetPreviousParty()',
	}

	EventCmds.GiveGP = EventCmd:subclass{
		cmd = 0x84,
		argtypes = {uint16_t},
		desc = 'giveGP(<?=args[1]?>)',
	}

	EventCmds.TakeGP = EventCmd:subclass{
		cmd = 0x85,
		argtypes = {uint16_t},
		desc = 'takeGP(<?=args[1]?>)',
	}

	EventCmds.GiveEsper = EventCmd:subclass{
		cmd = 0x86,
		argtypes = {uint8_t},
		desc = 'giveEsper(<?=args[1]?>)',
	}

	EventCmds.TakeEsper = EventCmd:subclass{
		cmd = 0x87,
		argtypes = {uint8_t},
		desc = 'takeEsper(<?=args[1]?>)',
	}

	EventCmds.RemoveCharacterStatus = EventCmd:subclass{
		cmd = 0x88,
		argtypes = {uint8_t, uint16_t},
		argnames = {'characterIndex', 'status'},
		desc = "characters[<?=characterIndex?>].status &= ~0x<?=bit.tohex(status, 4)?>",
	}

	EventCmds.ToggleCharacterStatus = EventCmd:subclass{
		cmd = 0x89,
		argtypes = {uint8_t, uint16_t},
		argnames = {'characterIndex', 'status'},
		desc = "characters[<?=characterIndex?>].status ~~= 0x<?=bit.tohex(status, 4)?>",
	}

	EventCmds.SetCharacterStatus = EventCmd:subclass{
		cmd = 0x8a,
		argtypes = {uint8_t, uint16_t},
		argnames = {'characterIndex', 'status'},
		desc = "characters[<?=characterIndex?>].status |= 0x<?=bit.tohex(status, 4)?>",
	}

	EventCmds.GiveCharacterHP = EventCmd:subclass{
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

	EventCmds.GiveCharacterMP = EventCmds.GiveCharacterHP:subclass{
		cmd = 0x8c,
		mp = true,
	}

	EventCmds.RemoveCharactersEquipment = EventCmd:subclass{
		cmd = 0x8d,
		argtypes = {uint8_t},
		desc = 'characters[<?=args[1]?>]:removeEquipment()',
	}

	EventCmds.MonsterInABox = EventCmd:subclass{
		cmd = 0x8e,
		desc = 'monsterInABoxBattle()',
	}

	EventCmds.UnlockAllSwdTechs = EventCmd:subclass{
		cmd = 0x8f,
		desc = 'unlockAllSwdTechs()',
	}

	EventCmds.UnlockBumRush = EventCmd:subclass{
		cmd = 0x90,
		desc = 'unlockBumRush()',
	}

	EventCmds.Sleep15 = EventCmd:subclass{
		cmd = 0x91,
		desc = 'sleep(1/4)',
	}

	EventCmds.Sleep30 = EventCmd:subclass{
		cmd = 0x92,
		desc = 'sleep(1/2)',
	}

	EventCmds.Sleep45 = EventCmd:subclass{
		cmd = 0x93,
		desc = 'sleep(3/4)',
	}

	EventCmds.Sleep60 = EventCmd:subclass{
		cmd = 0x94,
		desc = 'sleep(1)',
	}

	EventCmds.Sleep120 = EventCmd:subclass{
		cmd = 0x95,
		desc = 'sleep(2)',
	}

	EventCmds.FadeIn = EventCmd:subclass{
		cmd = 0x96,
		desc = 'fadeIn()',
	}

	EventCmds.FadeOut = EventCmd:subclass{
		cmd = 0x97,
		desc = 'fadeOut()',
	}

	EventCmds.OpenCharacterNameChangeMenu = EventCmd:subclass{
		cmd = 0x98,
		argtypes = {uint8_t},
		desc = 'characterNameChange(<?=args[1]?>)',
	}

	EventCmds.OpenSelectPartyMenu = EventCmd:subclass{
		cmd = 0x99,
		argtypes = {uint8_t,uint8_t, uint8_t},
		desc = 'openSelectPartyMenu(<?=args:concat", "?>)',
	}

	EventCmds.OpenColosseumMenu  = EventCmd:subclass{
		cmd = 0x9a,
		desc = 'openColosseumMenu()',
	}

	EventCmds.OpenShopMenu  = EventCmd:subclass{
		cmd = 0x9b,
		argtypes = {uint8_t},
		desc = 'openShopMenu(<?=args[1]?>)',
	}

	EventCmds.OptimizeCharacterEquipment = EventCmd:subclass{
		cmd = 0x9c,
		argtypes = {uint8_t},
		desc = 'characters[<?=args[1]?>]:optimizeEquipment()',
	}

	EventCmds.StartTimer = EventCmd:subclass{
		cmd = 0xa0,
		argtypes = {uint16_t, uint24_t},
		argnames = {'duration', 'arg'},
		desc = 'startTimer{'
			..'duration=<?=duration?>'
			..', addr=<?=startaddr + bit.band(0x3ffff, arg)?>'
			..', flags=<?=bit.rshift(arg, 20)?>'
		..'}',
	}

	EventCmds.StopTimer = EventCmd:subclass{
		cmd = 0xa1,
		argtypes = {uint8_t},
		desc = 'stopTimer(<?=args[1]?>)',
	}

	EventCmds.OpenFinalBattleMenu  = EventCmd:subclass{
		cmd = 0x9d,
		desc = 'openFinalBattleMenu()',
	}

	EventCmds.ShowPyramidObject = EventCmd:subclass{
		cmd = 0xa7,
		argtypes = {uint8_t},
		desc = 'showPyramidObject(<?=args[1]?>)',
	}

	EventCmds.ShowFloatingIslandCutscene = EventCmd:subclass{
		cmd = 0xa8,
		desc = 'cutscene"floating island"',
	}

	EventCmds.ShowTitleScreen = EventCmd:subclass{
		cmd = 0xa9,
		desc = 'showTitle()',
	}

	EventCmds.ShowIntro = EventCmd:subclass{
		cmd = 0xaa,
		desc = 'cutscene"intro"',
	}

	EventCmds.OpenGameLoadMenu = EventCmd:subclass{
		cmd = 0xab,
		desc = 'loadGameMenu()',
	}

	EventCmds.LoadSavedCharacterObjectData = EventCmd:subclass{
		cmd = 0xac,
		desc = 'loadSavedCharacterObjectData()',
	}

	EventCmds.ShowWoRCustscene = EventCmd:subclass{
		cmd = 0xad,
		desc = 'cutscene"world of ruin"',
	}

	EventCmds.ShowMagitechFactoryCutscene = EventCmd:subclass{
		cmd = 0xae,
		desc = 'cutscene"magitech factory"',
	}

	EventCmds.BeginRepeat = EventCmd:subclass{
		cmd = 0xb0,
		argtypes = {uint8_t},
		desc = 'for i=1,<?=args[1]?>',
	}

	EventCmds.EndRepeat = EventCmd:subclass{
		cmd = 0xb1,
		desc = 'end--for',
	}

	EventCmds.Call = EventCmd:subclass{
		cmd = 0xb2,
		argtypes = {uint24_t},
		getargs = function(self, destAddrOfs)
			self.destAddr = startaddr + destAddrOfs
		end,
		__tostring = function(self)
			return ('call $%06x'):format(self.destAddr)
		end,
	}

	EventCmds.CallRepeat = EventCmd:subclass{
		cmd = 0xb3,
		argtypes = {uint24_t, uint8_t},
		getargs = function(self, destAddrOfs, count)
			self.destAddr = startaddr + destAddrOfs
			self.count = count
		end,
		__tostring = function(self)
			return ('for i=1,%d do call $%06x end'):format(self.count, self.destAddr)
		end,
	}

	EventCmds.Sleep = EventCmd:subclass{
		cmd = 0xb4,
		argtypes = {uint8_t},
		getargs = function(self, frames)
			self.frames = frames
			self.seconds = frames / 60
		end,
		desc = 'sleep(<?=frames?>/60)'
	}

	EventCmds.SleepSeconds = EventCmd:subclass{
		cmd = 0xb5,
		argtypes = {uint8_t},
		argnames = {'seconds'},
		desc = 'sleep(<?=seconds?>)'
	}

	EventCmds.CallBasedOnDialogChoice = EventCmd:subclass{
		cmd = 0xb6,
		digest = function(self, read)
			self.addrs = table()
			local choices = 2	-- TODO depends on previous dialog text prompt count
			for i=1,choices do
				self.addrs:insert(startaddr + read(uint24_t))
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

	EventCmds.JumpBasedOnBattleSwitch = EventCmd:subclass{
		cmd = 0xb7,
		argtypes = {uint8_t, uint24_t},
		argnames = {'flagIndex', 'destAddrOfs'},
		desc = 'if gameState.battleFlag<?=flagIndex?> then goto <?=("$%06x"):format(startaddr + destAddrOfs)?>',
	}

	local EventSetBattleFlag = EventCmd:subclass{
		argtypes = {uint8_t},
		argnames = {'flagIndex'},
		desc = 'gameState.battleFlag<?=flagIndex?> = <?=value?>',
	}
	EventCmds.SetBattleFlag = EventSetBattleFlag:subclass{
		cmd = 0xb8,
		value = true,
	}
	EventCmds.ClearBattleFlag = EventSetBattleFlag:subclass{
		cmd = 0xb9,
		value = false,
	}

	EventCmds.ShowEndingCharacterCutscene = EventCmd:subclass{
		cmd = 0xba,
		argtypes = {uint8_t},
		desc = 'cutscene("ending character <?=args[1]?>")',
	}

	EventCmds.ShowEndCutscene = EventCmd:subclass{
		cmd = 0xbb,
		desc = 'cutscene"end"',
	}

	EventCmds.EndRepeatSwitch = EventCmd:subclass{
		cmd = 0xbc,
		argtypes = {uint8_t},
		desc = 'end--for switch',
	}

	EventCmds.Jump5050 = EventCmd:subclass{
		cmd = 0xbd,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = 'if math.random() < .5 then goto <?=("$%06x"):format(startaddr + destAddrOfs)?>',
	}

	EventCmds.JumpBasedOnCharacterSwitch = EventCmd:subclass{
		cmd = 0xbe,
		digest = function(self, read)
			local count = bit.band(0xf, read(uint8_t))
			self.addrs = table()
			for i=1,count do
				self.addrs:insert(read(uint24_t))
			end
		end,
		__tostring = function(self)
			return "jump based on characterFlag, count="..#self.addrs
				.." ??? = "..self.addrs:mapi(function(addr)
					return (' $%06x'):format(addr)
				end):concat' '
		end,
	}

	EventCmds.ShowAirshipEndingCutscene = EventCmd:subclass{
		cmd = 0xbf,
		desc = 'cutscene"airship ending"',
	}

	-- common parent class for EventCmd and WorldCmd
	local Switch = Cmd:subclass{
		digest = function(self, read)
			local count = 1 + bit.band(self.cmd, 7)
			self._and = 0 ~= bit.band(self.cmd, 8)
			self.conds = table()
			for i=1,count do
				local arg = read(uint16_t)
				self.conds:insert{
					mapFlagIndex = bit.band(0x3fff, arg),
					value = 0 ~= bit.rshift(arg, 15),
				}
			end
			self.destAddr = startaddr + read(uint24_t)
		end,
		__tostring = function(self)
			-- which flag? npc flag? map flag? treasure flag? etc flag?
			local gotostmt =
				-- used often enough as a return stmt...
				self.destAddr == commonReturnAddr
				and 'return'
				or 'goto '..('$%06x'):format(self.destAddr)
			return "if "
				..self.conds:mapi(function(cond)
					--[[ integer values ...
					return 'gameState.mapFlag'..cond.mapFlagIndex..' == '..(cond.value and '1' or '0')
					--]]
					-- [[ boolean values ...
					return (not cond.value and 'not ' or '')
						..'gameState.mapFlag'..cond.mapFlagIndex
					--]]
				end):concat(self._and and ' and ' or ' or ')
				..' then '..gotostmt..' end'
		end,
	}
	-- in EventCmds for 0xc0-0xcf and in WorldCmds for 0xb0-0xbf
	for cmd=0xc0,0xcf do
		EventCmds['Switch '..cmd] = EventCmd:subclass(Switch, {cmd = cmd})
	end

	for cmd=0xd0,0xdd do
		EventCmds['SetFlag'..('0x%02x'):format(cmd)] = EventCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			getargs = function(self, flagIndex)
				-- 0 = set, 1 = clear
				self.value = 0 == bit.band(1, cmd)
				self.flagIndex = bit.bor(
					bit.lshift(bit.band(self.cmd, 0xe), 7),	-- move bits 1:3 to bits 8:10
					flagIndex
				)
			end,
			desc = 'gameState.mapFlag<?=flagIndex?> = <?=tostring(value)?>',
		}
	end

	for cmd=0xde,0xe4 do
		EventCmds['SetControlFlag'..('0x%02x'):format(cmd)] = EventCmd:subclass{
			cmd = cmd,
			desc = 'setControlFlag '..('0x%02x'):format(cmd),
		}
	end

	EventCmds.ShowCharacterPortrait = EventCmd:subclass{
		cmd = 0xe7,
		argtypes = {uint8_t, uint8_t},
		argnames = {'characterIndex', 'portraitIndex'},
		desc = "show character[<?=characterIndex?>] portrait[<?=portraitIndex?>] ",
	}

	EventCmds.VarSet = EventCmd:subclass{
		cmd = 0xe8,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = <?=value?>',
	}

	EventCmds.VarAdd = EventCmd:subclass{
		cmd = 0xe9,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] + <?=value?>',
	}

	EventCmds.VarSub = EventCmd:subclass{
		cmd = 0xea,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] - <?=value?>',
	}

	EventCmds.VarCmp = EventCmd:subclass{
		cmd = 0xeb,
		argtypes = {uint8_t, uint8_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] < <?=value?> and 1 or 0',	-- idk what this is really
	}

	EventCmds.PlaySongVol = EventCmd:subclass{
		cmd = 0xef,
		argtypes = {uint8_t, uint8_t},
		desc = 'playSong{'
			..'<?=bit.band(0x7f, args[1])?>'
			..'<?=0~=bit.band(0x80,args[1]) and "altStart=true, " or ""?>'
			..', volume=<?=args[2]?>'
		..'}',
	}

	EventCmds.PlaySong = EventCmd:subclass{
		cmd = 0xf0,
		argtypes = {uint8_t},
		desc = 'playSong(<?=args[1]?>)',
	}

	EventCmds.PlaySongFadeIn = EventCmd:subclass{
		cmd = 0xf1,
		argtypes = {uint8_t, uint8_t},
		desc = 'fadeInSong{<?=args[1]?>, speed=<?=args[2]?>}',
	}

	EventCmds.PlaySongFadeOut = EventCmd:subclass{
		cmd = 0xf2,
		argtypes = {uint8_t, uint8_t},
		desc = 'fadeOutSong{speed=<?=args[1]?>}',
	}

	EventCmds.FadeInPrevSong = EventCmd:subclass{
		cm = 0xf3,
		argtypes = {uint8_t},
		desc = 'fadeInPrevSong{speed=<?=args[1]?>}',
	}

	EventCmds.PlaySound = EventCmd:subclass{
		cmd = 0xf4,
		argtypes = {uint8_t},
		argnames = {'sfx'},
		desc = "playSound(<?=sfx?>)",
	}

	EventCmds.PlaySoundPan = EventCmd:subclass{
		cmd = 0xf5,
		argtypes = {uint8_t, uint8_t, uint8_t},
		argnames = {'sfx', 'pan', 'envelope'},
		desc = 'playSound{sfx=<?=sfx?>, pan=<?=pan?>, envelope=<?=envelope?>}',
	}

	EventCmds.SPCInterrupt = EventCmd:subclass{
		cmd = 0xf6,
		argtypes = {uint24_t},
		argnames = {'destAddr'},
		desc = 'spcInterrupt <?=destAddr?>',
	}

	EventCmds.WaitForSPC = EventCmd:subclass{
		cmd = 0xf8,
		desc = 'waitForSPC{port=2}',
	}

	EventCmds.SyncSPC = EventCmd:subclass{
		cmd = 0xf9,
		argtypes = {uint8_t},
		argnames = {'pos'},
		desc = 'syncSPC{pos=<?=pos?>}',
	}

	EventCmds.WaitForSPC = EventCmd:subclass{
		cmd = 0xfa,
		desc = 'waitForSPC{port=3}',
	}

	EventCmds.Return = EventCmd:subclass{
		cmd = 0xfe,
		desc = 'return',
	}

	EventCmds.EndScript = EventCmd:subclass{
		cmd = 0xff,
		desc = 'endScript()',
	}

	-- EventCmds key by cmd (number) or by name (string)
	for _,k in ipairs(table.keys(EventCmds)) do
		local cl = EventCmds[k]
		if cl.cmd then	-- some abstract classes are in EventCmds but don't have a .cmd
			assert.type(cl.cmd, 'number')
			EventCmds[cl.cmd] = cl
		end
	end
	for i=0,255 do
		if not EventCmds[i] then
			EventCmds[i] = EventCmd:subclass{
				cmd = i,
				desc = '??? '..('0x%02x'):format(i),
			}
		end
	end




	-- ok now comes object-commands
	game.ObjectCmds = ObjectCmds
	local ObjectCmd = Cmd:subclass()
	game.ObjectCmd = ObjectCmd

	-- also in WorldCmds
	ObjectCmds.Action = ObjectCmd:subclass{
		desc = "<?=0~=bit.band(0x40, cmd) and 'obj.hflip=true ' or ''?>"
			.."obj:doAction(<?=bit.band(cmd, 0x3f)?>)",
	}
	for cmd=0,0x7f do
		ObjectCmds['Action '..cmd] = ObjectCmds.Action:subclass{
			cmd = cmd,
		}
	end

	ObjectCmds.Move = ObjectCmd:subclass{
		desc = 'obj.dir = <?=bit.band(cmd, 3)?> '
			..'obj:walkForward(<?=bit.band(bit.rshift(cmd, 2), 7)?>)',
	}
	for cmd=0x80,0x9f do
		ObjectCmds['Move '..cmd] = ObjectCmds.Move:subclass{cmd=cmd}
	end

	ObjectCmds.MoveDiagonal = ObjectCmd:subclass{
		--[[
		0: Up/Right 1×1,
		1: Down/Right 1×1,
		2: Down/Left 1×1,
		3: Up/Left 1×1,
		4: Up/Right 2×1,
		5: Up/Right 1×2,
		6: Down/Right 1×2,
		7: Down/Right 2×1,
		8: Down/Left 2×1,
		9: Down/Left 1×2,
		10: Up/Left 1×2,
		11: Up/Left 2×1
		--]]
		desc = 'obj:moveDiagonal(<?=cmd?>)',
	}
	for cmd=0xa0,0xab do
		ObjectCmds['MoveDiagonal '..cmd] = ObjectCmds.MoveDiagonal:subclass{cmd = cmd}
	end

	ObjectCmds.SetSpeed = ObjectCmd:subclass{
		desc = 'obj:setSpeed(<?=bit.band(cmd, 0xf)?>)',
	}
	for cmd=0xc0,0xc5 do
		ObjectCmds['SetSpeed '..cmd] = ObjectCmds.SetSpeed:subclass{cmd = cmd}
	end

	ObjectCmds.EnableAnimation = ObjectCmd:subclass{
		cmd = 0xc6,
		desc = 'obj:enableAnimation()',
	}
	ObjectCmds.DisableAnimation = ObjectCmd:subclass{
		cmd = 0xc7,
		desc = 'obj:disableAnimation()',
	}

	ObjectCmds.ChangeLayerPriority = ObjectCmd:subclass{
		cmd = 0xc8,
		argtypes = {uint8_t},
		argnames = {'arg'},
		-- is this applied to the current-object ?
		-- 0=default 1=top sprite only, 2=foreground, 3=background
		desc = 'changeLayerPriority(<?=arg?>)',
	}

	ObjectCmds.ChangeVehicle = ObjectCmd:subclass{
		cmd = 0xc9,
		argtypes = {uint8_t},
		argnames = {'vehicleIndex'},
		desc = 'obj:setVehicle{vehicle=<?=bit.band(0x7f, vehicleIndex)?>, showRider=<?=0 ~= bit.band(0x80, vehicleIndex)?>}',
	}

	ObjectCmds.ChangeDir = ObjectCmd:subclass{
		desc = 'obj:look(<?=bit.band(cmd, 3)?>)',
	}
	for cmd=0xcc,0xcf do
		ObjectCmds['ChangeDir '..cmd] = ObjectCmds.ChangeDir:subclass{cmd = cmd}
	end

	ObjectCmds.ShowObject = ObjectCmd:subclass{
		cmd = 0xd0,
		desc = 'obj.visible = true',
	}

	ObjectCmds.HideObject = ObjectCmd:subclass{
		cmd = 0xd1,
		desc = 'obj.visible = false',
	}

	ObjectCmds.SetPos = ObjectCmd:subclass{
		cmd = 0xd5,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = 'obj:setPos(<?=x?>, <?=y?>)',
	}

	ObjectCmds.ScrollToObject = ObjectCmd:subclass{
		cmd = 0xd7,
		desc = 'obj:scrollTo()',
	}

	ObjectCmds.Jump = ObjectCmd:subclass{
		cmd = 0xdc,
		desc = 'obj:jump()',
	}
	ObjectCmds.JumpHigh = ObjectCmds.Jump:subclass{
		cmd = 0xdd,
		desc = 'obj:jumpHigh()',
	}

	ObjectCmds.Sleep = ObjectCmd:subclass{
		cmd = 0xe0,
		argtypes = {uint8_t},
		argnames = {'frames'},
		desc = 'sleep(<?=frames?> * 4 / 60)',
	}

	for cmd=0xe1,0xe6 do
		ObjectCmds['ChangeFlag'..('0x%02x'):format(cmd)] = ObjectCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			-- not making sense of the json. what's the cmd for?
			desc = 'gameState.eventFlag<?=args[1]?> ~~= true',
		}
	end

	ObjectCmds.Goto = ObjectCmd:subclass{
		cmd = 0xf9,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = 'goto <?=("$%06x"):format(startaddr + destAddrOfs)?>',
	}

	ObjectCmds.Branch = ObjectCmd:subclass{
		--cmd = 0xfa,
		argtypes = {uint8_t},
		argnames = {'offset'},
		desc = "<?=random and 'if math.random() < .5 then ' or ''?>goto PC<?=dir?><?=offset?>",
	}
	ObjectCmds.BranchBack50 = ObjectCmds.Branch:subclass{
		cmd = 0xfa,
		random = true,
		dir = '-',
	}
	ObjectCmds.BranchFwd50 = ObjectCmds.Branch:subclass{
		cmd = 0xfb,
		random = true,
		dir = '+',
	}
	ObjectCmds.BranchBack = ObjectCmds.Branch:subclass{
		cmd = 0xfc,
		random = false,
		dir = '-',
	}
	ObjectCmds.BranchFwd = ObjectCmds.Branch:subclass{
		cmd = 0xfd,
		random = false,
		dir = '+',
	}

	-- also in EventEmds ... and WorldCmds ... basically everywhere
	ObjectCmds.EndScript = ObjectCmd:subclass{
		cmd = 0xff,
		digest = function(self, ...)
			assert.ne(objectScriptCmd, nil, "got an object end-script when there was no objectScriptCmd set ...")
			if self.addr ~= objectScriptCmd.addr + objectScriptCmd.length + 1 then
				print("!!! DANGER !!! object-script length doesn't align with end-of-script cmd:", ('$%06x'):format(self.addr), 'vs', ('$%06x'):format(objectScriptCmd.addr + objectScriptCmd.length + 1))
			end
			cmdset = startCmdSet	-- back you go ...
			objectScriptCmd = nil
		end,
		desc = 'end)',	-- and joinAll() if the objectScriptCmd had blocking ...
	}


	-- map the by-number for by-name
	for _,k in ipairs(table.keys(ObjectCmds)) do
		local cl = ObjectCmds[k]
		if cl.cmd then
			assert.type(cl.cmd, 'number')
			ObjectCmds[cl.cmd] = cl
		end
	end
	-- fill in empty numbers with unknowns
	for i=0,255 do
		if not ObjectCmds[i] then
			ObjectCmds[i] = ObjectCmd:subclass{
				cmd = i,
				desc = '??? '..('0x%02x'):format(i),
			}
		end
	end



	-- now comes world-commands
	-- this is the opcodes of the touch triggers in the world maps
	local WorldCmds = {}
	game.WorldCmds = WorldCmds
	local WorldCmd = Cmd:subclass()
	game.WorldCmd = WorldCmd

	-- same as in ObjectCmds
	WorldCmds.Action = WorldCmd:subclass{
		desc = "<?=0~=bit.band(0x40, cmd) and 'obj.hflip=true ' or ''?>"
			.."obj:doAction(<?=bit.band(cmd, 0x3f)?>)",
	}
	for cmd=0,0x7f do
		WorldCmds['Action '..cmd] = WorldCmds.Action:subclass{
			cmd = cmd,
		}
	end

	-- also in WorldCmds
	WorldCmds.Move = WorldCmd:subclass{
		desc = 'obj.dir = <?=bit.band(cmd, 3)?> '
			..'obj:walkForward(<?=bit.band(bit.rshift(cmd, 2), 7)?>)',
	}
	for cmd=0x80,0x9f do
		WorldCmds['Move '..cmd] = WorldCmds.Move:subclass{cmd=cmd}
	end

	-- also in WorldCmds
	WorldCmds.MoveDiagonal = WorldCmd:subclass{
		desc = 'obj:moveDiagonal(<?=cmd?>)',
	}
	for cmd=0xa0,0xab do
		WorldCmds['MoveDiagonal '..cmd] = WorldCmds.MoveDiagonal:subclass{cmd = cmd}
	end


	-- in EventCmds as 0x6a, 0x6b
	WorldCmds.SetMap = WorldCmd:subclass(SetMap, {cmd = 0xd2})
	WorldCmds.SetMap2 = WorldCmd:subclass(SetMap, {cmd = 0xd3})

	-- in EventCmds for 0xc0-0xcf and in WorldCmds for 0xb0-0xbf
	for cmd=0xb0,0xbf do
		WorldCmds['Switch '..cmd] = WorldCmd:subclass(Switch, {cmd = cmd})
	end

	-- same as in ObjectCmds
	WorldCmds.SetSpeed = WorldCmd:subclass{
		desc = 'obj:setSpeed(<?=bit.band(cmd, 0xf)?>)',
	}
	for cmd=0xc0,0xc5 do
		WorldCmds['SetSpeed '..cmd] = WorldCmds.SetSpeed:subclass{cmd = cmd}
	end

	-- kinda like EventCmds EventSetBattleFlag but with a bigger arg
	local WorldSetFlag = WorldCmd:subclass{
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		desc = 'gameState.mapFlag<?=flagIndex?> = <?=value?>',
	}
	WorldCmds.SetBattleFlag = WorldSetFlag:subclass{
		cmd = 0xb8,
		value = true,
	}
	WorldCmds.ClearBattleFlag = WorldSetFlag:subclass{
		cmd = 0xb9,
		value = false,
	}

	-- same as in ObjectCmds
	WorldCmds.ChangeDir = WorldCmd:subclass{
		desc = 'obj:look(<?=bit.band(cmd, 3)?>)',
	}
	for cmd=0xcc,0xcf do
		WorldCmds['ChangeDir '..cmd] = WorldCmds.ChangeDir:subclass{cmd = cmd}
	end

	-- same as ObjectCmds, but with the current-object being the party sprite ...
	WorldCmds.ShowObject = WorldCmd:subclass{
		cmd = 0xd0,
		desc = 'obj.visible = true',
	}

	WorldCmds.HideObject = WorldCmd:subclass{
		cmd = 0xd1,
		desc = 'obj.visible = false',
	}

	WorldCmds.IfKeyThenGoto = WorldCmd:subclass{
		cmd = 0xd4,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = 'if keypress then goto <?=("$%06x"):format(startaddr + destAddrOfs)?>',
	}

	WorldCmds.IfFacingThenGoto = WorldCmd:subclass{
		cmd = 0xd5,
		argtypes = {uint8_t, uint24_t},
		argnames = {'dir', 'destAddrOfs'},
		desc = 'if dir==<?=dir?> then goto <?=("$%06x"):format(startaddr + destAddrOfs)?>',
	}

	-- also EventCmds 0x96
	WorldCmds.FadeIn = WorldCmd:subclass{
		cmd = 0xd8,
		desc = 'fadeIn()',
	}

	-- also EventCmds 0x97
	WorldCmds.FadeOut = WorldCmd:subclass{
		cmd = 0xd9,
		desc = 'fadeOut()',
	}

	WorldCmds.ShowMiniMap = WorldCmd:subclass{
		cmd = 0xdd,
		desc = 'miniMapVisible = true',
	}
	WorldCmds.HideMiniMap = WorldCmd:subclass{
		cmd = 0xdf,
		desc = 'miniMapVisible = false',
	}

	-- also in ObjectCmds
	WorldCmds.Sleep = WorldCmd:subclass{
		cmd = 0xe0,
		argtypes = {uint8_t},
		argnames = {'frames'},
		desc = 'sleep(<?=frames?> * 4 / 60)',
	}

	WorldCmds.ChangeToShip = WorldCmd:subclass{
		cmd = 0xfc,
		desc = 'changeToShip()',
	}

	WorldCmds.FigaroSubmerge = WorldCmd:subclass{
		cmd = 0xfd,
		desc = 'figaroSubmerge()',
	}

	WorldCmds.FigaroEmerge = WorldCmd:subclass{
		cmd = 0xfe,
		desc = 'figaroEmerge()',
	}

	WorldCmds.EndScript = WorldCmd:subclass{
		cmd = 0xff,
		desc = 'endScript()',
	}


	-- map the by-number for by-name
	for _,k in ipairs(table.keys(WorldCmds)) do
		local cl = WorldCmds[k]
		if cl.cmd then
			assert.type(cl.cmd, 'number')
			WorldCmds[cl.cmd] = cl
		end
	end
	-- fill in empty numbers with unknowns
	for i=0,255 do
		if not WorldCmds[i] then
			WorldCmds[i] = WorldCmd:subclass{
				cmd = i,
				desc = '??? '..('0x%02x'):format(i),
			}
		end
	end




	-- useful function
	local function readAndInc(ctype, addr)
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
		elseif ctype == uint24_t then
			o = o:value()
		else
			o = ctype(o)
		end
		return o, addr
	end


	-- event code, pointed into by NPCs and maybe other things
	game.eventScriptAddrs = {}

	-- collect script addrs from npcs
	-- hmm should I use the master npc list or just which ones are in maps?
	-- should I cache disassembled chunks and only traverse instructions that are actually used? and track memory regions used?
	for mapIndex=0,countof(game.maps)-1 do
		local mapInfo = game.getMap(mapIndex)
		if mapInfo then

			-- if the mapIndex < 3 then it's a world-script, otherwise it's an event-script
			-- ... right?
			game.eventScriptAddrs[mapInfo.startEventScriptAddr] = game.eventScriptAddrs[mapInfo.startEventScriptAddr] or table()
			game.eventScriptAddrs[mapInfo.startEventScriptAddr]:insert{
				type = 'startEventScriptAddr',
				mapIndex = mapIndex,
			}

			for npcIndex,n in ipairs(mapInfo.npcs) do
				local scriptAddr = n:getScriptAddr()
				if scriptAddr then
					game.eventScriptAddrs[scriptAddr] = game.eventScriptAddrs[scriptAddr] or table()
					game.eventScriptAddrs[scriptAddr]:insert{
						npcIndex = npcIndex-1,
						mapIndex = mapIndex,
					}
				end
			end

			for touchTriggerIndex,t in ipairs(mapInfo.touchTriggers) do
				local scriptAddr = t:getScriptAddr()
				if scriptAddr then
					-- if the mapIndex < 3 then it's a world-script, otherwise it's an event-script
					game.eventScriptAddrs[scriptAddr] = game.eventScriptAddrs[scriptAddr] or table()
					game.eventScriptAddrs[scriptAddr]:insert{
						touchTriggerIndex = touchTriggerIndex-1,
						mapIndex = mapIndex,
					}
				end
			end
		end
	end



	game.eventScriptCmds = table()
	game.eventScriptCmdIndexForAddr = {}

	local addrsInOrder = table.keys(game.eventScriptAddrs):sort()

	-- warn about OOB addrs...
	for _,addr in ipairs(addrsInOrder) do
		if not (addr >= startaddr and addr < endaddr)
		and not (addr >= startaddr2 and addr < endaddr2)
		then
			print('TODO addr', number.hex(addr), 'oob!')	-- nothing found anymore, I guess that's good? does it matter?
		end
	end


	for i,startAddr in ipairs(addrsInOrder) do
		local nextAddr = addrsInOrder[i+1] or endaddr

		-- reset script decode state
		startCmdSet = nil
		local mapIndexes = game.eventScriptAddrs[startAddr]:mapi(function(info) return info.mapIndex end):sort()
		for _,info in ipairs(game.eventScriptAddrs[startAddr]) do
			if info.mapIndex < 3 then
				if not startCmdSet then
					startCmdSet = WorldCmds
				elseif startCmdSet ~= WorldCmds then
					if startAddr ~= commonReturnAddr then
						-- this will happen with those generic 'return' functions...
						print("!!! DANGER !!! got an addr used for both world and non-world map script:", ('$%06x'):format(startAddr), mapIndexes:mapi(tostring):concat', ')
						break
					end
				end
			else
				-- maybe 'event' should be 'non-world' or nah?
				if not startCmdSet then
					startCmdSet = EventCmds
				elseif startCmdSet ~= EventCmds then
					if startAddr ~= commonReturnAddr then
						-- this will happen with those generic 'return' functions...
						print("!!! DANGER !!! got an addr used for both world and non-world map script:", ('$%06x'):format(startAddr), mapIndexes:mapi(tostring):concat', ')
						break
					end
				end
			end
		end
		-- default us to EventCmds?
		if not startCmdSet then
			print("!!! DANGER !!! got an addr without a mapIndex set:", ('$%06x'):format(startAddr), mapIndexes:mapi(tostring):concat', ')
			startCmdSet = EventCmds
		end

		cmdset = startCmdSet
		objectScriptCmd = nil

		local addr = startAddr
		while addr < nextAddr do
			local function read(ctype)
				local o
				o, addr = readAndInc(ctype, addr)
				return o
			end

			local cmdaddr = addr
			local cmd = read(uint8_t)

--DEBUG:assert.index(cmdset, cmd, "failed to find class for script command")
			local cl = cmdset[cmd]
--DEBUG:assert.eq(cl.class, cl, "class is not a class for command 0x"..number.hex(cmd))
--DEBUG:assert.is(cl, Cmd, "somehow class of command 0x"..number.hex(cmd).." is not of Cmd")
			local cmdobj = cl()
			cmdobj.addr = cmdaddr

			-- hmm instead of just 'read' with 'addr', how about a whole interpretation-state, with 'cmdset' too?
			cmdobj:digest(read)

			game.eventScriptCmds:insert(cmdobj)
			game.eventScriptCmdIndexForAddr[cmdaddr] = #game.eventScriptCmds
		end

		if objectScriptCmd then
			print('!!! DANGER !!! event-script ended still inside an object-script:', ('$%06x'):format(startAddr))
		end
	end




	-- from addr to table-of-cmds, which should reassemble to produce the same bytes that is located at that addr to begin with *fingers crossed*
	-- hmm this whole process is assuming the cmds are not reusing bytes for other cmds
	-- hmmmmmm
	-- maybe I should do this trace first and skip the global disasm
	local traces = table()

	-- returns a table of disassembled cmds for this addr
	-- if it encounnters any gosubs/gotos then it'll disassemble and cache those as well
	local function traceCode(addr, instrSet)

	end


end
