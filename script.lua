--[[
still todo:
- 0a5ada ... needs to change to world-cmds somehow, but idk what determines this, proly need to trace cmd flow better
- 0a5edc ... is another of these.
	the start of the routine is map #0 i.e. world-map-cmds
	... then the condition before is "set map to 242 and return"
	but that is changing my cmdset to map #242 and off map #0 ...
	... hmmmmm

if battleanim.lua has battle-animation-scripts
then should i call this events.lua for event-scripts?
--]]
local ffi = require 'ffi'
local assert = require 'ext.assert'
local table = require 'ext.table'
local number = require 'ext.number'
local string = require 'ext.string'
local class = require 'ext.class'
local tolua = require 'ext.tolua'
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
	game.addrLabel = function(addr)
		return ('$%06x'):format(addr)
	end

	local Game = game.Game
	local rom = game.rom
	local romsize = game.romsize
	local countof = game.countof

	local uint24_t = game.uint24_t

	-- should I even bounds check?
	local scriptBaseAddr = ffi.offsetof(Game, 'eventScript')	-- 0xa0000
	local scriptBaseAddrEnd = ffi.offsetof(Game, 'eventScript') + ffi.sizeof(game.eventScript)

	local scriptBaseAddr2 = ffi.offsetof(Game, 'dialogBase')
	local scriptBaseAddrEnd2 = ffi.offsetof(Game, 'dialogBase') + ffi.sizeof(game.dialogBase)

--DEBUG:print('event script ranges:')
--DEBUG:print(game.addrLabel(scriptBaseAddr)..'-'..game.addrLabel(scriptBaseAddrEnd))
--DEBUG:print(game.addrLabel(scriptBaseAddr2)..'-'..game.addrLabel(scriptBaseAddrEnd2))

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
	Cmd.__concat = string.concat
	function Cmd:__tostring()
		if self.descstr then return self.descstr end
		return template(self.desc, self)
	end
	-- only for template env
	Cmd.game = game
	Cmd.scriptBaseAddr = scriptBaseAddr
	-- useful for the template env:
	Cmd.getGotoOfsStr = function(addrOfs, op)
		local addr = scriptBaseAddr + addrOfs
		local str
		if addr == commonReturnAddr then
			--str = 'return' 	-- "call return" is gonna look weird :shrug:
			return 'return'
		else
			str = game.addrLabel(addr)
		end
		if not op then
			return 'goto ::'..str..'::'
		end
		-- is it 'goto return' as in we jmp to the instruction and its a rts and so we return?
		--  and therefore a 'goto return' is the same as just 'return'
		-- but then how about the 'call' ... does that push to the same stack as whatever invoked the script in the first place,
		--  such that 'call return' is a 'nop'?
		--  or are all 'call return's erroneous, in that if the pc stack is different between invoking touch/event s and 'call' within the script, then a 'call return' would produce some kind of error?
		return op..str
	end

	-- generic cmds when you need a superclass transcending any specific cmdset:
	local Cmds = {}
	game.Cmds = Cmds
	Cmds.Cmd = Cmd	-- parent-class of all script cmds


	-- because we are using it in EventCmds:
	local EventCmds = {}
	local WorldCmds = {}
	local ObjectCmds = {}
	local VehicleCmds = {}


	-- event-commands:

	-- this will hold cmds 0-255 for event-scripts
	game.EventCmds = EventCmds
	local EventCmd = Cmd:subclass()
	game.EventCmd = EventCmd

	-- and now during this duration (.length arg), we are using 'object-script' opcodes instead of 'event-script' opcodes...
	EventCmds.ObjectScript = EventCmd:subclass{
		argtypes = {uint8_t},
		getargs = function(self, arg)
			--self.objIndex = self.cmd
			self.length = bit.band(arg, 0x7f)	-- in bytes, and the last byte is usually always 0xff <->> end-object-script
			self.blocking = 0 ~= bit.band(arg, 0x80)
		end,
		digest = function(self, ...)
			-- if it's an event-script command to start an object-script, then switch our opcodes...
			-- maybe todo push?  or will exiting object-scripts always revert to event-scripts?
--DEBUG:assert.len(self.trace.stateStack, 1, "can we go from something to event-cmds to object-cmds?")

			assert.eq(self.trace.stateStack:last().objectScriptCmd, nil, "got two object-scripts before the first one ended...")

			self.trace.stateStack:insert{
				cmdset = 'ObjectCmds',
				objectScriptCmd = self,
			}
			self.trace.indent = self.trace.indent + 1

			EventCmd.digest(self, ...)	-- call super
		end,
		desc = "objScript{"
			.."objIndex=<?=cmd?>"
			.."<?= blocking and ', block=true' or ''?>"
			..", cb=|objIndex|do"
		-- then upon end, "end}" and if blocking then "joinAll()" on all previous object-script forks
	}
	for i=0x00,0x34 do
		EventCmds['ObjectScript '..i] = EventCmds.ObjectScript:subclass{cmd=i}
	end

	EventCmds.WaitForObject = EventCmd:subclass{
		cmd = 0x35,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = "objWaitFor(<?=objIndex?>)",
	}

	EventCmds.EnableObjectPassability = EventCmd:subclass{
		cmd = 0x36,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = "objSetSolid(<?=objIndex?>, true)",
	}

	EventCmds.ChangeObjectSprite = EventCmd:subclass{
		cmd = 0x37,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objIndex', 'spriteIndex'},
		desc = 'objSetSprite(<?=objIndex?>, <?=spriteIndex?>)',
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
		desc = 'partySetChars(<?=args:concat", "?>)',
	}

	EventCmds.CreateObject = EventCmd:subclass{
		cmd = 0x3d,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = "createObject(<?=objIndex?>)",
	}

	EventCmds.DeleteObject = EventCmd:subclass{
		cmd = 0x3e,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = "deleteObject(<?=objIndex?>)",
	}

	EventCmds.SetCharacterParty = EventCmd:subclass{
		cmd = 0x3f,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objIndex', 'partyIndex'},
		desc = 'charSetParty(<?=objIndex?>, <?=partyIndex?>)',
	}

	EventCmds.CharResetStats = EventCmd:subclass{
		cmd = 0x40,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objIndex', 'statsIndex'},
		desc = 'charResetStats(<?=objIndex?>, <?=statsIndex?>)',
	}

	EventCmds.ShowObject = EventCmd:subclass{
		cmd = 0x41,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objSetVisible(<?=objIndex?>, true)',
	}

	EventCmds.HideObject = EventCmd:subclass{
		cmd = 0x42,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objSetVisible(<?=objIndex?>, false)',
	}

	EventCmds.ChangeObjectPalette = EventCmd:subclass{
		cmd = 0x43,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objIndex', 'paletteIndex'},
		__tostring = function(self)
			return 'change object #'..self.objIndex..' palette to #'..self.paletteIndex
		end,
	}

	EventCmds.ChangeObjectVehicle = EventCmd:subclass{
		cmd = 0x44,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, objIndex, arg)
			self.objIndex = objIndex
			-- TODO why to use struct bitfields......
			self.vehicleIndex = bit.band(3, bit.rshift(arg, 5))
			self.showRider = 0 ~= bit.band(0x80, arg)
		end,
		__tostring = function(self)
			return "change object #"..self.objIndex
				.." to vehicle #"..self.vehicleIndex
				..(self.showRider and ", show rider" or "")
		end,
	}

	EventCmds.UpdateCharacterObjects = EventCmd:subclass{
		cmd = 0x45,
		desc = "sortObjects()",
	}

	EventCmds.SetActiveParty = EventCmd:subclass{
		cmd = 0x46,
		argtypes = {uint8_t},
		argnames = {'partyIndex'},
		desc = 'setActiveParty(<?=partyIndex?>)',
	}

	EventCmds.CreatePartyObject = EventCmd:subclass{
		cmd = 0x47,
		desc = "updateParty()",
	}

	EventCmds.Dialog = EventCmd:subclass{
		cmd = 0x4b,
		argtypes = {uint16_t},
		getargs = function(self, arg)
			self.dialogIndex = bit.band(0x3fff, arg)
			self.showTextOnly = 0 ~= bit.band(0x4000, arg)
			self.bottomOfScreen = 0 ~= bit.band(0x8000, arg)

			local dlg = game.dialog[self.dialogIndex]
			if dlg then
				self.trace.lastDialogPromptCount = select(2, dlg:gsub('%[PROMPT%]', '%0'))
			end
		end,
		__tostring = function(self)
			local dlg = game.dialog[self.dialogIndex]
			local str = dlg and tolua(tostring(dlg)) or 'nil'
			if self.dontWait
			or self.showTextOnly
			or self.bottomOfScreen
			then
				return --'show dialog[0x'..number.hex(self.dialogIndex)..']:'
					'dialog('..str
					..', {'
					..table()
					:append{self.dontWait and 'dontWait=true' or nil}
					:append{self.showTextOnly and 'showTextOnly=true' or nil}
					:append{self.bottomOfScreen and 'bottomOfScreen=true' or nil}
					:concat','
				..'})'
			else
				return 'dialog'..tolua(tostring(dlg))
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

	local Battle = Cmd:subclass{
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, eventBattleOptionsIndex, arg)
			self.eventBattleOptionsIndex = eventBattleOptionsIndex	-- index into game.monsterEventBattles[]
			self.battleBG = bit.band(0x3f, arg)
			self.noSound = 0 ~= bit.band(0x40, arg)
			self.noBlur = 0 ~= bit.band(0x80, arg)
		end,
		__tostring = function(self)
			return 'battle{'
				..'eventBattleOptionsIndex='..self.eventBattleOptionsIndex
				..', background='..bit.band(0x3f, self.battleBG)
				..(self.noSound and ', noSound=true' or '')
				..(self.noBlur and ', noBlur=true' or '')
				..(self.extra and ', '..self.extra or '')
				..'}'
		end,
	}
	Cmds.Battle = Battle

	EventCmds.Battle = EventCmd:subclass(Battle, {cmd = 0x4d})

	EventCmds.TouchBattle = EventCmd:subclass(Battle, {
		cmd = 0x4c,
		extra = "onTouch=true",
	})

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
		paletteName = 'Background',
		desc = 'change<?=paletteName?>Palette{'
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
		paletteName = 'Sprite',
	}

	EventCmds.ChangeSpritePaletteRange = EventCmds.ChangeBackgroundPaletteRange:subclass{
		cmd = 0x53,
		paletteName = 'Sprite',
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
				argtypes = {int8_t, int8_t},
				argnames = {'hscroll', 'vscroll'},
				layer = layer,
				_1scomp = _1scomp==1,
				-- 1's comp is negative scrolling?
				desc = 'scrollBackgroundLayer{'
					..'layer=<?=layer?>'
					..'<?=_1scomp and ", 1sCompliment=true" or ""?>'
					..', hscroll=<?=hscroll?>'
					..', vscroll=<?=vscroll?>'
					..'}',
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

			-- does setting vehicle imply switching to vehicle opcodes?
			--  if so, then does setting vehicle to nonzero in ObjectCmds.ChangeVehicle or EventCmds.ChangeObjectVehicle also do this?
			--  also is it for event cmds, world cmds, or vehicle cmds? (ironic for vehicle cmds, you already need to be in this state...)
			self.vehicle = bit.band(3, flags)

			self.arg = arg
			-- hmm why is the map index off by 1? and not always too?
			-- off by 1 in the event entering Mt Kolts.
			-- not off by 1 in the event when entering the figaro caves from the figaro castle basement.
			self.mapIndex = bit.band(arg, 0x1ff)


			-- another 'gotcha' ...
			-- looks like if we set-map to maps 0-2 then we should also change our (underlying, non-vehicle) cmdset to world?
			local newCmdSet
			if self.mapIndex < 3 then
				newCmdSet = 'WorldCmds'
			--[[ this used to be here but it seems there are random cmdset changes between arbitrary commands
			-- so I just moved all the conditional stuff into the tracer
			elseif self.mapIndex == 511 then
				-- now we have to track previous map ... ?
				-- I guess there's only a few of these ...
			--]]
			else
				newCmdSet = 'EventCmds'
			end
			self.trace.stateStack[1].cmdset = newCmdSet

			if self.vehicle == 0 then
				-- now if we are clearing vehicle...
				-- if we were not in vehicle then nothing
				-- if we were in vehicle ... then pop state
				if self.trace.stateStack:last().cmdset == 'VehicleCmds' then
					self.trace.stateStack:remove()
				end
--DEBUG:assert.ge(#self.trace.stateStack, 1, "popped our last state stack when leaving vehicle state...")
--DEBUG:assert.ne(self.trace.stateStack:last().cmdset, 'VehicleCmds', "popped vehicle state and still ended up in vehicle state...")
			else
				-- you can set from the base event/world cmdset
				-- or you can also set while in the vehicle cmdset (in which case, don't push anything on the stack...)
				--assert.len(self.trace.stateStack, 1, "tried to set-map set-vehicle within a sub-state!")

				-- switching to vehicle state doesn't track length like switching to object state does
				-- hmm are the two orthogonal or not?
				-- i.e. can we switch-to-object, switch-to-vehicle, then end object script (while still processing vehicle cmds)
				-- I guess never since there is no Object SetMap
				-- so we're safe there.
				if self.trace.stateStack:last().cmdset ~= 'VehicleCmds' then
					self.trace.stateStack:insert{
						cmdset = 'VehicleCmds',
					}
				end
			end
		end,
		__tostring = function(self)
			return "setMap{"
				.."mapIndex="..self.mapIndex
				..(0 ~= bit.band(0x0200, self.arg) and ", setParentMap=true" or "")
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x0400, self.arg) and ", zLevel=1" or "")
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x0800, self.arg) and ", showMapTitle=true" or '')
				..(", dir="..bit.band(3, bit.rshift(self.arg, 12)))
				..(", pos={"..('0x%02x'):format(self.x)..", "..('0x%02x'):format(self.y).."}")
				..(", vehicle="..self.vehicle)
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x20, self.flags) and ", noSizeUpdate=true" or '')
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x40, self.flags) and ", manualFadeIn=true" or '')
				..(self.mapIndex >= 3 and 0 ~= bit.band(0x80, self.flags) and ", enableEapEvent=true" or '')
			..'}'
		end,
	}
	Cmds.SetMap = SetMap

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
		argnames = {'objIndex'},
		desc = 'objSetFullHPMP(<?=objIndex?>])',
	}

	EventCmds.DisablePassabilityOfObject = EventCmd:subclass{
		cmd = 0x78,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objSetSolid(<?=objIndex?>, false)',
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
		argnames = {'objIndex', 'newScriptAddrOfs'},
		desc = "objSetScript(<?=objIndex?>, <?=getGotoOfsStr(newScriptAddrOfs, '')?>)",

		getBranchAddrs = function(self)
			return {
				-- this will always use event-cmds, whereas typical getBranchAddrs for branch/goto/call will preserve cmdset
				{addr = scriptBaseAddr + self.newScriptAddrOfs, cmdset = 'EventCmds'},
			}
		end,
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
		desc = 'curPartySetPos(<?=x?>, <?=y?>)',
	}

	EventCmds.ChangeCharacterName = EventCmd:subclass{
		cmd = 0x7f,
		argtypes = {uint8_t, uint8_t},
		argnames = {'objIndex', 'nameIndex'},
		desc = "objSetName(<?=objIndex?>, <?=nameIndex?>)",
	}

	EventCmds.GiveItem = EventCmd:subclass{
		cmd = 0x80,
		argtypes = {uint8_t},
		argnames = {'itemIndex'},
		desc = 'giveItem(<?=itemIndex?>)',
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

	-- is there except for 'remove none' meaning 'remove all' ?
	-- also, chracterIndex is 0-15 ... or party indexes 49-52 ...
	--  so really this is an object command and not a character command, right?
	EventCmds.RemoveObjectStatus = EventCmd:subclass{
		cmd = 0x88,
		argtypes = {uint8_t, uint16_t},
		argnames = {'objIndex', 'status'},
		desc = "objRemoveStatus(<?=objIndex?>, 0x<?=bit.tohex(status, 4)?>)",
	}

	EventCmds.SetObjectStatus = EventCmd:subclass{
		cmd = 0x89,
		argtypes = {uint8_t, uint16_t},
		argnames = {'objIndex', 'status'},
		desc = "objSetStatus(<?=objIndex?>, 0x<?=bit.tohex(status, 4)?>)",
	}

	EventCmds.ToggleObjectStatus = EventCmd:subclass{
		cmd = 0x8a,
		argtypes = {uint8_t, uint16_t},
		argnames = {'objIndex', 'status'},
		desc = "objToggleStatus(<?=objIndex?>, 0x<?=bit.tohex(status, 4)?>)",
	}

	EventCmds.GiveCharacterHP = EventCmd:subclass{
		cmd = 0x8b,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, objIndex, signedAmount)
			self.objIndex = objIndex
			local log2amount = bit.band(0x7f, signedAmount)
			self.max = log2amount == 0x7f
			self.amount = bit.lshift(1, log2amount)
			self.take = 0 ~= bit.band(0x80, signedAmount)
			local hpmp = self.mp and 'MP' or 'HP'
			self.descstr = 'objInc'..hpmp
				..'('
				..self.objIndex..', '
				..(self.take and '-' or '')
				..(self.max and 'math.huge' or self.amount)
				..')'
		end,
	}

	EventCmds.GiveCharacterMP = EventCmds.GiveCharacterHP:subclass{
		cmd = 0x8c,
		mp = true,
	}

	EventCmds.RemoveCharactersEquipment = EventCmd:subclass{
		cmd = 0x8d,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objUnequipAll(<?=objIndex?>)',
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
		argnames = {'characterIndex'},
		desc = 'characterNameChange(<?=characterIndex?>)',
	}

	EventCmds.OpenSelectPartyMenu = EventCmd:subclass{
		cmd = 0x99,
		argtypes = {uint8_t,uint8_t, uint8_t},
		desc = 'openSelectPartyMenu(<?=args:concat", "?>)',
	}

	EventCmds.OpenColosseumMenu = EventCmd:subclass{
		cmd = 0x9a,
		desc = 'openColosseumMenu()',
	}

	EventCmds.OpenShopMenu = EventCmd:subclass{
		cmd = 0x9b,
		argtypes = {uint8_t},
		argnames = {'shopIndex'},
		desc = 'openShopMenu(<?=shopIndex?>)',
	}

	EventCmds.OptimizeCharacterEquipment = EventCmd:subclass{
		cmd = 0x9c,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objOptEquip(<?=objIndex?>)',
	}

	EventCmds.OpenFinalBattleMenu = EventCmd:subclass{
		cmd = 0x9d,
		desc = 'openFinalBattleMenu()',
	}

	EventCmds.StartTimer = EventCmd:subclass{
		cmd = 0xa0,
		argtypes = {uint16_t, uint24_t},
		argnames = {'duration', 'arg'},
		getargs = function(self, duration, arg)
			self.duration = duration
			self.destAddrOfs = bit.band(0x3ffff, arg)
			self.flags = bit.rshift(arg, 20)
		end,
		desc = 'startTimer{'
			..'duration=<?=duration?>'
			..', flags=<?=flags?>'
			..', cb=<?=getGotoOfsStr(destAddrOfs)?>'
		..'}',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}

	EventCmds.StopTimer = EventCmd:subclass{
		cmd = 0xa1,
		argtypes = {uint8_t},
		desc = 'stopTimer(<?=args[1]?>)',
	}

	EventCmds.ClearOverlay = EventCmd:subclass{
		cmd = 0xa2,
		desc = 'clearOverlay()',
	}

	EventCmds.ColosseumBattle = EventCmd:subclass{
		cmd = 0xaf,
		desc = 'colosseumBattle()',
	}

	EventCmds.HidePyramidObject = EventCmd:subclass{
		cmd = 0xa6,
		desc = 'hidePyramidObject()',
	}

	EventCmds.ShowPyramidObject = EventCmd:subclass{
		cmd = 0xa7,
		argtypes = {uint8_t},
		desc = 'showPyramidObject(<?=args[1]?>)',
	}

	EventCmds.ShowFloatingIslandCutscene = EventCmd:subclass{
		cmd = 0xa8,
		desc = "cutscene'floating island'",
	}

	EventCmds.ShowTitleScreen = EventCmd:subclass{
		cmd = 0xa9,
		desc = "cutscene'title'",
	}

	EventCmds.ShowIntro = EventCmd:subclass{
		cmd = 0xaa,
		desc = "cutscene'intro'",
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
		desc = "cutscene'world of ruin'",
	}

	EventCmds.ShowMagitechFactoryCutscene = EventCmd:subclass{
		cmd = 0xae,
		desc = "cutscene'magitech factory'",
	}

	EventCmds.BeginRepeat = EventCmd:subclass{
		cmd = 0xb0,
		argtypes = {uint8_t},
		argnames = {'count'},
		getargs = function(self, ...)
			EventCmd.getargs(self, ...)
			self.count = self.count + 1
			self.trace.indent = self.trace.indent + 1
		end,
		desc = 'for i=1,<?=count?> do',
	}

	EventCmds.EndRepeat = EventCmd:subclass{
		cmd = 0xb1,
		getargs = function(self)
			self.trace.indent = self.trace.indent - 1
		end,
		desc = 'end--for',
	}

	EventCmds.Call = EventCmd:subclass{
		cmd = 0xb2,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = "<?=getGotoOfsStr(destAddrOfs, 'call ')?>",

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}

	EventCmds.CallRepeat = EventCmd:subclass{
		cmd = 0xb3,
		argtypes = {uint8_t, uint24_t},
		argnames = {'count', 'destAddrOfs'},
		desc = "for i=1,<?=count?> do <?=getGotoOfsStr(destAddrOfs, 'call ')?> end",

		-- used by decompiler to determine where else to go
		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
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

	-- so this, for 12, is 3 seconds
	-- so this, for 4, is 1 second
	-- so this is really wait-1/4-second?
	-- and that makes it really 15 out of 60 frames?
	-- but don't we already have those commands?
	-- yeah, in object- world- and vehicle- cmds,
	-- so this is it in event- cmds as well.
	-- but really TODO fix alllll these sleep commands
	EventCmds.SleepSeconds = EventCmd:subclass{
		cmd = 0xb5,
		argtypes = {uint8_t},
		argnames = {'seconds'},
		desc = 'sleep(<?=seconds?>/15)'
	}

	EventCmds.GotoForDialogResult = EventCmd:subclass{
		cmd = 0xb6,
		digest = function(self, read)
			self.addrs = table()
			if not self.trace.lastDialogPromptCount then error("required choices but no dialog at "..game.addrLabel(self.addr)) end
			for i=1,self.trace.lastDialogPromptCount do
				self.addrs:insert(scriptBaseAddr + read(uint24_t))
			end
		end,
		__tostring = function(self)
			return 'callForDialogChoice('
				..self.addrs:mapi(function(addr)
					return game.addrLabel(addr, '')
				end):concat', '
				..')'
		end,

		getBranchAddrs = function(self)
			return self.addrs:mapi(function(addr)
				return {addr=addr}
			end)
		end,
	}

	EventCmds.JumpBasedOnBattleFlag = EventCmd:subclass{
		cmd = 0xb7,
		argtypes = {uint8_t, uint24_t},
		argnames = {'flagIndex', 'destAddrOfs'},
		desc = 'if gameState.battleFlag<?=flagIndex?> then <?=getGotoOfsStr(destAddrOfs)?> end',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
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
		desc = "cutscene'end'",
	}

	EventCmds.EndRepeatSwitch = EventCmd:subclass{
		cmd = 0xbc,
		argtypes = {uint16_t},
		argnames = {'cond'},
		desc = 'if cond <?=cond?> then break',
	}

	EventCmds.Jump5050 = EventCmd:subclass{
		cmd = 0xbd,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = 'if math.random() < .5 then <?=getGotoOfsStr(destAddrOfs)?> end',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}

	-- this is based on active char?
	-- or whether you have the char in your party?
	-- or based on opcode 0xe1?
	EventCmds.JumpBasedOnNPCFlag = EventCmd:subclass{
		cmd = 0xbe,
		digest = function(self, read)
			-- what's in the upper nibble here?
			local count = bit.band(0xf, read(uint8_t))
			self.options = table()
			for i=1,count do
				local addr = read(uint24_t)
				-- everything's says only 18 bits of address are used
				-- and then the top 4 are the character
				-- so there's 2 more bits left ...
				self.options:insert{
					characterIndex = bit.rshift(addr, 20),
					addrOfs = bit.band(0x3ffff, addr),
				}
			end
		end,
		__tostring = function(self)
			return 'callForCharacter{'
			..self.options:mapi(function(option)
				return '{'..option.characterIndex..', '..Cmd.getGotoOfsStr(option.addrOfs)..'}'
			end):concat', '..'}'
		end,

		getBranchAddrs = function(self)
			return self.options:mapi(function(option)
				return {addr=scriptBaseAddr + option.addrOfs}
			end)
		end,
	}

	EventCmds.ShowAirshipEndingCutscene = EventCmd:subclass{
		cmd = 0xbf,
		desc = "cutscene'airship ending'",
	}

	-- common parent class for EventCmd and WorldCmd
	local Cond = Cmd:subclass{
		flagName = 'mapFlag',
		digest = function(self, read)
			local count = 1 + bit.band(self.cmd, 7)
			self._and = 0 ~= bit.band(self.cmd, 8)
			self.conds = table()
			for i=1,count do
				local arg = read(uint16_t)
				self.conds:insert{
					flagIndex = bit.band(0x3fff, arg),
					value = 0 ~= bit.rshift(arg, 15),
				}
			end
			self.destAddrOfs = read(uint24_t)

			-- precache:
			self.condCode = self.conds:mapi(function(cond)
					return (not cond.value and 'not ' or '')
						-- which flag? npc flag? map flag? treasure flag? etc flag?
						..'gameState.'..self.flagName..cond.flagIndex
				end):concat(self._and and ' and ' or ' or ')
		end,
		desc = 'if <?=condCode?> then <?=getGotoOfsStr(destAddrOfs)?> end',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}
	Cmds.Cond = Cond

	-- in EventCmds for 0xc0-0xcf and in WorldCmds for 0xb0-0xbf
	for cmd=0xc0,0xcf do
		EventCmds['Cond '..cmd] = EventCmd:subclass(Cond, {cmd = cmd})
	end

	for cmd=0xd0,0xdd do
		EventCmds['SetFlag'..('0x%02x'):format(cmd)] = EventCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			getargs = function(self, flagIndex)
				-- 0 = set, 1 = clear
				self.value = 0 == bit.band(1, self.cmd)
				self.flagIndex = bit.bor(
					bit.lshift(bit.band(self.cmd, 0xe), 7),	-- move bits 1:3 to bits 8:10
					flagIndex
				)
				self.descstr = 'gameState.mapFlag'..self.flagIndex
					..' = '..tostring(self.value)
			end,
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
		argtypes = {uint8_t},
		argnames = {'characterIndex'},
		desc = "showPortraitForCharacter(<?=characterIndex?>)",
	}

	EventCmds.VarSet = EventCmd:subclass{
		cmd = 0xe8,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = <?=value?>',
	}

	EventCmds.VarAdd = EventCmd:subclass{
		cmd = 0xe9,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] + <?=value?>',
	}

	EventCmds.VarSub = EventCmd:subclass{
		cmd = 0xea,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] - <?=value?>',
	}

	EventCmds.VarCmp = EventCmd:subclass{
		cmd = 0xeb,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		desc = 'vars[<?=var?>] = vars[<?=var?>] < <?=value?> and 1 or 0',	-- idk what this is really
	}

	EventCmds.PlaySongVol = EventCmd:subclass{
		cmd = 0xef,
		argtypes = {uint8_t, uint8_t},
		argnames = {'song', 'volume'},
		desc = 'playSong{'
			..'<?=bit.band(0x7f, song)?>'
			..'<?=0 ~= bit.band(0x80, song) and "altStart=true, " or ""?>'
			..', volume=<?=volume?>'
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
		argnames = {'song', 'speed'},
		desc = 'fadeInSong{'
			..'song=<?=song?>'
			..', speed=<?=bit.band(0x7f, speed)?>'
			..'<?=0 ~= bit.band(0x80, speed) and ", altStart=true" or ""?>'
			..'}',
	}

	EventCmds.PlaySongFadeOut = EventCmd:subclass{
		cmd = 0xf2,
		argtypes = {uint8_t},
		argnames = {'speed'},
		desc = 'fadeOutSong{speed=<?=speed?>}',
	}

	EventCmds.FadeInPrevSong = EventCmd:subclass{
		cmd = 0xf3,
		argtypes = {uint8_t},
		argnames = {'speed'},
		desc = 'fadeInPrevSong{speed=<?=speed?>}',
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

	EventCmds.ContinueSong = EventCmd:subclass{
		cmd = 0xf7,
		desc = 'continueSong()',
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

	EventCmds.WaitForSound = EventCmd:subclass{
		cmd = 0xfb,
		desc = 'waitforSound()',
	}

	local Return = Cmd:subclass{
		desc = 'return',
		endTrace = true,
	}
	game.Cmds.Return = Return

	EventCmds.Return = EventCmd:subclass(Return, {
		cmd = 0xfe,
	})


	local EndScript = Cmd:subclass{
		desc = 'endScript()',
		endTrace = true
	}
	game.Cmds.EndScript = EndScript

	EventCmds.EndScript = EventCmd:subclass(EndScript, {
		cmd = 0xff,
	})

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

	local function popObjectCmdSet(trace, addr, dontDoTheProbablyWrongEndAddrCheck)
--DEBUG:assert.gt(#trace.stateStack, 1, "tried to pop a cmdset when the stack would become empty")
		local prevState = trace.stateStack:remove()
--DEBUG:assert.eq(prevState.cmdset, 'ObjectCmds', "got ObjectCmds.EndScript when it wasn't in the object cmdset...")
		local objectScriptCmd = prevState.objectScriptCmd
--DEBUG:assert.ne(objectScriptCmd, nil, "got an object end-script when there was no objectScriptCmd set ...")
		if not dontDoTheProbablyWrongEndAddrCheck
		and addr ~= objectScriptCmd.addr + objectScriptCmd.length + 1
		then
			print("!!! DANGER !!! object-script length doesn't align with end-of-script cmd:", game.addrLabel(addr), 'vs', game.addrLabel(objectScriptCmd.addr + objectScriptCmd.length + 1))
		end

		trace.indent = trace.indent - 1
	end

	-- also in WorldCmds
	ObjectCmds.Action = ObjectCmd:subclass{
		desc = "objAction(objIndex, <?=bit.band(cmd, 0x3f)?>, <?=0 ~= bit.band(0x40, cmd)?>)",
	}
	for cmd=0,0x7f do
		ObjectCmds['Action '..cmd] = ObjectCmds.Action:subclass{
			cmd = cmd,
		}
	end

	ObjectCmds.Move = ObjectCmd:subclass{
		digest = function(self, read)
			self.dir = bit.band(self.cmd, 3)
			self.dist = bit.band(bit.rshift(self.cmd, 2), 7) + 1
			self.descstr = 'objMove(objIndex, '..self.dir..', '..self.dist..')'
		end,
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
		desc = 'objMoveDiag(objIndex, <?=cmd - 0xa0?>)',
	}
	for cmd=0xa0,0xab do
		ObjectCmds['MoveDiagonal '..cmd] = ObjectCmds.MoveDiagonal:subclass{cmd = cmd}
	end

	ObjectCmds.SetSpeed = ObjectCmd:subclass{
		desc = 'objSetSpeed(objIndex, <?=bit.band(cmd, 0xf)?>)',
	}
	for cmd=0xc0,0xc5 do
		ObjectCmds['SetSpeed '..cmd] = ObjectCmds.SetSpeed:subclass{cmd = cmd}
	end

	ObjectCmds.EnableAnimation = ObjectCmd:subclass{
		cmd = 0xc6,
		desc = 'objEnableAnimation(objIndex)',
	}
	ObjectCmds.DisableAnimation = ObjectCmd:subclass{
		cmd = 0xc7,
		desc = 'objDisableAnimation(objIndex)',
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
		desc = 'objSetVehicle(objIndex, <?=bit.band(0x7f, vehicleIndex)?>, <?=0 ~= bit.band(0x80, vehicleIndex)?>)',
	}

	ObjectCmds.ChangeDir = ObjectCmd:subclass{
		desc = 'objSetDir(objIndex, <?=bit.band(cmd, 3)?>)',
	}
	for cmd=0xcc,0xcf do
		ObjectCmds['ChangeDir '..cmd] = ObjectCmds.ChangeDir:subclass{cmd = cmd}
	end

	ObjectCmds.ShowObject = ObjectCmd:subclass{
		cmd = 0xd0,
		desc = 'objSetVisible(objIndex, true)',
	}

	ObjectCmds.HideObject = ObjectCmd:subclass{
		cmd = 0xd1,
		desc = 'objSetVisible(objIndex, false)',
	}

	ObjectCmds.SetPos = ObjectCmd:subclass{
		cmd = 0xd5,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = 'objSetPos(objIndex, <?=x?>, <?=y?>)',
	}

	ObjectCmds.ScrollToObject = ObjectCmd:subclass{
		cmd = 0xd7,
		desc = 'objScrollTo(objIndex)',
	}

	ObjectCmds.Jump = ObjectCmd:subclass{
		cmd = 0xdc,
		desc = 'objJump(objIndex)',
	}
	ObjectCmds.JumpHigh = ObjectCmds.Jump:subclass{
		cmd = 0xdd,
		desc = 'objJumpHigh(objIndex)',
	}

	ObjectCmds.Sleep = ObjectCmd:subclass{
		cmd = 0xe0,
		argtypes = {uint8_t},
		argnames = {'frames'},
		desc = 'sleep(<?=frames?>/15)',
	}

	for cmd=0xe1,0xe6 do
		ObjectCmds['ChangeFlag'..('0x%02x'):format(cmd)] = ObjectCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			argnames = {'flagIndex'},
			-- not making sense of the json. what's the cmd for?
			-- I think similar to EventCmds 0xd0-0xdd ?
			desc = 'gameState.eventFlag<?=flagIndex?> ~~= true',
		}
	end

	-- "goto" or "call" ?
	ObjectCmds.Call = ObjectCmd:subclass{
		cmd = 0xf9,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = "<?=getGotoOfsStr(destAddrOfs, ' call')?>",

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
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
		-- [=[
		digest = function(self, ...)
			ObjectCmds.Branch.digest(self, ...)

			-- this will end an object-script...
			-- or TODO should I just do proper tracing and just redirect the object-opcode trace to PC+-offset ?
			-- namely 0xa2dd4 -> a2dd6 ... does anyone jump to a2dd6, or is a2dd4's branch unconditional, such that no more decoding makes any sense?
			-- in fact, nope, adding this command makes more problems than solutions
			-- TODO TODO this whole block of script
			-- even when only applying to branch-back ...
			-- or maybe this should only happen if our object-script length has expired?
			-- only for select operations still? or for any operation i.e. move to loop below?
			local objectScriptCmd = self.trace.stateStack:last().objectScriptCmd
			if objectScriptCmd
			and self.addr == objectScriptCmd.addr + objectScriptCmd.length
			then
				self.indent = self.indent - 1
				popObjectCmdSet(self.trace, self.addr, true)
			end
		end,
		--]=]
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
			self.indent = self.indent - 1
			popObjectCmdSet(self.trace, self.addr)
		end,
		desc = 'end}',	-- and joinAll() if the objectScriptCmd had blocking ...
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
	game.WorldCmds = WorldCmds
	local WorldCmd = Cmd:subclass()
	game.WorldCmd = WorldCmd

	-- same as in ObjectCmds
	-- but what's the object in world commands?  the airship? the party?
	WorldCmds.Action = WorldCmd:subclass{
		desc = "objAction(objIndex, <?=bit.band(cmd, 0x3f)?>, <?=0 ~= bit.band(0x40, cmd)?>)",
	}
	for cmd=0,0x7f do
		WorldCmds['Action '..cmd] = WorldCmds.Action:subclass{
			cmd = cmd,
		}
	end

	-- also in ObjectCmds
	WorldCmds.Move = WorldCmd:subclass{
		desc = 'objMove(objIndex, <?=bit.band(cmd, 3)?>, <?=bit.band(bit.rshift(cmd, 2), 7)?>)',
	}
	for cmd=0x80,0x9f do
		WorldCmds['Move '..cmd] = WorldCmds.Move:subclass{cmd=cmd}
	end

	-- also in WorldCmds
	WorldCmds.MoveDiagonal = WorldCmd:subclass{
		desc = 'objMoveDiag(objIndex, <?=cmd - 0xa0?>)',
	}
	for cmd=0xa0,0xab do
		WorldCmds['MoveDiagonal '..cmd] = WorldCmds.MoveDiagonal:subclass{cmd = cmd}
	end


	-- in EventCmds as 0x6a, 0x6b
	WorldCmds.SetMap = WorldCmd:subclass(SetMap, {cmd = 0xd2})
	WorldCmds.SetMap2 = WorldCmd:subclass(SetMap, {cmd = 0xd3})

	-- in EventCmds for 0xc0-0xcf and in WorldCmds for 0xb0-0xbf
	for cmd=0xb0,0xbf do
		WorldCmds['Cond '..cmd] = WorldCmd:subclass(Cond, {cmd = cmd})
	end

	-- same as in ObjectCmds
	WorldCmds.SetSpeed = WorldCmd:subclass{
		desc = 'objSetSpeed(objIndex, <?=bit.band(cmd, 0xf)?>)',
	}
	for cmd=0xc0,0xc5 do
		WorldCmds['SetSpeed '..cmd] = WorldCmds.SetSpeed:subclass{cmd = cmd}
	end

	-- also VehicleCmds
	WorldCmds.SetAirshipPos = WorldCmd:subclass{
		cmd = 0xc7,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = "airship:setPos(<?=x?>, <?=y?>)",
	}

	-- like VehicleCmds SetFlag
	local WorldSetFlag = WorldCmd:subclass{
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		desc = 'gameState.mapFlag<?=flagIndex?> = <?=value?>',
	}
	WorldCmds.SetFlag = WorldSetFlag:subclass{
		cmd = 0xc8,
		value = true,
	}
	WorldCmds.ClearFlag = WorldSetFlag:subclass{
		cmd = 0xc9,
		value = false,
	}

	-- same as in ObjectCmds
	WorldCmds.ChangeDir = WorldCmd:subclass{
		desc = 'objSetDir(objIndex, <?=bit.band(cmd, 3)?>)',
	}
	for cmd=0xcc,0xcf do
		WorldCmds['ChangeDir '..cmd] = WorldCmds.ChangeDir:subclass{cmd = cmd}
	end

	-- same as ObjectCmds, but with the current-object being the party sprite ...
	WorldCmds.ShowObject = WorldCmd:subclass{
		cmd = 0xd0,
		desc = 'objSetVisible(objIndex, true)',
	}
	WorldCmds.HideObject = WorldCmd:subclass{
		cmd = 0xd1,
		desc = 'objSetVisible(objIndex, false)',
	}

	WorldCmds.IfKeyThenGoto = WorldCmd:subclass{
		cmd = 0xd4,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = 'if keypress() then <?=getGotoOfsStr(destAddrOfs)?>',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}

	WorldCmds.IfFacingThenGoto = WorldCmd:subclass{
		cmd = 0xd5,
		argtypes = {uint8_t, uint24_t},
		argnames = {'dir', 'destAddrOfs'},
		desc = 'if dir==<?=dir?> then <?=getGotoOfsStr(destAddrOfs)?>',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}

	-- also EventCmds 0x96, 0x97
	WorldCmds.FadeIn = WorldCmd:subclass{
		cmd = 0xd8,
		desc = 'fadeIn()',
	}
	WorldCmds.FadeOut = WorldCmd:subclass{
		cmd = 0xd9,
		desc = 'fadeOut()',
	}

	-- also VehicleCmds 0xdd 0xdf
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
		desc = 'sleep(<?=frames?>/15)',
	}

	WorldCmds.ChangeToShip = WorldCmd:subclass{
		cmd = 0xfc,
		desc = 'changeToShip()',
	}

	WorldCmds.FigaroSubmerge = WorldCmd:subclass{
		cmd = 0xfd,
		desc = 'figaroSubmerge()',
	}

	--[[ everything8215 has this listed in his WorldScript as an opcode,
	--  but in the functions themselves it is clearly a return
	WorldCmds.FigaroEmerge = WorldCmd:subclass{
		cmd = 0xfe,
		desc = 'figaroEmerge()',
	}
	--]]
	-- [[
	WorldCmds.Return = WorldCmd:subclass(Return, {
		cmd = 0xfe,
	})
	--]]

	WorldCmds.EndScript = WorldCmd:subclass(EndScript, {
		cmd = 0xff,
	})

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



	-- now for vehicle cmds
	-- I guess whether these vs EventCmds are used is game state based, so there's no static measure for when in the script engine to decode as one vs the other ...
	game.VehicleCmds = VehicleCmds
	local VehicleCmd = Cmd:subclass()
	game.VehicleCmd = VehicleCmd

	for cmd=0,0x7f do
		VehicleCmds['MoveVehicle '..cmd] = VehicleCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			argnames = {'dist'},
			desc = "moveVehicle{dir=<?=cmd?>, dist=<?=dist?>}",
		}
	end

	-- also in EventCmds for 0xc0-0xcf and in WorldCmds for 0xb0-0xbf
	for cmd=0xb0,0xbf do
		VehicleCmds['Cond '..cmd] = VehicleCmd:subclass(Cond, {cmd = cmd})
	end

	VehicleCmds.Unknown_C0 = VehicleCmd:subclass{
		cmd = 0xc0,
		argtypes = {uint8_t},
		desc = "unknown_c0(<?=args:mapi(tostring):concat', '?>)",
	}

	-- how does this work?
	VehicleCmds.ChangeCameraDir = VehicleCmd:subclass{
		cmd = 0xc1,
		argtypes = {uint16_t},
		desc = "changeCameraDir(<?=args[1]?>)",
	}
	VehicleCmds.ChangeMoveDir = VehicleCmd:subclass{
		cmd = 0xc2,
		argtypes = {uint16_t},
		desc = "changeMoveDir(<?=args[1]?>)",
	}
	VehicleCmds.ChangeDirC3 = VehicleCmd:subclass{
		cmd = 0xc3,
		argtypes = {uint16_t},
		desc = "changeDirC3(<?=args[1]?>)",
	}
	VehicleCmds.ChangeDirC4 = VehicleCmd:subclass{
		cmd = 0xc4,
		argtypes = {uint16_t},
		desc = "changeDirC4(<?=args[1]?>)",
	}

	VehicleCmds.SetAirshipAltitude = VehicleCmd:subclass{
		cmd = 0xc5,
		argtypes = {uint16_t},
		argnames = {'alt'},
		desc = "airship.alt = <?=alt?>",
	}

	VehicleCmds.MoveForward = VehicleCmd:subclass{
		cmd = 0xc6,
		argtypes = {uint16_t},
		argnames = {'speed'},
		desc = "airship:moveFwd{speed=<?=speed?>}",
	}

	VehicleCmds.SetAirshipPos = VehicleCmd:subclass{
		cmd = 0xc7,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = "airship:setPos(<?=x?>, <?=y?>)",
	}

	-- like WorldSetFlag
	local VehicleSetFlag = VehicleCmd:subclass{
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		desc = 'gameState.mapFlag<?=flagIndex?> = <?=value?>',
	}
	VehicleCmds.SetFlag = VehicleSetFlag:subclass{
		cmd = 0xc8,
		value = true,
	}
	VehicleCmds.ClearFlag = VehicleSetFlag:subclass{
		cmd = 0xc9,
		value = false,
	}

	for cmd=0xca,0xcf do
		VehicleCmds['VehicleBattle '..cmd] = VehicleCmd:subclass(Battle, {cmd=cmd})
	end

	-- also WorldCmds 0xd0, 0xd1
	VehicleCmds.ShowObject = VehicleCmd:subclass{
		cmd = 0xd0,
		desc = 'objSetVisible(objIndex, true)',
	}
	VehicleCmds.HideObject = VehicleCmd:subclass{
		cmd = 0xd1,
		desc = 'objSetVisible(objIndex, false)',
	}

	-- also EventCmds 0x6a, 0x6b and WorldCmds 0xd2, 0xd3
	VehicleCmds.SetMap = VehicleCmd:subclass(SetMap, {cmd = 0xd2})
	VehicleCmds.SetMap2 = VehicleCmd:subclass(SetMap, {cmd = 0xd3})

	-- also WorldCmds 0xd8, 0xd9
	VehicleCmds.FadeIn = VehicleCmd:subclass{
		cmd = 0xd8,
		desc = 'fadeIn()',
	}
	VehicleCmds.FadeOut = VehicleCmd:subclass{
		cmd = 0xd9,
		desc = 'fadeOut()',
	}

	VehicleCmds.ShowDirArrows = VehicleCmd:subclass{
		cmd = 0xda,
		desc = 'vehicleDirArrows = "show"',
	}
	VehicleCmds.LockDirArrows = VehicleCmd:subclass{
		cmd = 0xdb,
		desc = 'vehicleDirArrows = "lock"',
	}
	VehicleCmds.HideDirArrows = VehicleCmd:subclass{
		cmd = 0xdc,
		desc = 'vehicleDirArrows = "hide"',
	}

	-- also WorldCmds 0xdd 0xdf
	VehicleCmds.ShowMiniMap = VehicleCmd:subclass{
		cmd = 0xdd,
		desc = 'miniMapVisible = true',
	}

	VehicleCmds.Unknown_DE = VehicleCmd:subclass{
		cmd = 0xde,
		argtypes = {uint8_t, uint8_t},
		desc = "unknown_de(<?=args:mapi(tostring):concat', '?>)",
	}

	VehicleCmds.HideMiniMap = VehicleCmd:subclass{
		cmd = 0xdf,
		desc = 'miniMapVisible = false',
	}

	-- also in ObjectCmds and WorldCmds
	VehicleCmds.Sleep = VehicleCmd:subclass{
		cmd = 0xe0,
		argtypes = {uint8_t},
		argnames = {'frames'},
		desc = 'sleep(<?=frames?>/15)',
	}

	VehicleCmds.Cinematic_EndingAirship = VehicleCmd:subclass{
		cmd = 0xf2,
		desc = "showEndingAirship()",
	}
	VehicleCmds.Cinematic_LightOfJudgment1 = VehicleCmd:subclass{
		cmd = 0xf3,
		desc = "showLightOfJudgment1()",
	}

	VehicleCmds.ChangeVehicleGraphicToFalcon = VehicleCmd:subclass{
		cmd = 0xf4,
		desc = 'vehicle.graphics = "falcon"',
	}

	VehicleCmds.Cinematic_LightOfJudgment2 = VehicleCmd:subclass{
		cmd = 0xf5,
		desc = "showLightOfJudgment2()",
	}
	VehicleCmds.Cinematic_F6 = VehicleCmd:subclass{
		cmd = 0xf6,
		desc = "showCinematicF6()",
	}
	VehicleCmds.ChangeVehicleGraphicToBird = VehicleCmd:subclass{
		cmd = 0xf7,
		desc = 'vehicle.graphics = "bird"',
	}
	VehicleCmds.Cinematic_LightOfJudgment3 = VehicleCmd:subclass{
		cmd = 0xf8,
		desc = "showLightOfJudgment3()",
	}
	VehicleCmds.Cinematic_LightOfJudgment4 = VehicleCmd:subclass{
		cmd = 0xf9,
		desc = "showLightOfJudgment4()",
	}
	VehicleCmds.Cinematic_FalconRiseFromWater = VehicleCmd:subclass{
		cmd = 0xfa,
		desc = "showFalconRiseFromWater()",
	}
	VehicleCmds.Cinematic_AirshipSmoking = VehicleCmd:subclass{
		cmd = 0xfb,
		desc = "showAirshipSmoking()",
	}
	VehicleCmds.Cinematic_AirshipCrashing = VehicleCmd:subclass{
		cmd = 0xfc,
		desc = "showAirshipCrashing()",
	}
	VehicleCmds.ChangeVehicleGraphicToMorphedTerra= VehicleCmd:subclass{
		cmd = 0xfd,
		desc = 'vehicle.graphics = "morphed_terra"',
	}
	-- another 0xFE opcode, like FigaroEmerge, I am suspicous this will not always be used ...
	VehicleCmds.Cinematic_VectorApproach = VehicleCmd:subclass{
		cmd = 0xfe,
		desc = "showVectorApproach()",
	}

	-- one common EndScript class?
	VehicleCmds.EndScript = VehicleCmd:subclass(EndScript, {
		cmd = 0xff,
		digest = function(self, ...)
-- assert we're in a vehicle state and pop state
--DEBUG:assert.gt(#self.trace.stateStack, 1, "how did we get here?")
--DEBUG:assert.eq(self.trace.stateStack:last().cmdset, 'VehicleCmds', "how did we get here?")
			self.trace.stateStack:remove()
		end,
		-- is a vehicle end-script on par with a world/event end-script, or is it more like object end-script that just ends the object-section ?
	})

	-- map the by-number for by-name
	for _,k in ipairs(table.keys(VehicleCmds)) do
		local cl = VehicleCmds[k]
		if cl.cmd then
			assert.type(cl.cmd, 'number')
			VehicleCmds[cl.cmd] = cl
		end
	end
	-- fill in empty numbers with unknowns
	for i=0,255 do
		if not VehicleCmds[i] then
			VehicleCmds[i] = VehicleCmd:subclass{
				cmd = i,
				desc = '??? '..('0x%02x'):format(i),
			}
		end
	end


	-- still to do, monster-script maybe?


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


	--[[
	TODO here instead of assuming the next-in-order address marks the end ...
	- we should be tracing until 'return'
	- and upon branch, recursive call the trace ...
	- and then warn if we ever trace over the same code block but offset or with a different opcode set...
	- and then ... i should be properly decoding world-vs-events, and also not decoding extra things past 'return'
	--]]

	game.eventScriptCmds = table()

	-- event code, pointed into by NPCs and maybe other things
	game.eventScriptAddrs = {}

	-- remember where we've started from so we don't go over the same area twice
	-- TODO this but for the state of decoding as well
	local decompileTraces = {}

	local Trace = class()
	Trace.indent = 0
	function Trace:printInterval()
		return game.addrLabel(self.addr)..' - '..game.addrLabel(self.endAddr)
	end

	local function decompileFrom(args)
		local startAddr = args.addr
		local startCmdSet = args.cmdset
--DEBUG:assert.type(startCmdSet, 'string')
		local reverseRefInfo = args.reverseRefInfo

--[==[ debugging
print('decompiling from '..require'ext.tolua'(reverseRefInfo, {
	indent = false,
	serializeForType = {
		number = function(state, x, tab, luapath, keyRef)
			return game.addrLabel(x)
		end,
	},
}))
--]==]
		game.eventScriptAddrs[startAddr] = game.eventScriptAddrs[startAddr] or table()
		game.eventScriptAddrs[startAddr]:insert(reverseRefInfo)

		-- force override addrs here
		do
			local newCmdSet = ({
				[0x0a5eb4] = 'EventCmds',	-- end-script used by event and world cmds
			})[startAddr]
			if newCmdSet then
				startCmdSet = newCmdSet
			end
		end

		local trace = Trace()
		trace.addr = startAddr
		trace.cmds = table()
		trace.cmdObjForAddr = {}	-- keys are cmds[i].addr
		trace.lastDialogPromptCount = args.lastDialogPromptCount

		-- not sure about this
		trace.stateStack = table()
		trace.stateStack:insert{
			cmdset = startCmdSet,
		}

		if args.inVehicle then
			trace.stateStack:insert{
				cmdset = 'VehicleCmds',
			}
		end

		-- make sure we haven't decoded this already
		for _,otherTrace in pairs(decompileTraces) do
			local cmdobj = otherTrace.cmdObjForAddr[startAddr]
			if cmdobj then
				if cmdobj.cmdset ~= trace.stateStack:last().cmdset then
					print('!!! DANGER !!! '
						..game.addrLabel(startAddr)
						..' decoding address from differing cmdset!'
						..' previous cmdset '..cmdobj.cmdset
						..' from trace at '..game.addrLabel(otherTrace.cmds[1].addr)
						..' vs new cmdset '..trace.stateStack:last().cmdset
					)
				else
					return
				end
			end
		end

		decompileTraces[startAddr] = trace

		local newBranches = table()

		local addr = startAddr
		local function read(ctype)
			local o
			o, addr = readAndInc(ctype, addr)
			return o
		end

--[==[ debugging:
print(('BEGIN '..game.addrLabel(startAddr))
--]==]

		while true do
			if not (
				(scriptBaseAddr <= addr and addr < scriptBaseAddrEnd)
				or (scriptBaseAddr2 <= addr and addr < scriptBaseAddrEnd2)
			) then
print('!!! script oob !!! '..game.addrLabel(addr))
				break
			end

			local cmdaddr = addr
			local cmd = read(uint8_t)

--DEBUG:assert.gt(#trace.stateStack, 0, "someone popped the last cmdset...")
			local cmdset = trace.stateStack:last().cmdset
--DEBUG:assert.type(cmdset, 'string')
			local cl = game[cmdset][cmd]
			local cmdobj = cl()
			cmdobj.trace = trace
			cmdobj.addr = cmdaddr
			cmdobj.cmdset = cmdset
			cmdobj.indent = trace.indent

			-- hmm instead of just 'read' with 'addr', how about a whole interpretation-state, with 'cmdset' too?
			cmdobj:digest(read)

			-- track all bytes used here, in case someone cares?
			cmdobj.sizeInBytes = addr - cmdaddr

			game.eventScriptCmds:insert(cmdobj)
			trace.cmds:insert(cmdobj)
			trace.cmdObjForAddr[cmdaddr] = cmdobj

--[==[ debugging print as we go
	io.write(game.addrLabel(cmdobj.addr), '\t')
	io.write(({
		EventCmds = 'EV ',
		WorldCmds = 'WO ',
		ObjectCmds = 'OB ',
		VehicleCmds = 'VE ',
	})[cmdobj.cmdset])
	local function align(n, s)
		s = tostring(s)
		return s..(' '):rep(n - #s)
	end
	io.write(align(
		24,
		ffi.string(rom + cmdobj.addr, cmdobj.sizeInBytes)
			:gsub('.', function(b)
				return ('%02x '):format(b:byte())
			end)
	))
	if game.ObjectCmd:isa(cmdobj)
	and not game.ObjectCmds.EndScript:isa(cmdobj)
	then
		io.write'    '
	end
	print((tostring(cmdobj)
		:gsub('\t', '\\t')
		:gsub('\r', '\\r')
		:gsub('\n', '\\n')
	))
--]==]

			if cmdobj.getBranchAddrs then
				-- decode branches ... now or later?
				for _,newBranch in ipairs(cmdobj:getBranchAddrs()) do
					newBranch.cmdset = newBranch.cmdset or trace.stateStack[1].cmdset		-- make sure we record the current cmdset
					newBranch.inVehicle = trace.stateStack:last().cmdset == 'VehicleCmds'	-- right now trace.stateStack is just 1 or 2 in size, and 2 is always VehicleCmds, and 1 is always not...
					newBranch.lastDialogPromptCount = trace.lastDialogPromptCount
					newBranch.reverseRefInfo = {
						branchFromAddr = cmdaddr,
						cmdset = newBranch.cmdset,
					}
					newBranches:insert(newBranch)
				end
			end

			-- this is gonna go before cmds very soon
			local newCmdSet = ({
				-- these are all SetMap's:
				[0x0a0096] = 'WorldCmds',	-- doomgaze defeated - the setmap sets vehicle but I guess don't?
				[0x0a00e3] = 'WorldCmds',	-- from map touch, specifically doomgaze
				[0x0a7a86] = 'EventCmds',	-- branch
				[0x0a8ff0] = 'EventCmds',	-- branch
				[0x0af4c1] = 'WorldCmds',	-- branch
				[0x0af4d7] = 'WorldCmds',	-- branch
				[0x0af4f3] = 'WorldCmds',	-- branch, so it matters
				[0x0af567] = 'EventCmds',	-- branch, so it matters
				[0x0af596] = 'EventCmds',	-- branch
				[0x0b4505] = 'EventCmds',	-- branch
				[0x0b67f7] = 'WorldCmds',	-- branch ... everything8215's event_main.asm says first set to World ... then after endScript set to Events ... does endScript always set cmdset to Events?
				[0x0c3383] = 'EventCmds',	-- branch
				-- sometimes just a goto, or a conditional-goto will change things?
				[0x0af4c7] = 'EventCmds',	-- $0af4c7	WO b0 76 01 b4 5e 00       if not gameState.mapFlag374 then goto $0a5eb4 end
				[0x0af4dd] = 'EventCmds',	-- $0af4dd	WO b0 76 01 b4 5e 00       if not gameState.mapFlag374 then goto $0a5eb4 end
				[0x0a8ca8] = 'EventCmds',	-- $0a8ca8	VE b0 27 01 e3 8b 00       if not gameState.mapFlag295 then goto $0a8be3 end
			})[cmdaddr]
			if newCmdSet then
				for i=2,#trace.stateStack do trace.stateStack[i] = nil end	-- clear VehicleCmds too
				trace.stateStack[1].cmdset = newCmdSet
			end


			if cmdobj.endTrace then break end
		end

		-- set this before trying recursive, so they know this trace's address interval
		trace.endAddr = addr

--[==[ debugging:
print('END '..game.addrLabel(startAddr))
print()
--]==]

		assert.ge(#trace.stateStack, 1, "somehow we popped all our states...")

		if trace.stateStack:last().objectScriptCmd then
			print('!!! DANGER !!! event-script ended still inside an object-script:',
				'script start = '..game.addrLabel(startAddr),
				'object-script start = '..game.addrLabel(trace.stateStack:last().objectScriptCmd.addr)
			)
		end

		for _,newBranch in ipairs(newBranches) do
			decompileFrom(newBranch)
		end
	end


	-- collect script addrs from npcs
	-- hmm should I use the master npc list or just which ones are in maps?
	-- should I cache disassembled chunks and only traverse instructions that are actually used? and track memory regions used?
	for mapIndex=0,countof(game.maps)-1 do
		local mapInfo = game.getMap(mapIndex)
		if mapInfo then

			local function decodeForMap(startAddr, reverseRefInfo)
				local startCmdSet = mapIndex < 3 and 'WorldCmds' or 'EventCmds'

				-- even if world-map is using commonReturnAddr, stillu se EventCmds, cuz I think this is the only address that could either be EventCmds or WorldCmds
				if startAddr == commonReturnAddr then
					startCmdSet = 'EventCmds'
				end

				-- blackjack book starts as 'inVehicle':
				-- needed especially for scripts that branch into this address
				local inVehicle
				if startAddr == 0x0aa6c0 then
					inVehicle = true
				end

				return decompileFrom{
					addr = startAddr,
					cmdset = startCmdSet,
					inVehicle = inVehicle,
					reverseRefInfo = reverseRefInfo,
				}
			end

			-- if the mapIndex < 3 then it's a world-script, otherwise it's an event-script
			-- ... right?
			decodeForMap(mapInfo.startEventScriptAddr, {
				type = 'startEventScriptAddr',
				mapIndex = mapIndex,
			})

			for npcIndex,n in ipairs(mapInfo.npcs) do
				local scriptAddr = n:getScriptAddr()
				if scriptAddr then
					decodeForMap(scriptAddr, {
						npcIndex = npcIndex-1,
						mapIndex = mapIndex,
					})
				end
			end

			for touchTriggerIndex,t in ipairs(mapInfo.touchTriggers) do
				local scriptAddr = t:getScriptAddr()
				if scriptAddr then
					-- if the mapIndex < 3 then it's a world-script, otherwise it's an event-script
					decodeForMap(scriptAddr, {
						touchTriggerIndex = touchTriggerIndex-1,
						mapIndex = mapIndex,
					})
				end
			end
		end
	end

	-- [[ add builtins?
	for addr,name in pairs{
		[0x0a0000] = "no event",
		[0x0a0001] = "wait for dialogue window",
		[0x0a0003] = "game start",
		[0x0a0008] = "chest: item",
		[0x0a000c] = "chest: spell",
		[0x0a0010] = "chest: gp",
		[0x0a0014] = "chest: empty",
		[0x0a0018] = "random battle",
		[0x0a0034] = "tent",
		[0x0a0039] = "warp/warp stone",
		[0x0a0040] = "chest: monster-in-a-box",
		[0x0a009d] = "doom gaze",
		[0x0a00ea] = "tent: animation",
		[0x0a0108] = "warp: animation",
		[0x0a015e] = "tent: animation (world)",
		[0x0a01a2] = "kefka's tower",
		[0x0a0405] = "phoenix cave",
		[0x0a057d] = "final battle/ending",
		[0x0a5ade] = "floating island: cinematic",
		[0x0a5e33] = "new game",
		[0x0a5ea9] = "post-battle",
		[0x0a5eb3] = "return",
		[0x0a5eb4] = "return (world)",
		[0x0aca64] = "check facing direction",
		[0x0acd31] = "inn: no dream",
		[0x0acd3c] = "inn: normal",
		[0x0acd5b] = "inn: dream 1",
		[0x0acdd9] = "inn: dream 2",
		[0x0ace51] = "inn: dream 3",
		[0x0acefe] = "inn: dream 4",
		[0x0acf67] = "inn: fade out",
		[0x0acf96] = "inn: fade in",
		[0x0b69ff] = "not enough gp",
		[0x0c985b] = "prologue",
		[0x0c9aeb] = "save point",
		[0x0ce566] = "game over",
	} do
		decompileFrom{
			addr = addr,
			cmdset = 'EventCmds',
			reverseRefInfo = {builtin = name},
		}
	end
	for addr,name in pairs{
		[0x0a004f] = 'world tent',
		[0x0a0059] = 'airship ground',
		[0x0a0068] = 'airship deck',
		[0x0a0078] = "falcon: deck",
		[0x0a0088] = "enter phoenix cave",
		[0x0a008f] = "enter gogo's lair",
		[0x0a0096] = "doom gaze defeated",
	} do
		decompileFrom{
			addr = addr,
			cmdset = 'WorldCmds',
			reverseRefInfo = {builtin = name},
		}
	end
	for addr,name in pairs{
		[0x0a007f] = "enter kefka's tower",
		[0x0aa6c0] = "blackjack book",
	} do
		decompileFrom{
			addr = addr,
			cmdset = 'EventCmds',
			inVehicle = true,
			reverseRefInfo = {builtin = name},
		}
	end
	--]]



	-- is this a good idea, or should I double-check by storing trace address ranges and make sure none overlap so that i'm not inserting cmds between/ontop one another?
	game.eventScriptCmds:sort(function(a,b) return a.addr < b.addr end)

	-- and now I have to recalculate eventScriptCmdIndexForAddr
	game.eventScriptCmdIndexForAddr = {}
	for i,cmd in ipairs(game.eventScriptCmds) do
		if cmd.addr then
			game.eventScriptCmdIndexForAddr[cmd.addr] = i
		end
	end

-- [=[ remove subsets
	local sortedTraces = table.values(decompileTraces)
	sortedTraces:sort(function(a,b) return a.addr < b.addr end)
	for i=#sortedTraces-1,1,-1 do
		local a = sortedTraces[i]
		local b = sortedTraces[i+1]
		-- if they have the same ending
		-- then chances are the smaller half decoded first then the larger earlier half
		if a.addr < b.addr
		and a.endAddr == b.endAddr
		then
--DEBUG:print('found possible subset trace '..game.addrLabel(a.addr)..' - '..game.addrLabel(a.endAddr)..' vs range '..game.addrLabel(b.addr)..' - '..game.addrLabel(b.endAddr))
			-- but first verify
			-- make sure all the cmds match
			local allmatch = true
			for j,bcmd in ipairs(b.cmds) do
				local acmd = a.cmds[#a.cmds-#b.cmds+j]
				if acmd.addr ~= bcmd.addr then
--DEBUG:print('...but cmd addr at '..game.addrLabel(acmd.addr)..' vs '..game.addrLabel(bcmd.addr).." didn't match")
					allmatch = false
					break
				end
				if acmd.cmdset ~= bcmd.cmdset then
					allmatch = false
					break
				end
				if getmetatable(acmd) ~= getmetatable(bcmd) then
--DEBUG:print('...but cmd metatable at '..game.addrLabel(acmd.addr).." didn't match")
					allmatch = false
					break
				end
			end
			if allmatch then
--DEBUG:print('removing subset trace '..game.addrLabel(b.addr)..' - '..game.addrLabel(b.endAddr))
				decompileTraces[b.addr] = nil
				sortedTraces:remove(i+1)
			end
		end
	end
--]=]

	-- double-check collisions
	local sortedTraces = table.values(decompileTraces)
	sortedTraces:sort(function(a,b) return a.addr < b.addr end)
	for i=2,#sortedTraces do
		local a = sortedTraces[i-1]
		local b = sortedTraces[i]
		if b.addr < a.endAddr then
			print('!!! collision detected between '..a:printInterval()..' and '..b:printInterval())
		end
	end

	game.decompileTraces = decompileTraces
end
