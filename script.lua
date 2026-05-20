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
	local scriptBaseAddr = ffi.offsetof(Game, 'eventScript')	-- 0xa0000
	local scriptBaseAddrEnd = ffi.offsetof(Game, 'eventScript') + ffi.sizeof(game.eventScript)

	local scriptBaseAddr2 = ffi.offsetof(Game, 'dialogBase')
	local scriptBaseAddrEnd2 = ffi.offsetof(Game, 'dialogBase') + ffi.sizeof(game.dialogBase)

--DEBUG:print('event script ranges:')
--DEBUG:print(('$%06x-$%06x'):format(scriptBaseAddr, scriptBaseAddrEnd))
--DEBUG:print(('$%06x-$%06x'):format(scriptBaseAddr2, scriptBaseAddrEnd2))

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
			str = ('$%06x'):format(addr)
		end
		-- is it 'goto return' as in we jmp to the instruction and its a rts and so we return?
		--  and therefore a 'goto return' is the same as just 'return'
		-- but then how about the 'call' ... does that push to the same stack as whatever invoked the script in the first place,
		--  such that 'call return' is a 'nop'?
		--  or are all 'call return's erroneous, in that if the pc stack is different between invoking touch/event s and 'call' within the script, then a 'call return' would produce some kind of error?
		return (op or 'goto ')..str
	end


	-- generic cmds when you need a superclass transcending any specific cmdset:
	local Cmds = {}
	game.Cmds = Cmds
	Cmds.Cmd = Cmd	-- parent-class of all script cmds


	-- here's some interpretation state-variables
	-- that I should probably move into an interpretation-context-object
	local stateStack
	local lastDialogPromptCount

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
			--self.objectIndex = self.cmd
			self.length = bit.band(arg, 0x7f)	-- in bytes, and the last byte is usually always 0xff <->> end-object-script
			self.blocking = 0 ~= bit.band(arg, 0x80)
		end,
		digest = function(self, ...)
			-- if it's an event-script command to start an object-script, then switch our opcodes...
			-- maybe todo push?  or will exiting object-scripts always revert to event-scripts?
assert.len(stateStack, 1, "can we go from something to event-cmds to object-cmds?")

			assert.eq(stateStack:last().objectScriptCmd, nil, "got two object-scripts before the first one ended...")

			stateStack:insert{
				cmdset = assert(ObjectCmds),
				objectScriptCmd = self,
			}

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
			self.dialogIndex = bit.band(0x3fff, arg)
			self.showTextOnly = 0 ~= bit.band(0x4000, arg)
			self.bottomOfScreen = 0 ~= bit.band(0x8000, arg)

			local dlg = game.dialog[self.dialogIndex]
			if dlg then
				lastDialogPromptCount = select(2, dlg:gsub('%[PROMPT%]', '%0'))
			end
		end,
		__tostring = function(self)
			local dlg = game.dialog[self.dialogIndex]
			local str = dlg and ('%q'):format(dlg) or 'nil'
			if self.dontWait
			or self.showTextOnly
			or self.bottomOfScreen
			then
				return --'show dialog[0x'..number.hex(self.dialogIndex)..']:'
					'dialog('..('%q'):format(str)
					..', {'
					..table()
					:append{self.dontWait and 'dontWait=true' or nil}
					:append{self.showTextOnly and 'showTextOnly=true' or nil}
					:append{self.bottomOfScreen and 'bottomOfScreen=true' or nil}
					:concat','
				..'})'
			else
				return 'dialog'..('%q'):format(dlg)
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
-- TODO doing this isn't good .. we gotta trace through each branch to properly find out how to interpet things.
-- need more than a state-stack
			if self.mapIndex < 3 then
				stateStack[1].cmdset = WorldCmds
			else
				stateStack[1].cmdset = EventCmds
			end


			if self.vehicle == 0 then
				-- now if we are clearing vehicle...
				-- if we were not in vehicle then nothing
				-- if we were in vehicle ... then pop state
				if stateStack:last().cmdset == VehicleCmds then
					stateStack:remove()
				end
				assert.ge(#stateStack, 1, "popped our last state stack when leaving vehicle state...")
				assert.ne(stateStack:last().cmdset, VehicleCmds, "popped vehicle state and still ended up in vehicle state...")
			else
				-- you can set from the base event/world cmdset
				-- or you can also set while in the vehicle cmdset (in which case, don't push anything on the stack...)
				--assert.len(stateStack, 1, "tried to set-map set-vehicle within a sub-state!")

				-- switching to vehicle state doesn't track length like switching to object state does
				-- hmm are the two orthogonal or not?
				-- i.e. can we switch-to-object, switch-to-vehicle, then end object script (while still processing vehicle cmds)
				-- I guess never since there is no Object SetMap
				-- so we're safe there.
				if stateStack:last().cmdset ~= VehicleCmds then
					stateStack:insert{
						cmdset = VehicleCmds,
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
		desc = "objs[<?=objectIndex?>].script = <?=getGotoOfsStr(newScriptAddrOfs, '')?>",

		getBranchAddrs = function(self)
			return {
				-- this will always use event-cmds, whereas typical getBranchAddrs for branch/goto/call will preserve cmdset
				{addr=scriptBaseAddr + self.newScriptAddrOfs, cmdset=EventCmds},
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
			..', addr=<?=scriptBaseAddr + bit.band(0x3ffff, arg)?>'
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
		argnames = {'count'},
		desc = 'for i=1,<?=count+1?>',
	}

	EventCmds.EndRepeat = EventCmd:subclass{
		cmd = 0xb1,
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

	EventCmds.SleepSeconds = EventCmd:subclass{
		cmd = 0xb5,
		argtypes = {uint8_t},
		argnames = {'seconds'},
		desc = 'sleep(<?=seconds?>)'
	}

	EventCmds.CallForDialogResult = EventCmd:subclass{
		cmd = 0xb6,
		digest = function(self, read)
			self.addrs = table()
if not lastDialogPromptCount then error("required choices but no dialog at "..('$%06x'):format(self.addr)) end
			for i=1,lastDialogPromptCount do
				self.addrs:insert(scriptBaseAddr + read(uint24_t))
			end
		end,
		__tostring = function(self)
			-- TODO is it 'goto' or is it 'call'?
			-- cuz if it's goto then the the next instruction after "want to learn about espers?" shouldn't be a 'return', because each jump option there has its own return ...
			return 'callForDialogChoice('
				..self.addrs:mapi(function(addr)
					return (' $%06x'):format(addr)
				end):concat' '
				..')'
		end,
	}

	EventCmds.JumpBasedOnBattleFlag = EventCmd:subclass{
		cmd = 0xb7,
		argtypes = {uint8_t, uint24_t},
		argnames = {'flagIndex', 'destAddrOfs'},
		desc = 'if gameState.battleFlag<?=flagIndex?> then <?=getGotoOfsStr(destAddrOfs)?>',

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
		desc = 'if math.random() < .5 then <?=getGotoOfsStr(destAddrOfs)?>',

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
			return 'gotoForCharacter({'
			..self.options:mapi(function(option)
				return '['..option.characterIndex..'] = '..Cmd.getGotoOfsStr(option.addrOfs)
			end):concat', '..'})'
		end,

		getBranchAddrs = function(self)
			return self.options:mapi(function(option)
				return {addr=scriptBaseAddr + option.addrOfs}
			end)
		end,
	}

	EventCmds.ShowAirshipEndingCutscene = EventCmd:subclass{
		cmd = 0xbf,
		desc = 'cutscene"airship ending"',
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
		endTrace = true,
	}
	EventCmds.EndScript = EventCmd:subclass{
		cmd = 0xff,
		desc = 'endScript()',
		endTrace = true,
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

	local function popObjectCmdSet(addr, dontDoTheProbablyWrongEndAddrCheck)
		assert.gt(#stateStack, 1, "tried to pop a cmdset when the stack would become empty")
		local prevState = stateStack:remove()
assert.eq(prevState.cmdset, ObjectCmds, "got ObjectCmds.EndScript when it wasn't in the object cmdset...")
		local objectScriptCmd = prevState.objectScriptCmd
assert.ne(objectScriptCmd, nil, "got an object end-script when there was no objectScriptCmd set ...")
		if not dontDoTheProbablyWrongEndAddrCheck
		and addr ~= objectScriptCmd.addr + objectScriptCmd.length + 1
		then
			print("!!! DANGER !!! object-script length doesn't align with end-of-script cmd:", ('$%06x'):format(addr), 'vs', ('$%06x'):format(objectScriptCmd.addr + objectScriptCmd.length + 1))
		end
	end

	-- also in WorldCmds
	ObjectCmds.Action = ObjectCmd:subclass{
		desc = "<?=0 ~= bit.band(0x40, cmd) and 'obj.hflip=true ' or ''?>"
			.."obj:doAction(<?=bit.band(cmd, 0x3f)?>)",
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
			self.descstr = 'obj.dir = '..self.dir
				..' obj:walkForward('..self.dist..')'
		end,
		-- 'desc' is fed thru 'template' so...
		__tostring = function(self) return self.descstr end,
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
			argnames = {'flagIndex'},
			-- not making sense of the json. what's the cmd for?
			-- I think similar to EventCmds 0xd0-0xdd ?
			desc = 'gameState.eventFlag<?=flagIndex?> ~~= true',
		}
	end

	ObjectCmds.Goto = ObjectCmd:subclass{
		cmd = 0xf9,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = '<?=getGotoOfsStr(destAddrOfs)?>',

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
		endTrace = true,
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
			local objectScriptCmd = stateStack:last().objectScriptCmd
			if objectScriptCmd
			and self.addr
				== objectScriptCmd.addr + objectScriptCmd.length
			then
				popObjectCmdSet(self.addr, true)
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
			popObjectCmdSet(self.addr)
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
	game.WorldCmds = WorldCmds
	local WorldCmd = Cmd:subclass()
	game.WorldCmd = WorldCmd

	-- same as in ObjectCmds
	WorldCmds.Action = WorldCmd:subclass{
		desc = "<?=0 ~= bit.band(0x40, cmd) and 'obj.hflip=true ' or ''?>"
			.."obj:doAction(<?=bit.band(cmd, 0x3f)?>)",
	}
	for cmd=0,0x7f do
		WorldCmds['Action '..cmd] = WorldCmds.Action:subclass{
			cmd = cmd,
		}
	end

	-- also in ObjectCmds
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
		WorldCmds['Cond '..cmd] = WorldCmd:subclass(Cond, {cmd = cmd})
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
		desc = 'if keypress then <?=getGotoOfsStr(destAddrOfs)?>',

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

	--[[ everything8215 has this listed in his WorldScript as an opcode,
	--  but in the functions themselves it is clearly a return
	WorldCmds.FigaroEmerge = WorldCmd:subclass{
		cmd = 0xfe,
		desc = 'figaroEmerge()',
	}
	--]]
	-- [[
	WorldCmds.Return = WorldCmd:subclass{
		cmd = 0xfe,
		desc = 'return',
		endTrace = true,
	}
	--]]

	WorldCmds.EndScript = WorldCmd:subclass{
		cmd = 0xff,
		desc = 'endScript()',
		endTrace = true,
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

	VehicleCmds.SetFlag = VehicleCmd:subclass{
		cmd = 0xc8,
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		desc = 'gameState.mapFlags<?=flagIndex?> = true',
	}
	VehicleCmds.ClearFlag = VehicleCmd:subclass{
		cmd = 0xc9,
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		desc = 'gameState.mapFlags<?=flagIndex?> = false',
	}

	for cmd=0xca,0xcf do
		VehicleCmds['VehicleBattle '..cmd] = VehicleCmd:subclass(Battle, {cmd=cmd})
	end

	-- also WorldCmds 0xd0, 0xd1
	VehicleCmds.ShowObject = VehicleCmd:subclass{
		cmd = 0xd0,
		desc = 'obj.visible = true',
	}
	VehicleCmds.HideObject = VehicleCmd:subclass{
		cmd = 0xd1,
		desc = 'obj.visible = false',
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
		desc = 'sleep(<?=frames?> * 4 / 60)',
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
	VehicleCmds.EndScript = VehicleCmd:subclass{
		cmd = 0xff,
		digest = function(self, ...)
			-- assert we're in a vehicle state and pop state
			assert.gt(#stateStack, 1, "how did we get here?")
			assert.eq(stateStack:last().cmdset, VehicleCmds, "how did we get here?")
			stateStack:remove()
		end,
		-- is a vehicle end-script on par with a world/event end-script, or is it more like object end-script that just ends the object-section ?
		desc = 'endScript()',
		endTrace = true,
	}

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
	local haveDecodedFrom = {}

	local function decompileFrom(args)
		local startAddr = assert.index(args, 'addr')
		local startCmdSet = assert.index(args, 'cmdset')
		local reverseRefInfo = args.reverseRefInfo

print('decompiling from '..require'ext.tolua'(reverseRefInfo, {indent=false}))
		game.eventScriptAddrs[startAddr] = game.eventScriptAddrs[startAddr] or table()
		game.eventScriptAddrs[startAddr]:insert(reverseRefInfo)


		-- make sure we haven't decoded this already
		-- hmm TODO maybe someday I'll return the decoded block and store that by the caller instead of storing per-cmd per-addr....
		if haveDecodedFrom[startAddr] then return end
		haveDecodedFrom[startAddr] = true

		-- reset script decode state
		lastDialogPromptCount = nil

		-- not sure about this
		stateStack = table()
		stateStack:insert{
			cmdset = startCmdSet,
		}

		local branchInfos = table()

		local addr = startAddr
-- [==[ debugging:
print(('BEGIN $%06x'):format(startAddr))
--]==]

		while true do
			local function read(ctype)
				local o
				o, addr = readAndInc(ctype, addr)
				return o
			end

			local cmdaddr = addr
			local cmd = read(uint8_t)

assert.gt(#stateStack, 0, "someone popped the last cmdset...")
			local cmdset = assert.index(stateStack:last(), 'cmdset')
--DEBUG:assert.index(cmdset, cmd, "failed to find class for script command")
			local cl = cmdset[cmd]
--DEBUG:assert.eq(cl.class, cl, "class is not a class for command 0x"..number.hex(cmd))
--DEBUG:assert.is(cl, Cmd, "somehow class of command 0x"..number.hex(cmd).." is not of Cmd")
			local cmdobj = cl()
			cmdobj.addr = cmdaddr

			-- hmm instead of just 'read' with 'addr', how about a whole interpretation-state, with 'cmdset' too?
			cmdobj:digest(read)

			-- track all bytes used here, in case someone cares?
			cmdobj.sizeInBytes = addr - cmdaddr

			game.eventScriptCmds:insert(cmdobj)

-- [==[ debug print as we go
	io.write(('$%06x'):format(cmdobj.addr), '\t')
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
		io.write'\t'
	end
	print((tostring(cmdobj)
		:gsub('\t', '\\t')
		:gsub('\r', '\\r')
		:gsub('\n', '\\n')
	))
--]==]

			if cmdobj.getBranchAddrs then
				-- decode branches ... now or later?
				for _,info in ipairs(cmdobj:getBranchAddrs()) do
					branchInfos:insert{
						addr = assert.index(args, 'addr'),
						cmdset = info.cmdset or stateStack[1].cmdset,	-- make sure we record the current cmdset
						reverseRefInfo = {branchAddr = cmdaddr},
					}
				end
			end

			if cmdobj.endTrace then break end
		end

-- [==[ debugging:
print(('END $%06x'):format(startAddr))
print()
--]==]

		assert.ge(#stateStack, 1, "somehow we popped all our states...")

		if stateStack:last().objectScriptCmd then
			print('!!! DANGER !!! event-script ended still inside an object-script:',
				('script start = $%06x'):format(startAddr),
				('object-script start = $%06x'):format(stateStack:last().objectScriptCmd.addr)
			)
		end

		for _,info in ipairs(branchInfos) do
			decompileFrom(info)
		end
	end


	-- collect script addrs from npcs
	-- hmm should I use the master npc list or just which ones are in maps?
	-- should I cache disassembled chunks and only traverse instructions that are actually used? and track memory regions used?
	for mapIndex=0,countof(game.maps)-1 do
		local mapInfo = game.getMap(mapIndex)
		if mapInfo then

			local function decodeForMap(startAddr, reverseRefInfo)
				local startCmdSet = mapIndex < 3 and WorldCmds or EventCmds
				-- even if world-map is using commonReturnAddr, stillu se EventCmds, cuz I think this is the only address that could either be EventCmds or WorldCmds
				if startAddr == commonReturnAddr then
					startCmdSet = EventCmds
				end

				return decompileFrom{
					addr = startAddr,
					cmdset = startCmdSet,
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

	--[[ add builtins?
	for addr,name in pairs{
		[0xca0000] = 'no event',
		[0xca0001] = 'wait for dialogue window',
		[0xca0003] = 'game start',
		[0xca0008] = 'chest: item',
		[0xca000c] = 'chest: spell',
		[0xca0010] = 'chest: gp',
		[0xca0014] = 'chest: empty',
		[0xca0018] = 'random battle',
		[0xca0034] = 'tent',
		[0xca0039] = 'warp/warp stone',
		[0xca0040] = 'chest: monster-in-a-box',
		[0xca0078] = 'falcon: deck',
		[0xca009d] = 'doom gaze',
		[0xca00ea] = 'tent: animation',
		[0xca0108] = 'warp: animation',
		[0xca015e] = 'tent: animation (world)',
		[0xca01a2] = 'kefka's tower',
		[0xca0405] = 'phoenix cave',
		[0xca057d] = 'final battle/ending',
		[0xca5ade] = 'floating island: cinematic',
		[0xca5e33] = 'new game',
		[0xca5ea9] = 'post-battle',
		[0xca5eb3] = 'return',
		[0xca5eb4] = 'return (world)',
		[0xcaca64] = 'check facing direction',
		[0xcacd31] = 'inn: no dream',
		[0xcacd3c] = 'inn: normal',
		[0xcacd5b] = 'inn: dream 1',
		[0xcacdd9] = 'inn: dream 2',
		[0xcace51] = 'inn: dream 3',
		[0xcacefe] = 'inn: dream 4',
		[0xcacf67] = 'inn: fade out',
		[0xcacf96] = 'inn: fade in',
		[0xcb69ff] = 'not enough gp',
		[0xcc985b] = 'prologue',
		[0xcc9aeb] = 'save point',
		[0xcce566] = 'game over',
	} do
		game.eventScriptAddrs[addr] = table{{builtin=name}}
	end
	for addr,name in pairs{
		[0xcaa6c0] = 'blackjack book',
	} do
		game.eventScriptAddrs[addr] = table{{builtin=name, cmdset=VehicleCmds}}
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

end
