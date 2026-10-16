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
local range = require 'ext.range'
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


local function hex2(x)
	return ('0x%02x'):format(x)
end
local function hex4(x)
	return ('0x%04x'):format(x)
end
local function hex6(x)
	return ('0x%06x'):format(x)
end
local function hex8(x)
	return ('0x%08x'):format(x)
end

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


	-- hack to fix a few obj-script branch-back out-of-function that point to bad addrs
	game.rom[0x0a20be] = 0x15
	game.rom[0x0a48b8] = 7
	game.rom[0x0a48bd] = 5
	game.rom[0x0aed7a] = 0x24
	game.rom[0x0c737b] = 0xc
	-- hack to fix a few obj-script branch-back within-function that point to bad addresses
	game.rom[0x0b9bf3] = 0x23	-- points to the objScript instead of to its starting command
	game.rom[0x0b9dd1] = 0x23


	-- should I even bounds check?
	local scriptBaseAddr = ffi.offsetof(Game, 'eventScript')	-- 0xa0000
	local scriptBaseAddrEnd = scriptBaseAddr + ffi.sizeof(game.eventScript)

	local scriptBaseAddr2 = ffi.offsetof(Game, 'dialogBase')
	local scriptBaseAddrEnd2 = scriptBaseAddr2 + ffi.sizeof(game.dialogBase)

	-- 0x109800 - 0x10d000
	local battleEventScriptAddr = ffi.offsetof(Game, 'battleEventScriptOfs')
	local battleEventScriptAddrEnd = battleEventScriptAddr + ffi.sizeof(game.battleEventScriptOfs) + ffi.sizeof(game.battleEventScripts)

	local monsterScriptAddr = ffi.offsetof(Game, 'monsterScriptData')	-- 0x0f8700
	local monsterScriptAddrEnd = monsterScriptAddr + ffi.sizeof(game.monsterScriptData)

	local battleAnimScriptAddr = ffi.offsetof(Game, 'battleAnimScriptData')	-- mine starts at 0x0fffbe but offsets are at 0x100000
	local battleAnimScriptAddrEnd = battleAnimScriptAddr + ffi.sizeof(game.battleAnimScriptData)

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

		if type(self.desc) ~= 'string' then
			error("for cmdset="..tostring(self.classCmdSet)..' cmd='..tostring(self.cmd).." __tostring failed to find self.desc")
		end

		assert.eq(self.self, nil)
		self.self = self	-- ugly hack for template cuz i dont want to do an ugly metatable __index hack
		local s = template(self.desc, self)
		self.self = nil

		return s
	end
	-- only for template env
	Cmd.game = game
	Cmd.scriptBaseAddr = scriptBaseAddr
	-- static, useful for the template env:
	Cmd.getGotoOfsStr = function(addrOfs, op)
		local addr = scriptBaseAddr + addrOfs
		return Cmd.getGotoStr(addr, op)
	end
	-- also static
	Cmd.getGotoStr = function(addr, op)
		local str
		if addr == commonReturnAddr then
			--str = 'return' 	-- "call return" is gonna look weird :shrug:
			return 'return'
		else
			str = game.addrLabel(addr)
		end
		if not op then
			return 'goto '..str
		end
		-- is it 'goto return' as in we jmp to the instruction and its a rts and so we return?
		--  and therefore a 'goto return' is the same as just 'return'
		-- but then how about the 'call ' ... does that push to the same stack as whatever invoked the script in the first place,
		--  such that 'call return' is a 'nop'?
		--  or are all 'call return's erroneous, in that if the pc stack is different between invoking touch/event s and 'call ' within the script, then a 'call return' would produce some kind of error?
		return op..str
	end


	-- used by run-event-script.lua (should rename to run-script since it does multiple right now)
	-- default impl:
	function Cmd.toCode(self, indent)
		return tostring(self)
	end

	local cmdSetAbbrev = {
		EventCmds = 'EV',
		WorldCmds = 'WO',
		ObjectCmds = 'OB',
		VehicleCmds = 'VE',
		BattleEventCmds = 'BE',
		MonsterCmds = 'MO',
		BattleAnimCmds = 'BA',
	}

	-- return comment with address, bytes, and disasm code
	-- TODO how about a few columsn on the left instead of every-other-line ?
	-- and if the byte dump is too big then give this a few newlines
	function Cmd.getAsmLine(self)
		if not self.addr then return end
		assert.index(self, 'cmdset')
		assert.index(self, 'sizeInBytes')
		-- print addr
		return
			('%06x '):format(self.addr)

			-- print shorthand cmdset
			..cmdSetAbbrev[self.cmdset]

			-- print out bytes
			-- TODO for cmds too big, put their data on multiple lines?
			..ffi.string(rom + self.addr, self.sizeInBytes)
				:gsub('.', function(b)
					return (' %02x'):format(b:byte())
				end)
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
	local BattleEventCmds = {}	-- used by game.battleEventScript[]
	local MonsterCmds = {}		-- used by game.monsterScriptOfs[] & game.monsterScriptData
	local BattleAnimCmds = {}	-- used by game.battleAnimScriptOffsets[] & game.battleAnimScriptData


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
		__tostring = function(self)
			return "objScript{"
				.."objIndex="..self.cmd
				..(self.blocking and ', block=true' or '')
				..", cb=|objIndex|do"
			-- then upon end, "obj-return" and if blocking then "joinAll()" on all previous object-script forks
		end,
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
		desc = 'setScreenLocked(true)',
	}

	EventCmds.UnlockScreen = EventCmd:subclass{
		cmd = 0x39,
		desc = 'setScreenLocked(false)',
	}

	EventCmds.EnableUserControl = EventCmd:subclass{
		cmd = 0x3a,
		desc = "setUserControl(true)",
	}

	EventCmds.DisableUserControl = EventCmd:subclass{
		cmd = 0x3b,
		desc = "setUserControl(false)",
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
		desc = "createObj(<?=objIndex?>)",
	}

	EventCmds.DeleteObject = EventCmd:subclass{
		cmd = 0x3e,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = "deleteObj(<?=objIndex?>)",
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
		desc = 'objSetPal(<?=objIndex?>, <?=paletteIndex?>)',
	}

	EventCmds.ChangeObjectVehicle = EventCmd:subclass{
		cmd = 0x44,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, objIndex, arg)
			self.objIndex = objIndex
			self.vehicleIndex = bit.band(3, bit.rshift(arg, 5))
			self.showRider = 0 ~= bit.band(0x80, arg)
			assert.eq(bit.band(0x1f, arg), 0)	-- is the rest empty?
		end,
		-- why do I need 'self.' for it not to be nil? I thought 'self' was the template env ...
		desc = 'objSetVehicle(<?=objIndex?>, <?=vehicleIndex?>, <?=self.showRider?>)',
	}

	EventCmds.UpdateCharacterObjects = EventCmd:subclass{
		cmd = 0x45,
		desc = "sortObjects()",
	}

	EventCmds.SetActiveParty = EventCmd:subclass{
		cmd = 0x46,
		argtypes = {uint8_t},
		argnames = {'partyIndex'},
		desc = 'partySetActive(<?=partyIndex?>)',
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
			self.hideBG = 0 ~= bit.band(0x4000, arg)
			self.bottomOfScreen = 0 ~= bit.band(0x8000, arg)

			local dlg = game.dialog[self.dialogIndex]
			if dlg then
				self.trace.lastDialogPromptCount = select(2, dlg:gsub('%[PROMPT%]', '%0'))
			end
		end,
		__tostring = function(self)
			local dlg = game.dialog[self.dialogIndex]
			local str = dlg and tolua(tostring(dlg)) or 'nil'
			if self.async
			or self.hideBG
			or self.bottomOfScreen
			then
				return --'show dialog[0x'..number.hex(self.dialogIndex)..']:'
					'dialog('..str
					..', {'
					..table()
					:append{self.async and 'async=true' or nil}
					:append{self.hideBG and 'hideBG=true' or nil}
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
		async = true,
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
			return 'eventBattle{'
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
			..'func=<?=bit.band(7, bit.rshift(arg, 5))?>'
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

	EventCmds.SetFixedColorMathOp = EventCmd:subclass{
		argtypes = {uint8_t},
		getargs = function(self, arg)
			self.intensity = bit.band(7, arg)
			self.speed = bit.band(3, bit.rshift(arg, 3))
			self.colorFlags = bit.band(3, bit.rshift(arg, 6))
		end,
		__tostring = function(self)
			return 'setFixedColorMath'
				..self.colorOp..'('
				..self.intensity..', '
				..self.speed..', '
				..self.colorFlags..')'
		end,
	}

	EventCmds.SetFixedColorMathAdd = EventCmds.SetFixedColorMathOp:subclass{
		cmd = 0x56,
		argtypes = {uint8_t},
		colorOp = 'Add',
	}

	EventCmds.SetFixedColorMathSub = EventCmds.SetFixedColorMathOp:subclass{
		cmd = 0x57,
		argtypes = {uint8_t},
		colorOp = 'Sub',
	}

	EventCmds.ShakeScreen = EventCmd:subclass{
		cmd = 0x58,
		argtypes = {uint8_t},
		desc = 'shakeScreen(<?=args[1]?>)',
	}

	EventCmds.FadeInRate = EventCmd:subclass{
		cmd = 0x59,
		argtypes = {uint8_t},
		argnames = {'dt'},
		desc = 'fadeIn(<?=dt?>)',
	}

	EventCmds.FadeOutRate = EventCmd:subclass{
		cmd = 0x5a,
		argtypes = {uint8_t},
		argnames = {'dt'},
		desc = 'fadeOut(<?=dt?>)'
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
			EventCmds['ScrollBackground'..hex2(cmd)] = EventCmd:subclass{
				cmd = cmd,
				argtypes = {int8_t, int8_t},
				argnames = {'hscroll', 'vscroll'},
				layer = layer,
				_1scomp = _1scomp==1,
				-- 1's comp is negative scrolling?
				desc = 'scrollBackgroundLayer{'
					..'layer=<?=layer?>'
					..'<?=_1scomp and ", _1scomp=true" or ""?>'
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
		argnames = {'colorFlags', 'first', 'last'},
		__tostring = function(self)
			return 'tintColors('
				..self.colorFlags..', '
				..self.first..', '
				..self.last..')'
		end,
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

			-- hmm why is the map index off by 1? and not always too?
			-- off by 1 in the event entering Mt Kolts.
			-- not off by 1 in the event when entering the figaro caves from the figaro castle basement.
			self.mapIndex = bit.band(arg, 0x1ff)
			self.setParentMap = 0 ~= bit.band(arg, 0x200)
			self.zUpper = self.mapIndex >= 3 and 0 ~= bit.band(0x400, arg)
			self.showTitle = self.mapIndex >= 3 and 0 ~= bit.band(0x800, arg)
			self.dir = bit.band(3, bit.rshift(arg, 12))

			-- does setting vehicle imply switching to vehicle opcodes?
			--  if so, then does setting vehicle to nonzero in ObjectCmds.ChangeVehicle or EventCmds.ChangeObjectVehicle also do this?
			--  also is it for event cmds, world cmds, or vehicle cmds? (ironic for vehicle cmds, you already need to be in this state...)
			self.vehicle = bit.band(3, flags)
			self.flag2 = 0 ~= bit.band(4, flags)
			self.flag3 = 0 ~= bit.band(8, flags)
			self.flag4 = 0 ~= bit.band(16, flags)
			self.noSizeUpdate = self.mapIndex >= 3 and 0 ~= bit.band(0x20, flags)
			self.noFadeIn = self.mapIndex >= 3 and 0 ~= bit.band(0x40, flags)
			self.useStartEvent = self.mapIndex >= 3 and 0 ~= bit.band(0x80, flags)

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
				..(self.setParentMap and ", setParentMap=true" or "")
				..(self.zUpper and ", zUpper=true" or "")
				..(self.showTitle and ", showTitle=true" or '')
				..(", dir="..self.dir)
				..(", pos={"..hex2(self.x)..", "..hex2(self.y).."}")
				..(self.vehicle > 0 and ", vehicle="..self.vehicle or '')
				..(self.flag2 and ', flag2=true' or '')
				..(self.flag3 and ', flag3=true' or '')
				..(self.flag4 and ', flag4=true' or '')
				..(self.noSizeUpdate and ", noSizeUpdate=true" or '')
				..(self.noFadeIn and ", noFadeIn=true" or '')
				..(self.useStartEvent and ", useStartEvent=true" or '')
				..(self.async and ", async=true" or '')	-- what does 'async' mean?
			..'}'
		end,
	}
	Cmds.SetMap = SetMap

	EventCmds.SetMap = EventCmd:subclass(SetMap, {cmd = 0x6a})
	EventCmds.SetMapAsync = EventCmd:subclass(SetMap, {cmd = 0x6b, async = true})

	EventCmds.SetParentMap = EventCmd:subclass{
		cmd = 0x6c,
		--[[
		16 bits for mapIndex, when it only needs 9 ...
		8 bits for dir when it only needs 2 ...
		and bit7 in dir is being used ...
		--]]
		argtypes = {uint16_t, uint8_t, uint8_t, uint8_t},
		argnames = {'mapIndex', 'x', 'y', 'dir'},
		desc = 'setParentMap{mapIndex=<?=mapIndex?>, dir=<?=dir?>, pos={<?=x?>, <?=y?>}}',
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
		desc = 'objSetFullHPMP(<?=objIndex?>)',
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
		desc = 'partySetMap(<?=partyIndex?>, <?=mapIndex?>)',
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

	EventCmds.RestoreDefaultParty = EventCmd:subclass{
		cmd = 0x7b,
		desc = 'restoreDefaultParty()',
	}

	EventCmds.EnableCollisionEvent = EventCmd:subclass{
		cmd = 0x7c,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objEnableCollisionEvent(<?=objIndex?>, true)',
	}

	EventCmds.DisableCollisionEvent = EventCmd:subclass{
		cmd = 0x7d,
		argtypes = {uint8_t},
		argnames = {'objIndex'},
		desc = 'objEnableCollisionEvent(<?=objIndex?>, false)',
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
		desc = "charSetName(<?=objIndex?>, <?=nameIndex?>)",
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

	EventCmds.ResetDefaultParty = EventCmd:subclass{
		cmd = 0x82,
		desc = 'resetDefaultParty()',
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
		argnames = {'esperIndex'},	-- TODO this is +54 what it should be
		desc = 'giveEsper(<?=esperIndex?>)',
	}

	EventCmds.TakeEsper = EventCmd:subclass{
		cmd = 0x87,
		argtypes = {uint8_t},
		argnames = {'esperIndex'},	-- TODO this is +54 what it should be
		desc = 'takeEsper(<?=esperIndex?>)',
	}

	-- is there except for 'remove none' meaning 'remove all' ?
	-- also, chracterIndex is 0-15 ... or party indexes 49-52 ...
	--  so really this is an object command and not a character command, right?
	EventCmds.RemoveObjectStatus = EventCmd:subclass{
		cmd = 0x88,
		argtypes = {uint8_t, uint16_t},
		argnames = {'objIndex', 'status'},
		desc = "objStatusAnd(<?=objIndex?>, 0x<?=bit.tohex(status, 4)?>)",
	}

	EventCmds.SetObjectStatus = EventCmd:subclass{
		cmd = 0x89,
		argtypes = {uint8_t, uint16_t},
		argnames = {'objIndex', 'status'},
		desc = "objStatusOr(<?=objIndex?>, 0x<?=bit.tohex(status, 4)?>)",
	}

	EventCmds.ToggleObjectStatus = EventCmd:subclass{
		cmd = 0x8a,
		argtypes = {uint8_t, uint16_t},
		argnames = {'objIndex', 'status'},
		desc = "objStatusXor(<?=objIndex?>, 0x<?=bit.tohex(status, 4)?>)",
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
		argnames = {'objIndex'},
		desc = 'charNameChange(<?=objIndex?>)',
	}

	EventCmds.OpenSelectPartyMenu = EventCmd:subclass{
		cmd = 0x99,
		argtypes = {uint8_t, uint16_t},
		getargs = function(self, numParties, eventIndex)
			self.numParties = bit.band(0x7f, numParties)
			self.reset = 0 ~= bit.band(0x80, numParties)
			self.eventIndex = eventIndex
		end,
		__tostring = function(self)
			return 'openSelectPartyMenu('
				..self.numParties..', '
				..self.eventIndex
				..(self.reset and ', true' or '')
				..')'
		end,
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
			self.timerIndex = bit.band(3, bit.rshift(arg, 18))		-- 0-3
			self.showInMenu = 0 ~= bit.band(arg, bit.lshift(1, 20))	-- timer 0 only
			-- "banquet" in everything8215's ff6
			self.endsBattle = 0 ~= bit.band(arg, bit.lshift(1, 21))
			-- "field visible" in everything8215's ff6
			self.showInMap = 0 ~= bit.band(arg, bit.lshift(1, 22))	-- timer 0 only
			-- "field only" in everything8215's ff6
			self.pauseInMenu = 0 ~= bit.band(arg, bit.lshift(1, 23))
		end,
		getDescArgsWithoutCB = function(self)
			return 'duration='..self.duration
				..', timerIndex='..self.timerIndex
				..(self.showInMenu and ', showInMenu=true' or '')
				..(self.endsBattle and ', endsBattle=true' or '')
				..(self.showInMap and ', showInMap=true' or '')
				..(self.pauseInMenu and ', pauseInMenu=true' or '')
		end,
		__tostring = function(self)
			return 'startTimer{'
				..self:getDescArgsWithoutCB()
				..', cb='..game.addrLabel(self:getDestAddr())
			..'}'
		end,
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
	}

	EventCmds.StopTimer = EventCmd:subclass{
		cmd = 0xa1,
		argtypes = {uint8_t},
		argnames = {'timerIndex'},
		desc = 'stopTimer(<?=timerIndex?>)',
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
		desc = 'loadGame()',
	}

	EventCmds.LoadSavedCharacterObjectData = EventCmd:subclass{
		cmd = 0xac,
		desc = 'restoreGame()',
	}

	EventCmds.ShowWoRCustscene = EventCmd:subclass{
		cmd = 0xad,
		desc = "cutscene'world of ruin'",
	}

	EventCmds.ShowMagitekFactoryCutscene = EventCmd:subclass{
		cmd = 0xae,
		desc = "cutscene'magitek factory'",
	}

	local ForLoop = Cmd:subclass{
		argtypes = {uint8_t},
		argnames = {'count'},
		getargs = function(self, ...)
			Cmd.getargs(self, ...)
			self.trace.indent = self.trace.indent + 1
		end,
		__tostring = function(self)
			return 'for '..self:getFor()..' do'
		end,
		getFor = function(self)
			return 'i=1,'..self.count
		end,
	}
	Cmds.ForLoop = ForLoop

	local EndFor = Cmd:subclass{
		getargs = function(self)
			self.indent = self.indent - 1
			self.trace.indent = self.trace.indent - 1
		end,
		desc = 'end--for',
	}
	Cmds.EndFor = EndFor

	EventCmds.ForLoop = EventCmd:subclass(ForLoop, {cmd=0xb0})
	EventCmds.EndFor = EventCmd:subclass(EndFor, {cmd=0xb1})

	EventCmds.Call = EventCmd:subclass{
		cmd = 0xb2,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = "<?=getGotoOfsStr(destAddrOfs, 'call ')?>",
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
	}

	EventCmds.CallRepeat = EventCmd:subclass{
		cmd = 0xb3,
		argtypes = {uint8_t, uint24_t},
		argnames = {'count', 'destAddrOfs'},
		desc = "for i=1,<?=count?> do <?=getGotoOfsStr(destAddrOfs, 'call ')?> end",
		-- helper function
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		-- used by decompiler to determine where else to go
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
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

	EventCmds.SleepQuarterSeconds = EventCmd:subclass{
		cmd = 0xb5,
		argtypes = {uint8_t},
		argnames = {'quarterSeconds'},
		desc = 'sleep(<?=quarterSeconds?>/4)'
	}

	EventCmds.GotoForDialogResult = EventCmd:subclass{
		cmd = 0xb6,
		digest = function(self, read)
			self.options = table()
			if not self.trace.lastDialogPromptCount then error("required choices but no dialog at "..game.addrLabel(self.addr)) end
			for i=1,self.trace.lastDialogPromptCount do
				self.options:insert{addrOfs = read(uint24_t)}
			end
		end,
		__tostring = function(self)
			-- it's a goto, so return
			return 'return callForDialogResult('
				..self.options:mapi(function(option)
					return game.addrLabel(scriptBaseAddr + option.addrOfs, '')
				end):concat', '
				..')'
		end,

		getBranchAddrs = function(self)
			return self.options:mapi(function(option)
				return {addr = scriptBaseAddr + option.addrOfs}
			end)
		end,
	}

	EventCmds.CondBattleFlag = EventCmd:subclass{
		cmd = 0xb7,
		argtypes = {uint8_t, uint24_t},
		argnames = {'flagIndex', 'destAddrOfs'},
		__tostring = function(self)
			return 'if '
				..self:getCond()
				..' then '
				..self.getGotoOfsStr(self.destAddrOfs)
				..' end'
		end,
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
		getCond = function(self)
			return 'battleFlagGet('..self.flagIndex..')'
		end,
	}

	local EventSetBattleFlag = EventCmd:subclass{
		argtypes = {uint8_t},
		argnames = {'flagIndex'},
		__tostring = function(self)
			return 'battleFlagSet('
				..self.flagIndex
				..', '
				..tostring(self.value)
			..')'
		end,
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
		digest = function(self, ...)
			EventCmd.digest(self, ...)
			self.indent = self.indent - 1
			self.trace.indent = self.trace.indent - 1
		end,
		__tostring = function(self)
			if self.skipEndFor then
				return 'if '..self:getCond()..' then break end'
			else
				return 'if '..self:getCond()..' then break end\n'
					..'end--for'
			end
		end,
		getCond = function(self)
			return 'mapFlagGet('..self.cond..')'
		end,
	}

	EventCmds.Jump5050 = EventCmd:subclass{
		cmd = 0xbd,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		__tostring = function(self)
			return 'if '
				..self:getCond()
				..' then '
				..self.getGotoOfsStr(self.destAddrOfs)
				..' end'
		end,
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
		getCond = function(self)
			return 'math.random() < .5'
		end,
	}

	-- this is based on active char?
	-- or whether you have the char in your party?
	-- or based on opcode 0xe1?
	EventCmds.CallSwitchNPCFlags = EventCmd:subclass{
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
			return 'charSwitch{'
			..self.options:mapi(function(option)
				return '{'..option.characterIndex..', '
					..(option.addrOfs
						and game.addrLabel(scriptBaseAddr + option.addrOfs)
						or 'stmts=...'
					)..'}'
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
						..'mapFlagGet('..cond.flagIndex..')'
				end):concat(self._and and ' and ' or ' or ')
		end,
		__tostring = function(self)
			return 'if '
				..self:getCond()
				..' then '
				..self.getGotoOfsStr(self.destAddrOfs)
				..' end'
		end,
		getCond = function(self)
			return self.condCode
		end,
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
	}
	Cmds.Cond = Cond

	-- in EventCmds for 0xc0-0xcf and in WorldCmds for 0xb0-0xbf
	for cmd=0xc0,0xcf do
		EventCmds['Cond '..hex2(cmd)] = EventCmd:subclass(Cond, {cmd = cmd})
	end

	--[[
	for EventCmds:
	d0,d2,d4,d6,d8,da,dc = set
	d1,d3,d5,d7,d9,db,dd = clear
	bit 0 = set vs clear
	bits 1-3 = bits 8-10 of the event flag index
	(no delay)
	--]]
	for cmd=0xd0,0xdd do
		EventCmds['SetFlag'..hex2(cmd)] = EventCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			getargs = function(self, flagIndex)
				-- 0 = set, 1 = clear
				self.flagValue = 0 == bit.band(1, self.cmd)
				-- move cmd bits 1:3 to event bits 8:10
				self.flagIndex = bit.bor(
					bit.lshift(bit.band(self.cmd, 0xe), 7),
					flagIndex
				)
			end,
			__tostring = function(self)
				return 'mapFlagSet('
					..self.flagIndex..', '
					..tostring(self.flagValue)
					..')'
			end,
		}
	end

	-- from everything8215's ff6/src/event/README.md ...
	--0xde = all characters in the current active party.
	--0xdf = all characters that are currently active objects.
	--0xe0 = all initialied characters (name, properties, etc)
	--0xe1 = all characters available in the party select screen (TODO what flags determine this?)
	--0xe2 = the topmost character in the party (/ leader)
	--0xe3 = all characters in all parties
	--0xe4 = "sets a switch for the current active party index" ... so like "TERRA"=0 for party1, "LOCKE"=1 for party2, etc?
	for cmd=0xde,0xe4 do
		EventCmds['SetCharSwitchFlags'..hex2(cmd)] = EventCmd:subclass{
			cmd = cmd,
			desc = 'setCharSwitchFlags('..hex2(cmd)..')',
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
		desc = 'eventVars[<?=var?>] = <?=value?>',
	}

	EventCmds.VarAdd = EventCmd:subclass{
		cmd = 0xe9,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		desc = 'eventVars[<?=var?>] += <?=value?>',
	}

	EventCmds.VarSub = EventCmd:subclass{
		cmd = 0xea,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		desc = 'eventVars[<?=var?>] -= <?=value?>',
	}

	EventCmds.VarCmp = EventCmd:subclass{
		cmd = 0xeb,
		argtypes = {uint8_t, uint16_t},
		argnames = {'var', 'value'},
		-- write charSwitch flags + 0 when equal, 1 when var is greater, 2 when var is less
		desc = 'eventVarCmp(eventVars[<?=var?>], <?=value?>)',
	}

	EventCmds.PlaySongVol = EventCmd:subclass{
		cmd = 0xef,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, song, volume)
			-- 7th bit of song is altStart
			-- but playSongFadeIn, 7th bit of speed is altStart
			-- hmmm
			self.song = bit.band(0x7f, song)
			self.altStart = 0 ~= bit.band(0x80, song)
			self.volume = volume
		end,
		__tostring = function(self)
			return 'playSong('
				..self.song..', '
				..self.volume
				..(self.altStart and ', true' or '')
				..')'
		end,
	}

	EventCmds.PlaySong = EventCmd:subclass{
		cmd = 0xf0,
		argtypes = {uint8_t},
		argnames = {'song'},
		desc = 'playSong(<?=song?>)',
	}

	EventCmds.PlaySongFadeIn = EventCmd:subclass{
		cmd = 0xf1,
		argtypes = {uint8_t, uint8_t},
		getargs = function(self, song, speed)
			self.song = song
			self.altStart = 0 ~= bit.band(0x80, speed)
			self.speed = bit.band(0x7f, speed)
		end,
		argnames = {'song', 'speed'},
		__tostring = function(self)
			return 'fadeInSong('
				..self.song..', '
				..self.speed
				..(self.altStart and ', true' or '')
				..')'
		end,
	}

	EventCmds.PlaySongFadeOut = EventCmd:subclass{
		cmd = 0xf2,
		argtypes = {uint8_t},
		argnames = {'speed'},
		desc = 'fadeOutSong(<?=speed?>)',
	}

	EventCmds.FadeInPrevSong = EventCmd:subclass{
		cmd = 0xf3,
		argtypes = {uint8_t},
		argnames = {'speed'},
		desc = 'fadeInPrevSong(<?=speed?>)',
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
		desc = 'playSound(<?=sfx?>, <?=pan?>, <?=envelope?>)',
	}

	EventCmds.SPCInterrupt = EventCmd:subclass{
		cmd = 0xf6,
		argtypes = {uint24_t},
		argnames = {'destAddr'},
		__tostring = function(self)
			return ('spcInterrupt(0x%06x)'):format(self.destAddr)
		end,
	}

	EventCmds.ContinueSong = EventCmd:subclass{
		cmd = 0xf7,
		desc = 'continueSong()',
	}

	EventCmds.WaitForSPC = EventCmd:subclass{
		cmd = 0xf8,
		desc = 'waitForSPC(2)',
	}

	EventCmds.SyncSPC = EventCmd:subclass{
		cmd = 0xf9,
		argtypes = {uint8_t},
		argnames = {'pos'},
		desc = 'syncSPC(<?=pos?>)',
	}

	EventCmds.WaitForSPC = EventCmd:subclass{
		cmd = 0xfa,
		desc = 'waitForSPC(3)',
	}

	-- 1-frame delay
	EventCmds.WaitForSound = EventCmd:subclass{
		cmd = 0xfb,
		desc = 'nop()',
	}

	local Return = Cmd:subclass{
		desc = 'return',
		endTrace = true,
	}
	Cmds.Return = Return

	EventCmds.Return = EventCmd:subclass(Return, {
		cmd = 0xfe,
	})


	local EndScript = Cmd:subclass{
		desc = 'endScript()',
		endTrace = true
	}
	Cmds.EndScript = EndScript

	EventCmds.EndScript = EventCmd:subclass(EndScript, {
		cmd = 0xff,
	})

	-- EventCmds key by cmd (number) or by name (string)
	for _,k in ipairs(table.keys(EventCmds)) do
		local cl = EventCmds[k]
		if cl.cmd then	-- some abstract classes are in EventCmds but don't have a .cmd
			assert.type(cl.cmd, 'number')
cl.classname = k
			EventCmds[cl.cmd] = cl
		end
	end
	for i=0,255 do
		if not EventCmds[i] then
			EventCmds[i] = EventCmd:subclass{
				cmd = i,
				desc = '??? '..hex2(i),
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
	-- 1000:0000b thru 1001:1111b
	--    y:yyxx
	-- x = dir, y = dist-1
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

	-- same as above but without objIndex
	ObjectCmds.ChangeVehicle = ObjectCmd:subclass{
		cmd = 0xc9,
		argtypes = {uint8_t},
		getargs = function(self, arg)
			self.vehicleIndex = bit.band(3, bit.rshift(arg, 5))
			self.showRider = 0 ~= bit.band(0x80, arg)
			assert.eq(bit.band(0x1f, arg), 0)	-- is the rest empty?
		end,
		desc = 'objSetVehicle(objIndex, <?=vehicleIndex?>, <?=self.showRider?>)',
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

	-- from everything8215's ff6/src/event/README.md:
	-- "event, world, and vehicle script modes operate at 60 fps"
	-- "object script mode operates at 15 fps"
	ObjectCmds.Sleep = ObjectCmd:subclass{
		cmd = 0xe0,
		argtypes = {uint8_t},
		argnames = {'frames_x4'},
		desc = 'sleep(<?=frames_x4?>/15)',	-- i.e. x4/60
	}

	--[[
	e1,e2,e3 = set ... 123 maps to 012 which is the high-byte value of the flag-index?
	e4,e5,e6 = clear
	--]]
	for cmd=0xe1,0xe6 do
		ObjectCmds['SetFlag'..hex2(cmd)] = ObjectCmd:subclass{
			cmd = cmd,
			argtypes = {uint8_t},
			getargs = function(self, flagIndex)
				if cmd <= 0xe3 then
					self.flagValue = true
					self.flagIndex = bit.bor(
						flagIndex,
						bit.lshift(cmd-0xe1, 8)
					)
				else
					self.flagValue = false
					self.flagIndex = bit.bor(
						flagIndex,
						bit.lshift(cmd-0xe4, 8)
					)
				end
			end,
			__tostring = function(self)
				return 'mapFlagSet('
					..self.flagIndex..', '
					..tostring(self.flagValue)
					..')'
			end,
		}
	end

	ObjectCmds.Call = ObjectCmd:subclass{
		cmd = 0xf9,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		desc = "<?=getGotoOfsStr(destAddrOfs, 'call ')?>",

		getBranchAddrs = function(self)
			return {
				{addr=scriptBaseAddr + self.destAddrOfs},
			}
		end,
	}

	--[[
	here's a fun fact
	branch-backwards is only ever with signed positive values
	no args are >= 0x80, so it might be signed
	except
	for 0xff
	there are lots of FC FF object commands
	and if the arg was signed, this could mean infinite-loop
	so here goes...
	in fact, looks like all branches are signed, but no negative values are ever used
	wonder why they even bothered separate branch-fwd and branch-back
	--]]
	ObjectCmds.Branch = ObjectCmd:subclass{
		--cmd = 0xfa,
		argtypes = {uint8_t},
		argnames = {'offset'},
		getDestAddr = function(self)
			if self.offset == 0xff then return end -- return self.addr end
			return self.dir == '-'
				and self.addr - self.offset
				or self.addr + self.offset
		end,
		-- TODO you will have to generate a label for this instruction, and for wherever it is going
		__tostring = function(self)
			-- no signed offset in any branch is >= 0x80
			--  except branch-backwards -1 i.e. "FC FF"
			-- in that case, insert an end.
			if self.offset == 0xff then
				return 'while true do yield() end'
			end

			local s = Cmd.getGotoStr(self:getDestAddr())
			if self.random then
				s = 'if math.random() < .5 then '..s..' end'
			end

			return s
		end,

		-- idk that i necessarily want a new trace, just a label...
		-- ... yeah this is currently a bad idea
		--[[
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
		--]]
	}
	ObjectCmds.BranchBack50 = ObjectCmds.Branch:subclass{
		cmd = 0xfa,
		random = true,
		dir = '-',
		getCond = function(self)
			return 'math.random() < .5'
		end,
	}
	ObjectCmds.BranchFwd50 = ObjectCmds.Branch:subclass{
		cmd = 0xfb,
		random = true,
		dir = '+',
		getCond = function(self)
			return 'math.random() < .5'
		end,
	}
	--[[
	NOTICE - every ObjectCmd BranchBack 0xFC will next have an optional 0xFF 'return' and then it will end the objScript.
	--]]
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
			and (
				self.addr == objectScriptCmd.addr + objectScriptCmd.length
				or self.offset == 0xff
			)
			then
				popObjectCmdSet(self.trace, self.addr, true)
			end
		end,
		--]=]
	}

	-- I don't see this ever used...
	ObjectCmds.BranchFwd = ObjectCmds.Branch:subclass{
		cmd = 0xfd,
		random = false,
		dir = '+',
	}

	-- EndScript 0xff is also in EventCmds, WorldCmds, VehicleCmds
	-- but in ObjectCmds, EndScript always precedes the end of an objScript block (but not always vice versa)
	ObjectCmds.EndScript = ObjectCmd:subclass{
		cmd = 0xff,
		digest = function(self, ...)
			popObjectCmdSet(self.trace, self.addr)
			self.indent = self.indent - 1
		end,
		desc = 'end}',
	}


	-- map the by-number for by-name
	for _,k in ipairs(table.keys(ObjectCmds)) do
		local cl = ObjectCmds[k]
		if cl.cmd then
			assert.type(cl.cmd, 'number')
cl.classname = k
			ObjectCmds[cl.cmd] = cl
		end
	end
	-- fill in empty numbers with unknowns
	for i=0,255 do
		if not ObjectCmds[i] then
			ObjectCmds[i] = ObjectCmd:subclass{
				cmd = i,
				desc = '??? '..hex2(i),
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

	-- same as ObjectCmds
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
	WorldCmds.SetMapAsync = WorldCmd:subclass(SetMap, {cmd = 0xd3, async = true})

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
		desc = "airshipSetPos(<?=x?>, <?=y?>)",
	}

	-- like VehicleCmds SetFlag
	local WorldSetFlag = WorldCmd:subclass{
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		__tostring = function(self)
			return 'mapFlagSet('
				..self.flagIndex
				..', '
				..tostring(self.flagValue)
				..')'
		end,
	}
	WorldCmds.SetFlag = WorldSetFlag:subclass{
		cmd = 0xc8,
		flagValue = true,
	}
	WorldCmds.ClearFlag = WorldSetFlag:subclass{
		cmd = 0xc9,
		flagValue = false,
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

	-- I don't see this used anywhere ...
	WorldCmds.IfKeyThenGoto = WorldCmd:subclass{
		cmd = 0xd4,
		argtypes = {uint24_t},
		argnames = {'destAddrOfs'},
		__tostring = function(self)
			return 'if '
				..self:getCond()
				..' then '
				..self.getGotoOfsStr(self.destAddrOfs)
				..' end'
		end,
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
		getCond = function(self)
			return 'keypress()'
		end,
	}

	-- also I don't see this used ...
	WorldCmds.IfFacingThenGoto = WorldCmd:subclass{
		cmd = 0xd5,
		argtypes = {uint8_t, uint24_t},
		argnames = {'dir', 'destAddrOfs'},
		__tostring = function(self)
			return 'if '
				..self:getCond()
				..' then '
				..self.getGotoOfsStr(self.destAddrOfs)
				..' end'
		end,
		getDestAddr = function(self)
			return scriptBaseAddr + self.destAddrOfs
		end,
		getBranchAddrs = function(self)
			return {
				{addr=self:getDestAddr()},
			}
		end,
		getCond = function(self)
			return 'dir=='..self.dir
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
		desc = 'sleep(<?=frames?>/60)',
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
	--  or wait, in everything8215-ff6/src/event/event_main.asm it is first just a `.byte $FE` then load_map then end-of-world-script (to event-script), and then a few more commands, and then a proper `return` from $FE...
	-- maybe you have to track the state of figaroSubmerge() to find when 0xfe will be emerge vs return?
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
cl.classname = k
			WorldCmds[cl.cmd] = cl
		end
	end
	-- fill in empty numbers with unknowns
	for i=0,255 do
		if not WorldCmds[i] then
			WorldCmds[i] = WorldCmd:subclass{
				cmd = i,
				desc = '??? '..hex2(i),
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
			desc = "vehicleMove(<?=cmd?>, <?=dist?>)",
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
		desc = "airshipSetAlt(<?=alt?>)",
	}

	VehicleCmds.MoveForward = VehicleCmd:subclass{
		cmd = 0xc6,
		argtypes = {uint16_t},
		argnames = {'speed'},
		desc = "airshipMoveFwd(<?=speed?>)",
	}

	VehicleCmds.SetAirshipPos = VehicleCmd:subclass{
		cmd = 0xc7,
		argtypes = {uint8_t, uint8_t},
		argnames = {'x', 'y'},
		desc = "airshipSetPos(<?=x?>, <?=y?>)",
	}

	-- like WorldSetFlag
	local VehicleSetFlag = VehicleCmd:subclass{
		argtypes = {uint16_t},
		argnames = {'flagIndex'},
		__tostring = function(self)
			return 'mapFlagSet('
				..self.flagIndex
				..', '
				..tostring(self.flagValue)
				..')'
		end,
	}
	VehicleCmds.SetFlag = VehicleSetFlag:subclass{
		cmd = 0xc8,
		flagValue = true,
	}
	VehicleCmds.ClearFlag = VehicleSetFlag:subclass{
		cmd = 0xc9,
		flagValue = false,
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
	VehicleCmds.SetMapAsync = VehicleCmd:subclass(SetMap, {cmd = 0xd3, async = true})

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
		desc = 'vehicleSetDirArrows(true)',
	}
	VehicleCmds.LockDirArrows = VehicleCmd:subclass{
		cmd = 0xdb,
		desc = 'vehicleSetDirArrows"lock"',
	}
	VehicleCmds.HideDirArrows = VehicleCmd:subclass{
		cmd = 0xdc,
		desc = 'vehicleSetDirArrows(false)',
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
	VehicleCmds.SleepQuarterSeconds = VehicleCmd:subclass{
		cmd = 0xe0,
		argtypes = {uint8_t},
		argnames = {'frames'},
		desc = 'sleep(<?=frames?>/60)',
	}

	VehicleCmds.Cinematic_EndingAirship = VehicleCmd:subclass{
		cmd = 0xf2,
		desc = "airshipShowEnding()",
	}
	VehicleCmds.Cinematic_LightOfJudgment1 = VehicleCmd:subclass{
		cmd = 0xf3,
		desc = "showLightOfJudgment1()",
	}

	VehicleCmds.ChangeVehicleGraphicToFalcon = VehicleCmd:subclass{
		cmd = 0xf4,
		desc = 'vehicleSetGfx"falcon"',
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
		desc = 'vehicleSetGfx"bird"',
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
		desc = "airshipShowSmoking()",
	}
	VehicleCmds.Cinematic_AirshipCrashing = VehicleCmd:subclass{
		cmd = 0xfc,
		desc = "airshipShowCrashing()",
	}
	VehicleCmds.ChangeVehicleGraphicToMorphedTerra= VehicleCmd:subclass{
		cmd = 0xfd,
		desc = 'vehicleSetGfx"morphed_terra"',
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
cl.classname = k
			VehicleCmds[cl.cmd] = cl
		end
	end
	-- fill in empty numbers with unknowns
	for i=0,255 do
		if not VehicleCmds[i] then
			VehicleCmds[i] = VehicleCmd:subclass{
				cmd = i,
				desc = '??? '..hex2(i),
			}
		end
	end



	-- battle-event-script:


	do
		game.BattleEventCmds = BattleEventCmds
		local BattleEventCmd = Cmd:subclass()
		game.BattleEventCmd  = BattleEventCmd

		BattleEventCmds.BattleDialogTop = BattleEventCmd:subclass{
			cmd = 0x00,
			argtypes = {uint8_t},
			argnames = {'index'},
			__tostring = function(self)
				return 'dialogTop('..tolua(tostring(game.battleDialog[self.index]))..')'
			end,
		}
		BattleEventCmds.BattleDialogBottom = BattleEventCmd:subclass{
			cmd = 0x01,
			argtypes = {uint8_t},
			argnames = {'index'},
			__tostring = function(self)
				return 'dialogBottom('..tolua(tostring(game.battleDialog[self.index]))..')'
			end,
		}
		-- 0x02 is unused?
		for i=0,3 do
			BattleEventCmds['CharAnim'..i] = BattleEventCmd:subclass{
				cmd = 0x03 + i,
				argtypes = {uint8_t, uint16_t},
				argnames = {'char', 'addrOfs'},	-- addrOfs relative to battleAnimScriptAddr / 0x100000 / 0xd00000 probably for all battle events
				__tostring = function(self)
					return 'charAnim('
						..i..', '
						..self.char..', '
						..hex4(self.addrOfs)
					..')'
				end,
			}
		end
		for i=0,5 do
			BattleEventCmds['MonsterAnim'..i] = BattleEventCmd:subclass{
				cmd = 0x07 + i,
				argtypes = {uint8_t, uint16_t},
				argnames = {'char', 'addrOfs'},	-- addrOfs relative to battleAnimScriptAddr / 0x100000 / 0xd00000 probably for all battle events
				__tostring = function(self)
					return 'monsterAnim('
						..i..', '
						..self.char..', '
						..hex4(self.addrOfs)
					..')'
				end,
			}
		end
		BattleEventCmds.BattleAnim = BattleEventCmd:subclass{
			cmd = 0x0d,
			argtypes = {uint8_t, uint8_t, uint8_t},
			argnames = {'anim', 'attacker', 'targets'},
			desc = 'battleAnim(<?=anim?>, <?=attacker?>, <?=targets?>)',
		}
		BattleEventCmds.ResetAnim = BattleEventCmd:subclass{
			cmd = 0x0e,
			desc = 'resetAnim()',
		}
		BattleEventCmds.ExecAnim = BattleEventCmd:subclass{
			cmd = 0x0f,
			desc = 'execAnim()',
		}
		BattleEventCmds.CloseDialog = BattleEventCmd:subclass{
			cmd = 0x10,
			desc = 'closeDialog()',
		}
		BattleEventCmds.OpenDialog = BattleEventCmd:subclass{
			cmd = 0x11,
			desc = 'openDialog()',
		}
		BattleEventCmds.AllCharsAnim = BattleEventCmd:subclass{
			cmd = 0x12,
			argtypes = table{uint16_t}:rep(24),
			__tostring = function(self)
				return 'allCharsAnim('
					..self.args
					:mapi(function(addr)
						return hex4(addr)
					end)
					:concat', '
				..')'
			end,
			getBranchAddrs = function(self)
				return self.args:mapi(function(addr)
					return {addr = battleAnimScriptAddr + addr, cmdset='BattleAnimCmds'}
				end)
			end,
		}
		BattleEventCmds.AddCharTarget = BattleEventCmd:subclass{
			cmd = 0x13,
			argtypes = {uint8_t},
			argnames = {'target'},
			desc = 'addCharTarget(<?=target?>)',
		}
		BattleEventCmds.ShowCharMenu = BattleEventCmd:subclass{
			cmd = 0x14,
			argtypes = {uint8_t},
			argnames = {'target'},
			desc = 'showCharMenu(<?=target?>)',
		}
		BattleEventCmds.EndScript = BattleEventCmd:subclass(EndScript, {
			cmd = 0xff,
		})
		for _,k in ipairs(table.keys(BattleEventCmds)) do
			local cl = BattleEventCmds[k]
			if cl.cmd then	-- some abstract classes are in BattleEventCmds but don't have a .cmd
				assert.type(cl.cmd, 'number')
	cl.classname = k
				BattleEventCmds[cl.cmd] = cl
			end
		end
		for i=0,255 do
			if not BattleEventCmds[i] then
				BattleEventCmds[i] = BattleEventCmd:subclass{
					cmd = i,
					desc = '??? '..hex2(i),
				}
			end
		end
	end


	-- monster script:


	do
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
				return 'getObj('..i..')'	-- 1+ cuz it's a 1-based table...
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


		game.MonsterCmds = MonsterCmds
		local MonsterCmd = Cmd:subclass()
		game.MonsterCmd = MonsterCmd

		for cmd=0,0xef do
			MonsterCmds['Attack '..hex2(cmd)] = MonsterCmd:subclass{
				cmd = cmd,
				__tostring = function(self)
					return	'doAttack('..self.cmd..')\t-- '..getSpellName(cmd)
				end,
			}
		end

		MonsterCmds.PickAttack = MonsterCmd:subclass{
			cmd = 0xf0,
			argtypes = {uint8_t, uint8_t, uint8_t},
			__tostring = function(self)
				return 'pickAttack('..self.args:concat', '..')\t-- options: '
					..self.args:mapi(getSpellName):concat', '
			end,
		}

		MonsterCmds.SetTargets = MonsterCmd:subclass{
			cmd = 0xf1,
			argtypes = {uint8_t},
			argnames = {'target'},
			__tostring = function(self)
				return 'setTargets('..getTarget(self.target)..')'
			end,
		}

		MonsterCmds.ChangeBattle = MonsterCmd:subclass{
			cmd = 0xf2,
			argtypes = {uint8_t, uint16_t},
			getargs = function(self, x1, x23)
				self.exitEffect = bit.band(0x7f, x1)
				self.scrollBG = 0 ~= bit.band(0x80, x1)
				self.battleIndex = bit.band(0x7fff, x23)
				self.restoreMonsters = 0 ~= bit.band(0x8000, x23)
			end,
			__tostring = function(self)
				return 'changeBattle{exitEffect='..self.exitEffect
					..', scrollBG='..tostring(self.scrollBG)
					..', battleIndex='..self.battleIndex
					..', restoreMonsters='..tostring(self.restoreMonsters)
					..'}'
			end,
		}

		MonsterCmds.Dialog = MonsterCmd:subclass{
			cmd = 0xf3,
			argtypes = {uint16_t},
			argnames = {'dialogIndex'},
			__tostring = function(self)
				-- i is from 0 to 140
				return 'dialog('..tolua(tostring(game.monsterDialog[self.dialogIndex]))..')'
			end,
		}

		MonsterCmds.IDKCmd = MonsterCmd:subclass{
			cmd = 0xf4,
			argtypes = {uint8_t, uint8_t, uint8_t},
			desc = 'cmd(<?=args:concat", "?>)',
		}

		MonsterCmds.SetVisible = MonsterCmd:subclass{
			cmd = 0xf5,
			argtypes = {uint8_t, uint8_t, uint8_t},
			getargs = function(self, anim, opIndex, enemyIndex)
				self.anim = anim
				self.opIndex = opIndex
				self.enemyIndex = enemyIndex
			end,
			__tostring = function(self)
				return (({
					[0] = 'setVis{visible=true, restoreHP=true',
					-- does "disable targeting" mean also "don't consider for win-condition?"
					[1] = 'setVis{visible=false, allowTargeting=false',
					[2] = 'setVis{visible=true',
					[3] = 'setVis{visible=false, allowTargeting=true',
					[4] = 'setVis{visible=false, dontEndBattle=true',
					[5] = 'setVis{visible=false, debugMode=true',	-- not used
				})[self.opIndex] or error'here')
					..', anim='..self.anim
					..', targets={'..getTargetMask(self.enemyIndex)..'}'
					..'}'
			end,
		}

		MonsterCmds.UseThrowItem = MonsterCmd:subclass{
			cmd = 0xf6,
			argtypes = {uint8_t, uint8_t, uint8_t},
			getargs = function(self, useVsThrow, item12, item3)
				self.useVsThrow = useVsThrow
				self.item12 = item12
				self.item3 = item3
			end,
			__tostring = function(self)
				if self.useVsThrow == 0 then
					return ('useItem(%q, %q)'):format(self.item12, self.item3)
				elseif self.useVsThrow == 1 then
					return ('throwItem(%q, %q)'):format(self.item12, self.item3)
				else
					error'here'
				end
			end,
		}

		MonsterCmds.BattleEvent = MonsterCmd:subclass{
			cmd = 0xf7,
			argtypes = {uint8_t},
			argnames = {'index'},
			desc = 'doBattleEvent(<?=index?>)',	-- runs game.battleEventScriptOfs
		}

		MonsterCmds.BattleVar = MonsterCmd:subclass{
			cmd = 0xf8,
			argtypes = {uint8_t, uint8_t},
			getargs = function(self, var, opval)
				self.var = var
				self.op = bit.band(3, bit.rshift(opval, 6))
				self.value = bit.band(0x3f, opval)
			end,
			__tostring = function(self)
				if self.op == 0 then
					return 'battleVarSet('..self.var..', '..self.value..')'
				elseif self.op == 1 then
					error'here'
				elseif self.op == 2 then
					return 'battleVarInc('..self.var..', '..self.value..')'
				elseif self.op == 3 then
					return 'battleVarDec('..self.var..', '..self.value..')'
				end
			end,
		}

		MonsterCmds.BattleFlag = MonsterCmd:subclass{
			cmd = 0xf9,
			argtypes = {uint8_t, uint8_t, uint8_t},
			getargs = function(self, p1, p2, p3)
				self.flagIndex = bit.bor(
					bit.lshift(p2, 3),
					bit.band(7, p3)
				)
				local rest = bit.rshift(p3, 3)
				assert.eq(rest, 0)	-- why even only use 3 lower bits? weird.
				self.value = p1
			end,
			__tostring = function(self)
				if self.value == 0 then
					return 'battleFlagSet('..self.flagIndex..', not battleFlagGet('..self.flagIndex..'))'
				elseif self.value == 1 then
					return 'battleFlagSet('..self.flagIndex..', true)'
				elseif self.value == 2 then
					return 'battleFlagSet('..self.flagIndex..', false)'
				else
					error'here'
				end
			end,
		}

		MonsterCmds.PlaySound = MonsterCmd:subclass{
			cmd = 0xfa,
			argtypes = {uint8_t, uint8_t, uint8_t},
			__tostring = function(self)
				local x1, x2, x3 = table.unpack(self.args)
				if x1 == 0x09 then
					return 'playSound('..x2..', '..x3..')'
				else
					-- is fa09 special?
					return 'anim('..x1..', '..x2..', '..x3..')'
				end
			end,
		}

		MonsterCmds.Misc = MonsterCmd:subclass{
			cmd = 0xfb,
			argtypes = {uint8_t, uint8_t},
			__tostring = function(self)
				local p1, p2 = table.unpack(self.args)
				if p1 == 0 then
					assert.eq(p2, 0)
					return 'resetMonsterTimer(self)'
				elseif p1 == 1 then
					return 'makeInvisible('..p2..')'
				elseif p1 == 2 then
					assert.eq(p2, 0)
					return 'endBattle()'
				elseif p1 == 3 then
					assert.eq(p2, 0)
					return 'addGauToParty()'
				elseif p1 == 4 then
					assert.eq(p2, 0)
					return 'resetBattleTimer()'
				elseif p1 == 5 then
					return 'removeInvisible('..p2..')'
				elseif p1 == 6 then
					return 'makeTargetable('..p2..')'
				elseif p1 == 7 then
					return 'makeUntargetable('..p2..')'
				elseif p1 == 8 then
					return 'setATBToMax('..p2..')'
				elseif p1 == 9 then
					assert.eq(p2, 0)
					return 'endBattleAfterGauReturnsFromVeldt()'
				elseif p1 == 11 then
					return 'setStatus(self, '..p2..')'	-- on who?
				elseif p1 == 12 then		-- not used?
					return 'removeStatus(self, '..p2..')'
				elseif p1 == 13 then
					assert.eq(p2, 0)
					return 'hideMonster(self)'
				else
					error'here'
				end
			end,
		}

		-- technically this can be thougth of as "if not X then goto the next end-if"
		-- because successif if's are and'd together
		MonsterCmds.If = MonsterCmd:subclass{
			cmd = 0xfc,
			argtypes = {uint8_t, uint8_t, uint8_t},
			__tostring = function(self)
				local p1, p2, p3 = table.unpack(self.args)
				local condIndex = p1
				if condIndex == 1 then
					-- p2 means command-2 which means menu-item-2 which is magic
					local action1 = ('%q'):format(getActionName(p2))
					local action2 = ('%q'):format(getActionName(p3))
					local actions = action1 == action2 and action1 or action1..', '..action2
					-- p3 == 2 is targeting <-> this monster?
					-- or p2 == 2, ip3 == 2 <-> action == magic, spell #2 == bolt?
					return 'if wasTargetedWithAction('..actions..')'
				elseif condIndex == 2 then
					return 'if wasTargetedWithSpell('..(
						p2 == p3
						and tostring(p2)
						or (p2..', '..p3)
					)..')'
				elseif condIndex == 3 then
					local item1 = ('%q'):format(getItemName(p2))
					local item2 = ('%q'):format(getItemName(p3))
					local items = item1 == item2 and item1 or item1..', '..item2
					return 'if wasTargetedWithItem('..items..')'
				elseif condIndex == 4 then
					assert.eq(p3, 0)
					return 'if wasHitByElement('..p2..')'
				elseif condIndex == 5 then
					assert.eq(p2, 0)
					assert.eq(p3, 0)
					return 'if wasTargeted()'	-- what's this mean? if any action targeted this monster?
				elseif condIndex == 6 then
					-- is it always < ?
					return 'if '..getTarget(p2)..'.hp < '..(p3 * 128)
				elseif condIndex == 7 then
					-- is it always < ?
					return 'if '..getTarget(p2)..'.mp < '..p3
				elseif condIndex == 8 then
					return 'if 0 ~= targetStatus('..getTarget(p2)..') & (1 << '..hex2(p3)..')'
				elseif condIndex == 9 then
					return 'if 0 == targetStatus('..getTarget(p2)..') & (1 << '..hex2(p3)..')'
				elseif condIndex == 11 then
					assert.eq(p3, 0)
					return 'if monsterTimer(self) > '..(p2 * 2)	-- seconds?
				elseif condIndex == 12 then
					return 'if battleVarGet('..p2..') < '..p3
				elseif condIndex == 13 then
					return 'if battleVarGet('..p2..') > '..p3
				elseif condIndex == 14 then
					return 'if '..getTarget(p2)..'.level <'..p3
				elseif condIndex == 15 then
					local target
					-- for all target conds or just this one?
					if p2 == 68 then
						return 'if randomTargetsLevelGt('..p3..')'
					elseif p2 < 16 then
						-- target 0-15 is character 0-15 is object 0-15
						target = 'getObj('..p2..')'
						return 'if '..target..'.level > '..p3
					else
						target = 'getTarget('..p2..')'
						return 'if '..target..'.level > '..p3
					end
				elseif condIndex == 16 then
					assert.eq(p2, 0)
					assert.eq(p3, 0)
					return 'if onlyOneTypeOfEnemyAlive()'
				elseif condIndex == 17 then
					assert.eq(p3, 0)
					return 'if isAlive('..getTargetMask(p2)..')'
				elseif condIndex == 18 then
					assert.eq(p3, 0)
					return 'if isDead('..getTargetMask(p2)..')'
				elseif condIndex == 19 then
					local func, cmp
					if p2 == 0 then
						func = 'numPartyAlive()'
						cmp = '>='
					elseif p2 == 1 then
						func = 'numEnemiesAlive()'
						cmp = '<='
					else
						func = '???'..p2
						cmp = '???'
					end
					return 'if '..func..' '..cmp..' '..p3
				elseif condIndex == 20 then
					-- {p2, p3}:
					-- {9, 1} = 0x61
					-- {6, 1} = 0x49
					local flagIndex = bit.bor(
						bit.lshift(p2, 3),
						bit.band(7, p3)
					)
					local rest = bit.rshift(p3, 3)
					assert.eq(rest, 0)	-- why even only use 3 lower bits? weird.
					return 'if battleFlagGet('..flagIndex..')'
				elseif condIndex == 21 then
					local flagIndex = bit.bor(
						bit.lshift(p2, 3),
						bit.band(7, p3)
					)
					local rest = bit.rshift(p3, 3)
					assert.eq(rest, 0)	-- why even only use 3 lower bits? weird.
					return 'if not battleFlagGet('..flagIndex..')'
				elseif condIndex == 22 then
					assert.eq(p3, 0)
					return 'if battleTimer() < '..p2
				elseif condIndex == 23 then
					assert.eq(p3, 0)
					return 'if targetIsValid('..p2..')'
				elseif condIndex == 24 then
					assert.eq(p2, 0)
					assert.eq(p3, 0)
					return 'if gauIsPresent()'
				elseif condIndex == 25 then
					assert.eq(p3, 0)
					local monsterName = p2 == 0 and 'self'
						or p2 == 0xff and '"all enemies"'
						or tostring(p2-1)
					return 'if battlePlaceAvailable('..monsterName..')'	-- what does 'this monster slot' or 'all monster slot' mean?
				elseif condIndex == 26 then
					return 'if wasHitWithElement('..p2..', '..p3..')'
				elseif condIndex == 27 then
					local p12 = bit.bor(p1, bit.lshift(p2, 8))
					return 'if battleIndex['..p12..']'
				else
					error'here'
				end
			end,
		}

		MonsterCmds.PassTurn = MonsterCmd:subclass{
			cmd = 0xfd,
			desc = 'passTurn()',
		}

		MonsterCmds.EndIf = MonsterCmd:subclass{
			cmd = 0xfe,
			desc = 'return end--if',
		}

		MonsterCmds.Return = MonsterCmd:subclass(Return, {
			cmd = 0xff,
		})

		for _,k in ipairs(table.keys(MonsterCmds)) do
			local cl = MonsterCmds[k]
			if cl.cmd then	-- some abstract classes are in MonsterCmds but don't have a .cmd
				assert.type(cl.cmd, 'number')
cl.classname = k
				MonsterCmds[cl.cmd] = cl
			end
		end
		for i=0,255 do
			if not MonsterCmds[i] then
				MonsterCmds[i] = MonsterCmd:subclass{
					cmd = i,
					desc = '??? '..hex2(i),
				}
			end
		end
	end


	-- battle-anim-script


	do
		local function getaddr(addr)
			return game.addrLabel(addr + battleAnimScriptAddr)
		end
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


		game.BattleAnimCmds = BattleAnimCmds
		local BattleAnimCmd = Cmd:subclass()
		game.BattleAnimCmd  = BattleAnimCmd

		local ShowFrame = BattleAnimCmd:subclass{
			desc = 'showFrame(<?=cmd?>)',
		}
		BattleAnimCmds.ShowFrame = ShowFrame
		for cmd=0,0x1f do
			BattleAnimCmds[('ShowFrame_0x%02x'):format(cmd)] = ShowFrame:subclass{cmd=cmd}
		end

		BattleAnimCmds.Use8x8BG1Tiles = BattleAnimCmd:subclass{
			cmd = 0x20,
			desc = 'use8x8BG1Tiles()',
		}

		BattleAnimCmds.Cmd_0x80 = BattleAnimCmd:subclass{
			cmd = 0x80,
			digest = function(self, read)
				-- maybe this deserves a lookup table of its own?
				local subcmd = read(uint8_t)
				if subcmd == 0x00 then
					self.descstr = 'quadraSlamSlice()'
				elseif subcmd == 0x01 then
					self.descstr = 'clearPosOffset()'
				elseif subcmd == 0x02 then
					self.descstr = 'attackDirPush()'
				elseif subcmd == 0x03 then
					self.descstr = 'attackDirPop()'
				elseif subcmd == 0x04 then
					self.descstr = 'randomizeVectorAngleAndPosInitFireDanceSprites()'
				elseif subcmd == 0x05 then
					self.descstr = 'bumRushInitPos()'
				elseif subcmd == 0x06 then
					self.descstr = 'initTornado()'
				elseif subcmd == 0x07 then
					self.descstr = 'moveTornadoToThreadPos()'
				elseif subcmd == 0x08 then
					self.descstr = 'moveThreadToVectorPosition()'
				elseif subcmd == 0x09 then
					self.descstr = 'updateObjSpriteTilePriorityForTornado()'
				elseif subcmd == 0x0a then
					self.descstr = 'whiteEffectMagicIntro()'
				elseif subcmd == 0x0b then
					self.descstr = 'updateEsperPreAnimationBallsPos()'
				elseif subcmd == 0x0c then
					self.descstr = 'moveToFirstThread()'
				elseif subcmd == 0x0e then
					self.descstr = 'initMonsterJumpPos()'
				elseif subcmd == 0x0f then
					self.descstr = 'gravBombCalcVec()'
				elseif subcmd == 0x10 then
					self.descstr = 'moveToTargetPos()'
				elseif subcmd == 0x11 then
					self.descstr = 'randomizeVectorAngle()'
				elseif subcmd == 0x12 then
					self.descstr = 'polarInit()'
				elseif subcmd == 0x13 then
					self.descstr = 'makeTargetImp()'
				elseif subcmd == 0x14 then
					self.descstr = 'makeTargetVanish()'
				elseif subcmd == 0x15 then
					self.descstr = 'moveCircleToThreadPos()'
				elseif subcmd == 0x16 then
					self.descstr = 'circleReset()'
				elseif subcmd == 0x17 then
					self.descstr = 'updateSpriteLayerPriorityBasedOnTarget()'
				elseif subcmd == 0x18 then
					self.descstr = 'loadSketchedMonsterPalette()'
				elseif subcmd == 0x19 then
					self.descstr = 'sketch()'
				elseif subcmd == 0x1A then
					local arg = read(uint8_t)
					self.descstr = 'validateMonsterEntry('..arg..')'
				elseif subcmd == 0x1B then
					self.descstr = 'transformIntoMagicite()'
				elseif subcmd == 0x1C then
					self.descstr = 'decrScreenBrightness()'
				elseif subcmd == 0x1D then
					self.descstr = 'transformIntoMagicite()'
				elseif subcmd == 0x1e then
					self.descstr = 'holyUpdateBG3Scroll()'
				elseif subcmd == 0x1f then
					self.descstr = 'holyInitBG3ScrollX()'
				elseif subcmd == 0x20 then
					self.descstr = 'holyInitBG3ScrollY()'
				elseif subcmd == 0x21 then
					self.descstr = 'updateRotatingSpriteLayerPriority()'
				elseif subcmd == 0x22 then
					self.descstr = 'pearlWind()'
				elseif subcmd == 0x23 then
					self.descstr = 'pearlWind()'
				elseif subcmd == 0x24 then
					self.descstr = 'clear_BG3_HDMA_scrolldata()'
				elseif subcmd == 0x25 then
					self.descstr = 'clear_BG1_HDMA_scrolldata()'
				elseif subcmd == 0x26 then
					local arg = read(uint8_t)
					self.descstr = 'setCharColorPalUpdates('..tostring(arg ~= 0)..')'
				elseif subcmd == 0x27 then
					local arg = read(uint8_t)
					self.descstr = 'showCharsForEsperAttack('..tostring(arg ~= 0)..')'
				elseif subcmd == 0x28 then
					local arg = read(uint8_t)
					self.descstr = 'setAllCharsSpritePriority('
						..bit.band(3, bit.rshift(arg, 4))
						..')'
				elseif subcmd == 0x29 then
					local arg = read(uint8_t)
					self.descstr = 'showEsperAttackCursorSprite('..tostring(arg ~= 0)..')'
				elseif subcmd == 0x2A then
					local arg = read(uint8_t)
					self.descstr = 'loadSpriteAndAnimPal('..hex2(arg)..')'
				elseif subcmd == 0x2B then
					local arg = read(uint8_t)
					self.descstr = 'loadBG1AnimPal('..hex2(arg)..')'
				elseif subcmd == 0x2C then
					local arg = read(uint8_t)
					self.descstr = 'loadBG3AnimPal('..hex2(arg)..')'
				elseif subcmd == 0x2D then
					local x = read(uint16_t)
					local y = read(uint16_t)
					local z = read(uint16_t)
					self.descstr = 'jumpBasedOnAttackType{'
						..'normal='..getaddr(x)
						..', backOrSide34='..getaddr(y)
						..', pincerOrSide12='..getaddr(z)
						..'}'
				elseif subcmd == 0x2E then
					local x = read(uint8_t)
					local y = read(uint8_t)
					self.descstr = 'moveSpriteTo('..x..', '..y..')'
				elseif subcmd == 0x30 then
					local arg = read(uint8_t)
					self.descstr = 'loadChar1AnimPal('..hex2(arg)..')'
				elseif subcmd == 0x31 then
					local speed = read(uint8_t)
					self.descstr = 'moveInWideSineWave('..hex2(speed)..')'
				elseif subcmd == 0x32 then
					-- TODO argument for emitting separate cmd objs
					-- / make a recursive emit() that does table lookup and class obj generation
					local x = read(uint16_t)
					local y = read(uint16_t)
					if x == y then
						self.descstr = 'jumpTo('..getaddr(x)..')'
					else
						self.descstr = 'jumpIfFacing('
							..getaddr(x)..', '
							..getaddr(y)..')'
					end
				elseif subcmd == 0x33 then
					local arg = read(uint8_t)
					self.descstr = 'updateRainbowGradient()'
				elseif subcmd == 0x34 then
					self.descstr = 'copyMonsterToCharPalettes()'
				elseif subcmd == 0x35 then
					self.descstr = 'useCharPalsForMonsterSpriteData()'
				elseif subcmd == 0x36 then
					self.descstr = 'restoreMonsterSpritePals()'
				elseif subcmd == 0x37 then
					self.descstr = 'clearFixedColorValueHDMAData()'
				elseif subcmd == 0x38 then
					self.descstr = 'enableHighPriorityBG3()'
				elseif subcmd == 0x39 then
					local arg = read(uint8_t)
					self.descstr = 'updateBlueGradient()'
				elseif subcmd == 0x3a then
					local arg = read(uint8_t)
					self.descstr = 'initBlueGradient('..arg..')'
				elseif subcmd == 0x3B then
					self.descstr = 'setTargetColorPalToAnimPal()'
				elseif subcmd == 0x3C then
					self.descstr = 'setTargetColorPaletteToNormal()'
				elseif subcmd == 0x3D then
					self.descstr = 'quadraSlamSlice()'
				elseif subcmd == 0x3E then
					local arg = read(uint8_t)
					self.descstr = 'setMainScreenDesignation()'
				elseif subcmd == 0x3F then
					self.descstr = 'sonicDive()'
				elseif subcmd == 0x40 then
					local arg = read(uint8_t)
					self.descstr = 'setScreenMode('..hex2(arg)..')'
				elseif subcmd == 0x41 then
					local cx, cy, dx, dy = read(int8_t), read(int8_t), read(int8_t), read(int8_t)
					self.descstr = 'shrinkBG1AndMove('
						..cx..','..cy..', '	-- shrink-x shrink-y
						..dx..','..dy..')'	-- move-x move-y
				elseif subcmd == 0x42 then
					local vh = read(uint8_t)
					self.descstr = 'set_MODE7_register('
						..tostring(0 ~= bit.band(2, vh))..', '		-- vflip
						..tostring(0 ~= bit.band(1, vh))..')'		-- hflip
				elseif subcmd == 0x43 then
					self.descstr = 'moonSongCharm()'
				elseif subcmd == 0x44 then
					self.descstr = 'fireBoltIceBeam()'
				elseif subcmd == 0x45 then
					local arg = read(uint8_t)
					self.descstr = 'set_BG12_mask_register()'
				elseif subcmd == 0x46 then
					local arg = read(uint8_t)
					self.descstr = 'setBG1TilemapLocation('..hex2(arg)..'00)'
				elseif subcmd == 0x47 then
					self.descstr = 'setBG1GfxAddr('..read(uint8_t)..')'
				elseif subcmd == 0x48 then
					self.descstr = 'clear'
				elseif subcmd == 0x49 then
					self.descstr = 'inkHitVirite()'
				elseif subcmd == 0x4a then
					self.descstr = 'disableStepBack()'
				elseif subcmd == 0x4B then
					self.descstr = 'updateRedYellowGradient()'
				elseif subcmd == 0x4C then
					self.descstr = 'moveTriangleToThreadPosition()'
				elseif subcmd == 0x4D then
					self.descstr = 'setVectorFromTriangleToTarget()'
				elseif subcmd == 0x4e then
					self.descstr = 'resetFrameOffset()'
				elseif subcmd == 0x4f then
					self.descstr = 'moveToAttacker_80_4f()'
				elseif subcmd == 0x50 then
					self.descstr = 'scrollBG3Random('..read(uint8_t)..')'
				elseif subcmd == 0x51 then
					self.descstr = 'rippler()'
				elseif subcmd == 0x52 then
					self.descstr = 'stone()'
				elseif subcmd == 0x53 then
					self.descstr = 'rpolarity(0x53)'
				elseif subcmd == 0x54 then
					self.descstr = 'rpolarity(0x54)'
				elseif subcmd == 0x55 then
					self.descstr = 'quasar()'
				elseif subcmd == 0x56 then
					self.descstr = 'bg1ScrollGoner()'
				elseif subcmd == 0x57 then
					local arg = read(uint8_t)
					self.descstr = 'set_BG34_window_mask('..hex2(arg)..')'
				elseif subcmd == 0x58 then
					local arg = read(uint8_t)
					self.descstr = 'setCircleShape('..hex2(arg)..')'
				elseif subcmd == 0x59 then
					local arg = read(uint8_t)
					self.descstr = 'setWindow2Size('..arg..')'
				elseif subcmd == 0x5A then
					self.descstr = 'mindBlast(0x5a)'
				elseif subcmd == 0x5B then
					self.descstr = 'mindBlast(0x5b)'
				elseif subcmd == 0x5C then
					self.descstr = 'mindBlast(0x5c)'
				elseif subcmd == 0x5d then
					self.descstr = 'updateVecWave()'
				elseif subcmd == 0x5E then
					self.descstr = 'overcast()'
				elseif subcmd == 0x5F then
					local arg = read(int8_t)
					self.descstr = 'incBlueBgGrad('..arg..')'
				elseif subcmd == 0x60 then
					local flags = read(uint32_t)	-- TODO is aabbccdd 4 sets of 2 bits or 4 bytes?
					self.descstr = 'toggleAttackerStatus('..hex8(flags)..')'
				elseif subcmd == 0x61 then
					local x, y, z = read(uint8_t), read(uint8_t), read(uint8_t)
					self.descstr = 'eyedropUpdate('..x..', '..y..', '..z..')'
				elseif subcmd == 0x62 then
					self.descstr = 'evilTootFader()'
				elseif subcmd == 0x63 then
					local arg = read(uint8_t)
					self.descstr = 'moveInNarrowVertSineWave('..arg..')'	-- speed
				elseif subcmd == 0x64 then
					self.descstr = 'purifierInvizEdge()'
				elseif subcmd == 0x65 then
					self.descstr = '-- unknown/nop 0x80 0x65'
				elseif subcmd == 0x66 then
					self.descstr = 'shockWave()'
				elseif subcmd == 0x67 then
					self.descstr = 'loadExtraEsperPalette()'
				elseif subcmd == 0x68 then
					self.descstr = 'purifier()'
				elseif subcmd == 0x69 then
					self.descstr = 'updateSpriteLayerPriorityBasedOnAttacker('
				elseif subcmd == 0x6A then
					self.descstr = 'alignBottomOfThreadWithBottomOfTarget()'
				elseif subcmd == 0x6B then
					self.descstr = 'lqpearl()'
				elseif subcmd == 0x6C then
					self.descstr = 'overcast()'
				elseif subcmd == 0x6D then
					self.descstr = 'disableBattleMenu()'
				elseif subcmd == 0x6f then
					self.descstr = 'updateWindowHDMA()'
				elseif subcmd == 0x70 then
					self.descstr = 'alignBG3VScroll()'
				elseif subcmd == 0x71 then
					self.descstr = 'restoreCharPals()'
				elseif subcmd == 0x72 then
					local arg = read(uint8_t)
					self.descstr = 'branchIfAttackHit('..getaddr(self.trace.pc + arg)..')'
				elseif subcmd == 0x73 then
					local arg = read(uint8_t)
					self.descstr = 'setDiceGraphics('..hex2(arg)..')'
				elseif subcmd == 0x75 then
					self.descstr = 'superball('..read(uint8_t)..', '..read(uint8_t)..', '..read(uint8_t)..')'
				elseif subcmd == 0x76 then
					self.descstr = 'seize '..read(uint8_t)
				elseif subcmd == 0x77 then
					self.descstr = 'seize'
				elseif subcmd == 0x78 then
					self.descstr = 'discard'
				elseif subcmd == 0x79 then
					self.descstr = 'characters run to left side of screen (takes 56 loops to reach other side)'
				elseif subcmd == 0x7A then
					self.descstr = 'characters run to right side of screen (takes 56 loops to reach other side)'
				elseif subcmd == 0x7B then
					self.descstr = 'flip all characters (after running to opposite side of screen)'
				elseif subcmd == 0x7C then
					self.descstr = 'swap target and attacker'
				elseif subcmd == 0x7D then
					local arg = read(uint8_t)
					self.descstr = 'if dragonHornEffectIsActive() then goto _'..getaddr(self.trace.pc + arg)..' end'
				elseif subcmd == 0x7E then
					self.descstr = 'flip target character vertically'
				elseif subcmd == 0x7F then
					self.descstr = 'hideAllMonsters()'
				elseif subcmd == 0x80 then
					self.descstr = 'bossDeathSetPal('..read(uint8_t)..', '..read(uint8_t)..', '..read(uint8_t)..')'
				elseif subcmd == 0x82 then
					self.descstr = 'enemyMoveX('..read(int8_t)..')'
				elseif subcmd == 0x84 then
					self.descstr = 'chadarnookExit(0x84)'
				elseif subcmd == 0x83 then
					self.descstr = 'chadarnookSavePos()'
				elseif subcmd == 0x85 then
					self.descstr = 'chadarnookExit(0x85)'
				elseif subcmd == 0x86 then
					local arg = read(uint8_t)
					self.descstr = 'playSound('
						..arg..') pan based on sprite X position'
				elseif subcmd == 0x87 then
					local arg = read(uint8_t)
					self.descstr = 'playSound('
						..arg..') pan based on sprite Y position'
				elseif subcmd == 0x89 then
					local arg = read(uint8_t)
					self.descstr = 'setKefkaHeadShakeEnabled('..arg..')'
				elseif subcmd == 0x8a then
					self.descstr = 'clearTargetMonsterSpritePriority()'
				elseif subcmd == 0x8b then
					self.descstr = 'playSoundChing()'
				elseif subcmd == 0x8c then
					local arg = read(uint8_t)
					self.descstr = 'playSound('
						..hex2(arg)..') pan center'
				elseif subcmd == 0xdb then
					local ofs = read(uint8_t)
					self.descstr = 'jumpStep('..Cmd.getGotoStr(self.addr + ofs, '')..')'
				else
					self.descstr = '??? 0x80 '..hex2(subcmd)
				end
			end,
		}

		BattleAnimCmds[0x81] = BattleAnimCmd:subclass{
			cmd = 0x81,
			argtypes = {uint8_t, uint8_t},
			argnames = {'leftSpriteIndex', 'rightSpriteIndex'},
			desc = 'setAttackerSprite(<?=leftSpriteIndex?>, <?=rightSpriteIndex?>)'	-- npc/char sprite, (left, right)
		}
		BattleAnimCmds[0x82] = BattleAnimCmd:subclass{
			cmd = 0x82,
			argtypes = {uint8_t, uint8_t},
			argnames = {'leftSpriteIndex', 'rightSpriteIndex'},
			desc ='setTargetSprite(<?=leftSpriteIndex?>, <?=rightSpriteIndex?>)',	-- (left, right)
		}
		BattleAnimCmds[0x83] = BattleAnimCmd:subclass{
			cmd = 0x83,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'move(currentEnt, '
					..movedirs[bit.rshift(self.arg, 5)]
					..', '..(bit.band(self.arg, 0x1f)+1)
					..')'
			end,
		}
		BattleAnimCmds[0x84] = BattleAnimCmd:subclass{
			cmd = 0x84,
			argtypes = {uint8_t},
			argnames = {'speed'},
			desc = 'setAnimSpeed(<?=speed+1?>)',
		}
		BattleAnimCmds.MoveToAttacker = BattleAnimCmd:subclass{
			cmd = 0x85,
			desc = 'moveToAttacker_84()',
		}
		BattleAnimCmds[0x86] = BattleAnimCmd:subclass{
			cmd = 0x86,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'move(attacker, '
					..movedirs[bit.rshift(self.arg, 5)]
					..', '..hex2(bit.band(self.arg, 0x1f)+1)..')'
			end,
		}
		BattleAnimCmds[0x87] = BattleAnimCmd:subclass{
			cmd = 0x87,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'move(target, '
					..movedirs[bit.rshift(self.arg, 5)]
					..', '..hex2(bit.band(self.arg, 0x1f)+1)..')'
			end,
		}
		BattleAnimCmds[0x88] = BattleAnimCmd:subclass{
			cmd = 0x88,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return [[$F71D fight: set frame to ]]
					..hex2(self.arg)..[[ and jump forward with weapon]]
			end,
		}

		-- same as eventcmds 0xb0 & 0xb1
		BattleAnimCmds.ForLoop = BattleAnimCmd:subclass(ForLoop, {
			cmd = 0x89,
			getargs = function(self, ...)
				Cmds.ForLoop.getargs(self, ...)
				self.count = self.count + 1
			end,
		})

		BattleAnimCmds.EndFor = EventCmd:subclass(EndFor, {cmd=0x8a})

		BattleAnimCmds.AnimForLoop = BattleAnimCmd:subclass(ForLoop, {
			cmd = 0x8b,
			argtypes = {uint8_t},
			getargs = function(self, arg)
				Cmds.ForLoop.getargs(self, arg)
				self.arg = arg
			end,
			getFor = function(self)
				return 'animFrameOfs=0,'..(self.arg-1)
			end,
		})

		BattleAnimCmds.AnimEndFor = BattleAnimCmd:subclass(EndFor, {
			cmd = 0x8c,
			desc = 'end -- for animFrameOfs',
		})

		BattleAnimCmds[0x8D] = BattleAnimCmd:subclass{
			cmd = 0x8D,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'if animationIsHflipped() then setDirAndMove('
					..movedirs[bit.rshift(self.arg, 5)]
					..', '..hex2(bit.band(self.arg, 0x1f)+1)..') end'
			end,
		}
		BattleAnimCmds[0x8E] = BattleAnimCmd:subclass{
			cmd = 0x8E,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc = 'showThread(<?=arg?>)',
			--[[
			__tostring = function(self)
				return 'showThread('
					..(0 ~= bit.band(0x80, self.arg) and ' below' or ' above')
					..(0 ~= bit.band(0x40, self.arg) and ' front' or ' back')
					..' other sprites (sprite priority) with'
					..(0 ~= bit.band(1, self.arg) and '' or ' opposite')..' weapon hand'
			end,
			--]]
		}
		BattleAnimCmds[0x8F] = BattleAnimCmd:subclass{
			cmd = 0x8F,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				-- TODO either 0x8D or 0x8F should probably be 'vflipped'
				return 'if animationIsHflipped() then setDirAndMove('
					..movedirs[bit.rshift(self.arg, 5)]
					..', '..hex2(bit.band(self.arg, 0x1f)+1)..') end'
			end,
		}
		BattleAnimCmds[0x90] = BattleAnimCmd:subclass{
			cmd = 0x90,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'set thread sprite tile priority to '
					..bit.band(3, bit.rshift(self.arg, 4))
					..' (tile priority)'
			end,
		}
		BattleAnimCmds[0x91] = BattleAnimCmd:subclass{
			cmd = 0x91,
			desc = 'move this thread to attacker thread position',
		}
		BattleAnimCmds[0x92] = BattleAnimCmd:subclass{
			cmd = 0x92,
			argtypes = {uint8_t, uint8_t},
			argnames = {'speed', 'branch'},
			__tostring = function(self)
				-- TODO this is an end-loop and branch is how many instructions to go back
				return 'moveAlongVec('
					..self.speed..', '
					..self.branch..')'
			end,
		}
		BattleAnimCmds[0x93] = BattleAnimCmd:subclass{
			cmd = 0x93,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc ='setVectorOffset(<?=arg?>)',
		}
		BattleAnimCmds[0x94] = BattleAnimCmd:subclass{
			cmd = 0x94,
			desc = 'setVecFromAttackerToRandomLocOnTarget()',
		}
		BattleAnimCmds[0x95] = BattleAnimCmd:subclass{
			cmd = 0x95,
			desc ='setVecFromAttackerToTarget()',
		}
		BattleAnimCmds[0x96] = BattleAnimCmd:subclass{
			cmd = 0x96,
			argtypes = {uint8_t, uint8_t},
			argnames = {'xx', 'yy'},
			__tostring = function(self)
				return 'if idk then jump backwards '..hex2(self.xx)
			end,
		}
		BattleAnimCmds[0x97] = BattleAnimCmd:subclass{
			cmd = 0x97,
			desc = 'boomerang/wing edge/full moon/rising sun',
		}
		BattleAnimCmds[0x98] = BattleAnimCmd:subclass{
			cmd = 0x98,
			argtypes = {uint8_t, uint8_t},
			argnames = {'numFrames', 'x'},
			desc = 'waitAndIncFrame(<?=numFrames?>, <?=x?>)',
		}
		BattleAnimCmds[0x99] = BattleAnimCmd:subclass{
			cmd = 0x99,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'setPalette('..hex2(bit.band(7, bit.rshift(self.arg, 1)))..')'
			end,
		}
		BattleAnimCmds[0x9A] = BattleAnimCmd:subclass{
			cmd = 0x9A,
			desc = 'set thread facing direction to match attacker',
		}

		BattleAnimCmds.AnimThreadLoop = BattleAnimCmd:subclass(ForLoop, {
			cmd = 0x9f,
			argtypes = {uint8_t},
			argnames = {'arg'},
			getFor = function(self)
				return 'i=1,numAnimThreads'..self.arg
			end,
		})

		BattleAnimCmds[0xA0] = BattleAnimCmd:subclass{
			cmd = 0xA0,
			argtypes = {uint8_t, uint8_t},
			getargs = function(self, speed, addrOfs)
				self.speed = speed
				self.destAddr = self.addr + 2 - addrOfs
			end,
			__tostring = function(self)
				return 'moveAlongVecAndGoto('
					..self.speed..', '
					..Cmd.getGotoStr(self.destAddr, '')
					..')'
			end,
		}
		BattleAnimCmds[0xA1] = BattleAnimCmd:subclass{
			cmd = 0xA1,
			argtypes = {uint8_t, uint8_t},
			argnames = {'arg', 'arg2'},
			__tostring = function(self)
				return 'jump backward along vector (speed '
					..hex2(self.arg)..', code branch '..hex2(self.arg2)..')'
			end,
		}
		BattleAnimCmds[0xA2] = BattleAnimCmd:subclass{
			cmd = 0xA2,
			desc = 'drill()',
		}
		BattleAnimCmds[0xA3] = BattleAnimCmd:subclass{
			cmd = 0xA3,
			argtypes = {uint16_t},
			argnames = {'arg'},
			desc = 'rotatePalette(<?=arg?>)',
		}
		BattleAnimCmds[0xA4] = BattleAnimCmd:subclass{
			cmd = 0xA4,
			argtypes = {uint8_t, uint8_t},
			argnames = {'arg', 'arg2'},
			__tostring = function(self)
				return 'rotatePalette{'
					..'numcolors='..bit.band(0xf, self.arg)
					..', offset='..bit.band(0xf, bit.rshift(self.arg, 4))
					..', speed='..bit.band(0xf, self.arg2)
					..', paletteIndex='..bit.band(0xf, bit.rshift(self.arg, 4))
					..'}'
			end,
		}
		BattleAnimCmds[0xA5] = BattleAnimCmd:subclass{
			cmd = 0xA5,
			argtypes = {int8_t, int8_t, uint8_t, uint8_t, uint16_t, uint8_t},
			getargs = function(self, x, y, dr, p, rmax, q)
				self.x = x + 0x80
				self.y = y + 0x80
				self.dr = dr
				self.rmax = rmax
				self.p = p
				self.q = q
			end,
			desc = 'circleSeProps(<?=x?>, <?=y?>, <?=dr?>, <?=rmax?>, <?=p?>, <?=q?>)'
		}
		BattleAnimCmds[0xA6] = BattleAnimCmd:subclass{
			cmd = 0xA6,
			argtypes = {int8_t, int8_t, int8_t},
			argnames = {'dx', 'dy', 'dr'},
			desc = 'circleMoveAndResize(<?=dx?>, <?=dy?>, <?=dr?>)',
		}
		BattleAnimCmds[0xA7] = BattleAnimCmd:subclass{
			cmd = 0xA7,
			desc = 'circleUpdate()',
		}
		BattleAnimCmds[0xA8] = BattleAnimCmd:subclass{
			cmd = 0xA8,
			desc = 'circleMoveToAttacker()',
		}
		BattleAnimCmds[0xA9] = BattleAnimCmd:subclass{
			cmd = 0xA9,
			argtypes = {int8_t, int8_t},
			argnames = {'dx', 'dy'},
			desc = 'circleMove(<?=dx?>, <?=dy?>)', --based on character facing direction
		}

		BattleAnimCmds[0xAC] = BattleAnimCmd:subclass{
			cmd = 0xAC,
			argtypes = {uint8_t, uint8_t},
			argnames = {'x', 'y'},
			desc = 'initScrollWave(<?=x?>, <?=y?>)',
		}
		BattleAnimCmds[0xAD] = BattleAnimCmd:subclass{
			cmd = 0xAD,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'setScrollHDMA('..bit.rshift(self.arg, 6)
					..', '..hex2(bit.band(self.arg, 0x3f))
					..')'
			end,
		}
		BattleAnimCmds[0xAE] = BattleAnimCmd:subclass{
			cmd = 0xAE,
			argtypes = {uint8_t},
			argnames = {'x'},	-- vh___123
			desc = 'updateScrollWave(<?=x?>)',
		}

		BattleAnimCmds[0xBD] = BattleAnimCmd:subclass{
			cmd = 0xBD,
			argtypes = {uint8_t},
			getargs = function(self, arg)
				self.arg = arg
				if 0 ~= bit.band(0x80, arg) then
					self.bg = 1
					self.visible = 0 == bit.band(0x20, arg)
				elseif 0 ~= bit.band(0x40, arg) then
					self.bg = 3
					self.visible = 0 == bit.band(0x10, arg)
				end
			end,
			__tostring = function(self)
				return 'setThreadVisible('..self.bg..', '..tostring(self.visible)..') -- '..self.arg
			end,
		}
		BattleAnimCmds[0xBE] = BattleAnimCmd:subclass{
			cmd = 0xBE,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'set screen mosaic to '..hex2(self.arg)..' ($2106)'
			end,
		}

		BattleAnimCmds[0xBF] = BattleAnimCmd:subclass{
			cmd = 0xBF,
			argtypes = {uint16_t},
			argnames = {'destAddr'},
			__tostring = function(self)
				return Cmd.getGotoStr(self:getDestAddr(), 'call ')
			end,
			getDestAddr = function(self)
				return battleAnimScriptAddr + self.destAddr
			end,
			getBranchAddrs = function(self)
				return {
					{addr=self:getDestAddr()},
				}
			end,
		}

		BattleAnimCmds[0xC0] = BattleAnimCmd:subclass(Return, {cmd=0xC0})

		BattleAnimCmds[0xC1] = BattleAnimCmd:subclass{
			cmd = 0xC1,
			argtypes = {uint8_t, uint8_t},
			argnames = {'speed', 'addrOfs'},
			__tostring = function(self)
				return 'moveAlongVecInArc('..self.speed..', '..Cmd.getGotoStr(self.addr + 2 - self.addrOfs, '')..')'
			end,
		}
		BattleAnimCmds[0xc2] = BattleAnimCmd:subclass{
			cmd = 0xc2,
			argtypes = {uint8_t},
			argnames = {'layer'},	-- abc_____
			desc = 'unpauseLayer(<?=layer?>)',
		}
		BattleAnimCmds[0xc3] = BattleAnimCmd:subclass{
			cmd = 0xc3,
			desc = 'moveCircleToTarget()',
		}
		BattleAnimCmds[0xC4] = BattleAnimCmd:subclass{
			cmd = 0xC4,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'moveThreadToThisThreadPos('
					..(0 ~= bit.band(self.arg, 0x80) and '1' or '')	-- BG1
					..(0 ~= bit.band(self.arg, 0x40) and '3' or '')	-- BG3
					..')'
			end,
		}
		BattleAnimCmds[0xC5] = BattleAnimCmd:subclass{
			cmd = 0xC5,
			argtypes = {uint16_t, uint16_t, uint16_t, uint16_t},
			argnames = {'a', 'b', 'c', 'd'},
			getargs = function(self, ...)
				self.destAddrs = table{...}:mapi(function(ofs)
					return ofs + battleAnimScriptAddr
				end)
			end,
			__tostring = function(self)
				return 'gotoForSwdtechHit('
					..self.destAddrs:mapi(function(addr)
						return Cmd.getGotoStr(addr, '')
					end):concat', '..')'
			end,
			getBranchAddrs = function(self)
				return self.destAddrs:mapi(function(addr)
					return {addr=addr}
				end)
			end,
		}
		BattleAnimCmds[0xC6] = BattleAnimCmd:subclass{
			cmd = 0xC6,
			argtypes = {uint8_t, uint8_t},
			argnames = {'xx', 'yy'},
			desc = 'quadraSlamSlice()',
		}
		BattleAnimCmds[0xC7] = BattleAnimCmd:subclass{
			cmd = 0xC7,
			digest = function(self, read)
				local subcmd = read(uint8_t)
				if subcmd == 0x00 then
					local arg = read(uint8_t)
					self.descstr = 'setAttackingCharacterDirectionToFace()'
						..(arg == 0 and 'left' or 'right')
				elseif subcmd == 0x01 then
					self.descstr = 'resetPositionOffsetsForAttackingCharacter()'
				elseif subcmd == 0x02 then
					self.descstr = 'saveAttackingCharacterPosition()'
				elseif subcmd == 0x03 then
					self.descstr = 'restoreAttackerPosAndResetOffsets()'
				elseif subcmd == 0x04 then
					self.descstr = 'restoreAttackerPos()'
				elseif subcmd == 0x05 then
					local arg = read(uint8_t)
					error'here'
					self.descstr = '(unused)'
				elseif subcmd == 0x06 then
					local arg, arg2 = read(uint8_t), read(uint8_t)
					self.descstr = ''
				elseif subcmd == 0x07 then
					self.descstr = 'updateCharacterActionBasedOnVectorDirectionWalking()'
				elseif subcmd == 0x08 then
					local x, y = read(uint8_t), read(uint8_t)
					self.descstr = 'setVectorTargetFromAttacker('..hex2(x)..','..hex2(y)..')'
				elseif subcmd == 0x09 then
					self.descstr = 'updateCharacterActionBasedOnVectorDirectionArmsUp()'
				elseif subcmd == 0x0A then
					local xx = read(uint8_t)
					error'here'
					self.descstr = '(unused)'
				elseif subcmd == 0x0B then
					local x, y, z = read(uint8_t), read(uint8_t), read(uint8_t)
					self.descstr = 'spc('
						..table{hex2(x), hex2(y), hex2(z)}:concat', '..')'
				elseif subcmd == 0x0C then
					local actor, graphicsIndex = read(uint8_t), read(uint8_t)
					self.descstr = 'setActorGraphicIndex('..hex2(actor)..', '..hex2(graphicsIndex)..')'
				elseif subcmd == 0x0D then
					local arg = read(uint8_t)
					self.descstr = ''
				elseif subcmd == 0x0E then
					local arg = read(uint8_t)
					self.descstr = 'screenShaking( '..hex2(arg)..')'	-- $6285
				elseif subcmd == 0x0F then
					error'here'
					self.descstr = '(unused)'
				elseif subcmd == 0x10 then
					local xx = read(uint8_t)
					self.descstr = ''
				elseif subcmd == 0x11 then
					self.descstr = 'disable run from battle'
				else
					self.descstr = '!!! unknown subcmd'
				end
			end,
		}
		BattleAnimCmds[0xC8] = BattleAnimCmd:subclass{
			cmd = 0xC8,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc = 'attackerAction(<?=arg?>)',
		}
		BattleAnimCmds[0xC9] = BattleAnimCmd:subclass{
			cmd = 0xC9,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return self.arg == 0
					and 'playSoundDefault()'
					or 'playSound('..hex2(self.arg)..')'
			end,
		}

		BattleAnimCmds[0xca] = BattleAnimCmd:subclass{
			cmd = 0xca,
			desc = 'doubleActionSpeed()',
		}

		BattleAnimCmds[0xCB] = BattleAnimCmd:subclass{
			cmd = 0xCB,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc = 'enableEchoSprites(<?=arg?>)'
		}

		for _,info in ipairs{
			{pal='Sprite1', cmds={0xb1, 0xb2, 0xb7, 0xb8}},
			{pal='Sprite2', cmds={0xcc, 0xcd, 0xce, 0xcf}},
			{pal='Sprite3', cmds={0xaa, 0xab, 0xb3, 0xb4}},
			{pal='Char', cmds={0xfb, 0xfc, 0xfd, 0xfe}},
			{pal='Background', cmds={0xaf, 0xb0, 0xb5, 0xb6}},
			{pal='Monster', cmds={0xb9, 0xba, 0xbb, 0xbc}},
		} do
			local pal = info.pal
			local cmd1, cmd2, cmd3, cmd4 = table.unpack(info.cmds)
			BattleAnimCmds[cmd1] = BattleAnimCmd:subclass{
				cmd = cmd1,
				argtypes = {uint8_t},
				argnames = {'arg'},
				__tostring = function(self)
					local arg = self.arg
					return 'change'..pal..'PaletteSubAbs{'
						..'intensity='..hex2(bit.band(0x1f, arg))
						..', red='..tostring(0 ~= bit.band(0x80, arg))
						..', green='..tostring(0 ~= bit.band(0x40, arg))
						..', blue='..tostring(0 ~= bit.band(0x20, arg))
						..'}'
				end,
			}
			BattleAnimCmds[cmd2] = BattleAnimCmd:subclass{
				cmd = cmd2,
				argtypes = {uint8_t},
				argnames = {'arg'},
				__tostring = function(self)
					local arg = self.arg
					return 'change'..pal..'PaletteAddAbs{'
						..'intensity='..hex2(bit.band(0x1f, arg))
						..', red='..tostring(0 ~= bit.band(0x80, arg))
						..', green='..tostring(0 ~= bit.band(0x40, arg))
						..', blue='..tostring(0 ~= bit.band(0x20, arg))
						..'}'
				end,
			}

			-- why is ce vs cf add/sub when their func is add/sub?
			BattleAnimCmds[cmd3] = BattleAnimCmd:subclass{
				cmd = cmd3,
				argtypes = {uint8_t},
				argnames = {'arg'},
				__tostring = function(self)
					local arg = self.arg
					return 'change'..pal..'PaletteAdd{'
						.."func='"..(0 ~= bit.band(0x10, arg) and ' sub' or ' add')..'"'
						..', intensity='..hex2(bit.band(0xf, arg))
						..', red='..tostring(0 ~= bit.band(0x80, arg))
						..', green='..tostring(0 ~= bit.band(0x40, arg))
						..', blue='..tostring(0 ~= bit.band(0x20, arg))
						..'}'
				end,
			}
			BattleAnimCmds[cmd4] = BattleAnimCmd:subclass{
				cmd = cmd4,
				argtypes = {uint8_t},
				argnames = {'arg'},
				__tostring = function(self)
					local arg = self.arg
					return 'change'..pal..'PaletteSub{'
						.." func='"..(0 ~= bit.band(0x10, arg) and ' sub' or ' add').."'"
						..', intensity='..hex2(bit.band(0xf, arg))
						..', red='..tostring(0 ~= bit.band(0x80, arg))
						..', green='..tostring(0 ~= bit.band(0x40, arg))
						..', blue='..tostring(0 ~= bit.band(0x20, arg))
						..'}'
				end,
			}
		end

		BattleAnimCmds[0xd0] = BattleAnimCmd:subclass{
			cmd = 0xd0,
			argtypes = {uint8_t},
			argnames = {'arg'},	-- vhftpppm
			desc = 'setAllObjsSpriteData(<?=arg?>)',
		}
		BattleAnimCmds.SetFixedDrawOrder = BattleAnimCmd:subclass{
			cmd = 0xd1,
			argtypes = {uint8_t},
			argnames = {'arg'},	-- 0 = normal, 1 = fixed
			desc = 'setFixedDrawOrder(<?=arg?>)',
		}
		BattleAnimCmds[0xD2] = BattleAnimCmd:subclass{
			cmd = 0xD2,
			argtypes = {uint8_t, uint8_t},
			argnames = {'x', 'y'},
			__tostring = function(self)
				return 'setTargetPos('..self.x..', '..self.y..')'
			end,
		}
		BattleAnimCmds[0xD3] = BattleAnimCmd:subclass{
			cmd = 0xD3,
			desc = 'moveCircleToAttackingCharacter()',
		}
		BattleAnimCmds[0xD4] = BattleAnimCmd:subclass{
			cmd = 0xD4,
			argtypes = {uint16_t, uint8_t},
			argnames = {'colors', 'layers'},
			desc = 'setBlend(<?=colors?>, <?=layers?>)',
		}
		BattleAnimCmds[0xD5] = BattleAnimCmd:subclass{
			cmd = 0xD5,
			argtypes = {uint8_t},
			argnames = {'arg'},
			__tostring = function(self)
				return 'set monster'
					..(0 ~= bit.band(1, self.arg) and ' hflip' or '')
					..(0 ~= bit.band(2, self.arg) and ' vflip' or '')
			end,
		}

		BattleAnimCmds.ScrollBG = BattleAnimCmd:subclass{
			cmd = 0xd6,
			argtypes = {int8_t, int8_t},
			argnames = {'x', 'y'},
			desc = 'scrollBG(<?=x?>, <?=y?>)',
		}

		BattleAnimCmds[0xD7] = BattleAnimCmd:subclass{
			cmd = 0xD7,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc ='fireDanceUpdate(<?=arg?>',
		}
		BattleAnimCmds[0xD8] = BattleAnimCmd:subclass{
			cmd = 0xD8,
			argtypes = {uint8_t, uint8_t, uint8_t},
			argnames = {'x', 'y', 'destOfs'},
			__tostring = function(self)
				return 'bumRushUpdate('..self.x..', '..self.y..', '..Cmd.getGotoStr(self.addr + 4 + self.destOfs, '')..')'
			end,
		}
		BattleAnimCmds[0xD9] = BattleAnimCmd:subclass{
			cmd = 0xD9,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc = 'attackerPriority(<?=arg?>)',
		}
		BattleAnimCmds[0xDA] = BattleAnimCmd:subclass{
			cmd = 0xDA,
			argtypes = {uint16_t},
			argnames = {'arg'},
			desc = 'tornadoUpdate(<?=arg?>)',
		}
		BattleAnimCmds[0xDB] = BattleAnimCmd:subclass{
			cmd = 0xDB,
			argtypes = {uint8_t},
			getargs = function(self, ofs)
				self.destAddr = self.addr + ofs + 2
			end,
			__tostring = function(self)
				local arg = self.arg
				return 'if '..self:getCond()
					..' then '..Cmd.getGotoStr(self.destAddr)
					..' end'
			end,
			getDestAddr = function(self)
				return self.destAddr
			end,
			getBranchAddrs = function(self)
				return {
					{addr=self.destAddr},
				}
			end,
			getCond = function(self)
				return 'currentEntHasSteppedForwardToAttack()'
			end,
		}
		BattleAnimCmds[0xDC] = BattleAnimCmd:subclass{
			cmd = 0xDC,
			desc = 'rotate triangle 2D',
		}
		BattleAnimCmds[0xDD] = BattleAnimCmd:subclass{
			cmd = 0xDD,
			argtypes = {uint8_t, uint8_t, uint8_t, uint8_t},
			--argnames = {'xx', 'yy', 'dd', 'rr'},
			__tostring = function(self)
				return 'initTriangle('..self.args:mapi(tostring):concat', '..')'
			end,
		}
		BattleAnimCmds[0xDE] = BattleAnimCmd:subclass{
			cmd = 0xDE,
			desc = 'moveTriangleToAttackerPos()',
		}
		BattleAnimCmds[0xDF] = BattleAnimCmd:subclass{
			cmd = 0xDF,
			desc = 'moveTriangleToTargetPosition()',
		}
		BattleAnimCmds[0xE0] = BattleAnimCmd:subclass{
			cmd = 0xE0,
			argtypes = {uint8_t, uint8_t, uint8_t, uint8_t},
			--argnames = {'xx', 'yy', 'dd', 'rr'},
			__tostring = function(self)
				return 'modifyTriangle('..self.args:mapi(tostring):concat', '..')'
			end,
		}
		BattleAnimCmds[0xe1] = BattleAnimCmd:subclass{
			cmd = 0xe1,
			argtypes = {uint8_t},
			argnames = {'arg'},
			desc = 'attackerSetVisible(<?=arg?>)',
		}

		BattleAnimCmds.MoveVecMonsterFwd = BattleAnimCmd:subclass{
			cmd = 0xe2,
			argtypes = {uint8_t, uint8_t},
			argnames = {'a', 'b'},
			desc = 'moveVecMonsterFwd(<?=a?>, <?=b?>)',
		}

		BattleAnimCmds.MoveVecMonsterBack = BattleAnimCmd:subclass{
			cmd = 0xe3,
			argtypes = {uint8_t, uint8_t},
			argnames = {'a', 'b'},
			desc = 'moveVecMonsterBack(<?=a?>, <?=b?>)',
		}

		BattleAnimCmds.CalcVelJump = BattleAnimCmd:subclass{
			cmd = 0xe4,
			desc = 'calcVelJump()',
		}

		BattleAnimCmds[0xE5] = BattleAnimCmd:subclass{
			cmd = 0xE5,
			argtypes = {uint8_t, uint8_t, uint8_t},
			argnames = {'x', 'addrOfs', 'y'},
			__tostring = function(self)
				return 'moveVecCharAndGoto('
					..self.x..', '
					..self.y..', '
					..Cmd.getGotoStr(self.addr + 2 - self.addrOfs, '')
					..')'
			end,
		}
		BattleAnimCmds[0xE6] = BattleAnimCmd:subclass{
			cmd = 0xE6,
			argtypes = {uint8_t, uint8_t, uint8_t},
			argnames = {'xx', 'yy', 'zz'},
			__tostring = function(self)
				local yy = self.yy
				return 'branch to '..getaddr(self.trace.pc - yy)..' (-'..hex2(yy)..')'
			end,
		}
		BattleAnimCmds[0xE7] = BattleAnimCmd:subclass{
			cmd = 0xE7,
			desc = 'calcVecFromAttackerToTarget()',
		}
		BattleAnimCmds[0xe8] = BattleAnimCmd:subclass{
			cmd = 0xe8,
			argtypes = {uint8_t, int8_t},
			argnames = {'r', 'theta'},
			desc = 'polarMove(<?=r?>, <?=theta?>)',
		}
		BattleAnimCmds[0xe9] = BattleAnimCmd:subclass{
			cmd = 0xe9,
			argtypes = {uint8_t, uint8_t},
			argnames = {'dx', 'dy'},
			__tostring = function(self)
				-- TODO should this be 0..dx or 0..(dx-1) ?
				return 'moveRandomly('..self.dx..', '..self.dy..')'
			end,
		}
		BattleAnimCmds[0xEA] = BattleAnimCmd:subclass{
			cmd = 0xEA,
			argtypes = {uint8_t},
			argnames = {'arg'},	-- _13__xxxx
			desc = 'setBGScreenPos(<?=arg?>)',
		}
		BattleAnimCmds.GotoForThread = BattleAnimCmd:subclass{
			cmd = 0xEB,
			digest = function(self, read)

				-- TODO this is gonna vary
				local numThreads = 4
				if self.addr == 0x100f5f then
					numThreads = 8
				elseif self.addr == 0x10139d then
					numThreads = 3
				elseif self.addr == 0x10141f then
					numThreads = 3
				elseif self.addr == 0x10147b then
					numThreads = 6
				elseif self.addr == 0x1018cd then
					numThreads = 3
				elseif self.addr == 0x101c21 then
					numThreads = 2
				elseif self.addr == 0x101d78 then
					numThreads = 6
				elseif self.addr == 0x101e89 then
					numThreads = 6
				elseif self.addr == 0x101f5c then
					numThreads = 6
				elseif self.addr == 0x101fc1 then
					numThreads = 6
				elseif self.addr == 0x102162 then
					numThreads = 6
				elseif self.addr == 0x102438 then
					numThreads = 3
				elseif self.addr == 0x10285b then
					numThreads = 6
				elseif self.addr == 0x102e73 then
					numThreads = 2
				elseif self.addr == 0x103468 then
					numThreads = 6
				elseif self.addr == 0x1034c0 then
					numThreads = 6
				elseif self.addr == 0x1035b4 then
					numThreads = 2
				elseif self.addr == 0x103670 then
					numThreads = 3
				elseif self.addr == 0x10390d then
					numThreads = 6
				elseif self.addr == 0x103bfc then
					numThreads = 6
				elseif self.addr == 0x103d5f then
					numThreads = 6
				elseif self.addr == 0x103e53 then
					numThreads = 3
				elseif self.addr == 0x10422d then
					numThreads = 3
				elseif self.addr == 0x10426c then
					numThreads = 3
				elseif self.addr == 0x10441f then
					numThreads = 3
				elseif self.addr == 0x10452e then
					numThreads = 3
				elseif self.addr == 0x10456b then
					numThreads = 8
				elseif self.addr == 0x104767 then
					numThreads = 8
				elseif self.addr == 0x104fc6 then
					numThreads = 3
				elseif self.addr == 0x105141 then
					numThreads = 6
				elseif self.addr == 0x10571e then
					numThreads = 8
				elseif self.addr == 0x105873 then
					numThreads = 3
				elseif self.addr == 0x1058f8 then
					numThreads = 8
				elseif self.addr == 0x105b42 then
					numThreads = 6
				elseif self.addr == 0x105d51 then
					numThreads = 3
				elseif self.addr == 0x105d73 then
					numThreads = 6
				elseif self.addr == 0x105f25 then
					numThreads = 8
				elseif self.addr == 0x10625a then
					numThreads = 6
				elseif self.addr == 0x1062a4 then
					numThreads = 6
				elseif self.addr == 0x10632f then
					numThreads = 6
				elseif self.addr == 0x1063a6 then
					numThreads = 2
				elseif self.addr == 0x106485 then
					numThreads = 6
				elseif self.addr == 0x1064d2 then
					numThreads = 6
				end

				self.destAddrs = table()
				for i=1,numThreads do
					self.destAddrs:insert(read(uint16_t) + battleAnimScriptAddr)
				end
			end,
			__tostring = function(self)
				-- TODO count as many as threads ... how to determine # of threads?
				return 'gotoForThread('
					..self.destAddrs:mapi(function(addr)
						return game.addrLabel(addr)
					end):concat', '
					..')'
			end,
			getBranchAddrs = function(self)
				return self.destAddrs:mapi(function(addr)
					return {addr = addr}
				end)
			end,
		}
		BattleAnimCmds[0xEC] = BattleAnimCmd:subclass{
			cmd = 0xEC,
			argtypes = {uint8_t},
			argnames = {'xx'},
			__tostring = function(self)
				return 'set thread layer (0 = sprite, 1 = BG1, 2 = BG3)'
			end,
		}
		BattleAnimCmds[0xED] = BattleAnimCmd:subclass{
			cmd = 0xED,
			argtypes = {uint8_t, uint8_t},
			argnames = {'arg', 'destOfs'},
			desc = 'moveVecGravBomb(<?=arg?>, <?=destOfs?>)',
		}
		BattleAnimCmds[0xEE] = BattleAnimCmd:subclass{
			cmd = 0xEE,
			argtypes = {uint8_t},
			argnames = {'__oo____'},
			__tostring = function(self)
				return 'set target sprite tile priority'
			end,
		}
		BattleAnimCmds[0xEF] = BattleAnimCmd:subclass{
			cmd = 0xEF,
			argtypes = {uint8_t, uint8_t},
			argnames = {'rr', 'tt'},
			__tostring = function(self)
				local rr, tt = self.rr, self.tt
				return 'move to polar coordinates r='..hex2(rr)..' theta='..hex2(tt)..' (similar to $E8)'
			end,
		}
		BattleAnimCmds[0xF0] = BattleAnimCmd:subclass{
			cmd = 0xF0,
			argtypes = {uint16_t, uint16_t, uint16_t, uint16_t, uint16_t},
			argnames = {'a', 'b', 'c', 'd', 'e'},
			__tostring = function(self)
				local a,b,c,d,e = self.a, self.b, self.c, self.d, self.e
				return 'jump based on current target'
					..' char1='..getaddr(a)
					..' char2='..getaddr(b)
					..' char3='..getaddr(c)
					..' char4='..getaddr(d)
					..' monster='..getaddr(e)
			end,
		}

		BattleAnimCmds.HideTargetMonsters = BattleAnimCmd:subclass{
			cmd = 0xf1,
			argtypes = {uint8_t},
			argnames = {'x'},
			desc = 'hideTargetMonsters(<?=x?>)',
		}

		BattleAnimCmds[0xF2] = BattleAnimCmd:subclass{
			cmd = 0xF2,
			desc = 'set a trajectory from target center to attacker',
		}
		BattleAnimCmds[0xF3] = BattleAnimCmd:subclass{
			cmd = 0xF3,
			argtypes = {uint16_t, uint16_t, uint16_t, uint16_t, uint16_t},
			argnames = {'a', 'b', 'c', 'd', 'e'},
			__tostring = function(self)
				local a,b,c,d,e = self.a, self.b, self.c, self.d, self.e
				return 'jump based on current attacker'
					..' char1='..getaddr(a)
					..' char2='..getaddr(b)
					..' char3='..getaddr(c)
					..' char4='..getaddr(d)
					..' monster='..getaddr(e)
			end,
		}
		BattleAnimCmds[0xF4] = BattleAnimCmd:subclass{
			cmd = 0xF4,
			argtypes = {uint8_t},
			argnames = {'x'},	-- _______t
			desc = 'setSpriteLayerPriority<?=x?>',
		}
		BattleAnimCmds[0xF5] = BattleAnimCmd:subclass{
			cmd = 0xF5,
			desc = 'until no threads are active',
		}
		BattleAnimCmds[0xF6] = BattleAnimCmd:subclass{
			cmd = 0xF6,
			desc ='rotate triangle 3D',
		}
		BattleAnimCmds[0xF7] = BattleAnimCmd:subclass{
			cmd = 0xF7,
			argtypes = {uint8_t},
			argnames = {'y'},
			desc = 'waitForScanline(<?=y?>)',
		}

		-- TODO use If
		BattleAnimCmds[0xF8] = BattleAnimCmd:subclass{
			cmd = 0xF8,
			argtypes = {uint16_t, uint16_t},
			getargs = function(self, gotoAddr, gotoElseAddr)
				self.gotoAddr = gotoAddr + battleAnimScriptAddr
				self.gotoElseAddr = gotoElseAddr + battleAnimScriptAddr
			end,
			__tostring = function(self)
				return 'if '..self:getCond()
					..' then '..Cmd.getGotoStr(self.gotoAddr)
					..' else '..Cmd.getGotoStr(self.gotoElseAddr)
					..' end'
			end,
			getCond = function(self)
				return 'not magitekMode()'
			end,
			getBranchAddrs = function(self)
				return {
					{addr=self.gotoAddr},
					{addr=self.gotoElseAddr},
				}
			end,
		}

		BattleAnimCmds[0xf9] = BattleAnimCmd:subclass{
			cmd = 0xf9,
			argtypes = {uint8_t, uint8_t, uint8_t},
			argnames = {'x', 'y', 'z'},
			desc ='magitekAction(<?=x?>, <?=y?>, <?=z?>)',
		}

		BattleAnimCmds.Goto = BattleAnimCmd:subclass{
			cmd = 0xFA,
			argtypes = {uint16_t},
			getargs = function(self, gotoAddr)
				self.gotoAddr = gotoAddr + battleAnimScriptAddr
			end,
			__tostring = function(self)
				return Cmd.getGotoStr(self.gotoAddr)
			end,
			getBranchAddrs = function(self)
				return {
					-- this will always use event-cmds, whereas typical getBranchAddrs for branch/goto/call will preserve cmdset
					{addr = self.gotoAddr},
				}
			end,
		}

		BattleAnimCmds.EndScript = BattleAnimCmd:subclass(EndScript, {cmd=0xff})

		-- used for the first two bytes of every BattleCmd
		BattleAnimCmds.SpeedAndAlign = BattleAnimCmd:subclass{
			argtypes = {uint8_t, uint8_t},
			argnames = {'speed', 'align'},
			desc = 'setSpeedAndAlign(<?=speed?>, <?=align?>)',
		}

		for _,k in ipairs(table.keys(BattleAnimCmds)) do
			local cl = BattleAnimCmds[k]
			if cl.cmd then	-- some abstract classes are in BattleAnimCmds but don't have a .cmd
				assert.type(cl.cmd, 'number')
cl.classname = k
				BattleAnimCmds[cl.cmd] = cl
			end
		end
		for i=0,255 do
			if not BattleAnimCmds[i] then
				BattleAnimCmds[i] = BattleAnimCmd:subclass{
					cmd = i,
					desc = '??? '..hex2(i),
				}
			end
		end
	end


	for _,cmdset in ipairs{
		'EventCmds',
		'ObjectCmds',
		'WorldCmds',
		'VehicleCmds',
		'BattleEventCmds',
		'MonsterCmds',
		'BattleAnimCmds',
	} do
		local cmdSetObj = game[cmdset]
		for _,cl in pairs(cmdSetObj) do
			-- why not .cmdset?
			-- the obj stores .cmdset dynamically
			-- but it should always match the cmd class's cmdset, which is this, right?
			-- right?
			cl.classCmdSet = cmdset
		end
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
	game.whatsPointingToAddr = {}

	-- remember where we've started from so we don't go over the same area twice
	-- TODO this but for the state of decoding as well
	local decompileTraces = {}

	local Trace = class()

	Trace.indent = 0

	function Trace:init(args)
		self.addr = assert.index(args, 'addr')

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
		game.whatsPointingToAddr[self.addr] = game.whatsPointingToAddr[self.addr] or table()
		game.whatsPointingToAddr[self.addr]:insert(reverseRefInfo)

		self.cmds = table()
		self.cmdObjForAddr = {}	-- keys are cmds[i].addr

		-- optional?
		self.lastDialogPromptCount = args.lastDialogPromptCount

		local cmdset = assert.index(args, 'cmdset')
--DEBUG:assert.type(cmdset, 'string')

		-- force override addrs here
		if self.addr == 0x0a5eb4 then
			cmdset = 'EventCmds'	-- end-script used by event and world cmds
		end

		-- not sure about this
		self.stateStack = table()
		self.stateStack:insert{
			cmdset = cmdset,
		}

		if args.inVehicle then
			self.stateStack:insert{
				cmdset = 'VehicleCmds',
			}
		end

		self.newBranches = table()

		self.pc = self.addr
	end

	function Trace:readAndInc(ctype)
		ctype = ffi.typeof(ctype)
		local ptrtype = ffi.typeof('$*', ctype)

		local o = ffi.cast(ptrtype, rom + self.pc)[0]
		self.pc = self.pc + ffi.sizeof(ctype)

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

		return o
	end

	function Trace:readNext()
		local cmdaddr = self.pc
		local cmd = self:readAndInc(uint8_t)

--DEBUG:assert.gt(#self.stateStack, 0, "someone popped the last cmdset...")
		local cmdset = self.stateStack:last().cmdset
--DEBUG:assert.type(cmdset, 'string')

		return self:readInstr(cmdaddr, cmdset, cmd)
	end

	-- cmdaddr has to be abstracted because typical cmds addrs are pc-1 from args, for reading the type of cmd
	-- but some hard-coded cmds for addrs dont have the extra byte and so they have to use pc itself
	function Trace:readInstr(cmdaddr, cmdset, cmd)
		local cl = game[cmdset][cmd]
		if not cl then error("failed to find cmd "..tolua(cmd).." in cmdset "..tolua(cmdset)) end

		local cmdobj = cl()
		cmdobj.trace = self
		cmdobj.addr = cmdaddr
		cmdobj.cmdset = cmdset
		cmdobj.indent = self.indent

		-- self:less function to pass to cmd digest()
		local function read(ctype)
			local o
			o = self:readAndInc(ctype)
			return o
		end

		-- hmm instead of just 'read' with 'addr', how about a whole interpretation-state, with 'cmdset' too?
		cmdobj:digest(read)

		-- track all bytes used here, in case someone cares?
		cmdobj.sizeInBytes = self.pc - cmdaddr

		game.eventScriptCmds:insert(cmdobj)
		self.cmds:insert(cmdobj)
		self.cmdObjForAddr[cmdaddr] = cmdobj

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
				newBranch.cmdset = newBranch.cmdset or self.stateStack[1].cmdset		-- make sure we record the current cmdset
				newBranch.inVehicle = self.stateStack:last().cmdset == 'VehicleCmds'	-- right now self.stateStack is just 1 or 2 in size, and 2 is always VehicleCmds, and 1 is always not...
				newBranch.lastDialogPromptCount = self.lastDialogPromptCount
				newBranch.reverseRefInfo = {
					cmdobj = cmdobj,
					branchFromAddr = cmdaddr,
					cmdset = newBranch.cmdset,
				}
				self.newBranches:insert(newBranch)
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
			[0x0af4c7] = 'EventCmds',	-- $0af4c7	WO b0 76 01 b4 5e 00       if not mapFlagGet(374) then goto $0a5eb4 end
			[0x0af4dd] = 'EventCmds',	-- $0af4dd	WO b0 76 01 b4 5e 00       if not mapFlagGet(374) then goto $0a5eb4 end
			[0x0a8ca8] = 'EventCmds',	-- $0a8ca8	VE b0 27 01 e3 8b 00       if not mapFlagGet(295) then goto $0a8be3 end
		})[cmdaddr]
		if newCmdSet then
			for i=2,#self.stateStack do self.stateStack[i] = nil end	-- clear VehicleCmds too
			self.stateStack[1].cmdset = newCmdSet
		end

		return cmdobj
	end

	function Trace:run()
		-- make sure we haven't decoded this already
		for _,otherTrace in pairs(decompileTraces) do
			local cmdobj = otherTrace.cmdObjForAddr[self.addr]
			if cmdobj then
				if cmdobj.cmdset ~= self.stateStack:last().cmdset then
					print('!!! DANGER !!! '
						..game.addrLabel(self.addr)
						..' decoding address from differing cmdset!'
						..' previous cmdset '..cmdobj.cmdset
						..' from trace at '..game.addrLabel(otherTrace.cmds[1].addr)
						..' vs new cmdset '..self.stateStack:last().cmdset
					)
				else
					return otherTrace
				end
			end
		end

		decompileTraces[self.addr] = self

--[==[ debugging:
print(('BEGIN '..game.addrLabel(self.addr))
--]==]

		while true do
			if not (
				(scriptBaseAddr <= self.pc and self.pc < scriptBaseAddrEnd)
				or (scriptBaseAddr2 <= self.pc and self.pc < scriptBaseAddrEnd2)
				or (battleEventScriptAddr <= self.pc and self.pc < battleEventScriptAddrEnd)
				or (monsterScriptAddr <= self.pc and self.pc < monsterScriptAddrEnd)
				or (battleAnimScriptAddr <= self.pc and self.pc < battleAnimScriptAddrEnd)
			) then
print('!!! script oob !!! '..game.addrLabel(self.pc))
				break
			end

			local cmdobj = self:readNext()

			if cmdobj.endTrace then break end
		end

		-- set this before trying recursive, so they know this trace's address interval
		self.endAddr = self.pc

--[==[ debugging:
print('END '..game.addrLabel(self.endAddr))
print()
--]==]

		assert.ge(#self.stateStack, 1, "somehow we popped all our states...")

		if self.stateStack:last().objectScriptCmd then
			print('!!! DANGER !!! event-script ended still inside an object-script:',
				'script start = '..game.addrLabel(self.addr),
				'object-script start = '..game.addrLabel(self.stateStack:last().objectScriptCmd.addr)
			)
		end

		for _,newBranch in ipairs(self.newBranches) do
			Trace(newBranch):run()
		end

		return self
	end

	function Trace:printInterval()
		return game.addrLabel(self.addr)..' - '..game.addrLabel(self.endAddr)
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

				-- BlackjackManual starts as 'inVehicle':
				-- needed especially for scripts that branch into this address
				local inVehicle
				if startAddr == 0x0aa6c0 then
					inVehicle = true
				end

				return Trace{
					addr = startAddr,
					cmdset = startCmdSet,
					inVehicle = inVehicle,
					reverseRefInfo = reverseRefInfo,
				}:run()
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
		[0x0a0000] = "NoEvent",
		[0x0a0001] = "WaitForDialogWindow",
		[0x0a0003] = "GameStart",
		[0x0a0008] = "Chest_Item",
		[0x0a000c] = "Chest_Spell",
		[0x0a0010] = "Chest_GP",
		[0x0a0014] = "Chest_Empty",
		[0x0a0018] = "RandomBattle",
		[0x0a0034] = "Tent",
		[0x0a0039] = "Warp",
		[0x0a0040] = "Chest_Monster",
		[0x0a009d] = "DoomGaze",
		[0x0a00ea] = "TentAnimation",
		[0x0a0108] = "WarpAnimation",
		[0x0a015e] = "TentAnimationWorld",
		[0x0a01a2] = "KefkasTower",
		[0x0a0405] = "PhoenixCave",
		[0x0a057d] = "FinalBattleEnding",
		[0x0a5ade] = "FloatingIslandCinematic",
		[0x0a5e33] = "NewGame",
		[0x0a5ea9] = "PostBattle",
		[0x0a5eb3] = "Return",
		[0x0a5eb4] = "ReturnWorld",
		[0x0ac6ac] = "SetPartyForCharFlag",
		[0x0ac766] = "UpdateAndShowParty",
		[0x0aca64] = "SetCharFlagForDir",
		[0x0aca8d] = "MoveToFaceUpAtNPC",
		[0x0acb95] = "PartyHideAllExceptLeaderAndGiveUserControl",
		[0x0acd31] = "InnNoDream",
		[0x0acd3c] = "InnNormal",
		[0x0acd5b] = "InnDream1",
		[0x0acdd9] = "InnDream2",
		[0x0ace51] = "InnDream3",
		[0x0acefe] = "InnDream4",
		[0x0acf67] = "InnFadeOut",
		[0x0acf96] = "InnFadeIn",
		[0x0b2e2b] = "PartySetSolid",
		[0x0b2e34] = "PartySetNotSolid",
		[0x0b69ff] = "NotEnoughGP",
		[0x0c36a6] = "MagiciteGhost",
		[0x0c985b] = "Prologue",
		[0x0c9a4f] = "StartNarsheIntro",
		[0x0c9aeb] = "SavePoint",
		[0x0ce499] = "RestoreParty",
		[0x0ce566] = "GameOver",
		[0x0cb1e7] = "NarsheOpenSecretCave",
	} do
		Trace{
			addr = addr,
			cmdset = 'EventCmds',
			reverseRefInfo = {builtin = name},
		}:run()
	end
	for addr,name in pairs{
		[0x0a004f] = 'WorldTent',
		[0x0a0059] = 'BlackjackGround',
		[0x0a0068] = 'AirshipDeck',
		[0x0a0078] = "FalconDeck",
		[0x0a0088] = "EnterPhoenixCave",
		[0x0a008f] = "EnterGogosLair",
		[0x0a0096] = "DoomGazeDefeated",
	} do
		Trace{
			addr = addr,
			cmdset = 'WorldCmds',
			reverseRefInfo = {builtin = name},
		}:run()
	end
	for addr,name in pairs{
		[0x0a007f] = "enter kefka's tower",
		[0x0aa6c0] = "BlackjackManual",
	} do
		Trace{
			addr = addr,
			cmdset = 'EventCmds',
			inVehicle = true,
			reverseRefInfo = {builtin = name},
		}:run()
	end

	-- battle-event scripts here too?
	-- they will have a different base-offset than the other 4 cmdsets
	for i=0,countof(game.battleEventScriptOfs)-1 do
		local addr = battleAnimScriptAddr + game.battleEventScriptOfs[i]
		Trace{
			addr = addr,
			cmdset = 'BattleEventCmds',
			reverseRefInfo = {builtin = 'battleEvent'..i},
		}:run()
	end


	-- monster-scripts too?
	do
		game.monsterScripts = {}

		local monstersForAddr = {}
		local scriptAddrs = {}
		for i=0,game.numMonsters-1 do
			local addr = monsterScriptAddr + game.monsterScriptOfs[i]
			scriptAddrs[addr] = true
			monstersForAddr[addr] = monstersForAddr[addr] or table()
			monstersForAddr[addr]:insert(i)
		end
		scriptAddrs = table.keys(scriptAddrs):sort()

		for i=1,#scriptAddrs do
			local actAddr = scriptAddrs[i]
			local nextAddr = scriptAddrs[i+1] or monsterScriptAddrEnd

			-- one each
			assert.eq(#monstersForAddr[actAddr], 1)
			local monsterIndex = monstersForAddr[actAddr][1]
			--local name = string.trim(tostring(game.monsterNames[monsterIndex]))
			--if name ~= '' then name = ' '..name end
			--print('\t['..monsterIndex..'] = {\t-- '..(' 0x%06x'):format(actAddr)..'-'..('0x%06x'):format(nextAddr)..name)
			local trace = Trace{
				addr = actAddr,
				cmdset = 'MonsterCmds',
				reverseRefInfo = {builtin = 'monsters_'..monsterIndex..'_act'},
			}:run()
			local reactAddr = trace.endAddr
			if reactAddr == nextAddr then
				-- most monsters with no react at least have their react ptr point to a 'return'
				-- but some (monster 243 etc) don't even have this
				reactAddr = nil
			else
				Trace{
					addr = reactAddr,
					cmdset = 'MonsterCmds',
					reverseRefInfo = {builtin = 'monsters_'..monsterIndex..'_react'},
				}:run()
			end

			game.monsterScripts[monsterIndex] = {
				act = actAddr,
				react = reactAddr,
			}
		end
	end
	--]]

	do
		local scriptAddrs = {}
		for i=0,game.countof(game.battleAnimEffects)-1 do
			local addr = battleAnimScriptAddr + game.battleAnimScriptOffsets[i]
			scriptAddrs[addr] = true
		end
		scriptAddrs = table.keys(scriptAddrs):sort()

		for i=1,#scriptAddrs do
			local addr = scriptAddrs[i]

			local trace = Trace{
				addr = addr,
				cmdset = 'BattleAnimCmds',
				reverseRefInfo = {builtin = ('BattleAnim_%06x'):format(addr)},
			}

			trace:readInstr(trace.pc, 'BattleAnimCmds', 'SpeedAndAlign')

			trace:run()

			-- and fix a bug...
			local t = decompileTraces[0x104cc2]
			if t then
				-- this'll inc .pc past .endAddr
				-- and put an instruction in overlapping memory
				-- so don't reassemble later I guess?
				local cmdobj = BattleAnimCmds.EndFor()
				cmdobj.trace = t
				cmdobj.cmdset = 'BattleAnimCmds'
				local lastcmd = t.cmds[#t.cmds]
				cmdobj.indent = lastcmd.indent
				cmdobj.addr = lastcmd.addr - .5	-- very cheap trick
				cmdobj.sizeInBytes = 0
				lastcmd.indent = lastcmd.indent - 1
				game.eventScriptCmds:insert(cmdobj)
				t.cmds:insert(#t.cmds-1, cmdobj)
			end

			-- another bug ... I bet an anim-for-begin should go in the beginning somewhere
			-- but just remove the last unmatched anim-end-for
			if trace.addr == 0x105e49 then
				local cmdobj = trace.cmds:remove(#trace.cmds-1)
				assert.eq(cmdobj.addr, 0x105e5e)
				trace.cmdObjForAddr[cmdobj.addr] = nil
				game.eventScriptCmds:removeObject(cmdobj)
			end
		end
	end

	-- is this a good idea, or should I double-check by storing trace address ranges and make sure none overlap so that i'm not inserting cmds between/ontop one another?
	game.eventScriptCmds:sort(function(a,b) return a.addr < b.addr end)

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

	-- calculate eventScriptCmdIndexForAddr
	-- and remove duplicates
	game.eventScriptCmdIndexForAddr = {}
	do
		local i = 1
		while i <= #game.eventScriptCmds do
			local cmd = game.eventScriptCmds[i]
			if game.eventScriptCmdIndexForAddr[cmd.addr] then
				-- then we have duplicates ....
				assert.lt(game.eventScriptCmdIndexForAddr[cmd.addr], i)
				local ocmd = game.eventScriptCmds[game.eventScriptCmdIndexForAddr[cmd.addr]]
				assert.ne(rawequal(cmd, ocmd))
				game.eventScriptCmds:remove(i)	-- removing cmd, keeping ocmd

				-- also remove duplicate callbacks anywhere that this might have pointed...
				-- [=[
				if cmd.getBranchAddrs then
					for _,branch in ipairs(cmd:getBranchAddrs()) do
						local branchAddr = branch.addr
						assert(game.whatsPointingToAddr[branchAddr])

						-- duplicate equal cmds, so remove only one
						local found
						for j=#game.whatsPointingToAddr[branchAddr],1,-1 do
							if game.whatsPointingToAddr[branchAddr][j].cmdobj == cmd then
								game.whatsPointingToAddr[branchAddr]:remove(j)
							elseif game.whatsPointingToAddr[branchAddr][j].cmdobj == ocmd then
								found = true
							end
						end
						assert(found)
						-- shouldn't be zero because we should have a duplicate entry
						assert.gt(#game.whatsPointingToAddr[branchAddr], 0)
					end
				end
				--]=]
			else
				game.eventScriptCmdIndexForAddr[cmd.addr] = i
				i = i + 1
			end
		end
	end

	-- addr -> cmd
	local cmdForAddr = {}
	for _,cmdobj in ipairs(game.eventScriptCmds) do
		cmdForAddr[cmdobj.addr] = cmdobj
	end

	for _,cmdobj in ipairs(game.eventScriptCmds) do
		if game.ObjectCmds.Branch:isa(cmdobj) then
			local destAddr = cmdobj:getDestAddr()
			if destAddr then
				game.whatsPointingToAddr[destAddr] = game.whatsPointingToAddr[destAddr] or table()
				local destCmdObj = cmdForAddr[destAddr]
				if not destCmdObj then
print("!!! WARNING !!!! branch from "
	..('_%06x'):format(cmdobj.addr)
	..' to unknown location '
	..('_%06x'):format(destAddr))
				end
				game.whatsPointingToAddr[destAddr]:insert{
					cmdobj = cmdobj,
					branchFromAddr = cmdobj.addr,
					cmdset = cmdobj.cmdset,
				}
			end
		end
	end
end
