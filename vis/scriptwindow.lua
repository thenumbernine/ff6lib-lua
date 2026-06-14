local ffi = require 'ffi'
local table = require 'ext.table'
local string = require 'ext.string'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local scriptBaseAddr = 0xa0000

local scriptDivider = {}

local EventScriptWindow = ArrayWindow:subclass()

EventScriptWindow.name = 'event script'

function EventScriptWindow:init(args, ...)
	self.indexStack = table()	-- for back button

	EventScriptWindow.super.init(self, args, ...)
	self.scrollOppositeWorldRow = ffi.new('int[1]', 0x7ffffffff)
	self.availSpace = ig.ImVec2()

	local game = self.app.game

	-- while we're here...
	-- insert into the beginning of each script function a reference back to whoever is calling it
	-- and an empty divider
	for i=#game.eventScriptCmds,1,-1 do
		local cmd = game.eventScriptCmds[i]
		local whatPointsToScriptAdAddr = game.eventScriptAddrs[cmd.addr]
		if whatPointsToScriptAdAddr then
			game.eventScriptCmds:insert(i, {what=whatPointsToScriptAdAddr})
			game.eventScriptCmds:insert(i, scriptDivider)
		end
	end

	-- and now I have to recalculate eventScriptCmdIndexForAddr
	game.eventScriptCmdIndexForAddr = {}
	for i,cmd in ipairs(game.eventScriptCmds) do
		if cmd.addr then
			game.eventScriptCmdIndexForAddr[cmd.addr] = i
		end
	end
end

function EventScriptWindow:getArray()
	return self.app.game.eventScriptCmds
end

-- used for buttons, index is 0-based
function EventScriptWindow:getIndexName(i)
	local cmd = self.app.game.eventScriptCmds[1+i]
	if not cmd.addr then return '' end
	return ('event $%06x'):format(cmd.addr)
end

local igzero = ig.ImVec2(0,0)
function EventScriptWindow:showIndexUI()
	local app = self.app
	local game = app.game

	local ar = self:getArray()

	do
		self.__tmp = ''
		local cmd = self:getIndex(self.index)
		if cmd and cmd.addr then
			self.__tmp = ('%06x'):format(cmd.addr)
		end
		if ig.luatableInputText('addr', self, '__tmp', ig.ImGuiInputTextFlags_EnterReturnsTrue) then
			local addr = tonumber(self.__tmp, 16)
			if addr then
				self:openScriptAddr(addr)
			end
		end
		ig.igSameLine()
		if ig.igButton'<' then
			local prevIndex = self.indexStack:remove()
			self:setIndex(prevIndex)
			self.indexStack:remove()	-- and remove the one we just put on the stack
		end
	end

	-- gahhhh
	--ig.igInputInt('current opposite-world row', self.scrollOppositeWorldRow)

	ig.igGetContentRegionAvail(self.availSpace)
	self.availSpace.x = 18

	--local numRowsVisible = 15
	-- TODO determine by window size, or child panel within window...
	-- igGetContentRegionAvail returns in pixels
	--local rowHeight = ig.igGetTextLineHeight()
	local rowHeight = 24
	local numRowsVisible = math.floor(self.availSpace.y / rowHeight) - 1
	local count = #ar
	local scrollMax = count - numRowsVisible - 1

	ig.igVSliderInt(
		'##scroll',							-- label
		self.availSpace,					-- size
		self.scrollOppositeWorldRow,		-- v
		0,									-- vmin
		scrollMax,							-- vmax
		'',									-- format
		0									-- flags
	)

	ig.igSameLine();
	ig.igBeginChild(
		'ScriptWindowEvents',
		igzero,
		0, --ig.ImGuiChildFlags_None,
		0 --bit.bor(ig.ImGuiWindowFlags_NoScrollbar, ig.ImGuiWindowFlags_NoScrollWithMouse)
	)

	ig.igGetContentRegionAvail(self.availSpace)

	if ig.igBeginTable(
		'ScriptWindowEvents',
		1,
		bit.bor(ig.ImGuiTableFlags_ScrollX, ig.ImGuiTableFlags_RowBg, ig.ImGuiTableFlags_Borders),
		ig.ImVec2(),
		0
	) then

		local wheel = ig.igGetIO().MouseWheel
		if wheel ~= 0 and ig.igIsWindowHovered(0)
		then
			self.scrollOppositeWorldRow[0] = self.scrollOppositeWorldRow[0] + wheel
		end

		if self.jumpRequested then
			self.scrollOppositeWorldRow[0] = scrollMax - self.jumpRequested
			self.jumpRequested = nil
		end

		if self.scrollOppositeWorldRow[0] < 0 then self.scrollOppositeWorldRow[0] = 0 end
		if self.scrollOppositeWorldRow[0] > scrollMax then
			self.scrollOppositeWorldRow[0] = scrollMax
		end

		ig.igTableSetupColumn('-', 0, 0, 0)--ig.ImGuiTableColumnFlags_WidthFixed, self.availSpace.x, 0)

		-- TODO maybe turn this into a table to force its row height...
		for visibleRowIndex = 0,numRowsVisible-1 do
			ig.igTableNextRow(0, rowHeight)
			if ig.igTableSetColumnIndex(0) then

				local i = (scrollMax - self.scrollOppositeWorldRow[0]) + visibleRowIndex
				if i >= 0 and i < count then
					ig.igPushID_Int(i)
					local cmd = ar[1+i]
					if cmd == scriptDivider then
						ig.igSeparator()
					elseif cmd
					and not game.Cmds.Cmd:isa(cmd)	-- i.e. this is just a marker, not for real Cmd's (some have a .what field too...)
					and cmd.what
					then
						for j,what in ipairs(cmd.what) do
							ig.igPushID_Int(j)
							if what.type == 'startEventScriptAddr' then
								ig.igSameLine()
								app.mapWindow:popupButton(what.mapIndex, 'onstart')
							elseif what.npcIndex then
								ig.igSameLine()
								if ig.igButton('map '..what.mapIndex..' npc '..what.npcIndex) then
									app.mapWindow:open(what.mapIndex)
									app.npcWindow:open(what.npcIndex)
									local n = app.npcWindow:getCurIndex()
									if n then
										app:centerView(n.x, n.y)
									end
								end
							elseif what.touchTriggerIndex then
								ig.igSameLine()
								if ig.igButton('map '..what.mapIndex..' touch '..what.touchTriggerIndex) then
									app.mapWindow:setIndex(what.mapIndex)
									app.touchTriggerWindow:open(what.touchTriggerIndex)
									local t = app.touchTriggerWindow:getCurIndex()
									if t then
										app:centerView(t.pos.x, t.pos.y)
									end
								end
							elseif what.branchFromAddr then
								ig.igSameLine()
								if ig.igButton(('branch from $%06x'):format(what.branchFromAddr)) then
									self:openScriptAddr(what.branchFromAddr)
								end
							elseif what.builtin then
								ig.igSameLine()
								ig.igText(tostring(what.builtin))
							else
								ig.igSameLine()
								ig.igText(require 'ext.tolua'(what))
							end
							ig.igPopID()
						end
					elseif cmd then
						ig.igPushID_Int(cmd.addr)

						local color = 0x3f7f7f7f
						if cmd.cmdset == 'EventCmds' then
							color = 0x3f007f00
						elseif cmd.cmdset == 'ObjectCmds' then
							if game.ObjectCmds.EndScript:isa(cmd) then
								color = 0x3f007f00
							else
								color = 0x3f00007f
							end
						elseif cmd.cmdset =='VehicleCmds' then
							color = 0x3f007f7f
						elseif cmd.cmdset == 'WorldCmds' then
							color = 0x3f7f0000
						else
							-- unknown?
						end
						local cursorPosY = ig.igGetCursorPosY()
						local winPos = ig.ImVec2()
						ig.igGetWindowPos(winPos)
						local drawList = ig.igGetWindowDrawList()
						ig.ImDrawList_AddRectFilled(
							drawList,
							ig.ImVec2(winPos.x, winPos.y + cursorPosY),
							ig.ImVec2(winPos.x + self.availSpace.x, winPos.y + cursorPosY + rowHeight),
							color,
							4,		-- rounding
							0		-- flags
						)

						ig.igText(
							(i == self.index and '>' or ' ')
							..('%06x: '):format(cmd.addr)
						)
						ig.igSameLine()

						-- indent object-cmds since they are blocks of cmds from event-cmds
						if cmd.cmdset == 'ObjectCmds'
						and not game.ObjectCmds.EndScript:isa(cmd)
						then
							ig.igText('  ')
							ig.igSameLine()
						end

						-- item links:
						if game.EventCmds.GiveItem:isa(cmd) then
							ig.igText'GiveItem'
							ig.igSameLine()
							app.itemWindow:popupButton(cmd.itemIndex)
						elseif game.EventCmds.TakeItem:isa(cmd) then
							ig.igText'TakeItem'
							ig.igSameLine()
							app.itemWindow:popupButton(cmd.itemIndex)

						-- shops:
						elseif game.EventCmds.OpenShopMenu:isa(cmd) then
							ig.igText'OpenShopMenu'
							ig.igSameLine()
							app.shopWindow:popupButton(cmd.shopIndex)

						-- battles:
						elseif game.Cmds.Battle:isa(cmd) then
							ig.igText'battle'
							ig.igSameLine()
							if app.eventBattleOptionsWindow:popupButton(cmd.eventBattleOptionsIndex) then
								-- then set the battle bg ...
								app.mapWindow:setBattleBgTex(cmd.battleBG)
							end

						-- maps:
						elseif game.Cmds.SetMap:isa(cmd)
						or game.EventCmds.SetParentMap:isa(cmd)	-- maybe SetParentMap should be a SetMap?
						then
							ig.igText(
								game.EventCmds.SetParentMap:isa(cmd)
								and 'setParentMap'
								or 'setMap'
							)
							ig.igSameLine()
							if app.mapWindow:popupButton(cmd.mapIndex) then
								app.tileWindow:setXY(cmd.x, cmd.y)
								app:centerView(cmd.x, cmd.y)
							end
						elseif game.EventCmds.MovePartyToMap:isa(cmd) then
							ig.igText('movePartyToMap '..cmd.partyIndex)
							ig.igSameLine()
							if app.mapWindow:popupButton(cmd.mapIndex) then
								-- hmm, no x,y?
							end

						-- scripts:
						elseif game.EventCmds.Call:isa(cmd) then
							ig.igText'call'
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.EventCmds.CallRepeat:isa(cmd) then
							ig.igText('for i=1,'..cmd.count..' call')
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.EventCmds.JumpBasedOnBattleFlag:isa(cmd) then
							ig.igText('if gameState.battleFlag'..cmd.flagIndex..' then goto')
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.EventCmds.Jump5050:isa(cmd) then
							ig.igText'if math.random() < .5 then goto'
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.Cmds.Cond:isa(cmd) then
							ig.igText('if '..cmd.condCode..' then goto')
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.ObjectCmds.Call:isa(cmd) then
							ig.igText'goto'
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.WorldCmds.IfKeyThenGoto:isa(cmd) then
							ig.igText'if keypress then goto'
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.WorldCmds.IfFacingThenGoto:isa(cmd) then
							ig.igText('if dir=='..cmd.dir..' then goto')
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
						elseif game.EventCmds.GotoForDialogResult:isa(cmd) then
							ig.igText'callForDialogResult'
							for _,addr in ipairs(cmd.addrs) do
								ig.igSameLine()
								self:popupButtonForAddr(addr)
							end
						elseif game.EventCmds.StartTimer:isa(cmd) then
							ig.igText('start timer, addr=')
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.destAddrOfs)
							ig.igSameLine()
							ig.igText(' duration='..cmd.duration..', flags='..cmd.flags)
						elseif game.EventCmds.ChangeObjectEvent:isa(cmd) then
							ig.igText('objs['..cmd.objectIndex..'].script =')
							ig.igSameLine()
							self:popupButtonForAddr(scriptBaseAddr + cmd.newScriptAddrOfs)
						elseif game.EventCmds.CallSwitchNPCFlags:isa(cmd) then
							ig.igText'gotoForCharacter'
							for _,option in ipairs(cmd.options) do
								ig.igSameLine()
								app.charWindow:popupButton(option.characterIndex)
								ig.igSameLine()
								self:popupButtonForAddr(scriptBaseAddr + option.addrOfs)
							end

						else
						-- default:
							ig.igText(string.trim(tostring(cmd):gsub('\n', '\\n')))
						end

						ig.igPopID()
					end
				end
				ig.igPopID()
			end
		end
		ig.igEndTable()
	end

	ig.igEndChild()
end

function EventScriptWindow:setIndex(newIndex)
	if EventScriptWindow.super.setIndex(self, newIndex) == false then return false end
	local cmd = self:getIndex(self.index)
	if cmd then
		self:openScriptAddr(cmd.addr)
	end
end

function EventScriptWindow:openScriptAddr(scriptAddr)
	self.indexStack:insert(self.index)
	self.index = self.app.game.eventScriptCmdIndexForAddr[scriptAddr]
	if not self.index then
		print(("couldn't find event script command at address $%06x"):format(scriptAddr))
		self.index = 0
	else
		self.index = self.index - 1
	end
	self:open()
	--[[
	ig.ImGuiListClipper_SeekCursorForItem(self.clipper, self.index)
	--]]
	-- [[
	self.jumpRequested = self.index
	--]]
end

-- popupButton is based on index, so this is based on address
function EventScriptWindow:popupButtonForAddr(addr, extraText)
	local text = ('$%06x'):format(addr)
	if extraText then text = text .. ' ' .. extraText end
	if ig.igButton(text) then
		self:openScriptAddr(addr)
		return true
	end
end

return EventScriptWindow
