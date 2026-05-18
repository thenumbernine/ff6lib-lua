local ffi = require 'ffi'
local string = require 'ext.string'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local scriptDivider = {}

local EventScriptWindow = ArrayWindow:subclass()

EventScriptWindow.name = 'event script'

function EventScriptWindow:init(args, ...)
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

	ig.igInputInt('current opposite-world row', self.scrollOppositeWorldRow)

	ig.igGetContentRegionAvail(self.availSpace)
	self.availSpace.x = 18

	--local numRowsVisible = 15
	-- TODO determine by window size, or child panel within window...
	-- igGetContentRegionAvail returns in pixels
	local numRowsVisible = math.floor(self.availSpace.y / ig.igGetTextLineHeight())
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
		ig.ImGuiChildFlags_None,
		bit.bor(ig.ImGuiWindowFlags_NoScrollbar, ig.ImGuiWindowFlags_NoScrollWithMouse)
	)

	local wheel = ig.igGetIO().MouseWheel
	if wheel ~= 0 and ig.igIsWindowHovered(0)
	then
		self.scrollOppositeWorldRow[0] = self.scrollOppositeWorldRow[0] + wheel * 3
	end

	if self.jumpRequested then
		self.scrollOppositeWorldRow[0] = scrollMax - self.jumpRequested
		self.jumpRequested = nil
	end

	if self.scrollOppositeWorldRow[0] < 0 then self.scrollOppositeWorldRow[0] = 0 end
	if self.scrollOppositeWorldRow[0] > scrollMax then
		self.scrollOppositeWorldRow[0] = scrollMax
	end

	-- TODO maybe turn this into a table to force its row height...
	for visibleRowIndex = 0,numRowsVisible-1 do
		local i = (scrollMax - self.scrollOppositeWorldRow[0]) + visibleRowIndex
		if i >= 0 and i < count then
			local cmd = ar[1+i]
			if cmd == scriptDivider then
				ig.igSeparator()
			elseif cmd and cmd.what then
				for j,what in ipairs(cmd.what) do
					ig.igPushID_Int(j)
					if what.startEventScriptAddr then
						ig.igSameLine()
						app.mapWindow:popupButton(what.mapIndex)
					elseif what.npcIndex then
						ig.igSameLine()
						if ig.igButton('map '..what.mapIndex..' npc '..what.npcIndex) then
							app.mapWindow:setIndex(what.mapIndex)
							app.npcWindow:setIndex(what.npcIndex)
							app.npcWindow.show[0] = true
							local n = app.npcWindow:getCurIndex()
							if n then
								app:centerView(n.x, n.y)
							end
						end
					elseif what.touchTriggerIndex then
						ig.igSameLine()
						if ig.igButton('map '..what.mapIndex..' touch '..what.touchTriggerIndex) then
							app.mapWindow:setIndex(what.mapIndex)
							app.touchTriggerWindow:setIndex(what.touchTriggerIndex)
							app.touchTriggerWindow.show[0] = true
							local t = app.touchTriggerWindow:getCurIndex()
							if t then
								app:centerView(t.pos.x, t.pos.y)
							end
						end
					else
						ig.igSameLine()
						ig.igText'???'
					end
					ig.igPopID()
				end
			elseif cmd then
				ig.igPushID_Int(cmd.addr)

				ig.igText(
					(i == self.index and '>' or ' ')
					..('%06x: '):format(cmd.addr)
				)
				ig.igSameLine()

				-- indent object-cmds since they are blocks of cmds from event-cmds
				if game.ObjectCmd:isa(cmd)
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

				-- battles:
				elseif game.EventCmds.TouchBattle:isa(cmd) then	-- TouchBattle is a subclass of Battle
					ig.igText'TouchBattle'
					ig.igSameLine()
					app.eventBattleOptionsWindow:popupButton(cmd.eventBattleOptionsIndex)
				elseif game.EventCmds.Battle:isa(cmd) then
					ig.igText'Battle'
					ig.igSameLine()
					app.eventBattleOptionsWindow:popupButton(cmd.eventBattleOptionsIndex)

				-- maps:
				elseif game.EventCmds.SetMap2:isa(cmd) then	-- SetMap2 is a subclass of SetMap
					ig.igText'SetMap2'
					ig.igSameLine()
					if app.mapWindow:popupButton(cmd.mapIndex) then
						local mapWidth, mapHeight = app.tileWindow:getMapSize()
						if mapWidth and mapHeight then
							app.tileWindow:setIndex(cmd.x + mapWidth * cmd.y)
						end
						app:centerView(cmd.x, cmd.y)
					end
				elseif game.EventCmds.SetMap:isa(cmd) then
					ig.igText'SetMap'
					ig.igSameLine()
					if app.mapWindow:popupButton(cmd.mapIndex) then
						local mapWidth, mapHeight = app.tileWindow:getMapSize()
						if mapWidth and mapHeight then
							app.tileWindow:setIndex(cmd.x + mapWidth * cmd.y)
						end
						app:centerView(cmd.x, cmd.y)
					end
				elseif game.EventCmds.MovePartyToMap:isa(cmd) then
					ig.igText('movePartyToMap '..cmd.partyIndex)
					ig.igSameLine()
					if app.mapWindow:popupButton(cmd.mapIndex) then
						-- hmm, no x,y?
					end

				else
				-- default:
					ig.igText(string.trim(tostring(cmd):gsub('\n', '\\n')))
				end

				ig.igPopID()
			end

		end
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
	self.index = self.app.game.eventScriptCmdIndexForAddr[scriptAddr]
	if not self.index then
		print(("couldn't find event script command at address $%06x"):format(scriptAddr))
		self.index = 0
	else
		self.index = self.index - 1
	end
	self.show[0] = true
	--[[
	ig.ImGuiListClipper_SeekCursorForItem(self.clipper, self.index)
	--]]
	-- [[
	self.jumpRequested = self.index
	--]]
end

-- popupButton is based on index, so this is based on address
function EventScriptWindow:popupButtonForAddr(addr, extraText)
	local text = ('event $%06x'):format(addr)
	if extraText then text = text .. ' ' .. extraText end
	if ig.igButton(text) then
		self:openScriptAddr(addr)
		return true
	end
end

return EventScriptWindow
