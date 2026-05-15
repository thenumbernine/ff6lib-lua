local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local EventScriptWindow = ArrayWindow:subclass()

function EventScriptWindow:init(...)
	EventScriptWindow.super.init(self, ...)
	self.clipper = ig.ImGuiListClipper_ImGuiListClipper()
	self.availableSpace = ig.ImVec2()
	-- on gc, but only before app shutdown:
	--ig.ImGuiListClipper_destroy(self.clipper)
end

EventScriptWindow.name = 'event script'

function EventScriptWindow:getArray()
	return self.app.game.eventScriptCmds
end

-- used for buttons, index is 0-based
function EventScriptWindow:getIndexName(i)
	return ('event $%06x'):format(self.app.game.eventScriptCmds[1+i].addr)
end

function EventScriptWindow:showIndexUI()
	local app = self.app
	local game = app.game

	local ar = self:getArray()
	-- TODO index isn't necessary for this window
	-- tho better TODO is to fix the scroll area of the clipper and make it jump correctly
	--if ig.igBeginChild('ScriptWindowEvents', ig.ImVec2(0, #ar), true) then
	-- this is even worse, now single wheel or scrollbar scrolls up and down jump over an entire page, and i can't find the item i'm looking for, and the clipper seek funciton doesn't work.
	ig.igGetContentRegionAvail(self.availableSpace)
	self.availableSpace.x = 0
	if ig.igBeginChild('ScriptWindowEvents', self.availableSpace, true) then

		if self.jumpRequested then
			ig.igSetScrollY_Float(self.jumpRequested)
			self.jumpRequested = nil
		end

		--[[ not working, very arbitrary
		if ig.igIsWindowHovered(0) then
			local wheel = ig.igGetIO().MouseWheel
			if wheel ~= 0 then
				ig.igSetScrollY_Float(ig.igGetScrollY() - wheel * 10)
			end
		end
		--]]

		ig.ImGuiListClipper_Begin(self.clipper, #ar, 1)
		while ig.ImGuiListClipper_Step(self.clipper) do
			for i=self.clipper.DisplayStart,self.clipper.DisplayEnd-1 do
				local cmd = ar[1+i]
				if cmd then
					ig.igPushID_Int(cmd.addr)

					ig.igText(
						(i == self.index and '>' or ' ')
						..('%06x: '):format(cmd.addr)
					)
					ig.igSameLine()

					-- item links:
					if game.ScriptCmds.GiveItem:isa(cmd) then
						ig.igText'GiveItem'
						ig.igSameLine()
						app.itemWindow:popupButton(cmd.itemIndex)
					elseif game.ScriptCmds.TakeItem:isa(cmd) then
						ig.igText'TakeItem'
						ig.igSameLine()
						app.itemWindow:popupButton(cmd.itemIndex)

					-- battles:
					elseif game.ScriptCmds.TouchBattle:isa(cmd) then	-- TouchBattle is a subclass of Battle
						ig.igText'TouchBattle'
						ig.igSameLine()
						app.eventBattleOptionsWindow:popupButton(cmd.eventBattleOptionsIndex)
					elseif game.ScriptCmds.Battle:isa(cmd) then
						ig.igText'Battle'
						ig.igSameLine()
						app.eventBattleOptionsWindow:popupButton(cmd.eventBattleOptionsIndex)

					-- maps:
					elseif game.ScriptCmds.SetMap2:isa(cmd) then	-- SetMap2 is a subclass of SetMap
						ig.igText'SetMap2'
						ig.igSameLine()
						if app.mapWindow:popupButton(cmd.mapIndex) then
							local mapWidth, mapHeight = app.tileWindow:getMapSize()
							if mapWidth and mapHeight then
								app.tileWindow:setIndex(cmd.x + mapWidth * cmd.y)
							end
							app:centerView(cmd.x, cmd.y)
						end
					elseif game.ScriptCmds.SetMap:isa(cmd) then
						ig.igText'SetMap'
						ig.igSameLine()
						if app.mapWindow:popupButton(cmd.mapIndex) then
							local mapWidth, mapHeight = app.tileWindow:getMapSize()
							if mapWidth and mapHeight then
								app.tileWindow:setIndex(cmd.x + mapWidth * cmd.y)
							end
							app:centerView(cmd.x, cmd.y)
						end
					elseif game.ScriptCmds.MovePartyToMap:isa(cmd) then
						ig.igText('movePartyToMap '..cmd.partyIndex)
						ig.igSameLine()
						if app.mapWindow:popupButton(cmd.mapIndex) then
							-- hmm, no x,y?
						end

					else
					-- default:
						ig.igText(tostring(cmd):gsub('\n', '\\n'))
					end

					ig.igPopID()
				end
			end
		end
		ig.ImGuiListClipper_End(self.clipper)
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
