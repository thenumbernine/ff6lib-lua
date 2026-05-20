local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local TouchTriggerWindow = ArrayWindow:subclass()

TouchTriggerWindow.name = 'touch trigger'

function TouchTriggerWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.touchTriggers
end

function TouchTriggerWindow:showIndexUI()
	local app = self.app
	local game = app.game

	local t = self:getIndex(self.index)
	if not t then return end

	self:editField(t, 'pos', game.XY8b)

	--self:editField(n, fieldname, ctype, field)
	local addr = t:getScriptAddr()
	if addr then
		self.app.scriptWindow:popupButtonForAddr(addr)
	end
end

return TouchTriggerWindow
