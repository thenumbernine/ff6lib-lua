local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local TouchTriggerWindow = ArrayWindow:subclass()

TouchTriggerWindow.name = 'event trigger'

function TouchTriggerWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.touchTriggers
end

function TouchTriggerWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end

	self:editField(e, 'pos')
	self.app.scriptWindow:popupButtonForAddr(e:getScriptAddr())
end

return TouchTriggerWindow
