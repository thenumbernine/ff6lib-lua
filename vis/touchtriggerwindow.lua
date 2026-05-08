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
	ig.igText(' pos = '..e.pos)
	-- absolute?
	local scriptAddr = e:getScriptAddr()
	if ig.igButton((' event script = $%06x'):format(scriptAddr)) then
		self.app.scriptWindow:openScriptAddr(scriptAddr)
	end
end

return TouchTriggerWindow
