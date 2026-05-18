local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local DoorWindow = ArrayWindow:subclass()

DoorWindow.name = 'door'

function DoorWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.doors
end

function DoorWindow:showIndexUI()
	local e = self:getCurIndex()
	if not e then return end

	ig.igPushID_Str(self.name)
	for fieldname, ctype, field in e:fielditer() do
		if fieldname == 'mapIndex' then
			if self:editRef(self.app.mapWindow, e, 'mapIndex') then
				self:goThruDoor()
			end
		else
			self:editField(e, fieldname, ctype, field)
		end
	end
	ig.igPopID()
end

function DoorWindow:goThruDoor()
	local e = self:getCurIndex()
	if not e then return end

	self.app.mapWindow:setIndex(e.mapIndex, 0 ~= e.setParentMap)
	self.app.tileWindow:setXY(e.dest.x, e.dest.y)
	self.app:centerView(e.dest.x, e.dest.y)
end

return DoorWindow
