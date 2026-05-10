local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

-- really it's just like DoorWindow, except different array/struct

local BigDoorWindow = ArrayWindow:subclass()

BigDoorWindow.name = 'big door'

function BigDoorWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.bigDoors
end

function BigDoorWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end
	ig.igText(' pos = '..e.pos)
	if ig.igButton(' map = '..e.mapIndex) then
		self:goThruDoor()
	end
	ig.igText(' length = '..(e.length+1))
	ig.igText(e.vertical==0 and ' horz' or ' vert')

	-- notice the rest is in common with typical Door:
	-- how about a common parent struct?
	ig.igText(' setParentMap = '..e.setParentMap)
	ig.igText(' zLevel = '..e.zLevel)
	ig.igText(' showDestName = '..e.showDestName)
	ig.igText(' destFacingDir = '..e.destFacingDir)
	ig.igText(' unknown_3_6 = '..e.unknown_3_6)
	ig.igText(' dest = '..e.dest)
end

function BigDoorWindow:goThruDoor()
	local e = self:getArray()[1+self.index]
	if not e then return end

	self.app.mapWindow:setIndex(e.mapIndex, 0 ~= e.setParentMap)

	-- new map should be loaded now
	local mapWidth, mapHeight = self.app.tileWindow:getMapSize()
	if mapWidth and mapHeight then
		self.app.tileWindow:setIndex(e.dest.x + mapWidth * e.dest.y)
	end

	self.app:centerView(e.dest.x, e.dest.y)
end

return BigDoorWindow
