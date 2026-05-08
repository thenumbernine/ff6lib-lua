local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


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
		self.app.mapWindow:setIndex(e.mapIndex, 0 ~= e.setParentMap)
		self.app:centerView(e.dest.x, e.dest.y)
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

return BigDoorWindow
