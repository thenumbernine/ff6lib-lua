local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local NPCWindow = ArrayWindow:subclass()

NPCWindow.name = 'npc'

function NPCWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.npcs
end

function NPCWindow:showIndexUI(ar)
	local n = ar[1+self.index]
	if not n then return end
	ig.igText(' pos = '..n.x..', '..n.y)

	self.app.scriptWindow:popupButtonForAddr(n:getScriptAddr())
	ig.igText(' movement = '..n.movement)
	ig.igText(' speed = '..n.speed)

	ig.igText(' graphics = '..n.graphics)
	ig.igText(' palette = '..n.palette)

	-- "speed" when vehicle == 0
	-- "vehicle" otherwise
	ig.igText(' vehicle_or_speed = '..n.vehicle_or_speed)	-- what's this speed vs the other speed?

	ig.igText(' spritePriority = '..n.spritePriority)

	-- "direction" when animation == 0
	-- "type" otherwise
	ig.igText(' direction_or_type = '..n.direction_or_type)

	-- "size" when vehicle == 0 && special npc != 0
	-- otherwise "talkDoesntTurn"
	ig.igText(' size_or_talkDoesntTurn = '..n.size_or_talkDoesntTurn)

	ig.igText(' layerPriority = '..n.layerPriority)

	ig.igText(' animation = '..n.animation)
end

return NPCWindow
