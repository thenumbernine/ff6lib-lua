local ArrayWindow = require 'ff6.vis.arraywindow'

local NPCWindow = ArrayWindow:subclass()

NPCWindow.name = 'npc'

function NPCWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.npcs
end

function NPCWindow:showIndexUI()
	local n = self:getIndex(self.index)
	if not n then return end

	for fieldname, ctype, field in n:fielditer() do
		if fieldname == 'script' then
			-- TODO how to edit script pointers?
			self.app.scriptWindow:popupButtonForAddr(n:getScriptAddr())
		else
			self:editField(n, fieldname, ctype, field)
		end
	end
end

return NPCWindow
