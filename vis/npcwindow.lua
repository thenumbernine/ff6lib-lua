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

	ig.igPushID_Str(self.name)
	for fieldname, ctype, field in n:fielditer() do
		if fieldname == 'script' then
			-- TODO how to edit script pointers?
			self.app.scriptWindow:popupButtonForAddr(n:getScriptAddr())
		else
			ig.igPushID_Str(fieldname)
			self:editField(n, fieldname, ctype, field)
			ig.igPopID()
		end
	end
	ig.igPopID()
end

return NPCWindow
