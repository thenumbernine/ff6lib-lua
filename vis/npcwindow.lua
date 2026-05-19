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
		elseif fieldname == 'graphics' then
			-- add in the ref to spritewindow on the rhs
			if self:editRef(self.app.spriteWindow, n, fieldname) then
				-- if we modified the value then refresh npc textures
				self.app.mapWindow:refreshNPCTexs()
			end
		else
			if self:editField(n, fieldname, ctype, field) then
				-- if the user changed graphics or palette then reset the npc textures to rebake the palette etc
				if fieldname == 'graphics'
				or fieldname == 'palette'
				then
					self.app.mapWindow:refreshNPCTexs()
				end
			end
		end
	end
end

return NPCWindow
