local ig = require 'imgui'
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
			--self:editField(n, fieldname, ctype, field)
			-- TODO how to edit script pointers?
			local addr = n:getScriptAddr()
			if addr then
				--ig.igSameLine()
				self.app.scriptWindow:popupButtonForAddr(addr)
			end
		elseif fieldname == 'graphics' then
			-- add in the ref to spritewindow on the rhs
			if self:editRef(self.app.spriteWindow, n, fieldname) then
				-- if we modified the value then refresh npc textures
				self.app.mapWindow:refreshNPCTexs()
			end
		elseif fieldname == 'palette' then
			-- use a spinner for palette
			self.__tmp = n[fieldname]
			if ig.luatableInputInt(fieldname, self, '__tmp') then
				n[fieldname] = self.__tmp
				-- ... and refresh tex when you're done
				self.app.mapWindow:refreshNPCTexs()
			end
		elseif fieldname == 'flag_duplicate'
		or fieldname == 'palette_duplicate'
		or fieldname == 'scrollingLayer_duplicate'
		then
			-- there's multiple fields, all with the same region / bitflag / type, so ...
		else
			self:editField(n, fieldname, ctype, field)
		end
	end
end

return NPCWindow
