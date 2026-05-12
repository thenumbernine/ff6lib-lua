local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local TreasureWindow = ArrayWindow:subclass()

TreasureWindow.name = 'treasure'

function TreasureWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.treasures
end

function TreasureWindow:showIndexUI(ar)
	local t = ar[1+self.index]
	if not t then return end

	local app = self.app

	ig.igPushID_Str(self.name)
	for fieldname, ctype, field in t:fielditer() do
		if fieldname == 'battleOrItemOrGP' then
			if t.type == 0 then	-- empty
				ig.igText(' empty = '..t.battleOrItemOrGP)
			elseif t.type == 1 then	-- monster
				app.eventBattleOptionsWindow:popupButton(t.battleOrItemOrGP)
			elseif t.type == 2 then	-- item
				app.itemWindow:popupButton(t.battleOrItemOrGP)
			elseif t.type == 3 then	-- GP
				ig.igText(' GP = '..(t.battleOrItemOrGP * 100))
			else
				ig.igText(' ??? = '..t.battleOrItemOrGP)
			end
		else
			ig.igPushID_Str(fieldname)
			self:editField(t, fieldname, ctype, field)
			ig.igPopID()
		end
	end
	ig.igPopID()
end

return TreasureWindow
