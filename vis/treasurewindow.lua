local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local TreasureWindow = ArrayWindow:subclass()

TreasureWindow.name = 'treasure'

function TreasureWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.treasures
end

function TreasureWindow:showIndexUI()
	local t = self:getIndex(self.index)
	if not t then return end

	local app = self.app
	local save = app.sramWindow:getCurIndex()

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
			local got
			if fieldname == 'flag' and save then
				local flag = t.flag
				got = 0 ~= bit.band(bit.lshift(1, bit.band(flag, 7)), save.treasureFlags[bit.rshift(flag, 3)])
				ig.igPushStyleColor_U32(ig.ImGuiCol_FrameBg, got and 0x5f009f00 or 0x5f00009f)
			end

			self:editField(t, fieldname, ctype, field)

			if got ~= nil then
				ig.igPopStyleColor(1)
			end
		end
	end
end

return TreasureWindow
