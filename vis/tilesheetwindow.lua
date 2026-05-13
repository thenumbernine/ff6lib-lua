local ffi = require 'ffi'
local math = require 'ext.math'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local ArrayWindow = require 'ff6.vis.arraywindow'

local TileSheetWindow = ArrayWindow:subclass()

TileSheetWindow.name = 'tile sheet'

function TileSheetWindow:init(...)
	self.array = range(256)
	TileSheetWindow.super.init(self, ...)
end

function TileSheetWindow:getArray()
	return self.array
end

local min = ig.ImVec2()
local max = ig.ImVec2()
local availSize = ig.ImVec2()
local drawSize = ig.ImVec2()
function TileSheetWindow:showIndexUI(ar)
	local layerTexs = self.app.map16x16tileTexs
	if not layerTexs then return end
	local texs = layerTexs[self.layerIndex]
	if not texs then return end
	-- TODO pick this based on animation frame...
	local tex = texs[(self.app.frameIndex % #texs) + 1]

	-- same as in BattleFormationWindow:showIndexUI
	ig.igGetContentRegionAvail(availSize)
	availSize.x = availSize.x - 16	-- make room for scrollbar
	availSize.y = availSize.y - 4
	local scale = math.max(1, availSize.x / tex.width, availSize.y / tex.height)
	drawSize.x = math.ceil(scale * tex.width)
	drawSize.y = math.ceil(scale * tex.height)

	-- ... and handle clicks ...
	-- [=[ how to handle click location:
	local texScreenPos = ig.igGetCursorScreenPos()
	local mousePos = ig.igGetMousePos()
	local cursorX = mousePos.x - texScreenPos.x - 4
	local cursorY = mousePos.y - texScreenPos.y - 4
	local x = math.clamp(math.floor(cursorX / tonumber(drawSize.x) * 16), 0, 15)
	local y = math.clamp(math.floor(cursorY / tonumber(drawSize.y) * 16), 0, 15)
	--]=]
	if ig.igImageButton(self.name, ffi.cast('ImTextureID', tex.id), drawSize) then
		self.index = bit.bor(x, bit.lshift(y, 4))
	end

	-- ... and draw highlights ...
	-- [=[ how to draw a highlight over an imgui box:
	if self.index >= 0 and self.index < 256 then
		ig.igGetItemRectMin(min)
		ig.igGetItemRectMax(max)
		local minx, miny = min.x, min.y
		local maxx, maxy = max.x, max.y
		local sizex = maxx - minx
		local sizey = maxy - miny
		local x = bit.band(0xf, self.index)
		local y = bit.band(0xf, bit.rshift(self.index, 4))
		min.x = x / 16 * sizex + minx
		max.x = (x+1) / 16 * sizex + minx
		min.y = y / 16 * sizey + miny
		max.y = (y+1) / 16 * sizey + miny
		ig.ImDrawList_AddRect(
			ig.igGetWindowDrawList(),
			min,
			max,
			0xff00ffff, -- yellow color ... is it ABGR ?
			0,			-- rounding
			0,			-- typical flags
			2			-- thickness
		)
	end
	--]=]
end

return TileSheetWindow
