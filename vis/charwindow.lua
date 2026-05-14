local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local CharWindow = ArrayWindow:subclass()

CharWindow.name = 'characters'

function CharWindow:init(...)
	CharWindow.super.init(self, ...)

	self.array = range(self.app.game.numCharacters)
end

function CharWindow:getArray()
	return self.array
end

function CharWindow:getIndexName(i)
	return self.app.game.characterNames[i]
end

function CharWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	if self.index < 0 or self.index >= game.numCharacters then return end

	if self.charSpriteTex then
		local y = ig.igGetCursorPosY()
		ig.igSetCursorPosY(32)	-- or wherever y should be after the title bar
		local avail = ig.ImVec2()
		ig.igGetContentRegionAvail(avail)
		local desiredX = ig.igGetCursorPosX() + avail.x - self.charSpriteTex.width
		ig.igSetCursorPosX(desiredX)
		ig.igImage(self.charSpriteTex.id, self.charSpriteTex.imsize)
		ig.igSetCursorPosY(y)
	end

	if ig.igCollapsingHeader'fields' then
		local ch = game.characters[self.index]
		ig.igPushID_Str(self.name)
		for fieldname, ctype, field in ch:fielditer() do
			self:editField(ch, fieldname, ctype, field)
		end
		ig.igPopID()
	end
end

function CharWindow:setIndex(...)
	local app = self.app
	local game = app.game

	CharWindow.super.setIndex(self, ...)

	-- refresh char sprite
	if self.charSpriteTex then
		self.charSpriteTex:delete()
	end
	if self.index >= 0 and self.index < game.numMenuChars then
		self.charSpriteTex = self:makeTex(
			game.getMenuCharImage(self.index):rgba()
		)
		-- how about another for the sprite sheet?
	end
end

return CharWindow
