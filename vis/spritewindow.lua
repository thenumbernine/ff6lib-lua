--[[
"sprite" is a loaded term ofc so maybe I'll rename this.
I already have routines for loading monster sprites.
this is for the sprite animation on the maps.
--]]
local ig = require 'imgui'
local makePalette = require 'ff6.graphics'.makePalette
local ArrayWindow = require 'ff6.vis.arraywindow'

local SpriteWindow = ArrayWindow:subclass()

SpriteWindow.name = 'sprite'
SpriteWindow.frameIndex = 0

function SpriteWindow:getCount()
	return self.app.game.numCharacterSprites
end

local availSize = ig.ImVec2()
local drawSize = ig.ImVec2()
local uv0 = ig.ImVec2(0,0)
local uv1 = ig.ImVec2(1,1)
function SpriteWindow:showIndexUI()
	if self.index < 0 or self.index >= self:getCount() then return end

	local app = self.app
	local game = app.game

	ig.igText(game.spriteNames[self.index] or '?')

	if ig.luatableInputInt('frameIndex', self, 'frameIndex') then
		self:refreshTex()
	end

	ig.igText(game.spriteFrameNames[self.frameIndex] or '?')

	local spriteTex = self.spriteTex
	if spriteTex then
		local y = ig.igGetCursorPosY()
		ig.igGetContentRegionAvail(availSize)
		availSize.x = availSize.x - 16	-- make room for scrollbar
		availSize.y = availSize.y - 4
		local scale = math.max(1, availSize.x / spriteTex.width)
		drawSize.x = math.ceil(scale * spriteTex.width)
		drawSize.y = math.ceil(scale * spriteTex.height)
		ig.igImage(spriteTex.id, drawSize, uv0, uv1)
		ig.igSetCursorPosY(y + math.ceil(spriteTex.height * scale) + 4)
	end

	if ig.igCollapsingHeader'fields:' then
		local lo = game.characterSpriteOffsetLo[self.index]
		local hiAndSize = game.characterSpriteOffsetHiAndSize + self.index
		local hi = hiAndSize.hi
		local size = hiAndSize.size
		local addr = bit.bor(lo, bit.lshift(hi, 16))
		ig.igText(('addr $%06x'):format(addr))
		ig.igText(('size 0x%02x'):format(size))

		self.__tmp = game.characterPaletteIndexes[self.index % game.countof(game.characterPaletteIndexes)]
		if ig.luatableInputInt('palette index', self, '__tmp') then
			game.characterPaletteIndexes[self.index % game.countof(game.characterPaletteIndexes)] = self.__tmp
			self:refreshTex()
		end

		ig.igText(game.spritePaletteNames[game.characterPaletteIndexes[self.index % game.countof(game.characterPaletteIndexes)]] or '?')
	end
end

-- TODO here and monsters and charwindow,
--  refresh image upon palette change
-- double-TODO is to somehow custom-render in Imgui the indexed sprite-sheet
-- triple-TODO is to finish my own UI, so I can use my own shader, and don't have to bake palettes at all.
function SpriteWindow:setIndex(...)
	SpriteWindow.super.setIndex(self, ...)
	self.frameIndex = 0
	self:refreshTex()
end

function SpriteWindow:refreshTex()
	local app = self.app
	local game = app.game

	if self.spriteTex then
		self.spriteTex:delete()
		self.spriteTex = nil
	end
	if self.index < 0 or self.index >= self:getCount() then return end

	local maxFrames = game.getSpriteFrameCount(self.index)
	self.frameIndex = self.frameIndex % math.max(1, maxFrames)

	local img = game.getSpriteFrameImage(self.index, self.frameIndex)

	img.palette = makePalette(
		game,
		game.characterPalettes + game.characterPaletteIndexes[self.index % 24],
		4,
		16
	)

	self.spriteTex = self:makeTex(img)
end

return SpriteWindow
