local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local SpellWindow = ArrayWindow:subclass()

SpellWindow.name = 'spell'

function SpellWindow:init(...)
	SpellWindow.super.init(self, ...)
	local game = self.app.game
	self.array = range((game.countof(game.spells)))
end

function SpellWindow:getArray()
	return self.array
end

function SpellWindow:getIndexName(i)
	return self.app.game.getSpellName(i)
end

function SpellWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game

	if self.index < 54 then
		ig.igText(' desc = "'..game.gamezstr(game.spellDescBase + game.spellDescOffsets[self.index])..'"')
	elseif self.index >= 54 and self.index < 64 then
		-- should I put esper descs here, or in the esper output, or both?
	end
	local spell = game.spells[self.index]
	for fieldname, ctype, field in spell:fielditer() do
		self:editField(spell, fieldname, ctype, field)
	end
end

--[[
TODO point back to ...
- item.spellLearn.spell.i
- item.spellCast
- monster attacks
- monster gau rages
- monster relm sketches
- strago lores
- cyan swdtechs
- sabin blitzes
- mog dances
- esper attacks
- esper learns
--]]

return SpellWindow
