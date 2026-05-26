local table = require 'ext.table'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'


local SpellWindow = ArrayWindow:subclass()

SpellWindow.name = 'spell'

function SpellWindow:getCount()
	local game = self.app.game
	return game.countof(game.spells)
end

function SpellWindow:getIndexName(i)
	return self.app.game.getSpellName(i)
end

function SpellWindow:showIndexUI()
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

	if not self.itemsWithThis then
		self.itemsWithThis = table()
		for i=0,game.countof(game.items)-1 do
			local item = game.items + i
			if item.spellLearn.spell.i == self.index then
				self.itemsWithThis:insert{itemIndex=i, type='spellLearn'}
			end
			if item.spellCast == self.index then
				self.itemsWithThis:insert{itemIndex=i, type='spellCast'}
			end
		end
	end
	ig.igText'items using this...'
	if #self.itemsWithThis == 0 then
		ig.igText'...none'
	else
		for _,info in ipairs(self.itemsWithThis) do
			self.app.itemWindow:popupButton(info.itemIndex, info.type)
		end
	end

	if not self.espersWithThis then
		self.espersWithThis = table()
		for i=0,game.countof(game.espers)-1 do
			local esper = game.espers + i
			for j=0,4 do
				if esper.spellLearn.s[j].spell.i == self.index then
					self.espersWithThis:insert(i)
					break
				end
			end
		end
	end
	ig.igText'espers using this...'
	if #self.espersWithThis == 0 then
		ig.igText'...none'
	else
		for _,esperIndex in ipairs(self.espersWithThis) do
			self.app.esperWindow:popupButton(esperIndex)
		end
	end

	if not self.monstersWithThis then
		self.monstersWithThis = table()
		for i=0,game.countof(game.monsters)-1 do
			if i < game.countof(game.monsterSpells) then
				local monsterSpells = game.monsterSpells + i
				for j=0,monsterSpells.dim-1 do
					if monsterSpells.s[j].i == self.index then
						self.monstersWithThis:insert{monsterIndex=i, type='attack'}
					end
				end
			end
			if i < game.countof(game.monsterSketches) then
				local monsterSketches = game.monsterSketches + i
				for j=0,monsterSketches.dim-1 do
					if monsterSketches.s[j].i == self.index then
						self.monstersWithThis:insert{monsterIndex=i, type='sketch'}
					end
				end
			end
			if i < game.countof(game.monsterRages) then
				local monsterRages = game.monsterRages + i
				for j=0,monsterRages.dim-1 do
					if monsterRages.s[j].i == self.index then
						self.monstersWithThis:insert{monsterIndex=i, type='rage'}
					end
				end
			end
		end
	end
	ig.igText'monsters using this...'
	if #self.monstersWithThis == 0 then
		ig.igText'...none'
	else
		for _,info in ipairs(self.monstersWithThis) do
			self.app.monsterWindow:popupButton(info.monsterIndex, info.type)
		end
	end
end

--[[
TODO point back to ...
- esper learns

just if it is or not (spell range based)
- esper attacks
- strago lores
- cyan swdtechs
- sabin blitzes
- mog dances
--]]

function SpellWindow:setIndex(...)
	SpellWindow.super.setIndex(self, ...)

	self.itemsWithThis = nil
	self.monstersWithThis = nil
	self.espersWithThis = nil
end

return SpellWindow
