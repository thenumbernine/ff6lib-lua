local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local EsperWindow = ArrayWindow:subclass()

EsperWindow.name = 'espers'

function EsperWindow:getCount()
	return self.app.game.numEspers
end

function EsperWindow:getIndexName(i)
	return tostring(self.app.game.getEsperName(i))
end

function EsperWindow:showIndexUI()
	local app = self.app
	local game = app.game

	ig.igText('AttackName = '..game.esperAttackNames[self.index])
	ig.igText('Desc = '..game.gamezstr(game.esperDescBase + game.esperDescOffsets[self.index]))
	local esper = game.espers[self.index]
	for fieldname, ctype, field in esper:fielditer() do
		if fieldname ~= 'bonus' then
			self:editField(esper, fieldname, ctype, field)
		end
	end

	local bonusIndex = esper.bonus
	ig.igText('bonus:')
	ig.igSameLine()
	ig.igText(tostring(esper.bonus) or 'none')
end

--also game.esperMenuOrder if you want

return EsperWindow
