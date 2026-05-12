local table = require 'ext.table'
local range = require 'ext.range'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local MetamorphWindow = ArrayWindow:subclass()

MetamorphWindow.name = 'metamorph sets'

function MetamorphWindow:init(...)
	MetamorphWindow.super.init(self, ...)
	local game = self.app.game
	self.array = range(game.countof(game.metamorphSets))
end

function MetamorphWindow:getArray()
	return self.array
end

function MetamorphWindow:showIndexUI(ar)
	local app = self.app
	local game = app.game
	local mmset = game.metamorphSets + self.index

	for i=0,mmset.dim-1 do
		ig.igPushID_Int(i)
		self:editItemRef(mmset.s[i], 'i')
		--app.itemWindow:popupButton(mmset.s[i].i)
		ig.igPopID()
	end

	-- reverse-references:

	if not self.monstersWithThis then
		self.monstersWithThis = table()
		for i=0,game.countof(game.monsters)-1 do
			local monster = game.monsters + i
			if monster.metamorphSet == self.index then
				self.monstersWithThis:insert(i)
			end
		end
	end

	ig.igSeparator()
	ig.igText'monsters with this metamorph set...'
	if #self.monstersWithThis == 0 then
		ig.igText'...none'
	else
		ig.igPushID_Str'metamorphWindow-monstersWithThis'
		for j,monsterIndex in ipairs(self.monstersWithThis) do
			ig.igPushID_Int(j)
			self.app.monsterWindow:popupButton(monsterIndex)
			ig.igPopID()
		end
		ig.igPopID()
	end
end

function MetamorphWindow:setIndex(...)
	MetamorphWindow.super.setIndex(self, ...)

	self.monstersWithThis = nil
end

return MetamorphWindow
