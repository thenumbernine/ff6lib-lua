local table = require 'ext.table'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local ShopWindow = ArrayWindow:subclass()

ShopWindow.name = 'shop'

function ShopWindow:getCount()
	local game = self.app.game
	return game.countof(game.shops)
end

function ShopWindow:getIndex(index)
	local game = self.app.game
	return game.shops + index
end

function ShopWindow:showIndexUI()
	local app = self.app
	local game = app.game

	local shop = self:getCurIndex()
	if not shop then return end

	ig.igText(game.shopTypes[1+shop.shopinfo.shopType] or '?')
	ig.igText(game.shopPriceTypes[1+shop.shopinfo.priceType] or '?')
	self:editField(shop, 'shopinfo', game.ShopInfo)

	local items = shop.items
	for i=0,items.dim-1 do
		ig.igPushID_Int(i)
		self:editItemRef(items.s+i, 'i')
		local item = game.items + items.s[i].i
		local price = item.price
		ig.igSameLine()
		ig.igText('$'..price)
		ig.igPopID()
	end


	-- reverse-references:


	if not self.eventScriptCmdsWithThis then
		self.eventScriptCmdsWithThis = table()
		for _,cmd in ipairs(game.eventScriptCmds) do
			if game.EventCmds.OpenShopMenu:isa(cmd)
			and cmd.shopIndex == self.index
			then
				self.eventScriptCmdsWithThis:insert(cmd.addr)
				break
			end
		end
	end
	ig.igSeparator()
	ig.igText'event scripts ...'
	if #self.eventScriptCmdsWithThis == 0 then
		ig.igText'...none'
	else
		for _,addr in ipairs(self.eventScriptCmdsWithThis) do
			app.scriptWindow:popupButtonForAddr(addr)
		end
	end
end

function ShopWindow:setIndex(...)
	ShopWindow.super.setIndex(self, ...)

	self.eventScriptCmdsWithThis = nil
end

return ShopWindow
