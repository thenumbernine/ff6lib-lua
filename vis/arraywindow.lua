local ffi = require 'ffi'
local assert = require 'ext.assert'
local table = require 'ext.table'
local class = require 'ext.class'
local ig = require 'imgui'


local Window = class()
-- .name
-- .update

local ArrayWindow = Window:subclass()

function ArrayWindow:init(args)
	self.children = table(args.children)
	self.show = ffi.new('bool[1]', not not args.show)
	self.app = assert.index(args, 'app')
	self.getArray = args.getArray
	self:setIndex(args.index or 0)
end

function ArrayWindow:setIndex(index)
	if index == self.index then return false end
	self.index = index
end

function ArrayWindow:getIndexName() end

function ArrayWindow:popupButton(targetIndex, extraText)
	if targetIndex ~= nil then
		assert.type(targetIndex, 'number')
	end
	local result
	ig.igPushID_Str(self.name)
	ig.igPushID_Str'popup button'
	local ar = self:getArray()
	local has = ar and #ar > 0
	local indexName = has and targetIndex and self:getIndexName(targetIndex)
	local k = self.name..': '
		..(targetIndex and '#'..targetIndex..'/' or '')
		..(has and #ar or 'none')
		..(indexName and ' '..indexName or '')
	if extraText then k = k .. ' '.. extraText end
	if not has then
		ig.igText(k)
		self.show[0] = false
	else
		if ig.igButton(k) then
			self.show[0] = true
			if targetIndex then
				print('setting', self.name, 'to', targetIndex)
				self:setIndex(targetIndex)
			end
			result = true
		end
	end
	ig.igPopID()
	ig.igPopID()
	return result
end

function ArrayWindow:update()
	if not self.show[0] then return end
	local ar = self:getArray()
	if not (ar and #ar > 0) then return end
	ig.igPushID_Str(self.name)
	if ig.igBegin(self.name, self.show, 0) then
		ig.igText(self.name..' #'..self.index..'/'..#ar)

		local pushIndex = self.index
		if ig.luatableInputInt('index', self, 'index') then
			local newIndex = self.index % #ar
			self.index = pushIndex	-- so setIndex registers a change
			self:setIndex(newIndex)
		end


		local name = self:getIndexName(self.index) or ''
		ig.igText('name = '..name)
		if not self.searchText then
			ig.igSameLine()
			if ig.igButton'...' then
				self.searchText = name
			end
		else
			ig.luatableInputText('find', self, 'searchText')
			if ig.igButton'go' then
				-- TODO also count initially and cahce and cycle and show 1/40 or whatever
				for d=1,#self:getArray() do
					local i = (self.index + d) % #self:getArray()
					local n = tostring(self:getIndexName(i))
					if n:lower():find(self.searchText:lower(), 1, true) then
						self:setIndex(i)
					end
				end
			end
			ig.igSameLine()
			if ig.igButton'cancel' then
				self.searchText = nil
			end
		end

		self:showIndexUI(ar)
	end
	ig.igEnd()
	ig.igPopID()
end

return ArrayWindow
