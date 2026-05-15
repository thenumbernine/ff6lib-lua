local ffi = require 'ffi'
local assert = require 'ext.assert'
local table = require 'ext.table'
local class = require 'ext.class'
local ig = require 'imgui'


local Window = class()

--Window.name = ...

function Window:init(args)
	self.app = assert.index(args, 'app')
	self.children = table(args.children)	-- used in App
	self.show = ffi.new('bool[1]', not not args.show)
end

function Window:isOpen()
	return self.show[0]
end

function Window:update()
	if not self:isOpen() then return end

	-- do I need igPushID_Str and igBegin? or does igBegin push ID?
	-- assert or segfault
	ig.igPushID_Str((assert.type(self.name, 'string')))

	if ig.igBegin(self.name, self.show, 0) then
		self:updateWindow()
	end
	ig.igEnd()
	ig.igPopID()
end

--function Window:updateWindow() ... end

function Window:popupButton()
	local result
	ig.igPushID_Str((assert.type(self.name, 'string')))
	ig.igPushID_Str'popup button'
	if ig.igButton(self.name) then
		self.show[0] = true
		result = true
	end
	ig.igPopID()
	ig.igPopID()
	return result
end

return Window
