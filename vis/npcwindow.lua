local ffi = require 'ffi'
local table = require 'ext.table'
local ig = require 'imgui'
local ArrayWindow = require 'ff6.vis.arraywindow'

local NPCWindow = ArrayWindow:subclass()

NPCWindow.name = 'npc'

function NPCWindow:getArray()
	local mapInfo = self.app.mapWindow:getMapInfo()
	return mapInfo and mapInfo.npcs
end

local function getCTypeForStructField(obj, fieldname)
	for name, ctype, field in obj:fielditer() do
		if name == fieldname then return ctype end
	end
	error("couldn't find "..fieldname)
end

function NPCWindow:showIndexUI()
	local n = self:getIndex(self.index)
	if not n then return end

	local function editField(fieldname, displayName)
		-- TODO displayName override in editField
		self:editField(
			n,	-- obj
			fieldname, -- fieldname
			getCTypeForStructField(n, fieldname),	-- ctype
			nil,		-- field but it's never used
			displayName	-- displayName
		)
	end

	-- copied out of NPC'c ctor args .metatable's __tostring
	-- (TODO generalize both into its fielditer override maybe?)
	local vehicle = n.vehicle_or_animSpeed
	local specialNPC = n.showRider_or_specialGraphics
	local specialAndNotVehicle = specialNPC ~= 0 and vehicle == 0
	if specialAndNotVehicle then
		editField'vramAddr' -- 0.0-0.6
		editField'hflip'	-- 0.7
		if n.isSlave ~= 0 then
			editField'masterNPC' -- 1.0-1.4
			editField'masterOffset' -- 1.5-1.7
			editField'masterOffsetAxis' -- 2.0
			editField'isSlave'	-- 2.1
		end
	else
		--self:editField(n, 'script', ctype, field)
		-- TODO how to edit script pointers?
		local addr = n:getScriptAddr()
		if addr then
			--ig.igSameLine()
			self.app.scriptWindow:popupButtonForAddr(addr)
		end
	end

	--editField'palette' -- 2.2-2.4
	do
		-- use a spinner for palette
		self.__tmp = n.palette
		if ig.luatableInputInt('palette', self, '__tmp') then
			n.palette = self.__tmp
			-- ... and refresh tex when you're done
			self.app.mapWindow:refreshNPCTexs()
		end
	end

	editField'scrollingLayer' -- 2.5
	editField'flag' -- 2.6-3.7
	editField'x' -- 4.0-4.6

	if specialAndNotVehicle then
		editField('showRider_or_specialGraphics', 'specialGraphics')	-- 4.7
	else
		if vehicle ~= 0 then
			editField('showRider_or_specialGraphics', 'showRider')	-- 4.7
		end
	end
	editField'y' -- 5.0-5.5
	editField'speed' -- 5.6-5.7

	--editField'graphics' -- 6.0-6.7
	do
		-- add in the ref to spritewindow on the rhs
		if self:editRef(self.app.spriteWindow, n, 'graphics') then
			-- if we modified the value then refresh npc textures
			self.app.mapWindow:refreshNPCTexs()
		end
	end

	editField'movement' -- 7.0-7.3
	editField'spritePriority' -- 7.4-7.5
	if self.animation ~= 0 then
		editField('vehicle_or_animSpeed', 'animSpeed') -- 7.6-7.7
		editField('dir_or_animType', 'animType') -- 8.0-8.1
	else
		if not specialAndNotVehicle then
			editField('vehicle_or_animSpeed', 'vehicle') -- 7.6-7.7
		end
		editField('dir_or_animType', 'dir') -- 8.0-8.1
	end
	if specialAndNotVehicle then
		editField('specialGFXSize_or_talkDoesntTurn', 'is32x32') -- 8.2
	else
		editField('specialGFXSize_or_talkDoesntTurn', 'talkDoesntTurn') -- 8.2
	end
	editField'layerPriority' -- 8.3-8.4
	editField'animation' -- 8.5-8.7
end

return NPCWindow
