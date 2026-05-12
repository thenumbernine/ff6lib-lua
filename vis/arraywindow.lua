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


		-- no name on any <-> no name on all
		local name = self:getIndexName(self.index)
		if name then
			ig.igText('name = '..name)
			if not self.searchText then
				ig.igSameLine()
				if ig.igButton'find' then
					self.searchText = name
				end
			else
				ig.igSeparator()
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
				if ig.igButton'close' then
					self.searchText = nil
				end
				ig.igSeparator()
			end
		end

		self:showIndexUI(ar)
	end
	ig.igEnd()
	ig.igPopID()
end

-- ctype is probably a ffi ctype object, unless it's a bitfield, then no such object exists (right?) and it's just a string
function ArrayWindow:editField(obj, fieldname, ctype, field)
	local app = self.app
	local game = app.game

	--[[ view only:
	ig.igText(' '..fieldname..' = '..tostring(obj[fieldname]))
	--]]
	-- [[ edit:
	ctype = ctype or ffi.typeof(obj[fieldname])
	local ctypeobj = require 'ext.op'.land(pcall(function() return ffi.typeof(ctype) end))

	-- checkboxes:
	if type(ctype) == 'string'
	and ctype:match':1$'
	then
		return ig.luatableCheckbox(fieldname, obj, fieldname)

	-- TODO maybe also for Ref's, dropdowns from lists for their windows?  and auto popup buttons?

	-- structs with sub-fields (esp vectors)
	elseif ctypeobj == game.XY4b
	or ctypeobj == game.XY8sb
	or ctypeobj == game.XY8b
	or ctypeobj == game.EquipFlags
	or ctypeobj == game.Effect1
	or ctypeobj == game.Effect2
	or ctypeobj == game.Effect3
	or ctypeobj == game.Element
	or ctypeobj == game.Targetting
	then
		-- i think imgui has vector inputs... hmmm
		local modified
		ig.igText(fieldname)

		ig.igPushID_Str(fieldname)
		local subobj = obj[fieldname]
		for subfieldname, subctype, subfield in subobj:fielditer() do
			-- TOOD is there left tab padding margin whatever support in imgui?
			ig.igText(' ')
			ig.igSameLine()
			modified = self:editField(subobj, subfieldname, subctype, subfield) or modified
		end
		ig.igPopID()
		return modified

	-- default:
	else
		return ig.luatableInputFloatAsText(fieldname, obj, fieldname)
	end
	--]]
end

local tmpvec = ig.ImVec2()
-- static method
function ArrayWindow:igSetNextWidthProp(x)
	ig.igGetContentRegionAvail(tmpvec)
	ig.igSetNextItemWidth(tmpvec.x * x)
end

function ArrayWindow:editRef(win, obj, fieldname)
	local app = self.app
	local modified
-- [[ regular

	--self:igSetNextWidthProp(.5)
	ig.igSetNextItemWidth(100)

	modified = ig.luatableInputInt(fieldname, obj, fieldname)
	--obj[fieldname] = obj[field] % #win:getArray()
	-- but maps, 511 is previous-map and is oob, so ... allow for now?
	--	or make that / other special-values into checkboxes...
--]]
--[[ don't ref past end of morph set
-- but then you have to modulo the number before reassignment or else it'll cast to the bitfield size and the modulo will be way off
	local tmp = {tonumber(obj[fieldname])}
	ig.luatableInputInt(fieldname, tmp, 1)
	obj[fieldname] = tmp[1] % #win:getArray()
--]]
	ig.igSameLine()
	modified = win:popupButton(obj[fieldname]) or modified
	return modified
end

function ArrayWindow:editMetamorphRef(...)
	return self:editRef(self.app.metamorphWindow, ...)
end
function ArrayWindow:editMonsterRef(...)
	return self:editRef(self.app.monsterWindow, ...)
end
function ArrayWindow:editSpellRef(...)
	return self:editRef(self.app.spellWindow, ...)
end
function ArrayWindow:editItemRef(...)
	return self:editRef(self.app.itemWindow, ...)
end

return ArrayWindow
