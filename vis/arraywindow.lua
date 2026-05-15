local ffi = require 'ffi'
local assert = require 'ext.assert'
local table = require 'ext.table'
local range = require 'ext.range'
local gl = require 'gl'
local GLTex2D = require 'gl.tex2d'
local ig = require 'imgui'
local Window = require 'ff6.vis.window'


local ArrayWindow = Window:subclass()

function ArrayWindow:init(args)
	ArrayWindow.super.init(self, args)
	self:setIndex(args.index or 0)
end

function ArrayWindow:getCount()
	local ar = self:getArray()
	if not ar then return end	-- return 0?
	return #ar
end

-- 0-based index
function ArrayWindow:getIndex(index)
	local ar = self:getArray()
	if not ar then return end
	return ar[1+index]
end

-- TODO rename to "showIndex"
function ArrayWindow:setIndex(index)
	if index == self.index then return false end
	self.index = index
end

function ArrayWindow:getIndexName(i) end

function ArrayWindow:isOpen()
	if not ArrayWindow.super.isOpen(self) then return false end

	-- extra visibility test for array-windows?
	local count = self:getCount()
	if not count or count == 0 then return false end

	return true
end

function ArrayWindow:updateWindow()
	local count = self:getCount()
	ig.igText(self.name..' #'..self.index..'/'..count)

	local pushIndex = self.index
	if ig.luatableInputInt('index', self, 'index') then
		local newIndex = self.index % count
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
			if ig.luatableInputText('find', self, 'searchText') then
				-- on change:
				self.searchOccurrences = table()
				for i=0,count-1 do
					local n = tostring(self:getIndexName(i))
					if n:lower():find(self.searchText:lower(), 1, true) then
						self.searchOccurrences:insert(i)
					end
				end
			end

			if self.searchOccurrences then
				local currentIndex = self.searchOccurrences:find(self.index)
				ig.igSameLine()
				ig.igText((currentIndex or '')..'/'..#self.searchOccurrences)
			end

			if ig.igButton'next' then
				if self.searchOccurrences
				and #self.searchOccurrences > 0 then
					if self.index >= self.searchOccurrences:last() then
						self:setIndex(self.searchOccurrences[1])
					else
						for j,i in ipairs(self.searchOccurrences) do
							if i > self.index then
								self:setIndex(i)
								break
							end
						end
					end
				end
			end
			ig.igSameLine()
			if ig.igButton'close' then
				self.searchText = nil
				self.searchOccurrences = nil
			end
			ig.igSeparator()
		end
	end

	self:showIndexUI()
end

function ArrayWindow:popupButton(targetIndex, extraText)
	if targetIndex ~= nil then
		assert.type(targetIndex, 'number')
	end
	local result
	ig.igPushID_Str(self.name)
	ig.igPushID_Str'popup button'

	local count = self:getCount()
	local has = count > 0
	local indexName = has and targetIndex and self:getIndexName(targetIndex)
	local k = self.name..': '
		..(targetIndex and '#'..targetIndex..'/' or '')
		..(has and count or 'none')
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

-- ctype is probably a ffi ctype object, unless it's a bitfield, then no such object exists (right?) and it's just a string
function ArrayWindow:editField(obj, fieldname, ctype, field)
	local app = self.app
	local game = app.game
	local modified

	if type(fieldname) == 'string' then
		ig.igPushID_Str(fieldname)
	elseif type(fieldname) == 'number' then
		ig.igPushID_Int(fieldname)
	else
		error("unknown fieldname type "..type(fieldname))
	end

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
		modified = ig.luatableCheckbox(fieldname, obj, fieldname)

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
	or ctypeobj == game.SpellLearn
	or ctypeobj == game.MenuNameRef4
	or ctypeobj == game.ItemRef2
	then
		-- i think imgui has vector inputs... hmmm
		ig.igText(fieldname)

		local subobj = obj[fieldname]
		for subfieldname, subctype, subfield in subobj:fielditer() do
			-- TOOD is there left tab padding margin whatever support in imgui?
			ig.igText(' ')
			ig.igSameLine()
			modified = self:editField(subobj, subfieldname, subctype, subfield) or modified
		end

	-- refs?
	elseif ctypeobj == game.SpellRef
	or ctypeobj == game.ItemRef
	or ctypeobj == game.MonsterRef
	then
		local win
		if ctypeobj == game.SpellRef then
			win = app.spellWindow
		elseif ctypeobj == game.ItemRef then
			win = app.itemWindow
		elseif ctypeobj == game.MonsterRef then
			win = app.monsterWindow
		else
			error("idk what win to use")
		end

		--[=[
		self:editSpellRef(obj[fieldname], 'i')
		--]=]
		-- [=[ editSpellRef contents:
		ig.igSetNextItemWidth(100)
		modified = ig.luatableInputInt(tostring(fieldname), obj[fieldname], 'i')
		ig.igSameLine()
		local targetIndex = obj[fieldname].i
		--[==[
		modified = win:popupButton(targetIndex) or modified
		--]==]
		-- [==[ popupButton contents:
		local k = '>'
		local name = win:getIndexName(targetIndex)
		if name then
			k = name..' '..k
		end
		if ig.igButton(k) then
			win.show[0] = true
			print('setting', win.name, 'to', targetIndex)
			win:setIndex(targetIndex)
		end
		--]==]
		--]=]

	elseif ctypeobj == game.MenuNameRef then

		self.tmpInt = self.tmpInt or ffi.new('int[1]')
		self.tmpInt[0] = obj[fieldname].i
		if ig.igCombo('', self.tmpInt, range(0, game.countof(game.menuNames)-1):mapi(function(i)
			return tostring(game.menuNames[i])
		end)) then
			obj[fieldname].i = self.tmpInt[0]
		end

	-- default:
	else
		modified = ig.luatableInputFloatAsText(fieldname, obj, fieldname)
	end
	--]]

	ig.igPopID()

	return modified
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
	--obj[fieldname] = obj[field] % win:getCount()
	-- but maps, 511 is previous-map and is oob, so ... allow for now?
	--	or make that / other special-values into checkboxes...
--]]
--[[ don't ref past end of morph set
-- but then you have to modulo the number before reassignment or else it'll cast to the bitfield size and the modulo will be way off
	local tmp = {tonumber(obj[fieldname])}
	ig.luatableInputInt(fieldname, tmp, 1)
	obj[fieldname] = tmp[1] % win:getCount()
--]]
	ig.igSameLine()
	--[=[
	modified = win:popupButton(obj[fieldname]) or modified
	--]=]
	-- [=[
	local targetIndex = obj[fieldname]
	local k = '>'
	local name = win:getIndexName(targetIndex)
	if name then
		k = name..' '..k
	end
	if ig.igButton(k) then
		win.show[0] = true
		print('setting', win.name, 'to', targetIndex)
		win:setIndex(targetIndex)
	end
	--]=]
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

-- helper for our textures and imgui ...
function ArrayWindow:makeTex(image)
	local tex = GLTex2D{
		image = image:rgba(),	-- bake palette so imgui can use it
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_NEAREST,
	}
	tex.image = image	-- tempted to do this in gl...
	-- so I don't have to keep dynamically allocating for the imgui api ...
	tex.imsize = ig.ImVec2(tex.width, tex.height)
	return tex
end

return ArrayWindow
