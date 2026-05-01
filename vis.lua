#!/usr/bin/env luajit
local cmdline = require 'ext.cmdline'(...)
local ffi = require 'ffi'
local table = require 'ext.table'
local class = require 'ext.class'
local math = require 'ext.math'
local assert = require 'ext.assert'
local timer = require 'ext.timer'
local number = require 'ext.number'
local path = require 'ext.path'
local vec2d = require 'vec-ffi.vec2d'
local sdl = require 'sdl'
local gl = require 'gl.setup'(cmdline.gl)
local Image = require 'image'
local GLTex2D = require 'gl.tex2d'
local GLGeometry = require 'gl.geometry'
local GLSceneObject = require 'gl.sceneobject'
local ig = require 'imgui'
local makePalette = require 'ff6.graphics'.makePalette

local infn = cmdline[1]
assert(infn, "missing filename")
local game = require 'ff6'((assert(path(infn):read())))
local game_t = game.game_t
local rom = game.rom
local countof = game.countof

local function settableindex(t, i, ...)
	if select('#', ...) == 0 then return end
	t[i] = ...
	settableindex(t, i+1, select(2, ...))
end
local function settable(t, ...)
	settableindex(t, 1, ...)
end


local zAndLayersWithoutLayer3Priority = {
	{0,3},
	-- priority 0 sprites here
	--{1,3},	-- does layer 3 have a zlevel?  where is it?
	-- priority 1 sprites here
	{0,2},
	{0,1},
	-- priority 2 sprites here
	{1,2},
	{1,1},
	-- priority 3 sprites here
}

local zAndLayersWithLayer3Priority = {
	{0,2},
	{0,1},
	{1,2},
	{1,1},
	{0,3},
}


local ArrayWindow = class()

function ArrayWindow:init(args)
	self.show = false
	self.index = 0
	self.app = assert.index(args, 'app')
	self.getArray = args.getArray
end

function ArrayWindow:popupButton()
	local ar = self:getArray()
	local has = ar and #ar > 0
	local k = (has and #ar or 'no')..' '..self.name
	if not has then
		ig.igText(k)
		self.show = false
	else
		if ig.igButton(k) then
			self.show = not self.show
		end
	end
end

function ArrayWindow:update()
	if not self.show then return end
	local ar = self:getArray()
	if not (ar and #ar > 0) then return end
	if ig.igBegin(self.name, nil, 0) then
		ig.igText(self.name..' #'..self.index..'/'..#ar)
		if ig.luatableInputInt('index', self, 'index') then
			self.index = self.index % #ar
		end
		self:showIndexUI(ar)
	end
	ig.igEnd()
end



local TreasureWindow = ArrayWindow:subclass()
TreasureWindow.name = 'treasure' 
function TreasureWindow:getArray()
	local mapInfo = self.app:getMapInfo()
	return mapInfo and mapInfo.treasures
end
function TreasureWindow:showIndexUI(ar)
	local t = ar[1+self.index]
	if not t then return end
	ig.igText(' pos = '..t.pos)
	ig.igText(' switch = '..t.switch)
	ig.igText(' empty = '..t.empty)
	ig.igText(' type = '..t.type)	-- combo: empty, monster, item, gp
	if t.type == 0 then	-- empty
		ig.igText(' empty = '..t.battleOrItemOrGP)
	elseif t.type == 1 then	-- monster
		ig.igText(' monster formation = #'..t.battleOrItemOrGP)
		local formation = game.formations + t.battleOrItemOrGP
		for j=1,6 do
			if formation:getMonsterActive(j) then
				ig.igText('  monster = '..game.monsterNames[formation:getMonsterIndex(j)])
			end
		end
	elseif t.type == 2 then	-- item
		ig.igText(' item = '..game.itemNames[t.battleOrItemOrGP])
	elseif t.type == 3 then	-- GP
		ig.igText(' GP = '..(t.battleOrItemOrGP * 100))
	else
		ig.igText(' ??? = '..t.battleOrItemOrGP)
	end
end


local EventTriggerWindow = ArrayWindow:subclass()
EventTriggerWindow.name = 'event trigger' 
function EventTriggerWindow:getArray()
	local mapInfo = self.app:getMapInfo()
	return mapInfo and mapInfo.eventTriggers
end

function EventTriggerWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end
	ig.igText(' pos = '..e.pos)
	-- absolute?
	local scriptAddr = e:getScriptAddr()
	if ig.igButton((' event script = $%06x'):format(scriptAddr)) then
		self.app.scriptWindow:openScriptAddr(scriptAddr)
	end
end


local EntranceTriggerWindow = ArrayWindow:subclass()
EntranceTriggerWindow.name = 'entrance trigger'
function EntranceTriggerWindow:getArray()
	local mapInfo = self.app:getMapInfo()
	return mapInfo and mapInfo.entranceTriggers
end
function EntranceTriggerWindow:showIndexUI(ar)
	local e = ar[1+self.index]
	if not e then return end
	ig.igText(' pos = '..e.pos)
	if ig.igButton(' map = '..e.mapIndex) then
		self.app:setMapIndex(e.mapIndex)
		self.app:centerView(e.dest.x, e.dest.y)
	end
	ig.igText(' setParentMap = '..e.setParentMap)
	ig.igText(' zLevel = '..e.zLevel)
	ig.igText(' showDestName = '..e.showDestName)
	ig.igText(' destFacingDir = '..e.destFacingDir)
	ig.igText(' unknown_3_6 = '..e.unknown_3_6)
	ig.igText(' dest = '..e.dest)
end


local NPCWindow = ArrayWindow:subclass()
NPCWindow.name = 'npc'
function NPCWindow:getArray()
	local mapInfo = self.app:getMapInfo()
	return mapInfo and mapInfo.npcs
end
function NPCWindow:showIndexUI(ar)
	local n = ar[1+self.index]
	if not n then return end
	ig.igText(' pos = '..n.x..', '..n.y)

	local scriptAddr = n:getScriptAddr()
	if ig.igButton((' script $%06x'):format(scriptAddr)) then
		self.app.scriptWindow:openScriptAddr(scriptAddr)
	end
	ig.igText(' movement = '..n.movement)
	ig.igText(' speed = '..n.speed)

	ig.igText(' graphics = '..n.graphics)
	ig.igText(' palette = '..n.palette)

	-- "speed" when vehicle == 0
	-- "vehicle" otherwise
	ig.igText(' vehicle_or_speed = '..n.vehicle_or_speed)	-- what's this speed vs the other speed?

	ig.igText(' spritePriority = '..n.spritePriority)

	-- "direction" when animation == 0
	-- "type" otherwise
	ig.igText(' direction_or_type = '..n.direction_or_type)

	-- "size" when vehicle == 0 && special npc != 0
	-- otherwise "talkDoesntTurn"
	ig.igText(' size_or_talkDoesntTurn = '..n.size_or_talkDoesntTurn)

	ig.igText(' layerPriority = '..n.layerPriority)

	ig.igText(' animation = '..n.animation)
end


local ScriptWindow = ArrayWindow:subclass()
function ScriptWindow:init(...)
	ScriptWindow.super.init(self, ...)
end
ScriptWindow.name = 'script'
function ScriptWindow:getArray()
	return game.eventScriptCmds
end
function ScriptWindow:showIndexUI(ar)
	-- TODO index isn't necessary for this window
	-- tho better TODO is to fix the scroll area of the clipper and make it jump correctly
	if ig.igBeginChild('ScriptWindowEvents', ig.ImVec2(0, #ar), true) then
		local clipper = ig.ImGuiListClipper_ImGuiListClipper()
		ig.ImGuiListClipper_Begin(clipper, #ar, 1)
		while ig.ImGuiListClipper_Step(clipper) do
			for i=clipper.DisplayStart,clipper.DisplayEnd-1 do
				local cmd = ar[1+i]
				if cmd then
					ig.igText(
						(i == self.index and '>' or ' ')
						..('%06x: '):format(cmd.addr)
						..tostring(cmd)
							:gsub('\n', '\\n')
					)
				end
			end
		end
		ig.ImGuiListClipper_End(clipper)
		ig.ImGuiListClipper_destroy(clipper)
	end
	ig.igEndChild()
end
function ScriptWindow:openScriptAddr(scriptAddr)
	self.index = game.eventScriptCmdIndexForAddr[scriptAddr]
	if not self.index then
		print(("couldn't find event script command at address $%06x"):format(scriptAddr))
		self.index = 0
	else
		self.index = self.index - 1
	end
	self.show = true
end
--[[ it'll just dtor out of order anwyays, so meh
function ScriptWindow:__gc()
	if self.clipper ~= nil then
		ig.ImGuiListClipper_destroy(self.clipper)
	end
end
--]]


local App = require 'imgui.appwithorbit'()
App.title = 'FF6 Data Visualizer'

function App:initGL(...)
	App.super.initGL(self, ...)

	self.view.ortho = true
	self.view.orthoSize = 256
	self.animSpeed = 15
	self.useBlend = true
	self.showTileProps = false
	self.showTileMask = 0xff
	self.showTileOfs = 0

	self.showAnimTexs = true

	self.layerDrawObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
in vec2 vertex;
out vec2 tcv;
uniform mat4 mvProjMat;
void main() {
	tcv = vertex;
	gl_Position = mvProjMat * vec4(vertex, 0., 1.);
}
]],
			fragmentCode = [[
uniform usampler2D tex;
uniform sampler2D palTex;
uniform int mask;
uniform int offset;
in vec2 tcv;
out vec4 fragColor;
void main() {
	int index = int(texture(tex, tcv, 0).r);
	index &= mask;		// mask = what in input tex to draw
	index += offset;	// offset = where in palette to draw it
	index &= 0x7f;		// pal size is 128 so...
	fragColor = texture(palTex, vec2(
		(float(index) + .5) / 128.,
		0.
	), 0);
}
]],
			uniforms = {
				tex = 0,
				palTex = 1,
			},
		},
		vertexes = {
			data = {
				0, 0,
				1, 0,
				0, 1,
				1, 1,
			},
			dim = 2,
		},
		geometry = {
			mode = gl.GL_TRIANGLE_STRIP,
		},
	}

	self.rectObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
in vec2 vertex;
uniform mat4 mvProjMat;
uniform vec4 bbox;
void main() {
	gl_Position = mvProjMat * vec4(
		bbox.x + bbox.z * vertex.x,
		bbox.y + bbox.w * vertex.y,
		0.,
		1.);
}
]],
			fragmentCode = [[
out vec4 fragColor;
uniform vec4 color;
void main() {
	fragColor = color;
}
]],
		},
		vertexes = {
			data = {
				0, 0,
				1, 0,
				0, 1,
				1, 1,
			},
			dim = 2,
		},
		geometry = {
			mode = gl.GL_TRIANGLE_STRIP,
		},
		uniforms = {
			bbox = {0,0,1,1},
			color = {1,1,1,1},
		},
	}

	self.showTreasures = true
	self.treasureWindow = TreasureWindow{app=self}
	

	self.showEventTriggers = true
	self.eventTriggerWindow = EventTriggerWindow{app=self}
	
	self.showEntranceTriggers = true
	self.entranceTriggerWindow = EntranceTriggerWindow{app=self}

	self.showNPCs = true
	self.npcWindow = NPCWindow{app=self}

	self.scriptWindow = ScriptWindow{app=self}

	self.mapSize = vec2d()

	self:setMapIndex(cmdline[2] and assert(tonumber(cmdline[2])) or 0)
end

local function mat4x4mul(m, x, y, z, w)
	x = tonumber(x)
	y = tonumber(y)
	z = tonumber(z) or 0
	w = tonumber(w) or 1
	return
		m[0] * x + m[4] * y + m[ 8] * z + m[12] * w,
		m[1] * x + m[5] * y + m[ 9] * z + m[13] * w,
		m[2] * x + m[6] * y + m[10] * z + m[14] * w,
		m[3] * x + m[7] * y + m[11] * z + m[15] * w
end

require 'vec-ffi.vec4f'
local vec4x4fcol = require 'vec-ffi.create_vec4x4'{
	vectype = 'vec4x4fcol',
	ctype = 'vec4f',
	colMajor = true,
}
local mvInv = vec4x4fcol():setIdent()
local projInv = vec4x4fcol():setIdent()
function App:invTransform(x,y,z)
	x = tonumber(x)
	y = tonumber(y)
	z = tonumber(z) or 0
	x = -1 + 2 * x / tonumber(self.width)
	y = 1 - 2 * y / tonumber(self.height)
	mvInv:inv4x4(self.view.mvMat)
	projInv:inv4x4(self.view.projMat)
	local w = 1
	x,y,z,w = mat4x4mul(projInv.ptr,x,y,z,w)
	x,y,z,w = mat4x4mul(mvInv.ptr,x,y,z,w)
	return x,y,z,w
end

function App:centerView(x, y)
	self.view.pos.x = tonumber(x)+.5
	self.view.pos.y = -tonumber(y)-.5
end

function App:getMapInfo()
	return game.getMap(self.mapIndex)
end

function App:setMapIndex(mapIndex)
	self.mapIndex = mapIndex
	self.npcIndex = 0

	if self.palTex then
		self.palTex:delete()
		self.palTex = nil
	end
	if self.layerTexs then
		for _,tex in ipairs(self.layerTexs) do
			tex:delete()
		end
		self.layerTexs = nil
	end

	collectgarbage()

	local mapInfo = game.getMap(mapIndex)
	if not mapInfo then
		print("map "..mapIndex.." missing")
		return
	end

	local map = mapInfo.map
	local paletteIndex = tonumber(map.palette)
	local gfxLayer3 = mapInfo.gfxLayer3
	local tilesetDatas = mapInfo.tilesetDatas
	local layerPos = mapInfo.layerPos
	local layerSizes = mapInfo.layerSizes
	local layouts = mapInfo.layouts
	local palette = mapInfo.palette
	local tilePropsData = mapInfo.tilePropsData

	print('maps[0x'..number.hex(mapIndex)..'] addr '
		..'0x'..number.hex(ffi.cast('uint8_t*', map) - rom)
		..' = '..map[0])

	local gfxstr = mapInfo.gfxIndexes:mapi(tostring):concat'/'
	for i=1,2 do
		local tilesetData = tilesetDatas[i]
		print('map tileset'..i..' data size', tilesetData and #tilesetData)
	end

	for i=1,3 do
		print('map layer '..i..' size', layerSizes[i], 'volume', layerSizes[i]:volume())
		print('map layout'..i..' data size', layouts[i] and #layouts[i].data)
		if i > 1 then
			print('map layer'..i..' pos', layerPos[i])
		end
	end

	local palette = mapInfo.palette

	if not palette then
		print("map "..mapIndex.." has no palette")
		return
	end

	local palData = ffi.new'uint8_t[128*4]'
	ffi.fill(palData, ffi.sizeof(palData))
	for i=0,127 do
		for j=0,3 do
			palData[bit.bor(j,bit.lshift(i, 2))] = palette[i+1][j+1]
		end
	end
	if self.palTex then
		self.palTex:delete()
	end
	self.palTex = GLTex2D{
		width = 16*8,
		height = 1,
		internalFormat = gl.GL_RGBA,
		format = gl.GL_RGBA,
		type = gl.GL_UNSIGNED_BYTE,
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_NEAREST,
		data = palData,
	}:unbind()
assert.eq(self.palTex.data, palData)

	-- layer images already have 16x16 tiles baked into them...
	local imgToTex = function(img)
		local tex = GLTex2D{
			width = img.width,
			height = img.height,
			internalFormat = gl.GL_R8UI,
			format = gl.GL_RED_INTEGER,
			type = gl.GL_UNSIGNED_BYTE,
			minFilter = gl.GL_NEAREST,
			magFilter = gl.GL_NEAREST,
			data = img.buffer,
		}:unbind()
		tex.image = img
		return tex
	end
	local layerImgs, layerAnimImgs = mapInfo:getLayerImages()

	if self.layerTexs then
		for _,tex in ipairs(self.layerTexs) do
			tex:delete()
		end
	end
	self.layerTexs = layerImgs:mapi(imgToTex)

	if self.layerAnimTexs then
		for z, layerAnimImgs_z in pairs(self.layerAnimTexs) do
			for layer, layerAnimImgs_z_layer in pairs(layerAnimImgs_z) do
				for _,tex in ipairs(layerAnimImgs_z_layer) do
					tex:delete()
				end
			end
		end
	end
	self.layerAnimTexs = table()
	for z, layerAnimImgs_z in pairs(layerAnimImgs) do
		self.layerAnimTexs[z] = table()
		for layer, layerAnimImgs_z_layer in pairs(layerAnimImgs_z) do
			self.layerAnimTexs[z][layer] = layerAnimImgs_z_layer:mapi(imgToTex)
		end
	end

	if #self.layerTexs > 0 then
		self.mapSize:set(self.layerTexs[1].width, self.layerTexs[1].height)
	end

	if self.tilePropsTex then
		self.tilePropsTex:delete()
	end
	self.tilePropsTex = nil
	local layout1Data = layouts[1] and layouts[1].data
	if layout1Data then
		-- uint8_t into the tilePropsPtr table, which is a table of 2-byte-sized either WorldTileProps_t or mapTileProps_t
		local layoutptr = ffi.cast('uint8_t*', layout1Data)
		--if tilePropsData then
		--local tilePropsPtr = ffi.cast('WorldTileProps_t*', tilePropsData)

		self.tilePropsTex = GLTex2D{
			width = layerSizes[1].x,
			height = layerSizes[1].y,
			internalFormat = gl.GL_R8UI,
			format = gl.GL_RED_INTEGER,
			type = gl.GL_UNSIGNED_BYTE,
			minFilter = gl.GL_NEAREST,
			magFilter = gl.GL_NEAREST,
			data = layout1Data,
		}:unbind()
	end
end


-- called by update and called by save ...
function App:draw(animFrameIndex)
	self.layerDrawObj.uniforms.mvProjMat = self.view.mvProjMat.ptr

	local drawTex = function(tex)
		self.layerDrawObj.texs[2] = self.palTex
		local blend = tex.image.blend
		if self.useBlend and blend then
			gl.glEnable(gl.GL_BLEND)
			if bit.band(blend, 2) ~= 0 then -- sub
				--gl.glBlendEquation(gl.GL_FUNC_SUBTRACT)		-- sprite minus framebuffer
				gl.glBlendEquation(gl.GL_FUNC_REVERSE_SUBTRACT)	-- framebuffer minus sprite
			else
				gl.glBlendEquation(gl.GL_FUNC_ADD)
			end
			local half = bit.band(blend, 1) ~= 0
			gl.glBlendColor(1, 1, 1, half and .5 or 1)
			if half then
				gl.glBlendFunc(gl.GL_CONSTANT_ALPHA, gl.GL_CONSTANT_ALPHA)
			else
				gl.glBlendFunc(gl.GL_ONE, gl.GL_ONE)
			end
		end

		gl.glEnable(gl.GL_ALPHA_TEST)
		gl.glAlphaFunc(gl.GL_GREATER, .5)

		self.layerDrawObj.texs[1] = tex
		self.layerDrawObj.uniforms.mask = 0xff
		self.layerDrawObj.uniforms.offset = 0
		self.layerDrawObj:draw()

		gl.glDisable(gl.GL_BLEND)
		gl.glDisable(gl.GL_ALPHA_TEST)
	end

	local mapInfo = game.getMap(self.mapIndex)
	local map = mapInfo and mapInfo.map
	if self.showAnimTexs then
		if map
		and self.layerAnimTexs
		then
			-- also in maps.lua ...
			local zAndLayers = map.layer3Priority == 0
				and zAndLayersWithoutLayer3Priority
				or zAndLayersWithLayer3Priority
			for _,zAndLayer in ipairs(zAndLayers) do
				local z, layer = table.unpack(zAndLayer)
				local k = 'showAnimTex_'..z..'_'..layer
				if self[k] == nil then self[k] = true end
				if self[k]
				and self.layerAnimTexs[z]
				and self.layerAnimTexs[z][layer]
				then
					local animTexs = self.layerAnimTexs[z][layer]
					if animTexs and #animTexs > 0 then
						drawTex(animTexs[animFrameIndex % #animTexs + 1])
					end
				end
			end
		end
	else
		if self.layerTexs then
			for i,tex in ipairs(self.layerTexs) do
				local k = 'drawLayer'..i
				if self[k] == nil or self[k] == true then
					drawTex(tex)
				end
			end
		end
	end
end

function App:update()
	gl.glClear(gl.GL_COLOR_BUFFER_BIT)

	local view = self.view
	-- mapSize is in texels
	-- so now coords are in 16x16 tiles
	view.mvMat:applyScale(1, -1)
	view.mvMat:applyScale(self.mapSize.x / 16, self.mapSize.y / 16, 1)
	view.mvProjMat:mul4x4(view.projMat, view.mvMat)

	-- draw layers blended together
	self:draw(math.floor(timer.getTime() * self.animSpeed) % 8)	-- mod by max anim frame gcd

	-- draw overlays of things in the map:

	gl.glEnable(gl.GL_BLEND)
	gl.glBlendEquation(gl.GL_FUNC_ADD)
	gl.glBlendColor(1, 1, 1, .5)
	gl.glBlendFunc(gl.GL_CONSTANT_ALPHA, gl.GL_CONSTANT_ALPHA)

	-- TODO enable/disable flags with this ...
	if self.showTileProps
	and self.tilePropsTex
	then
		self.layerDrawObj.texs[1] = self.tilePropsTex
		self.layerDrawObj.uniforms.mask = self.showTileMask
		self.layerDrawObj.uniforms.offset = self.showTileOfs
		self.layerDrawObj:draw()
	end


	local mapInfo = game.getMap(self.mapIndex)
	if mapInfo then
		view:setupModelView()
		view.mvMat:applyScale(1, -1)
		view.mvProjMat:mul4x4(view.projMat, view.mvMat)

		local rectObj = self.rectObj
		local uniforms = rectObj.uniforms
		uniforms.mvProjMat = view.mvProjMat.ptr

		local function showHL()
			local x,y,w,h = table.unpack(uniforms.bbox)
			local eps = .1
			settable(uniforms.color, 1,1,1,1)
			settable(uniforms.bbox, x-eps, y-eps, 2*eps, 1+2*eps)
			rectObj:draw()
			settable(uniforms.bbox, x-eps, y-eps, 1+2*eps, 2*eps)
			rectObj:draw()
			settable(uniforms.bbox, x+1-eps, y-eps, 2*eps, 1+2*eps)
			rectObj:draw()
			settable(uniforms.bbox, x-eps, y+1-eps, 1+2*eps, 2*eps)
			rectObj:draw()
		end

		local mx, my, mz, mw = self:invTransform(self.mouse.pos.x * self.width, self.mouse.pos.y * self.height)
mx = mx + self.view.pos.x
my = my + self.view.pos.y	-- why isn't htis in the matrix and therefore in invTransform ?
my = -my	-- oonce again, why ???? it's like i'm uisng the wrong mv matrix

		local leftPress = self.mouse.leftPress

		if self.showTreasures then
			for i,t in ipairs(mapInfo.treasures) do
				local x, y = tonumber(t.pos.x), tonumber(t.pos.y)
				if leftPress
				and x <= mx and mx <= x+1
				and y <= my and my <= y+1
				then
					self.treasureWindow.index = i-1
					self.treasureWindow.show = true
				end
				settable(uniforms.bbox, x, y, 1, 1)
				settable(uniforms.color, 0,0,1,1)
				rectObj:draw()
				if i-1 == self.treasureWindow.index then
					showHL()
				end
			end
		end
		if self.showEventTriggers then
			for i,e in ipairs(mapInfo.eventTriggers) do
				local x, y = tonumber(e.pos.x), tonumber(e.pos.y)
				if leftPress
				and x <= mx and mx <= x+1
				and y <= my and my <= y+1
				then
					self.eventTriggerWindow.index = i-1
					self.eventTriggerWindow.show = true
				end
				settable(uniforms.bbox, x, y, 1, 1)
				settable(uniforms.color, 0,0,1,1)
				rectObj:draw()
				if i-1 == self.eventTriggerWindow.index then
					showHL()
				end
			end
		end
		if self.showEntranceTriggers then
			for i,e in ipairs(mapInfo.entranceTriggers) do
				local x, y = tonumber(e.pos.x), tonumber(e.pos.y)
				if leftPress
				and x <= mx and mx <= x+1
				and y <= my and my <= y+1
				then
					self.entranceTriggerWindow.index = i-1
					self.entranceTriggerWindow.show = true
				end
				settable(uniforms.bbox, x, y, 1, 1)
				settable(uniforms.color, 1,0,0,1)
				rectObj:draw()
				if i-1 == self.entranceTriggerWindow.index then
					showHL()
				end
			end
		end
		if self.showNPCs then
			for i,n in ipairs(mapInfo.npcs) do
				local x, y = tonumber(n.x), tonumber(n.y)
				if leftPress
				and x <= mx and mx <= x+1
				and y <= my and my <= y+1
				then
					self.npcWindow.index = i-1
					self.npcWindow.show = true
				end			
				settable(uniforms.bbox, x, y, 1, 1)
				settable(uniforms.color, 0,1,0,1)
				rectObj:draw()
				if i-1 == self.npcWindow.index then
					showHL()
				end
			end
		end
	end

	gl.glDisable(gl.GL_BLEND)

	-- draw gui
	App.super.update(self)
end

function App:updateGUI()
	local mapInfo = game.getMap(self.mapIndex)
	local map = mapInfo and mapInfo.map

	if ig.igBeginMainMenuBar() then
		if ig.igBeginMenu'map' then
			ig.luatableTooltipCheckbox('useBlend', self, 'useBlend')
			if self.layerTexs then
				for i=1,#self.layerTexs do
					if i > 1 then
						ig.igSameLine()
					end
					local k = 'drawLayer'..i
					if self[k] == nil then self[k] = true end
					ig.luatableTooltipCheckbox('tex '..i, self, k)
				end
			end
			if self.layerAnimTexs then
				ig.luatableTooltipCheckbox('showAnimTexs', self, 'showAnimTexs')

				if self.layerAnimTexs then
					local zAndLayers = map.layer3Priority == 0
						and zAndLayersWithoutLayer3Priority
						or zAndLayersWithLayer3Priority
					for _,zAndLayer in ipairs(zAndLayers) do
						local z, layer = table.unpack(zAndLayer)
						if self.layerAnimTexs[z]
						and self.layerAnimTexs[z][layer]
						then
							local k = 'showAnimTex_'..z..'_'..layer
							if self[k] == nil then self[k] = true end
							ig.igSameLine()
							ig.luatableTooltipCheckbox(k, self, k)
						end
					end
				end
			end

			if self.layerAnimTexs then
				local doSaveLayerPNGs = ig.igButton'save layer pngs'
				local doSaveGIF = ig.igButton'save animated gif'
				if doSaveLayerPNGs
				or doSaveGIF
				then
					local animScreenshotPath = path'vis-map-animframes'
					animScreenshotPath:mkdir(true)

					local numFrames = 1
					local zAndLayers = map.layer3Priority == 0
						and zAndLayersWithoutLayer3Priority
						or zAndLayersWithLayer3Priority
					for _,zAndLayer in ipairs(zAndLayers) do
						local z, layer = table.unpack(zAndLayer)
						if self.layerAnimTexs[z]
						and self.layerAnimTexs[z][layer]
						then
							numFrames = math.max(numFrames, #self.layerAnimTexs[z][layer])
							if doSaveLayerPNGs then
								for frameIndex,frameTex in ipairs(self.layerAnimTexs[z][layer]) do
									frameTex.image:save((animScreenshotPath/(
										'map'..self.mapIndex
										..'_z='..z
										..'_layer='..layer
										..'_frame='..frameIndex
										..'.png'
									)).path)
								end
							end
						end
					end

					-- save a composite image while we're here
					local GLPingPong = require 'gl.pingpong'
					local pp = GLPingPong{
						numBuffers = 1,
						width = tonumber(self.mapSize.x),
						height = tonumber(self.mapSize.y),
						internalFormat = gl.GL_RGBA,
						format = gl.GL_RGBA,
						type = gl.GL_UNSIGNED_BYTE,
						minFilter = gl.GL_NEAREST,
						magFilter = gl.GL_NEAREST,
					}
					local fboTex = pp:cur()


					local pushMvMat = self.view.mvMat:clone()
					local pushProjMat = self.view.projMat:clone()
					local view = self.view
					view.mvMat
						:setIdent()
						:applyScale(1, -1)
						:applyScale(fboTex.width / 16, fboTex.height / 16, 1)
					view.projMat:setOrtho(0, fboTex.width / 16, -fboTex.height / 16, 0, -1000, 1000)
					view.mvProjMat:mul4x4(view.projMat, view.mvMat)

					gl.glViewport(0, 0, fboTex.width, fboTex.height)

					local compositeImgs = table()
					for frameIndex=1,numFrames do
						local fbo = pp.fbo
						fbo:bind()
							:setColorAttachmentTex2D(fboTex.id, 0)
							:drawBuffers(gl.GL_COLOR_ATTACHMENT0)
						assert(fbo:check())

						gl.glClearColor(0,0,0,1)
						gl.glClear(gl.GL_COLOR_BUFFER_BIT)

						self:draw(frameIndex-1)

						-- readpixels while we're here ...
						local image = Image(fboTex.width, fboTex.height, 4, 'uint8_t')	-- TODO tex :getChannels() :getCType()
						gl.glReadBuffer(gl.GL_COLOR_ATTACHMENT0)
						gl.glReadPixels(
							0, 0, fboTex.width, fboTex.height,
							gl.GL_RGBA, --fboTex.format,
							gl.GL_UNSIGNED_BYTE, --fboTex.type,
							image.buffer
						)
						gl.glReadBuffer(gl.GL_BACK)

						image = image:flip()
						compositeImgs:insert(image)
						if doSaveLayerPNGs then
							image:save((animScreenshotPath/(
								'map'..self.mapIndex
								..'_composite'
								..'_frame='..frameIndex
								..'.png'
							)).path)
						end

						fbo:unbind()
					end

					-- save the whole as an animation
					if doSaveGIF then
						compositeImgs[1]:save(
							(animScreenshotPath/('map'..self.mapIndex..'_animated.gif')).path,
							compositeImgs:unpack(2)
						)
					end

					gl.glViewport(0, 0, self.width, self.height)

					self.view.mvMat:copy(pushMvMat)
					self.view.projMat:copy(pushProjMat)
					self.view.mvProjMat:mul4x4(self.view.projMat, self.view.mvMat)
				end
			end

			--[[ TODO flgs for both WorldTileProps_t and mapTileProps_t
			for name in WorldTileProps_t:fielditer() do
				ig.luatableTooltipCheckbox('traversible '..name, self.traverseFlags, name)
			end
			--]]
			-- [[ until then
			local layouts = mapInfo and mapInfo.layouts
			local layout1Data = layouts and layouts[1] and layouts[1].data
			if layout1Data then
				ig.luatableTooltipCheckbox('showTileProps', self, 'showTileProps')
				ig.luatableInputInt('showTileMask', self, 'showTileMask')
				ig.luatableInputInt('showTileOfs', self, 'showTileOfs')
			end
			--]]

			ig.luatableInputFloat('animSpeed', self, 'animSpeed')

			ig.igEndMenu()
		end

		--[[
		local label = 'viewing map #'..self.mapIndex
		if ig.igBeginMenu(label) then
			ig.igEndMenu()
		end
		--]]

		if ig.igBeginMenu'view' then
			if ig.igButton'reset view' then
				self.view.ortho = true
				self.view.orthoSize = 256
				self.view.pos:set(0,0,10)
				self.view.orbit:set(0,0,0)
				self.view.angle:set(0,0,0,1)
			end

			ig.igEndMenu()
		end

		ig.igEndMainMenuBar()
	end

	if mapInfo then
		if ig.igBegin('map', nil, 0) then
			if ig.luatableInputInt('mapIndex', self, 'mapIndex') then
				self:setMapIndex(math.clamp(math.floor(self.mapIndex), 0, countof(game.maps)-1))
			end

			ig.luatableTooltipCheckbox('showTreasures', self, 'showTreasures')
			ig.igSameLine()
			self.treasureWindow:popupButton()

			ig.luatableTooltipCheckbox('showEventTriggers', self, 'showEventTriggers')
			ig.igSameLine()
			self.eventTriggerWindow:popupButton()

			ig.luatableTooltipCheckbox('showEntranceTriggers', self, 'showEntranceTriggers')
			ig.igSameLine()
			self.entranceTriggerWindow:popupButton()

			ig.luatableTooltipCheckbox('showNPCs', self, 'showNPCs')
			ig.igSameLine()
			self.npcWindow:popupButton()

			local map = mapInfo.map
			if map then
				ig.igText('map #'..self.mapIndex)
				for name in map[0]:fielditer() do
					ig.igText(' '..name..' = '..tostring(map[0][name]))
				end
			end

			-- TODO hyperlink to popup the formation-list window
			ig.igText'battles:'
			local formationCounts = {}
			if mapInfo.monsterRandomBattleOptionIndex >= 0
			and mapInfo.monsterRandomBattleOptionIndex < countof(game.monsterRandomBattles)
			then
				local mapMonsterRandomBattles = game.monsterRandomBattles + mapInfo.monsterRandomBattleOptionIndex
				for j=0,3 do
					local formationEntry = mapMonsterRandomBattles.s[j]
					local formationIndex = formationEntry.formation
					local formationDesc = 'formation=0x'..number.hex(formationIndex)..':'
					if formationIndex < game.numFormations then
						local formation = game.formations + formationIndex
						local monsterCounts = {}
						for k=1,6 do
							if formation:getMonsterActive(k) then
								local monsterIndex = formation:getMonsterIndex(k)
								local key = '#'..monsterIndex
								if monsterIndex < game.numMonsters then
									key = key ..':'..tostring(game.monsterNames[monsterIndex])
								end
								monsterCounts[key] = (monsterCounts[key] or 0) + 1
							end
						end
						formationDesc = formationDesc .. ' '
						..table.keys(monsterCounts):sort():mapi(function(key)
							local count = monsterCounts[key]
							if count == 1 then return key end
							return key..' x'..count
						end):concat', '
					end
					if formationEntry.chooseFromNextFour ~= 0 then
						formationDesc = formationDesc .. ' (chooseFromNextFour)'
					end
					formationCounts[formationDesc] = (formationCounts[formationDesc] or 0) + (j == 3 and 1 or 5)
					--local formation2 = game.formation2s + mapInfo.monsterRandomBattleOptionIndex
				end
				for _,formationDesc in ipairs(table.keys(formationCounts):sort()) do
					local formationCount = formationCounts[formationDesc]
					ig.igText(' '..formationCount..'/16 '..formationDesc)
				end
			end	
		end
		ig.igEnd()
	end

	self.treasureWindow:update()
	self.eventTriggerWindow:update()
	self.entranceTriggerWindow:update()
	self.npcWindow:update()
	self.scriptWindow:update()
end

function App:event(e)
	App.super.event(self, e)
	--local canHandleMouse = not ig.igGetIO()[0].WantCaptureMouse
	local canHandleKeyboard = not ig.igGetIO()[0].WantCaptureKeyboard

	if canHandleKeyboard then
		if e.type == sdl.SDL_EVENT_KEY_UP then
			if e.key.key == sdl.SDLK_LEFT then
				self:setMapIndex(math.clamp(math.floor(self.mapIndex - 1), 0, countof(game.maps)-1))
			elseif e.key.key == sdl.SDLK_RIGHT then
				self:setMapIndex(math.clamp(math.floor(self.mapIndex + 1), 0, countof(game.maps)-1))
			end
		end
	end
end

return App():run()
