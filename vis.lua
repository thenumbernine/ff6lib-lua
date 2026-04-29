#!/usr/bin/env luajit
local cmdline = require 'ext.cmdline'(...)
local ffi = require 'ffi'
local table = require 'ext.table'
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
	self.showTreasures = true
	self.showEventTriggers = true
	self.showEntranceTriggers = true
	self.showNPCs = true

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

	self.mapSize = vec2d()

	self.mapIndex = cmdline[2] and assert(tonumber(cmdline[2])) or 0
	self:updateMapIndex()
end

function App:updateMapIndex()
	local mapIndex = self.mapIndex
print('updateMapIndex', mapIndex)

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
		self.rectObj.uniforms.mvProjMat = view.mvProjMat.ptr

		if self.showTreasures then
			for _,t in ipairs(mapInfo.treasures) do
				self.rectObj.uniforms.bbox[1] = t.pos.x
				self.rectObj.uniforms.bbox[2] = t.pos.y
				self.rectObj.uniforms.bbox[3] = 1
				self.rectObj.uniforms.bbox[4] = 1
				self.rectObj.uniforms.color = {0,0,1,1}
				self.rectObj:draw()
			end
		end
		if self.showEventTriggers then
			for _,e in ipairs(mapInfo.eventTriggers) do
				self.rectObj.uniforms.bbox[1] = e.pos.x
				self.rectObj.uniforms.bbox[2] = e.pos.y
				self.rectObj.uniforms.bbox[3] = 1
				self.rectObj.uniforms.bbox[4] = 1
				self.rectObj.uniforms.color = {0,0,1,1}
				self.rectObj:draw()
			end
		end
		if self.showEntranceTriggers then
			for _,e in ipairs(mapInfo.entranceTriggers) do
				self.rectObj.uniforms.bbox[1] = e.pos.x
				self.rectObj.uniforms.bbox[2] = e.pos.y
				self.rectObj.uniforms.bbox[3] = 1
				self.rectObj.uniforms.bbox[4] = 1
				self.rectObj.uniforms.color = {1,0,0,1}
				self.rectObj:draw()
			end
		end
		if self.showNPCs then
			for _,n in ipairs(mapInfo.npcs) do
				self.rectObj.uniforms.bbox[1] = n.x
				self.rectObj.uniforms.bbox[2] = n.y
				self.rectObj.uniforms.bbox[3] = 1
				self.rectObj.uniforms.bbox[4] = 1
				self.rectObj.uniforms.color = {0,1,0,1}
				self.rectObj:draw()
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
				self.mapIndex = math.clamp(math.floor(self.mapIndex), 0, countof(game.maps)-1)
				self:updateMapIndex()
			end

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
			ig.igEnd()
		end

		if ig.igBegin('objects', nil, 0) then
			ig.luatableCheckbox('showTreasures', self, 'showTreasures')
			ig.luatableCheckbox('showEventTriggers', self, 'showEventTriggers')
			ig.luatableCheckbox('showEntranceTriggers', self, 'showEntranceTriggers')
			ig.luatableCheckbox('showNPCs', self, 'showNPCs')

			ig.igText('# treasures = '..tostring(#mapInfo.treasures))
			for i,t in ipairs(mapInfo.treasures) do
				ig.igText('treasure #'..(i-1))
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

			ig.igText('# event triggers = '..tostring(#mapInfo.eventTriggers))
			for i,e in ipairs(mapInfo.eventTriggers) do
				ig.igText('event trigger #'..(i-1))
				ig.igText(' pos = '..e.pos)
				ig.igText(' event code = $'..number.hex(e.eventCode:value()))
			end

			ig.igText('# entrance triggers = '..tostring(#mapInfo.entranceTriggers))
			for i,e in ipairs(mapInfo.entranceTriggers) do
				ig.igText('entrance trigger #'..(i-1))
				ig.igText(' pos = '..e.pos)
				ig.igText(' map = '..e.mapIndex)
				ig.igText(' setParentMap = '..e.setParentMap)
				ig.igText(' zLevel = '..e.zLevel)
				ig.igText(' showDestName = '..e.showDestName)
				ig.igText(' destFacingDir = '..e.destFacingDir)
				ig.igText(' unknown_3_6 = '..e.unknown_3_6)
				ig.igText(' dest = '..e.dest)
			end

			ig.igText('# NPCs = '..tostring(#mapInfo.npcs))
			for i,n in ipairs(mapInfo.npcs) do
				ig.igText('npc #'..(i-1))
				ig.igText(' pos = '..n.x..', '..n.y)

				ig.igText(' script '..n.script)
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

			ig.igEnd()
		end
	end
end

function App:event(e)
	App.super.event(self, e)
	--local canHandleMouse = not ig.igGetIO()[0].WantCaptureMouse
	local canHandleKeyboard = not ig.igGetIO()[0].WantCaptureKeyboard

	if canHandleKeyboard then
		if e.type == sdl.SDL_EVENT_KEY_UP then
			if e.key.key == sdl.SDLK_LEFT then
				self.mapIndex = math.clamp(math.floor(self.mapIndex - 1), 0, countof(game.maps)-1)
				self:updateMapIndex()
			elseif e.key.key == sdl.SDLK_RIGHT then
				self.mapIndex = math.clamp(math.floor(self.mapIndex + 1), 0, countof(game.maps)-1)
				self:updateMapIndex()
			end
		end
	end
end

return App():run()
