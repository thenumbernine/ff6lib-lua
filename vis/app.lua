local ffi = require 'ffi'
local table = require 'ext.table'
local range = require 'ext.range'
local math = require 'ext.math'
local assert = require 'ext.assert'
local timer = require 'ext.timer'
local path = require 'ext.path'
local vec2d = require 'vec-ffi.vec2d'
local vec3d = require 'vec-ffi.vec3d'
local vec4x4fcol = require 'vec-ffi.vec4x4fcol'
local LiteThread = require 'thread.lite'
local Semaphore = require 'thread.semaphore'
local vector = require 'stl.vector-lua'
local Image = require 'image'
local sdl = require 'sdl'
local sdlAssertNonNull = require 'sdl.assert'.nonnull
local gl = require 'gl'
local GLTex2D = require 'gl.tex2d'
local GLPingPong = require 'gl.pingpong'
local GLSceneObject = require 'gl.sceneobject'
local ig = require 'imgui'
local ImGuiAppWithOrbit = require 'imgui.appwithorbit'

local vis_util = require 'ff6.vis.util'
local zAndLayersWithoutLayer3Priority = vis_util.zAndLayersWithoutLayer3Priority
local zAndLayersWithLayer3Priority = vis_util.zAndLayersWithLayer3Priority
local numTilePropsBits = vis_util.numTilePropsBits
local settable = vis_util.settable


local intptr_t = ffi.typeof'intptr_t'


local startTime = timer.getTime()

local app	-- singleton, save for the sdl open file callback

local App = ImGuiAppWithOrbit()

App.hasFocus = true
App.title = 'FF6 Data Visualizer'

function App:initGL(...)
	app = self

	App.super.initGL(self, ...)

	self.view.ortho = true
	self.view.orthoSize = 16
	self.animSpeed = 15
	self.useBlend = true
	self.showTileMask = 0	--0xffff
	self.showAnimTexs = true
	self.autoReloadSRAM = true

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
precision highp usampler2D;	// needed by #version 300 es
uniform usampler2D tex;
uniform sampler2D palTex;
in vec2 tcv;
out vec4 fragColor;
void main() {
	int index = int(texture(tex, tcv, 0.).r);
	fragColor = texelFetch(palTex, ivec2(index, 0), 0);

	// GL_ALPHA_TEST isn't in GLES3:
	if (fragColor.a < .5) discard;
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

	self.rgbaTexObj = GLSceneObject{
		program = {
			version = 'latest',
			precision = 'best',
			vertexCode = [[
in vec2 vertex;
out vec2 tcv;
uniform mat4 mvProjMat;
uniform vec4 bbox;
void main() {
	tcv = vertex;
	gl_Position = mvProjMat * vec4(
		bbox.x + bbox.z * vertex.x,
		bbox.y + bbox.w * vertex.y,
		0.,
		1.);
}
]],
			fragmentCode = [[
uniform sampler2D tex;
in vec2 tcv;
out vec4 fragColor;
void main() {
	fragColor = texture(tex, tcv);

	// GL_ALPHA_TEST isn't in GLES3:
	if (fragColor.a < .5) discard;
}
]],
			uniforms = {
				tex = 0,
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
		uniforms = {
			bbox = {0,0,1,1},
		},
	}

	local colorsForBits = table{
		vec3d(1,.5,0),
		vec3d(0,1,0),
		vec3d(0,0,1),
		vec3d(0,1,1),
		vec3d(1,0,1),
		vec3d(1,1,0),
		vec3d(1,1,1),
		vec3d(.75, .75, .75),

		vec3d(1, .5, .5),
		vec3d(.5, 1, .5),
		vec3d(0,0,.5),
		vec3d(0,.5,.5),
		vec3d(.5,0,.5),
		vec3d(.5,.5,0),
		vec3d(.5,.5,.5),
		vec3d(.25, .25, .25),

		vec3d(1,0,.5),
		vec3d(1,0,0),
	}
	local tilePropsPalSize = 32
	assert.ge(tilePropsPalSize, numTilePropsBits)	-- TODO rup2 or something
	local tilePropPalData = ffi.new('uint8_t[?]', tilePropsPalSize * 4)
	ffi.fill(tilePropPalData, ffi.sizeof(tilePropPalData))
	for i=0,tilePropsPalSize-1 do
		local c = colorsForBits[(i % #colorsForBits) + 1]
		tilePropPalData[bit.bor(0, bit.lshift(i, 2))] = 255*c.x
		tilePropPalData[bit.bor(1, bit.lshift(i, 2))] = 255*c.y
		tilePropPalData[bit.bor(2, bit.lshift(i, 2))] = 255*c.z
		tilePropPalData[bit.bor(3, bit.lshift(i, 2))] = 1
	end
	self.tilePropsPalTex = GLTex2D{
		width = tilePropsPalSize,
		height = 1,
		internalFormat = gl.GL_RGBA,
		minFilter = gl.GL_NEAREST,
		magFilter = gl.GL_NEAREST,
		data = tilePropPalData,
	}:unbind()

	self.flagsDrawObj = GLSceneObject{
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
precision highp usampler2D;	// needed by #version 300 es
uniform usampler2D tex;
uniform sampler2D palTex;
uniform int palIndex;
uniform float alpha;
uniform float t;
in vec2 tcv;
out vec4 fragColor;
void main() {

	if (((int(gl_FragCoord.x) - int(gl_FragCoord.y) - int(t)) & 15) < 8) discard;
	//if (mod((gl_FragCoord.x + gl_FragCoord.y) / 10., 1.) < .5) discard;

	int flags = int(texture(tex, tcv, 0.).r);
	flags &= 1 << palIndex;
	if (flags == 0) discard;
	fragColor = texelFetch(palTex, ivec2(palIndex, 0), 0);
	fragColor.a = alpha;
}
]],
			uniforms = {
				tex = 0,
				palTex = 1,
				alpha = .3,
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


	--[[
	args:
		name = menu name
	returns:
		function to call for showing the ig button and sdl popup
		function to call to fwd to callback with open name
	--]]
	local function makeControlForOpen()
		-- how to let the app know when SDL tells the sub-lua-state when it wants to open a file ...
		-- such a mess just to avoid the lua panic bad callback errors
		local sem = Semaphore()

		-- I had these as object-scope fields that only were allocated once i needed them
		-- but i kept getting panic errors upon save
		-- so here they are at global scope

		local liteThread = LiteThread{
			init = function(thread)
				thread.lua[[
-- set required modules as globals
ffi = require 'ffi'
Semaphore = require 'thread.semaphore'
require 'sdl'	-- load SDL_DialogFileCallback
sdlAssertNonNull = require 'sdl.assert'.nonnull
]]
				thread.lua([[
local semOpenID = ffi.cast('void*', ...)
sem = Semaphore:wrap(semOpenID)
]], ffi.cast(intptr_t, sem.id+0))
			end,
			threadFuncTypeName = 'SDL_DialogFileCallback',
			code = [[
local userdata, filelist, filter = ...
xpcall(function()
	sdlAssertNonNull(filelist)	-- error
	if filelist[0] == nil then return end	-- no file picked

	-- set a global for the app to read
	_G.callbackFilename  = ffi.string(filelist[0])

	-- tell the app to open the ROM
	sem:post()

end, function(err)
	print(err..'\n'..debug.traceback())
end)
]],
		}

		return {
			-- putting 'name' here instead of upon creation so imgui can keep pretending to be immediate-mode...
			show = function(control, name)
				if not ig.igButton(name) then return end

				sdl.SDL_ShowOpenFileDialog(
					liteThread.funcptr,	-- callback
					nil,				-- userdata
					self.window,		-- window
					nil,				-- filters
					0,					-- nfilters
					path:cwd().path,	-- default_location
					0					-- allow_many
				)
			end,
			check = function(control, callback)
				if not sem:trywait() then return end
				local fn = liteThread.lua.global.callbackFilename
				callback(fn)
			end,

			-- try to stop __gc's to stop segfaults
			sem = sem,
			liteThread = liteThread,
		}
	end

	local function makeControlForSave()
		local sem = Semaphore()

		-- do callbacks in a separate state to stop panic bad callbacks
		local liteThread = LiteThread{
			init = function(thread)
				thread.lua[[
-- set required modules as globals
ffi = require 'ffi'
Semaphore = require 'thread.semaphore'
require 'sdl'	-- load SDL_DialogFileCallback
sdlAssertNonNull = require 'sdl.assert'.nonnull
]]
				thread.lua([[
local semOpenID = ffi.cast('void*', ...)
sem = Semaphore:wrap(semOpenID)
]], ffi.cast(intptr_t, sem.id+0))
			end,
			threadFuncTypeName = 'SDL_DialogFileCallback',
			code = [[
local userdata, filelist, filter = ...
xpcall(function()
	sdlAssertNonNull(filelist)	-- error
	if filelist[0] == nil then return end	-- no file picked
	local fn = ffi.string(filelist[0])

	_G.callbackFilename = ffi.string(filelist[0])
	sem:post()

end, function(err)
	print(err..'\n'..debug.traceback())
end)
]],
		}

		-- now we can use liteThread.funcptr
		-- be sure to set liteThread's _G.app

		return {
			show = function(control, name)
				if not ig.igButton(name) then return end

				sdl.SDL_ShowSaveFileDialog(
					--_G.sdlSaveFileDialogClosure,	-- callback
					liteThread.funcptr,
					nil,							-- userdata
					self.window,					-- window
					nil,							-- filters
					0,								-- nfilters
					path:cwd().path					-- default_location
				)
			end,
			check = function(control, callback)
				if not sem:trywait() then return end
				local fn = liteThread.lua.global.callbackFilename
				callback(fn)
			end,

			-- try to stop __gc's to stop segfaults
			sem = sem,
			liteThread = liteThread,
		}
	end

	self.menuOpenROM = makeControlForOpen()
	self.menuSaveROM = makeControlForSave()

	self.menuOpenSRAM = makeControlForOpen()
	self.menuSaveSRAM = makeControlForSave()


	if cmdline[1] then
		self:onLoadROM(cmdline[1], cmdline[2])
	end

	if cmdline[3] then
		self:onLoadSRAM(cmdline[3], cmdline[4])
	elseif self.romPath then
		local sramPath = self.romPath:setext'srm'
		if sramPath:exists() then
			self:onLoadSRAM(sramPath)
		end
	end
end

function App:exit(...)
	if self.voxelmapWindow then
		self.voxelmapWindow:exit()
	end

	App.super.exit(self, ...)
end

function App:onLoadROM(infn, mapIndex)
	self.romPath = path(infn)

	local game = require 'ff6'((assert(self.romPath:read())))
	self.game = game


	-- help imgui out
	self.itemNames = range(0,255):mapi(function(i)
		return tostring(game.itemNames[i])
	end)


	local CharWindow = require 'ff6.vis.charwindow'
	self.charWindow = CharWindow{app=self}

	local SpriteWindow = require 'ff6.vis.spritewindow'
	self.spriteWindow = SpriteWindow{app=self}

	local MonsterWindow = require 'ff6.vis.monsterwindow'
	self.monsterWindow = MonsterWindow{app=self}

	local BattleFormationWindow = require 'ff6.vis.battleformationwindow'
	self.battleFormationWindow = BattleFormationWindow{app=self}

	local BattleOptionsWindow = require 'ff6.vis.battleoptionswindow'
	self.randomBattleOptionsWindow = BattleOptionsWindow.RandomBattleOptionsWindow{app=self}
	self.eventBattleOptionsWindow = BattleOptionsWindow.EventBattleOptionsWindow{app=self}

	self.baseWindows = table()
	self.baseWindows:append{
		self.charWindow,
		self.spriteWindow,
		self.monsterWindow,
		self.battleFormationWindow,
		self.randomBattleOptionsWindow,
		self.eventBattleOptionsWindow,
	}

	-- make mapWindow's windows first:
	self.showTiles = true
	local TileWindow = require 'ff6.vis.tilewindow'
	self.tileWindow = TileWindow{app=self}

	local TileSheetWindow = require 'ff6.vis.tilesheetwindow'
	self.layerTileSheetWindows = table()
	for i=1,2 do
		self.layerTileSheetWindows[i] = TileSheetWindow{app=self}
		self.layerTileSheetWindows[i].name = 'layer '..i..' tile sheet'
		self.layerTileSheetWindows[i].layerIndex = i
	end

	local TreasureWindow = require 'ff6.vis.treasurewindow'
	self.showTreasures = true
	self.treasureWindow = TreasureWindow{app=self}

	local TouchTriggerWindow = require 'ff6.vis.touchtriggerwindow'
	self.showTouchTriggers = true
	self.touchTriggerWindow = TouchTriggerWindow{app=self}

	local DoorWindow = require 'ff6.vis.doorwindow'
	self.showDoors = true
	self.doorWindow = DoorWindow{app=self}

	local BigDoorWindow = require 'ff6.vis.bigdoorwindow'
	self.showBigDoors = true
	self.bigDoorWindow = BigDoorWindow{app=self}

	local NPCWindow = require 'ff6.vis.npcwindow'
	self.showNPCs = true
	self.npcWindow = NPCWindow{app=self}


	local WorldEncounterSectorWindow = require 'ff6.vis.worldencountersectorwindow'
	self.showWorldEncounterSectors = true
	self.worldEncounterSectorWindow = WorldEncounterSectorWindow{app=self}

	-- then make mapWindow:
	local MapWindow = require 'ff6.vis.mapwindow'
	self.mapWindow = MapWindow{
		app = self,
		index = tonumber(mapIndex) or 0,
		show = true,
		children = table{
			self.tileWindow,
			self.treasureWindow,
			self.touchTriggerWindow,
			self.doorWindow,
			self.bigDoorWindow,
			self.npcWindow,
			self.worldEncounterSectorWindow,
		}:append(self.layerTileSheetWindows),
	}

	local ItemWindow = require 'ff6.vis.itemwindow'
	self.itemWindow = ItemWindow{app=self}

	local SpellWindow = require 'ff6.vis.spellwindow'
	self.spellWindow = SpellWindow{app=self}

	local EsperWindow = require 'ff6.vis.esperwindow'
	self.esperWindow = EsperWindow{app=self}

	local ShopWindow = require 'ff6.vis.shopwindow'
	self.shopWindow = ShopWindow{app=self}

	local MetamorphWindow = require 'ff6.vis.metamorphwindow'
	self.metamorphWindow = MetamorphWindow{app=self}

	local ScriptWindow = require 'ff6.vis.scriptwindow'
	self.scriptWindow = ScriptWindow{app=self}

	local SRAMWindow = require 'ff6.vis.sramwindow'
	self.sramWindow = SRAMWindow{app=self}

	-- idk i guess this is a 'child of' map window whatever that means
	-- but i'm still putting it as a base window
	local VoxelmapWindow = require 'ff6.vis.voxelmapwindow'
	self.showVoxelmapFloodFills = true
	self.voxelmapWindow = VoxelmapWindow{app=self}

	-- base-level not dependent on another window:
	self.baseWindows:append{
		self.mapWindow,
		self.itemWindow,
		self.spellWindow,
		self.esperWindow,
		self.shopWindow,
		self.metamorphWindow,
		self.scriptWindow,
		self.sramWindow,
		self.voxelmapWindow,
	}

	self.scriptWindow.show[0] = false	-- who keeps opening this?
end

function App:onLoadSRAM(fn, index)
	self.sramPath = path(fn)
	local game = self.game
	assert(game, "the menu to load shouldn't be active ... did you only pass the 3rd cli arg or something?")
	local sramstr = assert(self.sramPath):read()
	assert.eq(ffi.sizeof(game.SRAM), #sramstr)	-- make sure it fits
	-- hmm do I need the vector version? or just the SRAM version, and union it with a .s[] field?
	self.sramvec = vector('uint8_t', #sramstr)
	self.sram = ffi.cast(ffi.typeof('$*', game.SRAM), self.sramvec.v)
	ffi.copy(self.sramvec.v, sramstr, #sramstr)

	-- 'modification' is change to file contents...
	self.sramLastWriteTime = ffi.new('struct timespec', self.sramPath:attr().modification_ns)

	index = index and tonumber(index) or 0

	self.sramWindow:open(index)
	local save = self.sramWindow:getCurIndex()
	self.mapWindow:open(save.map)
	-- pos, pos2, pos3, mapPos, lastTownPos, airshipPos ... which to use ...
	if save.map < 3 then
		self.tileWindow:setXY(save.mapPos.x, save.mapPos.y)
		self:centerView(save.mapPos.x, save.mapPos.y)
	else
		self.tileWindow:setXY(save.pos.x, save.pos.y)
		self:centerView(save.pos.x, save.pos.y)
	end
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


-- called by update and called by save ...
function App:draw(animFrameIndex)
	self.layerDrawObj.uniforms.mvProjMat = self.view.mvProjMat.ptr

	local drawTex = function(tex)
		local blend = tex.image.blend
		if self.useBlend and blend then
			gl.glEnable(gl.GL_BLEND)
			if bit.band(blend, 2) ~= 0 then -- sub
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

		--gl.glEnable(gl.GL_ALPHA_TEST)
		--gl.glAlphaFunc(gl.GL_GREATER, .5)

		self.layerDrawObj.texs[1] = tex
		self.layerDrawObj.texs[2] = self.mapWindow.mapPalTex
		self.layerDrawObj:draw()

		--gl.glDisable(gl.GL_ALPHA_TEST)
		gl.glDisable(gl.GL_BLEND)
	end

	local mapInfo = self.mapWindow:getMapInfo()
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
	-- open here somehow
	self.menuOpenROM:check(function(fn)
		self:onLoadROM(fn)
	end)
	self.menuSaveROM:check(function(fn)
		assert(path(fn):write((
			-- no ROM header for now
			self.game.romvec:dataToStr()
		)))
	end)
	self.menuOpenSRAM:check(function(fn)
		self:onLoadSRAM(fn)
	end)
	self.menuSaveSRAM:check(function(fn)
		for i=0,self.sramWindow:getCount()-1 do
			if self.sramWindow['calcChecksumOnSave'..i] then
				self.sramWindow:recalcChecksum(i)
			end
		end
		assert(path(fn)):write((
			self.sramvec:dataToStr()
		))
	end)

	local game = self.game

	-- even without blur
	local didReload
	if self.sram
	and self.autoReloadSRAM
	and self.sramLastWriteTime
	then
		local mtime = ffi.new('struct timespec', self.sramPath:attr().modification_ns)
		if mtime.tv_sec > self.sramLastWriteTime.tv_sec
		or (mtime.tv_sec == self.sramLastWriteTime.tv_sec
			and mtime.tv_nsec > self.sramLastWriteTime.tv_nsec)
		then
print('auto-reloading sram...')
			self:onLoadSRAM(self.sramPath)
			didReload = true
		end
	end

	if not didReload
	and not self.hasFocus
	then
		sdl.SDL_Delay(100)
		return
	end

	gl.glClear(gl.GL_COLOR_BUFFER_BIT)

	if game then
		local t = timer.getTime() - startTime
		self.frameIndex = math.floor(t * self.animSpeed)
		local view = self.view
		self.layerDrawObj.uniforms.mvProjMat = self.view.mvProjMat.ptr

		self.tooltipText = nil

		-- mapSize is in texels
		-- so now coords are in 16x16 tiles
		view.mvMat:applyScale(1, -1)
		view.mvMat:applyScale(self.mapSize.x / 16, self.mapSize.y / 16, 1)
		view.mvProjMat:mul4x4(view.projMat, view.mvMat)

		-- draw layers blended together
		self:draw(bit.band(self.frameIndex, 7))	-- mod by max anim frame gcd

		-- draw overlays of things in the map:

		gl.glEnable(gl.GL_BLEND)
		gl.glBlendEquation(gl.GL_FUNC_ADD)
		gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE)

		if self.layerDrawObj
		and self.tilePropsTex
		then
			--[[
			local totalBits = 0
			for i=0,numTilePropsBits-1 do
				if 0 ~= bit.band(bit.lshift(1, i), self.showTileMask) then
					totalBits = totalBits + 1
				end
			end
			--]]
			for i=0,numTilePropsBits-1 do
				if 0 ~= bit.band(bit.lshift(1, i), self.showTileMask) then
					self.flagsDrawObj.texs[1] = self.tilePropsTex
					self.flagsDrawObj.texs[2] = self.tilePropsPalTex
					self.flagsDrawObj.uniforms.t = 20 * t - i
					self.flagsDrawObj.uniforms.mvProjMat = view.mvProjMat.ptr
					self.flagsDrawObj.uniforms.palIndex = i
					--self.flagsDrawObj.uniforms.alpha = 1 / math.max(1, totalBits)	-- for additive blending
					self.flagsDrawObj.uniforms.alpha = .5
					self.flagsDrawObj:draw()
				end
			end
		end

		--gl.glBlendFunc(gl.GL_ONE, gl.GL_ONE)
		gl.glBlendFunc(gl.GL_SRC_ALPHA, gl.GL_ONE_MINUS_SRC_ALPHA)

		local mapInfo = self.mapWindow:getMapInfo()
		if mapInfo then
			local mapWidth, mapHeight = self.tileWindow:getMapSize()

			view:setupModelView()
			view.mvMat:applyScale(1, -1)
			view.mvProjMat:mul4x4(view.projMat, view.mvMat)

			local rectObj = self.rectObj
			local uniforms = rectObj.uniforms
			uniforms.mvProjMat = view.mvProjMat.ptr

			self.rgbaTexObj.uniforms.mvProjMat = view.mvProjMat.ptr

			local function showHL()
				local x,y,w,h = table.unpack(uniforms.bbox)
				local eps = .1
				settable(uniforms.color, 1,1,1,.5)
				settable(uniforms.bbox, x-eps, y-eps, 2*eps, h+2*eps)
				rectObj:draw()
				settable(uniforms.bbox, x-eps, y-eps, w+2*eps, 2*eps)
				rectObj:draw()
				settable(uniforms.bbox, x+w-eps, y-eps, 2*eps, h+2*eps)
				rectObj:draw()
				settable(uniforms.bbox, x-eps, y+h-eps, w+2*eps, 2*eps)
				rectObj:draw()
			end


			local mx, my, mz, mw = self:invTransform(self.mouse.pos.x * self.width, self.mouse.pos.y * self.height)
	mx = mx + self.view.pos.x
	my = my + self.view.pos.y	-- why isn't htis in the matrix and therefore in invTransform ?
	my = -my	-- oonce again, why ???? it's like i'm uisng the wrong mv matrix

-- if we're not mouse over a imgui window...
if self.canHandleMouse then
	self.tooltipText = math.floor(mx)..', '..math.floor(my)
end

			local leftPress = self.mouse.leftPress
			local leftDoubleClick = self.mouse.leftDoubleClick

			-- tempting to implement this as a bitflag layer ...
			if self.showVoxelmapFloodFills then
				self.voxelmapWindow:showTiles(mx, my, showHL)
			end

			if (self.mapWindow.index == 0 or self.mapWindow.index == 1)
			and self.showWorldEncounterSectors
			then
				for sectorIndex=0,0x3f do
					local x = bit.lshift(bit.band(sectorIndex, 7), 5)
					local y = bit.lshift(bit.band(bit.rshift(sectorIndex, 3), 7), 5)
					local w, h = 32, 32

					local i = bit.bor(sectorIndex, bit.lshift(self.mapWindow.index, 6))
					if (leftPress or leftDoubleClick)
					and x <= mx and mx < x+w
					and y <= my and my < y+h
					then
						self.worldEncounterSectorWindow:open(i)
						if leftDoubleClick then
							-- if we double-clicked then also bring up the battle-formation for the terrain-type
							-- TODO
							-- but is the terrain-type just the background?
							-- no, because terrain is 2 bits and background is 4 bits in WorldTileProps
							--[[
							backgrounds:	terrain:
							wob:
							0 = grass		0 = grass
							3 = forest		1 = forest
							2 = desert		2 = desert
							6 = veldt		... special code
							wor:
							4 = grass		0 = grass
							1 = forest		1 = forest
							2 = desert		2 = desert
							5 = dirt		3 = dirt
							--]]
							local tilePropsPtr = self.tileWindow:getPropsPtr()
							if tilePropsPtr then
								local battleBgIndex = ffi.cast(ffi.typeof('$*', game.WorldTileProps), tilePropsPtr).battleBG
								local terrainTypeIndex
								if battleBgIndex == 0 or battleBgIndex == 4 then
									terrainTypeIndex = 0	-- grass
								elseif battleBgIndex == 1 or battleBgIndex == 3 then
									terrainTypeIndex = 1	-- forest
								elseif battleBgIndex == 2 then
									terrainTypeIndex = 2	-- desert
								elseif battleBgIndex == 5 then
									terrainTypeIndex = 3	-- dirt
								elseif battleBgIndex == 6 then
									-- veldt
								elseif battleBgIndex == 7 then
									-- WoR ocean
								end
								if terrainTypeIndex then
									-- open the formation for this type
									local randomBattlesPerTerrain = game.worldSectorRandomBattlesPerTerrain + self.worldEncounterSectorWindow.index
									local formationIndex = randomBattlesPerTerrain[game.terrainTypes[terrainTypeIndex+1]]
									self.randomBattleOptionsWindow:open(formationIndex)

									-- and change the battle formation window too?
									if formationIndex < game.numFormations then
										local battleEntries = game.monsterRandomBattles + formationIndex
										self.battleFormationWindow:open(battleEntries.s[0].formation)
									end
								end
							end
						end
					end
					settable(uniforms.color, .7, .7, .7, .5)
					settable(uniforms.bbox, x, y, w, h)
					--rectObj:draw()
					if i == self.worldEncounterSectorWindow.index then
						showHL()
					end
				end
			end

			if self.showTiles then
				if mapWidth and mapHeight then
					-- show tile under mouse
					local x = math.floor(mx)
					local y = math.floor(my)
					if x >= 0 and y >= 0
					and x < mapWidth
					and y < mapHeight
					then
						settable(uniforms.bbox, x, y, 1, 1)
						settable(uniforms.color, 1,1,1,.5)
						showHL()
						if leftPress then
							local i = x + mapWidth * y
							self.tileWindow:open(i)
						end
					end
					-- show selected tile
					local x = self.tileWindow.index % mapWidth
					local y = (self.tileWindow.index - x) / mapWidth
					if x >= 0 and y >= 0
					and x < mapWidth
					and y < mapHeight
					then
						settable(uniforms.bbox, x, y, 1, 1)
						settable(uniforms.color, 1,1,1,.5)
						showHL()
					end
				end
			end
			if self.showTreasures then
				local save = self.sramWindow:getCurIndex()
				for i,t in ipairs(mapInfo.treasures) do
					local x, y = tonumber(t.pos.x), tonumber(t.pos.y)
					if leftPress
					and x <= mx and mx < x+1
					and y <= my and my < y+1
					then
						self.treasureWindow:open(i-1)
					end
					settable(uniforms.bbox, x, y, 1, 1)

					local got
					if save then
						local flag = t.flag
						got = 0 ~= bit.band(bit.lshift(1, bit.band(flag, 7)), save.treasureFlags[bit.rshift(flag, 3)])
						if got then
							settable(uniforms.color, 0,1,0,.5)
						else
							settable(uniforms.color, 1,0,0,.5)
						end
					else
						settable(uniforms.color, 0,0,1,.5)
					end

					rectObj:draw()
					if i-1 == self.treasureWindow.index then
						showHL()
					end
				end
			end
			if self.showTouchTriggers then
				for i,t in ipairs(mapInfo.touchTriggers) do
					local x, y = tonumber(t.pos.x), tonumber(t.pos.y)
					if (leftPress or leftDoubleClick)
					and x <= mx and mx < x+1
					and y <= my and my < y+1
					then
						self.touchTriggerWindow:open(i-1)
						if leftDoubleClick then
							local addr = t:getScriptAddr()
							if addr then
								self.scriptWindow:openScriptAddr(addr)
							end
						end
					end
					settable(uniforms.bbox, x, y, 1, 1)
					settable(uniforms.color, 1,0,1,.5)
					rectObj:draw()
					if i-1 == self.touchTriggerWindow.index then
						showHL()
					end
				end
			end
			if self.showDoors then
				for i,d in ipairs(mapInfo.doors) do
					local x, y = tonumber(d.pos.x), tonumber(d.pos.y)
					if (leftPress or leftDoubleClick)
					and x <= mx and mx < x+1
					and y <= my and my < y+1
					then
						self.doorWindow:open(i-1)
						-- double-click to quick-traverse map
						if leftDoubleClick then
							self.doorWindow:goThruDoor()
						end
					end
					settable(uniforms.bbox, x, y, 1, 1)
					settable(uniforms.color, 0,1,0,.5)
					rectObj:draw()
					if i-1 == self.doorWindow.index then
						showHL()
					end
				end
			end
			if self.showBigDoors then
				for i,d in ipairs(mapInfo.bigDoors) do
					local x, y = tonumber(d.pos.x), tonumber(d.pos.y)
					local w, h
					if d.vertical == 0 then
						w, h = d.length+1, 1
					else
						w, h = 1, d.length+1
					end
					if (leftPress or leftDoubleClick)
					and x <= mx and mx < x+w
					and y <= my and my < y+h
					then
						self.bigDoorWindow:open(i-1)
						-- double-click to quick-traverse map
						if leftDoubleClick then
							self.bigDoorWindow:goThruDoor()
						end
					end
					settable(uniforms.bbox, x, y, w, h)
					settable(uniforms.color, 0,1,0,.5)
					rectObj:draw()
					if i-1 == self.bigDoorWindow.index then
						showHL()
					end
				end
			end
			if self.showNPCs then
				if self.mapWindow.npcOrder then	-- if not then there's no mapInfo
					for _,i in ipairs(self.mapWindow.npcOrder) do
						local n = mapInfo.npcs[i]
						if n then	-- when is npcOrder going out of sync with the current map??????
							local x, y = tonumber(n.x), tonumber(n.y)
							if (leftPress or leftDoubleClick)
							and x <= mx and mx < x+1
							and y <= my and my < y+1
							then
								self.npcWindow:open(i-1)
								if leftDoubleClick then
									local addr = n:getScriptAddr()
									if addr then
										self.scriptWindow:openScriptAddr(addr)
									end
								end
							end
							settable(uniforms.bbox, x, y, 1, 1)
							settable(uniforms.color, 0,1,1,.5)
							rectObj:draw()
							if i-1 == self.npcWindow.index then
								showHL()
							end

							local tex = self.mapWindow.npcTexs[i]
							if tex then
								self.rgbaTexObj.texs[1] = tex
								settable(self.rgbaTexObj.uniforms.bbox, x,y-1,1,1.5)
								self.rgbaTexObj:draw()
							end
						end
					end
				end
			end
		end

		gl.glDisable(gl.GL_BLEND)
	end

	-- draw gui
	App.super.update(self)
end

function App:updateGUI()
	local game = self.game

	local mapInfo = self.mapWindow and self.mapWindow:getMapInfo()
	local map = mapInfo and mapInfo.map
	if ig.igBeginMainMenuBar() then

		if ig.igBeginMenu'File' then
			self.menuOpenROM:show'Open...'

			if game then
				self.menuSaveROM:show'Save...'
				ig.igSeparator()
				self.menuOpenSRAM:show'Open SRAM...'
				self.menuSaveSRAM:show'Save SRAM...'
			end

			ig.igEndMenu()
		end

		if ig.igBeginMenu'Window' then
			if self.baseWindows then
				for _,w in ipairs(self.baseWindows) do
					if ig.igButton(w.name) then
						w:open()
					end
				end
			end

			ig.igEndMenu()
		end

		if ig.igBeginMenu'Map' then

			if ig.igButton'Reset View' then
				self.view.ortho = true
				self.view.orthoSize = 256
				self.view.pos:set(0,0,10)
				self.view.orbit:set(0,0,0)
				self.view.angle:set(0,0,0,1)
			end

			ig.luatableInputFloat('animSpeed', self, 'animSpeed')

			if map then
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
										frameTex.image:save(animScreenshotPath/(
											'map'..self.mapWindow.index
											..'_z='..z
											..'_layer='..layer
											..'_frame='..frameIndex
											..'.png'
										))
									end
								end
							end
						end

						-- save a composite image while we're here
						local pp = GLPingPong{
							numBuffers = 1,
							width = tonumber(self.mapSize.x),
							height = tonumber(self.mapSize.y),
							internalFormat = gl.GL_RGBA,
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
								image:save(animScreenshotPath/(
									'map'..self.mapWindow.index
									..'_composite'
									..'_frame='..frameIndex
									..'.png'
								))
							end

							fbo:unbind()
						end

						-- save the whole as an animation
						if doSaveGIF then
							compositeImgs[1]:save(
								(animScreenshotPath/('map'..self.mapWindow.index..'_animated.gif')).path,
								compositeImgs:unpack(2)
							)
						end

						gl.glViewport(0, 0, self.width, self.height)

						self.view.mvMat:copy(pushMvMat)
						self.view.projMat:copy(pushProjMat)
						self.view.mvProjMat:mul4x4(self.view.projMat, self.view.mvMat)
					end
				end
			end

			ig.igEndMenu()
		end

		if self.sramPath then
			if ig.igButton'Reload Last SRAM...' then
				self:onLoadSRAM(self.sramPath)
			end
			ig.luatableCheckbox('auto-reload', self, 'autoReloadSRAM')
		end

		ig.igEndMainMenuBar()
	end

	local function updateRecursive(chs)
		for _,ch in ipairs(chs) do
			ch:update()
			if ch.children
			and #ch.children > 0
			then
				updateRecursive(ch.children)
			end
		end
	end
	if self.baseWindows then	-- if game is loaded...
		updateRecursive(self.baseWindows)
	end

	if self.tooltipText then
		ig.igBeginTooltip()
		ig.igText(self.tooltipText)
		ig.igEndTooltip()
	end
end

function App:event(e)
	if e[0].type == sdl.SDL_EVENT_WINDOW_FOCUS_GAINED then
		self.hasFocus = true
		return
	elseif e[0].type == sdl.SDL_EVENT_WINDOW_FOCUS_LOST then
		self.hasFocus = false
		return
	end

	App.super.event(self, e)
	self.canHandleMouse = not ig.igGetIO()[0].WantCaptureMouse
	local canHandleKeyboard = not ig.igGetIO()[0].WantCaptureKeyboard

	local game = self.game
	if not game then return end
	local countof = game.countof

	if canHandleKeyboard then
		if e.type == sdl.SDL_EVENT_KEY_UP then
			if e.key.key == sdl.SDLK_LEFT then
				self.mapWindow:setIndex(math.clamp(math.floor(self.mapWindow.index - 1), 0, countof(game.maps)-1))
			elseif e.key.key == sdl.SDLK_RIGHT then
				self.mapWindow:setIndex(math.clamp(math.floor(self.mapWindow.index + 1), 0, countof(game.maps)-1))
			end
		end
	end

	-- because sdl handles double-clicks on its own:
	if self.canHandleMouse then
		if e.type == sdl.SDL_EVENT_MOUSE_BUTTON_DOWN then
			if e.button.clicks == 2 then
				self.mouse.leftDoubleClick = true
			end
		elseif e.type == sdl.SDL_EVENT_MOUSE_BUTTON_UP then
			if e.button.clicks == 2 then
				self.mouse.leftDoubleClick = false
			end
		end
	end
end

return App
