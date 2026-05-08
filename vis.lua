#!/usr/bin/env luajit
local cmdline = require 'ext.cmdline'(...)

-- do this before requiring 'App'
local gl = require 'gl.setup'(cmdline.gl)

-- [[ alright this is unfortunately rigid in my vis design ... feel free to comment this out and just not do any flood-filling voxel-generation ...
local path = require 'ext.path'
if path'../numo9':exists() then
	package.path = package.path .. ';../numo9/?.lua'
	Voxel = require 'numo9.rom'.Voxel
	BlobVoxelMap = require 'numo9.blob.voxelmap'
	BlobVoxelMap.skipGPU = true	-- we're not rendering, just saving
end
--]]


local App = require 'ff6.vis.app'

local app = App()

if cmdline[1] then
	app:onLoadROM(cmdline[1], cmdline[2])
end

app:run()
