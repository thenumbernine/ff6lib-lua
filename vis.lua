#!/usr/bin/env luajit

-- global, since app calls onLoadROM at the end of initGL 
cmdline = require 'ext.cmdline'(...)

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
return App():run()
