#!/usr/bin/env luajit
--[[
oh yeah, 0x150000 - 0x185000 is where the tile data for character sprites goes
but the end of it is vehicles
so this is now the vehicle-extraction script

this has some overlap with charsprites
it's probably gonna also output all npc sprites

and it's gonna use `characterFrameTileOffset`
... and the next struct that I haven't charted yet

--]]
local ffi = require 'ffi'
local path = require 'ext.path'
local table = require 'ext.table'
local assert = require 'ext.assert'

local uint8_t = ffi.typeof'uint8_t'

local function run(game)
	local rom = game.rom

	-- points into fieldSpriteGraphics == 0x150000-0x183000
	local function getTileOffsetForSprite(sprite)
		if sprite < 0 or sprite >= game.numCharacterSprites then return end
		return bit.band(
			bit.bnot(0xc00000),
			-- these are only for sprite < 87?
			bit.bor(
				game.characterSpriteOffsetLo[sprite],
				bit.lshift(game.characterSpriteOffsetHiAndSize[sprite].hi, 16)
			))
	end

	local Image = require 'image'
	local makePalette = require 'ff6.graphics'.makePalette
	local readTile = require 'ff6.graphics'.readTile
	-- 8x8 right?
	local tileWidth = require 'ff6.graphics'.tileWidth
	local tileHeight = require 'ff6.graphics'.tileHeight
	local bpp = 4

	-- from everything8215/ff6/src/gfx/map_sprite_pal.inc:
	local paletteNames = {
		[0] = 'EDGAR_SABIN_CELES',
		[1] = 'LOCKE',
		[2] = 'TERRA',
		[3] = 'STRAGO_RELM_GAU_GOGO',
		[4] = 'CYAN_SHADOW_SETZER',
		[5] = 'MOG_UMARO',
		[6] = 'RAINBOW',
		[7] = 'VEHICLE',
		[8] = 'ESPER_TERRA',
		[9] = 'EDGAR_SABIN_CELES_ALT',
		[10] = 'MACHINERY_1',
		[11] = 'RAFT',
		[12] = 'MACHINERY_2',
		[13] = 'GUARDIAN',
		[14] = 'SEALED_GATE',
		[15] = 'VECTOR_CRANE',
		[16] = 'STATUE_SMOKE',
		[17] = 'TREASURE_CHEST',
		[18] = 'CHADARNOOK',
		[19] = 'ROCK',
		[20] = 'FALCON',
		[21] = 'ODIN',
		[22] = 'KEFKAS_TOWER_PARALLAX_1',
		[23] = 'KEFKAS_TOWER_PARALLAX_2',
		[24] = 'KEFKAS_TOWER_PARALLAX_3',
		[25] = 'DADALUMA',
		[26] = 'GREEN_MAGICITE_SMOKE',
		[27] = 'UNUSED_27',
		[28] = 'UNUSED_28',
		[29] = 'AIRSHIP_PARALLAX',
		[30] = 'UNUSED_30',
		[31] = 'UNUSED_31',
	}
	local paletteIndexes = table.map(paletteNames, function(name,index) return index, name end):setmetatable(nil)
	paletteIndexes.EDGAR = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.SABIN = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.CELES = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.IMP = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.LEO = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.GHOST = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.GREEN_SOLDIER = paletteIndexes.EDGAR_SABIN_CELES
	paletteIndexes.MERCHANT = paletteIndexes.LOCKE
	paletteIndexes.BROWN_SOLDIER = paletteIndexes.LOCKE
	paletteIndexes.STRAGO = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.RELM = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.GAU = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.GOGO = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.BANON = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.KEFKA = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.GESTAHL = paletteIndexes.STRAGO_RELM_GAU_GOGO
	paletteIndexes.CYAN = paletteIndexes.CYAN_SHADOW_SETZER
	paletteIndexes.SHADOW = paletteIndexes.CYAN_SHADOW_SETZER
	paletteIndexes.SETZER = paletteIndexes.CYAN_SHADOW_SETZER
	paletteIndexes.MOG = paletteIndexes.MOG_UMARO
	paletteIndexes.UMARO = paletteIndexes.MOG_UMARO


	-- from everything8215/ff6/src/gfx/map_sprite_gfx.inc:
	local spriteNames = {
		[0] = 'TERRA',
		[1] = 'LOCKE',
		[2] = 'CYAN',
		[3] = 'SHADOW',
		[4] = 'EDGAR',
		[5] = 'SABIN',
		[6] = 'CELES',
		[7] = 'STRAGO',
		[8] = 'RELM',
		[9] = 'SETZER',
		[10] = 'MOG',
		[11] = 'GAU',
		[12] = 'GOGO',
		[13] = 'UMARO',
		[14] = 'SOLDIER',
		[15] = 'IMP',
		[16] = 'LEO',
		[17] = 'BANON',
		[18] = 'ESPER_TERRA',
		[19] = 'MERCHANT',
		[20] = 'GHOST',
		[21] = 'KEFKA',
		[22] = 'GESTAHL',
		[23] = 'OLD_MAN',
		[24] = 'MAN',
		[25] = 'DOG',
		[26] = 'CELES_DRESS',
		[27] = 'RICH_MAN',
		[28] = 'DRACO',
		[29] = 'ARVIS',
		[30] = 'PILOT',
		[31] = 'ULTROS',
		[32] = 'SPIFFY_GAU',
		[33] = 'HOOKER',
		[34] = 'CHANCELLOR',
		[35] = 'CLYDE',
		[36] = 'OLD_WOMAN',
		[37] = 'WOMAN',
		[38] = 'BOY',
		[39] = 'GIRL',
		[40] = 'BIRD',
		[41] = 'RACHEL',
		[42] = 'KATARIN',
		[43] = 'IMPRESARIO',
		[44] = 'ESPER_ELDER',
		[45] = 'YURA',
		[46] = 'SIEGFRIED',
		[47] = 'CID',
		[48] = 'MADUIN',
		[49] = 'BANDIT',
		[50] = 'VARGAS',
		[51] = 'MONSTER',
		[52] = 'NARSHE_GUARD',
		[53] = 'TRAIN_CONDUCTOR',
		[54] = 'SHOPKEEPER',
		[55] = 'FAERIE',
		[56] = 'WOLF',
		[57] = 'DRAGON',
		[58] = 'FISH',
		[59] = 'FIGARO_GUARD',
		[60] = 'DARILL',
		[61] = 'CHUPON',
		[62] = 'EMPEROR_SERVANT',
		[63] = 'RAMUH',
		[64] = 'FIGARO_GUARD_RIDING',
		[65] = 'CELES_CHAINS',
		[66] = 'GAU_KUNG_FU',
		[67] = 'GAU_BANDANA',
		[68] = 'KING_DOMA',
		[69] = 'NUMBER_128',
		[70] = 'MAGI_WARRIOR_1',
		[71] = 'SKULL_STATUE',
		[72] = 'IFRIT',
		[73] = 'PHANTOM',
		[74] = 'SHIVA',
		[75] = 'UNICORN',
		[76] = 'BISMARK',
		[77] = 'CARBUNKL',
		[78] = 'SHOAT',
		[79] = 'OWZER_1',
		[80] = 'OWZER_2',
		[81] = 'BLACKJACK',
		[82] = 'FIGARO_GUARD_DEAD',
		[83] = 'NUMBER_024',
		[84] = 'TREASURE_CHEST',
		[85] = 'MAGI_WARRIOR_2',
		[86] = 'ATMA',
		[87] = 'SMALL_STATUE',
		[88] = 'FLOWERS',
		[89] = 'ENVELOPE',
		[90] = 'PLANT',
		[91] = 'MAGICITE',
		[92] = 'BOOK',
		[93] = 'BABY',
		[94] = 'QUESTION_MARK',
		[95] = 'EXCLAMATION_POINT',
		[96] = 'SLAVE_CROWN',
		[97] = 'WEIGHT',
		[98] = 'BIRD_BANDANA',
		[99] = 'EYES',
		[100] = 'BANDANA',
		[101] = 'NOTHING',
		[102] = 'FLYING_BIRD_1',
		[103] = 'FLYING_BIRD_2',
		[104] = 'BIG_SPARKLE',
		[105] = 'MULTI_SPARKLES',
		[106] = 'SMALL_SPARKLE',
		[107] = 'COIN',
		[108] = 'RAT',
		[109] = 'TURTLE',
		[110] = 'SMALL_BIRD_UP',
		[111] = 'SAVE_POINT',
		[112] = 'FLAME',
		[113] = 'EXPLOSION',
		[114] = 'TENTACLE_1',
		[115] = 'TENTACLE_2',
		[116] = 'BIG_SWITCH',
		[117] = 'FLOOR_SWITCH',
		[118] = 'ROCK',
		[119] = 'CRANE_HOOK_3',
		[120] = 'ELEVATOR',
		[121] = 'FLYING_TERRA_1',
		[122] = 'FLYING_TERRA_2',
		[123] = 'ENDING_TERRA_3',
		[124] = 'DIVING_HELMET',
		[125] = 'GUARDIAN_1',
		[126] = 'GUARDIAN_2',
		[127] = 'GUARDIAN_3',
		[128] = 'CRANE_HOOK_2',
		[129] = 'GUARDIAN_4',
		[130] = 'GUARDIAN_5',
		[131] = 'GUARDIAN_6',
		[132] = 'CRANE_HOOK_1',
		[133] = 'MAGITEK_MACHINE',
		[134] = 'GATE_1',
		[135] = 'GATE_2',
		[136] = 'GATE_3',
		[137] = 'AIR_FORCE',
		[138] = 'LEO_SWORD',
		[139] = 'MAGITEK_TRAIN_1',
		[140] = 'MAGITEK_TRAIN_2',
		[141] = 'MAGITEK_TRAIN_3',
		[142] = 'MAGITEK_TRAIN_4',
		[143] = 'CRANE_1',
		[144] = 'CRANE_2',
		[145] = 'CRANE_3',
		[146] = 'CHADARNOOK_1',
		[147] = 'CHADARNOOK_2',
		[148] = 'CHADARNOOK_3',
		[149] = 'FALCON_1',
		[150] = 'FALCON_2',
		[151] = 'FALCON_3',
		[152] = 'FLYING_TERRA_3',
		[153] = 'TRITOCH',
		[154] = 'ODIN',
		[155] = 'GODDESS_1',
		[156] = 'DOOM_1',
		[157] = 'POLTERGEIST_1',
		[158] = 'GODDESS_2',
		[159] = 'GODDESS_3',
		[160] = 'DOOM_2',
		[161] = 'DOOM_3',
		[162] = 'ENDING_TERRA_1',
		[163] = 'ENDING_TERRA_2',
		[164] = 'SMALL_BIRD_LEFT',
	}
	local spriteIndexes = table.map(spriteNames, function(name,index) return index, name end):setmetatable(nil)

	-- grep'ing everything8215/ff6 and getting all npc_gfx sprite + palette instances...
	-- then remove NOTHING sprites and remove no explicit palette npcs
	-- then sort and remove duplicates
	local palettesForSprites = {}
local function addSpritePal(spriteName, paletteIndex)
	if not paletteIndex then return end
	palettesForSprites[spriteName] = palettesForSprites[spriteName] or table()
	palettesForSprites[spriteName]:removeObject(paletteIndex)
	palettesForSprites[spriteName]:insert(paletteIndex)
end
	-- set all npc_gfx
-- [[
	for _,kv in ipairs{
		{'AIR_FORCE', 'CYAN_SHADOW_SETZER'},
		{'ARVIS', 'CYAN_SHADOW_SETZER'},
		{'ATMA', 'STRAGO_RELM_GAU_GOGO'},
		{'BABY', 'TERRA'},
		{'BANDANA', 'EDGAR_SABIN_CELES'},
		{'BANDIT', 'EDGAR_SABIN_CELES'},
		{'BANDIT', 'LOCKE'},
		{'BANDIT', 'STRAGO_RELM_GAU_GOGO'},
		{'BANON', 'LOCKE'},
		{'BANON', 'STRAGO_RELM_GAU_GOGO'},
		{'BIG_SPARKLE', 'RAINBOW'},
		{'BIG_SPARKLE', 'STRAGO_RELM_GAU_GOGO'},
		{'BIG_SWITCH', 'RAINBOW'},
		{'BIRD', 'CYAN_SHADOW_SETZER'},
		{'BIRD_BANDANA', 'CYAN_SHADOW_SETZER'},
		{'BISMARK', 'CYAN_SHADOW_SETZER'},
		{'BLACKJACK', 'CYAN_SHADOW_SETZER'},
		{'BOOK', 'EDGAR_SABIN_CELES'},
		{'BOY', 'EDGAR_SABIN_CELES'},
		{'BOY', 'LOCKE'},
		{'BOY', 'STRAGO_RELM_GAU_GOGO'},
		{'BOY', 'TERRA'},
		{'BOY', 'VEHICLE'},
		{'CARBUNKL', 'TERRA'},
		{'CELES_CHAINS', 'EDGAR_SABIN_CELES'},
		{'CELES_DRESS', 'EDGAR_SABIN_CELES'},
		{'CELES_DRESS', 'VEHICLE'},
		{'CHADARNOOK_1', 'RAINBOW'},
		{'CHADARNOOK_2', 'RAINBOW'},
		{'CHADARNOOK_3', 'RAINBOW'},
		{'CHANCELLOR', 'TERRA'},
		{'CHUPON', 'EDGAR_SABIN_CELES'},
		{'CHUPON', 'MOG_UMARO'},
		{'CID', 'STRAGO_RELM_GAU_GOGO'},
		{'CLYDE', 'LOCKE'},
		{'COIN', 'EDGAR_SABIN_CELES'},
		{'COIN', 'RAINBOW'},
		{'CRANE_1', 'RAINBOW'},
		{'CRANE_2', 'RAINBOW'},
		{'CRANE_3', 'RAINBOW'},
		{'CRANE_HOOK_1', 'RAINBOW'},
		{'CRANE_HOOK_2', 'RAINBOW'},
		{'CRANE_HOOK_3', 'RAINBOW'},
		{'CYAN', 'RAINBOW'},
		{'CYAN', 'VEHICLE'},
		{'DARILL', 'TERRA'},
		{'DIVING_HELMET', 'STRAGO_RELM_GAU_GOGO'},
		{'DOG', 'CYAN_SHADOW_SETZER'},
		{'DOG', 'LOCKE'},
		{'DOOM_1', 'VEHICLE'},
		{'DOOM_2', 'VEHICLE'},
		{'DOOM_3', 'VEHICLE'},
		{'DRACO', 'CYAN_SHADOW_SETZER'},
		{'DRAGON', 'CYAN_SHADOW_SETZER'},
		{'DRAGON', 'EDGAR_SABIN_CELES'},
		{'DRAGON', 'LOCKE'},
		{'DRAGON', 'STRAGO_RELM_GAU_GOGO'},
		{'DRAGON', 'TERRA'},
		{'EDGAR', 'LOCKE'},
		{'ELEVATOR', 'RAINBOW'},
		{'ELEVATOR', 'VEHICLE'},
		{'EMPEROR_SERVANT', 'CYAN_SHADOW_SETZER'},
		{'EMPEROR_SERVANT', 'TERRA'},
		{'EMPEROR_SERVANT', 'VEHICLE'},
		{'ENDING_TERRA_1', 'TERRA'},
		{'ENDING_TERRA_2', 'TERRA'},
		{'ENDING_TERRA_3', 'TERRA'},
		{'ENVELOPE', 'STRAGO_RELM_GAU_GOGO'},
		{'ESPER_ELDER', 'CYAN_SHADOW_SETZER'},
		{'ESPER_TERRA', 'RAINBOW'},
		{'ESPER_TERRA', 'VEHICLE'},
		{'EXCLAMATION_POINT', 'RAINBOW'},
		{'EXPLOSION', 'RAINBOW'},
		{'EXPLOSION', 'VEHICLE'},
		{'EYES', 'EDGAR_SABIN_CELES'},
		{'FAERIE', 'TERRA'},
		{'FALCON_1', 'VEHICLE'},
		{'FALCON_2', 'VEHICLE'},
		{'FALCON_3', 'VEHICLE'},
		{'FIGARO_GUARD', 'LOCKE'},
		{'FIGARO_GUARD', 'TERRA'},
		{'FIGARO_GUARD_DEAD', 'TERRA'},
		{'FISH', 'CYAN_SHADOW_SETZER'},
		{'FLAME', 'RAINBOW'},
		{'FLOOR_SWITCH', 'CYAN_SHADOW_SETZER'},
		{'FLOWERS', 'STRAGO_RELM_GAU_GOGO'},
		{'FLYING_BIRD_1', 'CYAN_SHADOW_SETZER'},
		{'FLYING_BIRD_2', 'CYAN_SHADOW_SETZER'},
		{'FLYING_TERRA_1', 'RAINBOW'},
		{'FLYING_TERRA_3', 'RAINBOW'},
		{'GATE_1', 'VEHICLE'},
		{'GATE_2', 'VEHICLE'},
		{'GATE_3', 'VEHICLE'},
		{'GAU_BANDANA', 'STRAGO_RELM_GAU_GOGO'},
		{'GAU_KUNG_FU', 'STRAGO_RELM_GAU_GOGO'},
		{'GESTAHL', 'STRAGO_RELM_GAU_GOGO'},
		{'GHOST', 'EDGAR_SABIN_CELES'},
		{'GHOST', 'RAINBOW'},
		{'GIRL', 'EDGAR_SABIN_CELES'},
		{'GIRL', 'LOCKE'},
		{'GODDESS_1', 'VEHICLE'},
		{'GODDESS_2', 'VEHICLE'},
		{'GODDESS_3', 'VEHICLE'},
		{'GOGO', 'MOG_UMARO'},
		{'GUARDIAN_1', 'RAINBOW'},
		{'GUARDIAN_1', 'VEHICLE'},
		{'GUARDIAN_2', 'RAINBOW'},
		{'GUARDIAN_2', 'VEHICLE'},
		{'GUARDIAN_3', 'RAINBOW'},
		{'GUARDIAN_3', 'VEHICLE'},
		{'GUARDIAN_4', 'RAINBOW'},
		{'GUARDIAN_4', 'VEHICLE'},
		{'GUARDIAN_5', 'RAINBOW'},
		{'GUARDIAN_5', 'VEHICLE'},
		{'GUARDIAN_6', 'RAINBOW'},
		{'GUARDIAN_6', 'VEHICLE'},
		{'HOOKER', 'EDGAR_SABIN_CELES'},
		{'HOOKER', 'TERRA'},
		{'IFRIT', 'STRAGO_RELM_GAU_GOGO'},
		{'IMP', 'EDGAR_SABIN_CELES'},
		{'IMPRESARIO', 'CYAN_SHADOW_SETZER'},
		{'KATARIN', 'CYAN_SHADOW_SETZER'},
		{'KEFKA', 'STRAGO_RELM_GAU_GOGO'},
		{'KING_DOMA', 'TERRA'},
		{'LEO', 'EDGAR_SABIN_CELES'},
		{'LEO_SWORD', 'CYAN_SHADOW_SETZER'},
		{'MADUIN', 'CYAN_SHADOW_SETZER'},
		{'MADUIN', 'TERRA'},
		{'MAGICITE', 'TERRA'},
		{'MAGITEK_MACHINE', 'VEHICLE'},
		{'MAGITEK_TRAIN_1', 'RAINBOW'},
		{'MAGITEK_TRAIN_1', 'VEHICLE'},
		{'MAGITEK_TRAIN_2', 'RAINBOW'},
		{'MAGITEK_TRAIN_2', 'VEHICLE'},
		{'MAGITEK_TRAIN_3', 'RAINBOW'},
		{'MAGITEK_TRAIN_3', 'VEHICLE'},
		{'MAGITEK_TRAIN_4', 'RAINBOW'},
		{'MAGITEK_TRAIN_4', 'VEHICLE'},
		{'MAGI_WARRIOR_1', 'CYAN_SHADOW_SETZER'},
		{'MAGI_WARRIOR_2', 'CYAN_SHADOW_SETZER'},
		{'MAN', 'EDGAR_SABIN_CELES'},
		{'MAN', 'LOCKE'},
		{'MAN', 'STRAGO_RELM_GAU_GOGO'},
		{'MERCHANT', 'EDGAR_SABIN_CELES'},
		{'MERCHANT', 'LOCKE'},
		{'MERCHANT', 'TERRA'},
		{'MONSTER', 'CYAN_SHADOW_SETZER'},
		{'MONSTER', 'TERRA'},
		{'MULTI_SPARKLES', 'RAINBOW'},
		{'NARSHE_GUARD', 'EDGAR_SABIN_CELES'},
		{'NARSHE_GUARD', 'LOCKE'},
		{'NUMBER_024', 'MOG_UMARO'},
		{'NUMBER_128', 'CYAN_SHADOW_SETZER'},
		{'ODIN', 'RAINBOW'},
		{'OLD_MAN', 'CYAN_SHADOW_SETZER'},
		{'OLD_MAN', 'EDGAR_SABIN_CELES'},
		{'OLD_MAN', 'LOCKE'},
		{'OLD_MAN', 'STRAGO_RELM_GAU_GOGO'},
		{'OLD_WOMAN', 'CYAN_SHADOW_SETZER'},
		{'OLD_WOMAN', 'EDGAR_SABIN_CELES'},
		{'OLD_WOMAN', 'STRAGO_RELM_GAU_GOGO'},
		{'OWZER_1', 'STRAGO_RELM_GAU_GOGO'},
		{'OWZER_2', 'STRAGO_RELM_GAU_GOGO'},
		{'PHANTOM', 'CYAN_SHADOW_SETZER'},
		{'PILOT', 'LOCKE'},
		{'PILOT', 'STRAGO_RELM_GAU_GOGO'},
		{'PLANT', 'EDGAR_SABIN_CELES'},
		{'POLTERGEIST_1', 'VEHICLE'},
		{'QUESTION_MARK', 'RAINBOW'},
		{'RACHEL', 'EDGAR_SABIN_CELES'},
		{'RAMUH', 'CYAN_SHADOW_SETZER'},
		{'RAT', 'STRAGO_RELM_GAU_GOGO'},
		{'RICH_MAN', 'CYAN_SHADOW_SETZER'},
		{'RICH_MAN', 'EDGAR_SABIN_CELES'},
		{'RICH_MAN', 'LOCKE'},
		{'RICH_MAN', 'RAINBOW'},
		{'RICH_MAN', 'TERRA'},
		{'ROCK', 'RAINBOW'},
		{'SABIN', 'RAINBOW'},
		{'SAVE_POINT', 'MOG_UMARO'},
		{'SAVE_POINT', 'RAINBOW'},
		{'SHIVA', 'EDGAR_SABIN_CELES'},
		{'SHOAT', 'TERRA'},
		{'SHOPKEEPER', 'LOCKE'},
		{'SIEGFRIED', 'CYAN_SHADOW_SETZER'},
		{'SKULL_STATUE', 'CYAN_SHADOW_SETZER'},
		{'SLAVE_CROWN', 'CYAN_SHADOW_SETZER'},
		{'SMALL_BIRD_LEFT', 'CYAN_SHADOW_SETZER'},
		{'SMALL_BIRD_UP', 'CYAN_SHADOW_SETZER'},
		{'SMALL_SPARKLE', 'MOG_UMARO'},
		{'SMALL_SPARKLE', 'RAINBOW'},
		{'SMALL_STATUE', 'STRAGO_RELM_GAU_GOGO'},
		{'SOLDIER', 'CYAN_SHADOW_SETZER'},
		{'SOLDIER', 'EDGAR_SABIN_CELES'},
		{'SOLDIER', 'LOCKE'},
		{'SOLDIER', 'RAINBOW'},
		{'SOLDIER', 'TERRA'},
		{'SOLDIER', 'VEHICLE'},
		{'SPIFFY_GAU', 'STRAGO_RELM_GAU_GOGO'},
		{'TENTACLE_1', 'STRAGO_RELM_GAU_GOGO'},
		{'TENTACLE_2', 'STRAGO_RELM_GAU_GOGO'},
		{'TRAIN_CONDUCTOR', 'CYAN_SHADOW_SETZER'},
		{'TRAIN_CONDUCTOR', 'EDGAR_SABIN_CELES'},
		{'TREASURE_CHEST', 'RAINBOW'},
		{'TREASURE_CHEST', 'VEHICLE'},
		{'TRITOCH', 'TERRA'},
		{'TURTLE', 'VEHICLE'},
		{'ULTROS', 'MOG_UMARO'},
		{'ULTROS', 'TERRA'},
		{'UMARO', 'MOG_UMARO'},
		{'UNICORN', 'STRAGO_RELM_GAU_GOGO'},
		{'VARGAS', 'CYAN_SHADOW_SETZER'},
		{'VARGAS', 'VEHICLE'},
		{'WEIGHT', 'CYAN_SHADOW_SETZER'},
		{'WOLF', 'CYAN_SHADOW_SETZER'},
		{'WOLF', 'TERRA'},
		{'WOMAN', 'EDGAR_SABIN_CELES'},
		{'WOMAN', 'LOCKE'},
		{'WOMAN', 'STRAGO_RELM_GAU_GOGO'},
		{'WOMAN', 'TERRA'},
		{'WOMAN', 'VEHICLE'},
		{'YURA', 'CYAN_SHADOW_SETZER'},
	} do
		local spriteName, paletteName = table.unpack(kv)
		addSpritePal(spriteName, paletteIndexes[paletteName])
	end
--]]
	-- if any palette indexes have matching names with sprite names then set those
	-- do this last to override defaults
	for name,index in pairs(paletteIndexes) do
		addSpritePal(name, index)
	end

	-- filling in some that are missing or had multiple options...
	addSpritePal('SOLDIER', 1)
	addSpritePal('DOG', 4)
	addSpritePal('CELES_DRESS', 0)
	addSpritePal('PILOT', 1)
	addSpritePal('ULTROS', 5)
	addSpritePal('WOMAN', 1)
	addSpritePal('BOY', 3)
	addSpritePal('VARGAS', 4)
	addSpritePal('MONSTER', 4)
	addSpritePal('TRAIN_CONDUCTOR', 4)
	addSpritePal('WOLF', 4)
	addSpritePal('EMPEROR_SERVANT', 2)
	addSpritePal('FIGARO_GUARD_RIDING', 2)
	addSpritePal('BIG_SPARKLE', 6)
	addSpritePal('COIN', 0)
	addSpritePal('FLYING_TERRA_1', 2)
	addSpritePal('FLYING_TERRA_2', 2)
	addSpritePal('ENDING_TERRA_3', 2)
	addSpritePal('FLYING_TERRA_3', 2)
	addSpritePal('ENDING_TERRA_1', 2)
	addSpritePal('ENDING_TERRA_2', 2)

	do
		--4bpp means 32 bytes per 8x8 tile ...
		--  0x150000 - 0x185000 has 6784 8x8 tiles = 1696 16x16 tiles
		local ptr = game.fieldSpriteGraphics
		local tilesWide = 83
		local tilesHigh = 108

		local tilesImg = Image(tileWidth*tilesWide, tileHeight*tilesHigh, 4, uint8_t):clear()
		local tileImg = Image(tileWidth, tileHeight, 1, uint8_t)

		print'spritePalettes = {'
		for sprite=0,game.numCharacterSprites-1 do
			local palIndexes = palettesForSprites[spriteNames[sprite]]
			local palIndex = palIndexes and palIndexes:last() or 0
			print('\t['..sprite..'] = '..palIndex..',\t-- '..spriteNames[sprite]..' = '..tostring(paletteNames[palIndex]))
		end
		print'}'

		local dstx = 0
		local dsty = 0
		for sprite=0,game.numCharacterSprites-1 do
			local palIndexes = palettesForSprites[spriteNames[sprite]]
			local palIndex = palIndexes and palIndexes:last()
-- see all?
--print('sprite', spriteNames[sprite], 'using palettes', palIndexes and palIndexes:concat', ')
			palIndex = bit.band(palIndex or 0, 0x1f)
			local palette = makePalette(game, game.characterPalettes + palIndex, 4, 16)
			tileImg.palette = palette

			local function getTileXY(spriteTileIndex)
				local x = spriteTileIndex % 2
				local y = (spriteTileIndex - x) / 2
				return x, y
			end

			-- TODO 120 elevator is messed up...
			local maxFrames =
				sprite < 22 and 41
				or sprite < 63 and 9
				or 1

			-- these two sprites have 11 & 10 tiles respectively
			-- 6 tiles are needed for a 16x24 animation-frame
			-- so they look like they want to have 2 frames...
			-- ... but idk where the tile layout data is...
			--if sprite == 63 or sprite == 64 then maxFrames = 2 end

			-- how many tiles per frame
			local spriteTileCount =
				sprite < 87 and 6
				or sprite < 116 and 5
				or 4
			if sprite >= 87 and sprite < 116 then
				getTileXY = function(spriteTileIndex)
					local x = (spriteTileIndex+1) % 2
					local y = (spriteTileIndex+1 - x) / 2
					return x, y
				end
			end


			if dstx + 16*maxFrames >= tilesImg.width then
				dstx = 0
				dsty = dsty + 24
			end

			local charBaseOffset = getTileOffsetForSprite(sprite)

-- tiles are 8x8x4bpp = 32 bytes = 0x20 bytes ...
-- for a 16x16 that is 0x80 bytes
local tileDataSize = (getTileOffsetForSprite(sprite+1) or 0x183000) - charBaseOffset
print(
	'sprite', sprite, spriteNames[sprite],
	'tile ofs', ('%x'):format(charBaseOffset),
	'size', ('%x'):format(tileDataSize),
	'='..(tileDataSize/0x20)..' unique 8x8x4bpp tiles'
)

			for frame=0,maxFrames-1 do
				-- blit to our 8x8 with palette set up
				tileImg:clear()

				-- TODO sometimes this is characterFrameTileOffsets, sometimes I bet it is what's next ...
				local frameTileOffset = game.characterFrameTileOffsets + frame * spriteTileCount
				for spriteTileIndex=0,spriteTileCount-1 do
					local x, y = getTileXY(spriteTileIndex)

					local tile = rom + charBaseOffset + frameTileOffset[spriteTileIndex]
					readTile(tileImg, 0, 0, tile, bpp)
					-- and then to our master sheet
					local tiledstx = dstx + x * 8
					local tiledsty = dsty + y * 8
					if tiledstx > tilesImg.width then
						print('!!! WARNING !!! tiledstx='..tiledstx..' > tilesImg.y='..tilesImg.width)
					end
					if tiledsty > tilesImg.height then
						print('!!! WARNING !!! tiledsty='..tiledsty..' > tilesImg.y='..tilesImg.height)
					end
					tilesImg:pasteInto{
						image = tileImg:rgba(),
						x = tiledstx,
						y = tiledsty,
					}
				end
				dstx = dstx + 16
			end
			dstx = dstx + 8
		end

		tilesImg:save'all-field-tiles.png'
	end

	-- use this for later making vehicle-sheet
	local tilesImg
	do
		-- 0x183000 - 0x185000 has 256 8x8 tiles = 64 16x16 tiles
		local ptr = game.rom + 0x183000
		local tilesWide = 16
		local tilesHigh = 16

		tilesImg = Image(tileWidth*tilesWide, tileHeight*tilesHigh, 1, uint8_t):clear()

		--everything8215 vehicleGraphics says mapSpritePalettes[7] and [11]
		tilesImg.palette = table.append(
			makePalette(game, game.characterPalettes + 7, 4, 16),
			makePalette(game, game.characterPalettes + 11, 4, 16)
		)

		local tileIndex = 0
		for ty=0,tilesHigh-1 do
			for tx=0,tilesWide-1 do
				local tileImg = Image(8, 8, 1, uint8_t):clear()
				local palor = tileIndex < 32 and 0x10 or 0
				readTile(tileImg, 0, 0, ptr, bpp, false, false, palor)
				tilesImg:pasteInto{image=tileImg, x=tx*tileWidth, y=ty*tileHeight}
				ptr = ptr + 32
				tileIndex = tileIndex + 1
			end
		end

		tilesImg:save'vehicle-tiles.png'
	end

	-- now rearrange them and put them into a 256x256 sprite-sheet (this is ff6t3d-specific)
	-- is it just me or does it look like the tiles were meant to be laid out in rows of 16 to make a 128x128 sheet?
	-- i'll just go with that ....
	local tile16x16Imgs = table()
	for j=0,7 do
		for i=0,7 do
			tile16x16Imgs:insert(tilesImg:copy{x=i*16, y=j*16, width=16, height=16})
		end
	end
	assert.len(tile16x16Imgs, 64)

	local sheetImg = Image(256,256,1,uint8_t):clear()
	sheetImg.palette = tilesImg.palette

	local i = 1
	local function pasteNext(args)
		args.image = tile16x16Imgs[i]
		i = i + 1
		sheetImg:pasteInto(args)
	end
	-- raft up/down
	pasteNext{x=6*16, y=0*16}
	pasteNext{x=6*16, y=1*16}
	pasteNext{x=7*16, y=0*16}
	pasteNext{x=7*16, y=1*16}
	-- raft left/right
	pasteNext{x=6*16, y=2*16}
	pasteNext{x=6*16, y=3*16}
	pasteNext{x=7*16, y=2*16}
	pasteNext{x=7*16, y=3*16}

	pasteNext{x=80, y=96}	-- chocobo head
	pasteNext{x=0, y=96}	-- headless chocobo tail facing down
	pasteNext{x=0, y=112}	--
	pasteNext{x=16, y=96}	-- headless chocobo tail facing down #2
	pasteNext{x=16, y=112}	--
	pasteNext{x=80, y=112}	-- chocobo tail
	pasteNext{x=32, y=96}	-- headless chocobo tail facing up
	pasteNext{x=32, y=112}	--
	pasteNext{x=48, y=96}	-- headless chocobo tail facing up #2
	pasteNext{x=48, y=112}	--
	-- chocobo standing left
	pasteNext{x=0, y=128}
	pasteNext{x=0, y=144}
	pasteNext{x=16, y=128}
	pasteNext{x=16, y=144}
	-- chocobo run left #1
	pasteNext{x=32, y=128}
	pasteNext{x=32, y=144}
	pasteNext{x=48, y=128}
	pasteNext{x=48, y=144}
	-- chocobo run left #2
	pasteNext{x=64, y=128}
	pasteNext{x=64, y=144}
	pasteNext{x=80, y=128}
	pasteNext{x=80, y=144}

	pasteNext{x=64, y=96}	-- chocobo wark
	pasteNext{x=64, y=112}	-- chocobo eyes closed

	-- magitek stand d
	local magitek = i
	pasteNext{x=0, y=0}
	pasteNext{x=0, y=16}
	-- and then the last two, hflipped
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+0]:mirror(), x=16, y=0}
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+1]:mirror(), x=16, y=16}

	-- magitek walk d
	pasteNext{x=32, y=0}
	pasteNext{x=32, y=16}
	-- and then walk d #2 hflipped
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+4]:mirror(), x=48, y=0}
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+5]:mirror(), x=48, y=16}

	-- magitek walk d #2
	pasteNext{x=64, y=0}
	pasteNext{x=64, y=16}
	-- and then walk d #1 hflipped
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+2]:mirror(), x=80, y=0}
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+3]:mirror(), x=80, y=16}

	-- magitek stand u
	pasteNext{x=0, y=32}
	pasteNext{x=0, y=48}
	-- and then the last two, hflipped
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+6]:mirror(), x=16, y=32}
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+7]:mirror(), x=16, y=48}

	-- magitek walk u
	pasteNext{x=32, y=32}
	pasteNext{x=32, y=48}
	-- and then walk u #2 hflipped
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+10]:mirror(), x=48, y=32}
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+11]:mirror(), x=48, y=48}

	-- magitek walk u #2
	pasteNext{x=64, y=32}
	pasteNext{x=64, y=48}
	-- and then walk u #1 hflipped
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+8]:mirror(), x=80, y=32}
	sheetImg:pasteInto{image=tile16x16Imgs[magitek+9]:mirror(), x=80, y=48}

	-- stand l
	pasteNext{x=0, y=64}
	pasteNext{x=16, y=64}
	pasteNext{x=0, y=80}
	pasteNext{x=16, y=80}
	-- walk l #1
	pasteNext{x=32, y=64}
	pasteNext{x=48, y=64}
	pasteNext{x=32, y=80}
	pasteNext{x=48, y=80}
	-- walk l #2
	pasteNext{x=64, y=64}
	pasteNext{x=80, y=64}
	pasteNext{x=64, y=80}
	pasteNext{x=80, y=80}
	-- walk l #3
	pasteNext{x=96, y=64}
	pasteNext{x=112, y=64}
	pasteNext{x=96, y=80}
	pasteNext{x=112, y=80}

	sheetImg:save'vehicle-sheet.png'
end

--print('...', select('#', ...), ...)
if select('#', ...) > 0 then	-- luajit #... == 0 <-> this file was require'd
	local game = require 'ff6'(( assert(path((...)):read()) ))
	run(game)
end

return run
