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
local range = require 'ext.range'
local assert = require 'ext.assert'
local tolua = require 'ext.tolua'
local vec2i = require 'vec-ffi.vec2i'

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
		[0] = 'edgar_sabin_celes',
		[1] = 'locke',
		[2] = 'terra',
		[3] = 'strago_relm_gau_gogo',
		[4] = 'cyan_shadow_setzer',
		[5] = 'mog_umaro',
		[6] = 'rainbow',
		[7] = 'vehicle',
		[8] = 'morphed_terra',
		[9] = 'edgar_sabin_celes_alt',
		[10] = 'machinery_1',
		[11] = 'raft',
		[12] = 'machinery_2',
		[13] = 'guardian',
		[14] = 'sealed_gate',
		[15] = 'vector_crane',
		[16] = 'statue_smoke',
		[17] = 'treasure_chest',
		[18] = 'chadarnook',
		[19] = 'rock',
		[20] = 'falcon',
		[21] = 'odin',
		[22] = 'kefkas_tower_parallax_1',
		[23] = 'kefkas_tower_parallax_2',
		[24] = 'kefkas_tower_parallax_3',
		[25] = 'dadaluma',
		[26] = 'green_magicite_smoke',
		[27] = 'unused_27',
		[28] = 'unused_28',
		[29] = 'airship_parallax',
		[30] = 'unused_30',
		[31] = 'unused_31',
	}
	local paletteIndexes = table.map(paletteNames, function(name,index) return index, name end):setmetatable(nil)
	paletteIndexes.edgar = paletteIndexes.edgar_sabin_celes
	paletteIndexes.sabin = paletteIndexes.edgar_sabin_celes
	paletteIndexes.celes = paletteIndexes.edgar_sabin_celes
	paletteIndexes.imp = paletteIndexes.edgar_sabin_celes
	paletteIndexes.leo = paletteIndexes.edgar_sabin_celes
	paletteIndexes.ghost = paletteIndexes.edgar_sabin_celes
	paletteIndexes.green_soldier = paletteIndexes.edgar_sabin_celes
	paletteIndexes.merchant = paletteIndexes.locke
	paletteIndexes.brown_soldier = paletteIndexes.locke
	paletteIndexes.strago = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.relm = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.gau = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.gogo = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.banon = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.kefka = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.gestahl = paletteIndexes.strago_relm_gau_gogo
	paletteIndexes.cyan = paletteIndexes.cyan_shadow_setzer
	paletteIndexes.shadow = paletteIndexes.cyan_shadow_setzer
	paletteIndexes.setzer = paletteIndexes.cyan_shadow_setzer
	paletteIndexes.mog = paletteIndexes.mog_umaro
	paletteIndexes.umaro = paletteIndexes.mog_umaro


	-- from everything8215/ff6/src/gfx/map_sprite_gfx.inc:
	-- I renamed some to match the names I already had in ff6t3d...
	local spriteNames = {
		[0] = 'terra',
		[1] = 'locke',
		[2] = 'cyan',
		[3] = 'shadow',
		[4] = 'edgar',
		[5] = 'sabin',
		[6] = 'celes',
		[7] = 'strago',
		[8] = 'relm',
		[9] = 'setzer',
		[10] = 'mog',
		[11] = 'gau',
		[12] = 'gogo',
		[13] = 'umaro',
		[14] = 'soldier',
		[15] = 'imp',
		[16] = 'leo',
		[17] = 'banon',
		[18] = 'morphed_terra',	-- was esper_terra
		[19] = 'merchant',
		[20] = 'ghost',
		[21] = 'kefka',
		[22] = 'gestahl',
		[23] = 'elder',	-- was old_man
		[24] = 'man',
		[25] = 'dog',
		[26] = 'celes_in_dress',	-- was celes_dress
		[27] = 'scholar',	-- was rich_man
		[28] = 'draco',
		[29] = 'arvis',
		[30] = 'returner',	-- was pilot
		[31] = 'ultros',
		[32] = 'gau_dressed_up', -- was spiffy_gau
		[33] = 'hooker',
		[34] = 'figaro_chancellor',	-- was chancellor
		[35] = 'clyde',
		[36] = 'matron',	-- was old_woman
		[37] = 'woman',
		[38] = 'boy',
		[39] = 'girl',
		[40] = 'bird',
		[41] = 'rachel',
		[42] = 'katarin',
		[43] = 'opera_impresario',	-- was impresario
		[44] = 'elder_esper',	-- was esper_elder
		[45] = 'yura',
		[46] = 'siegfried',
		[47] = 'cid',
		[48] = 'maduin',
		[49] = 'bandit',
		[50] = 'vargas',
		[51] = 'monster',
		[52] = 'narshe_guard',
		[53] = 'train_conductor',
		[54] = 'shopkeeper',
		[55] = 'faerie',
		[56] = 'wolf',
		[57] = 'dragon',
		[58] = 'fish',
		[59] = 'figaro_guard',
		[60] = 'darill',
		[61] = 'chupon',
		[62] = 'emperor_servant',
		[63] = 'ramuh',
		[64] = 'figaro_guard_riding',
		[65] = 'celes_chains',
		[66] = 'gau_kung_fu',
		[67] = 'gau_bandana',
		[68] = 'king_doma',
		[69] = 'number_128',
		[70] = 'magi_warrior_1',
		[71] = 'skull_statue',
		[72] = 'ifrit',
		[73] = 'phantom',
		[74] = 'shiva',
		[75] = 'unicorn',
		[76] = 'bismark',
		[77] = 'carbunkl',
		[78] = 'shoat',
		[79] = 'owzer_1',
		[80] = 'owzer_2',
		[81] = 'blackjack',
		[82] = 'figaro_guard_dead',
		[83] = 'number_024',
		[84] = 'treasure_chest',
		[85] = 'magi_warrior_2',
		[86] = 'atma',
		[87] = 'small_statue',
		[88] = 'flowers',
		[89] = 'envelope',
		[90] = 'plant',
		[91] = 'magicite',
		[92] = 'book',
		[93] = 'baby',
		[94] = 'question_mark',
		[95] = 'exclamation_point',
		[96] = 'slave_crown',
		[97] = 'weight',
		[98] = 'bird_bandana',
		[99] = 'eyes',
		[100] = 'bandana',
		[101] = 'nothing',
		[102] = 'flying_bird_1',
		[103] = 'flying_bird_2',
		[104] = 'big_sparkle',
		[105] = 'multi_sparkles',
		[106] = 'small_sparkle',
		[107] = 'coin',
		[108] = 'rat',
		[109] = 'turtle',
		[110] = 'small_bird_up',
		[111] = 'save_point',
		[112] = 'flame',
		[113] = 'explosion',
		[114] = 'tentacle_1',
		[115] = 'tentacle_2',
		[116] = 'big_switch',
		[117] = 'floor_switch',
		[118] = 'rock',
		[119] = 'crane_hook_3',
		[120] = 'elevator',
		[121] = 'flying_terra_1',
		[122] = 'flying_terra_2',
		[123] = 'ending_terra_3',
		[124] = 'diving_helmet',
		[125] = 'guardian_1',
		[126] = 'guardian_2',
		[127] = 'guardian_3',
		[128] = 'crane_hook_2',
		[129] = 'guardian_4',
		[130] = 'guardian_5',
		[131] = 'guardian_6',
		[132] = 'crane_hook_1',
		[133] = 'magitek_machine',
		[134] = 'gate_1',
		[135] = 'gate_2',
		[136] = 'gate_3',
		[137] = 'air_force',
		[138] = 'leo_sword',
		[139] = 'magitek_train_1',
		[140] = 'magitek_train_2',
		[141] = 'magitek_train_3',
		[142] = 'magitek_train_4',
		[143] = 'crane_1',
		[144] = 'crane_2',
		[145] = 'crane_3',
		[146] = 'chadarnook_1',
		[147] = 'chadarnook_2',
		[148] = 'chadarnook_3',
		[149] = 'falcon_1',
		[150] = 'falcon_2',
		[151] = 'falcon_3',
		[152] = 'flying_terra_3',
		[153] = 'tritoch',
		[154] = 'odin',
		[155] = 'goddess_1',
		[156] = 'doom_1',
		[157] = 'poltergeist_1',
		[158] = 'goddess_2',
		[159] = 'goddess_3',
		[160] = 'doom_2',
		[161] = 'doom_3',
		[162] = 'ending_terra_1',
		[163] = 'ending_terra_2',
		[164] = 'small_bird_left',
	}
	local spriteIndexes = table.map(spriteNames, function(name,index) return index, name end):setmetatable(nil)

	-- grep'ing everything8215/ff6 and getting all npc_gfx sprite + palette instances...
	-- then remove "nothing" sprites and remove no explicit palette npcs
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
		{'air_force', 'cyan_shadow_setzer'},
		{'arvis', 'cyan_shadow_setzer'},
		{'atma', 'strago_relm_gau_gogo'},
		{'baby', 'terra'},
		{'bandana', 'edgar_sabin_celes'},
		{'bandit', 'edgar_sabin_celes'},
		{'bandit', 'locke'},
		{'bandit', 'strago_relm_gau_gogo'},
		{'banon', 'locke'},
		{'banon', 'strago_relm_gau_gogo'},
		{'big_sparkle', 'rainbow'},
		{'big_sparkle', 'strago_relm_gau_gogo'},
		{'big_switch', 'rainbow'},
		{'bird', 'cyan_shadow_setzer'},
		{'bird_bandana', 'cyan_shadow_setzer'},
		{'bismark', 'cyan_shadow_setzer'},
		{'blackjack', 'cyan_shadow_setzer'},
		{'book', 'edgar_sabin_celes'},
		{'boy', 'edgar_sabin_celes'},
		{'boy', 'locke'},
		{'boy', 'strago_relm_gau_gogo'},
		{'boy', 'terra'},
		{'boy', 'vehicle'},
		{'carbunkl', 'terra'},
		{'celes_chains', 'edgar_sabin_celes'},
		{'celes_in_dress', 'edgar_sabin_celes'},
		{'celes_in_dress', 'vehicle'},
		{'chadarnook_1', 'rainbow'},
		{'chadarnook_2', 'rainbow'},
		{'chadarnook_3', 'rainbow'},
		{'figaro_chancellor', 'terra'},
		{'chupon', 'edgar_sabin_celes'},
		{'chupon', 'mog_umaro'},
		{'cid', 'strago_relm_gau_gogo'},
		{'clyde', 'locke'},
		{'coin', 'edgar_sabin_celes'},
		{'coin', 'rainbow'},
		{'crane_1', 'rainbow'},
		{'crane_2', 'rainbow'},
		{'crane_3', 'rainbow'},
		{'crane_hook_1', 'rainbow'},
		{'crane_hook_2', 'rainbow'},
		{'crane_hook_3', 'rainbow'},
		{'cyan', 'rainbow'},
		{'cyan', 'vehicle'},
		{'darill', 'terra'},
		{'diving_helmet', 'strago_relm_gau_gogo'},
		{'dog', 'cyan_shadow_setzer'},
		{'dog', 'locke'},
		{'doom_1', 'vehicle'},
		{'doom_2', 'vehicle'},
		{'doom_3', 'vehicle'},
		{'draco', 'cyan_shadow_setzer'},
		{'dragon', 'cyan_shadow_setzer'},
		{'dragon', 'edgar_sabin_celes'},
		{'dragon', 'locke'},
		{'dragon', 'strago_relm_gau_gogo'},
		{'dragon', 'terra'},
		{'edgar', 'locke'},
		{'elevator', 'rainbow'},
		{'elevator', 'vehicle'},
		{'emperor_servant', 'cyan_shadow_setzer'},
		{'emperor_servant', 'terra'},
		{'emperor_servant', 'vehicle'},
		{'ending_terra_1', 'terra'},
		{'ending_terra_2', 'terra'},
		{'ending_terra_3', 'terra'},
		{'envelope', 'strago_relm_gau_gogo'},
		{'elder_esper', 'cyan_shadow_setzer'},
		{'morphed_terra', 'rainbow'},
		{'morphed_terra', 'vehicle'},
		{'exclamation_point', 'rainbow'},
		{'explosion', 'rainbow'},
		{'explosion', 'vehicle'},
		{'eyes', 'edgar_sabin_celes'},
		{'faerie', 'terra'},
		{'falcon_1', 'vehicle'},
		{'falcon_2', 'vehicle'},
		{'falcon_3', 'vehicle'},
		{'figaro_guard', 'locke'},
		{'figaro_guard', 'terra'},
		{'figaro_guard_dead', 'terra'},
		{'fish', 'cyan_shadow_setzer'},
		{'flame', 'rainbow'},
		{'floor_switch', 'cyan_shadow_setzer'},
		{'flowers', 'strago_relm_gau_gogo'},
		{'flying_bird_1', 'cyan_shadow_setzer'},
		{'flying_bird_2', 'cyan_shadow_setzer'},
		{'flying_terra_1', 'rainbow'},
		{'flying_terra_3', 'rainbow'},
		{'gate_1', 'vehicle'},
		{'gate_2', 'vehicle'},
		{'gate_3', 'vehicle'},
		{'gau_bandana', 'strago_relm_gau_gogo'},
		{'gau_kung_fu', 'strago_relm_gau_gogo'},
		{'gestahl', 'strago_relm_gau_gogo'},
		{'ghost', 'edgar_sabin_celes'},
		{'ghost', 'rainbow'},
		{'girl', 'edgar_sabin_celes'},
		{'girl', 'locke'},
		{'goddess_1', 'vehicle'},
		{'goddess_2', 'vehicle'},
		{'goddess_3', 'vehicle'},
		{'gogo', 'mog_umaro'},
		{'guardian_1', 'rainbow'},
		{'guardian_1', 'vehicle'},
		{'guardian_2', 'rainbow'},
		{'guardian_2', 'vehicle'},
		{'guardian_3', 'rainbow'},
		{'guardian_3', 'vehicle'},
		{'guardian_4', 'rainbow'},
		{'guardian_4', 'vehicle'},
		{'guardian_5', 'rainbow'},
		{'guardian_5', 'vehicle'},
		{'guardian_6', 'rainbow'},
		{'guardian_6', 'vehicle'},
		{'hooker', 'edgar_sabin_celes'},
		{'hooker', 'terra'},
		{'ifrit', 'strago_relm_gau_gogo'},
		{'imp', 'edgar_sabin_celes'},
		{'opera_impresario', 'cyan_shadow_setzer'},
		{'katarin', 'cyan_shadow_setzer'},
		{'kefka', 'strago_relm_gau_gogo'},
		{'king_doma', 'terra'},
		{'leo', 'edgar_sabin_celes'},
		{'leo_sword', 'cyan_shadow_setzer'},
		{'maduin', 'cyan_shadow_setzer'},
		{'maduin', 'terra'},
		{'magicite', 'terra'},
		{'magitek_machine', 'vehicle'},
		{'magitek_train_1', 'rainbow'},
		{'magitek_train_1', 'vehicle'},
		{'magitek_train_2', 'rainbow'},
		{'magitek_train_2', 'vehicle'},
		{'magitek_train_3', 'rainbow'},
		{'magitek_train_3', 'vehicle'},
		{'magitek_train_4', 'rainbow'},
		{'magitek_train_4', 'vehicle'},
		{'magi_warrior_1', 'cyan_shadow_setzer'},
		{'magi_warrior_2', 'cyan_shadow_setzer'},
		{'man', 'edgar_sabin_celes'},
		{'man', 'locke'},
		{'man', 'strago_relm_gau_gogo'},
		{'merchant', 'edgar_sabin_celes'},
		{'merchant', 'locke'},
		{'merchant', 'terra'},
		{'monster', 'cyan_shadow_setzer'},
		{'monster', 'terra'},
		{'multi_sparkles', 'rainbow'},
		{'narshe_guard', 'edgar_sabin_celes'},
		{'narshe_guard', 'locke'},
		{'number_024', 'mog_umaro'},
		{'number_128', 'cyan_shadow_setzer'},
		{'odin', 'rainbow'},
		{'elder', 'cyan_shadow_setzer'},
		{'elder', 'edgar_sabin_celes'},
		{'elder', 'locke'},
		{'elder', 'strago_relm_gau_gogo'},
		{'matron', 'cyan_shadow_setzer'},
		{'matron', 'edgar_sabin_celes'},
		{'matron', 'strago_relm_gau_gogo'},
		{'owzer_1', 'strago_relm_gau_gogo'},
		{'owzer_2', 'strago_relm_gau_gogo'},
		{'phantom', 'cyan_shadow_setzer'},
		{'returner', 'locke'},
		{'returner', 'strago_relm_gau_gogo'},
		{'plant', 'edgar_sabin_celes'},
		{'poltergeist_1', 'vehicle'},
		{'question_mark', 'rainbow'},
		{'rachel', 'edgar_sabin_celes'},
		{'ramuh', 'cyan_shadow_setzer'},
		{'rat', 'strago_relm_gau_gogo'},
		{'scholar', 'cyan_shadow_setzer'},
		{'scholar', 'edgar_sabin_celes'},
		{'scholar', 'locke'},
		{'scholar', 'rainbow'},
		{'scholar', 'terra'},
		{'rock', 'rainbow'},
		{'sabin', 'rainbow'},
		{'save_point', 'mog_umaro'},
		{'save_point', 'rainbow'},
		{'shiva', 'edgar_sabin_celes'},
		{'shoat', 'terra'},
		{'shopkeeper', 'locke'},
		{'siegfried', 'cyan_shadow_setzer'},
		{'skull_statue', 'cyan_shadow_setzer'},
		{'slave_crown', 'cyan_shadow_setzer'},
		{'small_bird_left', 'cyan_shadow_setzer'},
		{'small_bird_up', 'cyan_shadow_setzer'},
		{'small_sparkle', 'mog_umaro'},
		{'small_sparkle', 'rainbow'},
		{'small_statue', 'strago_relm_gau_gogo'},
		{'soldier', 'cyan_shadow_setzer'},
		{'soldier', 'edgar_sabin_celes'},
		{'soldier', 'locke'},
		{'soldier', 'rainbow'},
		{'soldier', 'terra'},
		{'soldier', 'vehicle'},
		{'gau_dressed_up', 'strago_relm_gau_gogo'},
		{'tentacle_1', 'strago_relm_gau_gogo'},
		{'tentacle_2', 'strago_relm_gau_gogo'},
		{'train_conductor', 'cyan_shadow_setzer'},
		{'train_conductor', 'edgar_sabin_celes'},
		{'treasure_chest', 'rainbow'},
		{'treasure_chest', 'vehicle'},
		{'tritoch', 'terra'},
		{'turtle', 'vehicle'},
		{'ultros', 'mog_umaro'},
		{'ultros', 'terra'},
		{'umaro', 'mog_umaro'},
		{'unicorn', 'strago_relm_gau_gogo'},
		{'vargas', 'cyan_shadow_setzer'},
		{'vargas', 'vehicle'},
		{'weight', 'cyan_shadow_setzer'},
		{'wolf', 'cyan_shadow_setzer'},
		{'wolf', 'terra'},
		{'woman', 'edgar_sabin_celes'},
		{'woman', 'locke'},
		{'woman', 'strago_relm_gau_gogo'},
		{'woman', 'terra'},
		{'woman', 'vehicle'},
		{'yura', 'cyan_shadow_setzer'},
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
	addSpritePal('soldier', 1)
	addSpritePal('dog', 4)
	addSpritePal('celes_in_dress', 0)
	addSpritePal('returner', 1)
	addSpritePal('ultros', 5)
	addSpritePal('woman', 1)
	addSpritePal('boy', 3)
	addSpritePal('vargas', 4)
	addSpritePal('monster', 4)
	addSpritePal('train_conductor', 4)
	addSpritePal('wolf', 4)
	addSpritePal('emperor_servant', 2)
	addSpritePal('figaro_guard_riding', 2)
	addSpritePal('big_sparkle', 6)
	addSpritePal('coin', 0)
	addSpritePal('flying_terra_1', 2)
	addSpritePal('flying_terra_2', 2)
	addSpritePal('ending_terra_3', 2)
	addSpritePal('flying_terra_3', 2)
	addSpritePal('ending_terra_1', 2)
	addSpritePal('ending_terra_2', 2)
	addSpritePal('elevator', paletteIndexes.vehicle)
	addSpritePal('falcon_1', paletteIndexes.vehicle)
	addSpritePal('falcon_2', paletteIndexes.vehicle)
	addSpritePal('falcon_3', paletteIndexes.vehicle)


	-- sprite frames ...
	local frameNames = {
		[0] = 'walkd1', --'walking_down_1',
		'standd',		--'walking_down_2',
		'walkd2',		--'walking_down_3',
		'walku1',		--'walking_up_1',
		'standu',		--'walking_up_2',
		'walku2',		--'walking_up_3',
		'walkl1',		--'walking_left_1',
		'standl',		--'walking_left_2',
		'walkl2',		--'walking_left_3',
		'wound',		--'near_fatal',
		'ready',
		'pain',			--'hit',
		'stand',		--'attacking_1',
		'swing',		--'attacking_2',
		'handsupl1',	--'attacking_3',
		'handsupl2',	--'jumping',
		'cast1',		--'casting_1',
		'cast2',		--'casting_2',
		'dead',			--'dead_vert',
		'eyesclosed',	--'eyes_closed_down',
		'winkd',		--'winking_down',
		'eyesclosedl',	--'eyes_closed_left',
		'handsupd',		--'arms_up_down',
		'handsupu',		--'arms_up_up',
		'growl',		--'angry',
		'waved1',		--'waving_1_down',
		'waved2',		--'waving_2_down',
		'waveu1',		--'waving_1_up',
		'waveu2',		--'waving_2_up',
		'laugh1',		--'laughing_1',
		'laugh2',		--'laughing_2',
		'startled',		--'surprised',
		'sadd',			--'head_down_down',
		'sadu',			--'head_down_up',
		'sadl',			--'head_down_left',
		'peeved',		--'head_turned',
		'finger1',		--'wagging_finger_1',
		'finger2',		--'wagging_finger_2',
		'special',
		'tent',
		'dead2',		--'dead_horz',
		'npc_special_1',
		'npc_waving_1',
		'npc_waving_2',
		'npc_head_down_down',
		'npc_special_2',
		'riding_left_1',
		'riding_left_2',
		'ramuh_staff_raised',
		'ramuh_eyes_closed',
		'special_anim_1',
		'special_anim_2',
		'special_anim_3',
		'special_anim_4',
		'opera_singer_mouth_open',
		'opera_singer_mouth_closed',
		'opera_singer_unused',
		'map_sprite_frame_57',	-- 57
	}
	local frameIndexes = table.map(frameNames, function(name,index) return index, name end):setmetatable(nil)


	--[[
	alright now to track what sprites use what frames
	since it doesn't look too obvious...
	--]]
	local spriteFrames = {}
	for i=0,game.numCharacterSprites-1 do
		spriteFrames[i] = {}
	end
	-- terra-imp have only up to finger2, then tent, then two riding
	for sprite=spriteIndexes.terra,spriteIndexes.kefka do	-- terra - imp
		for frame=0,frameIndexes.finger2 do
			spriteFrames[sprite][frame] = true
		end
		spriteFrames[sprite][frameIndexes.tent] = true
		-- these also have dead_horz but it's a copy of dead...
		-- leo-kefka same but no riding
		if sprite <= spriteIndexes.imp then
			spriteFrames[sprite][frameIndexes.riding_left_1] = true
			spriteFrames[sprite][frameIndexes.riding_left_2] = true
		end
	end
	-- terra alone has special
	spriteFrames[spriteIndexes.terra][frameIndexes.special] = true
	-- these only have 9 walking frames, then some extras
	for sprite=spriteIndexes.gestahl,spriteIndexes.emperor_servant do
		for frame=0,8 do
			spriteFrames[sprite][frame] = true
		end
		-- gestahl and old man have this:
		if sprite <= spriteIndexes.elder then
			spriteFrames[sprite][frameIndexes.npc_special_1] = true
		end
		if sprite <= spriteIndexes.man then
			spriteFrames[sprite][frameIndexes.npc_waving_1] = true
			spriteFrames[sprite][frameIndexes.npc_waving_2] = true
			spriteFrames[sprite][frameIndexes.npc_head_down_down] = true
		end
	end
	spriteFrames[spriteIndexes.dog][frameIndexes.handsupl1] = true
	spriteFrames[spriteIndexes.dog][frameIndexes.npc_head_down_down] = true
	for sprite=spriteIndexes.celes_in_dress,spriteIndexes.draco do
		spriteFrames[sprite][frameIndexes.opera_singer_mouth_open] = true
		spriteFrames[sprite][frameIndexes.opera_singer_mouth_closed] = true
		spriteFrames[sprite][frameIndexes.opera_singer_unused] = true
	end
	for sprite=spriteIndexes.celes_in_dress,spriteIndexes.maduin do
		if sprite ~= spriteIndexes.gau_dressed_up
		and sprite ~= spriteIndexes.elder_esper
		and sprite ~= spriteIndexes.cid
		then
			spriteFrames[sprite][frameIndexes.npc_special_2] = true
		end
	end
	spriteFrames[spriteIndexes.figaro_guard][frameIndexes.riding_left_1] = true
	spriteFrames[spriteIndexes.figaro_guard][frameIndexes.riding_left_2] = true
	-- the rest are single-frame
	for sprite=spriteIndexes.ramuh,spriteIndexes.atma do
		spriteFrames[sprite][0] = true
	end
	spriteFrames[spriteIndexes.ramuh][frameIndexes.ramuh_staff_raised] = true
	spriteFrames[spriteIndexes.ramuh][frameIndexes.ramuh_eyes_closed] = true
	-- or it's just a coincicdence that this matches figaro_guard_dead
	--spriteFrames[spriteIndexes.figaro_guard_riding][frameIndexes.handsupu] = true

	-- animated don't use 0, but do use special thru special + #frames-1 enabled
	-- find these by searching everything8215/ff6/src/event/npc_prop.asm
	--  and looking for all npcs flagged with npc_anim, then note the npc_gfx, and note the animation count and the frame name
	local animationFrameCount = {
		flying_bird_1 = 2,
		flying_bird_2 = 2,
		big_sparkle = 2,
		multi_sparkles = 2,
		small_sparkle = 2,
		coin = 2,
		rat = 2,
		turtle = 2,
		small_bird_up = 2,
		save_point = 4,
		flame = 4,
		explosion = 4,
		tentacle_1 = 4,
		tentacle_2 = 4,
	}
	for sprite=spriteIndexes.small_statue,spriteIndexes.tentacle_2 do
		local frameCount = animationFrameCount[spriteNames[sprite]] or 1
		for i=0,frameCount-1 do
			spriteFrames[sprite][frameIndexes.special_anim_1+i] = true
		end
	end

	-- ok now the only way to find if a sprite is 16x24, 16x16, or 32x32 is to look at all the NPCs that use it
	-- these are everything8215/ff6/src/event/npc_prop.asm flagged as 'special_npc_prop' 
	-- kind of like how to tell what palettes it works with
	-- true means 16x16
	-- 32x32 means 32x32
	-- and '2 frames' is me noticing they are set to a npc_anim, but i'm lazy about getting into that just yet
	local spriteSpecial = {
		air_force = true,
		falcon_1 = '32x32',
		falcon_2 = true,
		falcon_3 = true,
		flying_terra_1 = '2 frames',
		flying_terra_2 = '2 frames',	-- not in the list, hmm
		flying_terra_3 = '2 frames',
		ending_terra_1 = '2 frames',
		ending_terra_2 = '2 frames',
		ending_terra_3 = true,
		tritoch = '32x32',
		leo_sword = true,
		diving_helmet = true,
		chadarnook_3 = '32x32',
		chadarnook_1 = true,
		chadarnook_2 = true,
		small_bird_left = '2 frames',
		gate_1 = '32x32',
		gate_2 = true,
		gate_3 = true,
		crane_hook_2 = true,
		crane_hook_1 = true,
		crane_hook_3 = true,
		magitek_train_1 = true,
		magitek_train_3 = true,
		magitek_train_2 = true,
		magitek_train_4 = true,
		crane_1 = true,
		crane_2 = true,
		crane_3 = true,
		guardian_1 = true,
		guardian_2 = true,
		guardian_3 = true,
		guardian_4 = true,
		guardian_5 = true,
		guardian_6 = true,
		floor_switch = true,
		big_switch = true,
		rock = true,
		magitek_machine = '32x32',
		elevator = '32x32',
		poltergeist_1 = '32x32',
		doom_1 = '32x32',
		doom_2 = true,
		doom_3 = true,
		goddess_1 = '32x32',
		goddess_2 = true,
		goddess_3 = true,
		odin = '32x32',
	}

	do
		--4bpp means 32 bytes per 8x8 tile ...
		--  0x150000 - 0x185000 has 6784 8x8 tiles = 1696 16x16 tiles
		local ptr = game.fieldSpriteGraphics

		print'spritePalettes = {'
		for sprite=0,game.numCharacterSprites-1 do
			local palIndexes = palettesForSprites[spriteNames[sprite]]
			local palIndex = palIndexes and palIndexes:last() or 0
			print('\t['..sprite..'] = '..palIndex..',\t-- '..spriteNames[sprite]..' = '..tostring(paletteNames[palIndex]))
		end
		print'}'


		-- collect into a tilesImgWriter-list and into a multiple-sheet list
		local tilesImgWriter = {}
		function tilesImgWriter:init()
			self.tilesWide = 58*2+1
			self.tilesHigh = 356
			self.img = Image(tileWidth*self.tilesWide, tileHeight*self.tilesHigh, 4, uint8_t):clear()
			self.dst = vec2i()
		end
		function tilesImgWriter:writeSprite(args)
			local allFramesWidths = args.frameImgs:mapi(function(img) return img.width end):sum()
			local maxFrameHeight = math.max(args.frameImgs:mapi(function(img) return img.height end):unpack())
			if self.dst.x + allFramesWidths >= self.img.width then
				self.dst.x = 0
				self.dst.y = self.dst.y + maxFrameHeight
			end
			for framePlus1,frameImg in ipairs(args.frameImgs) do
				self:writeFrame{
					sprite = args.sprite,
					frame = framePlus1-1,
					frameImg = frameImg,
				}
			end
			self.dst.x = self.dst.x + 8
		end
		function tilesImgWriter:writeFrame(args)
			local img = self.img

			-- frame to our tilesImgWriter sheet...
			if self.dst.x > img.width then
				print('!!! WARNING !!! dst.x='..self.dst.x..' > img.y='..img.width)
			end
			if self.dst.y > img.height then
				print('!!! WARNING !!! dst.y='..self.dst.y..' > img.y='..img.height)
			end
			img:pasteInto{
				image = args.frameImg:rgba(),
				x = self.dst.x,
				y = self.dst.y,
			}

			if not spriteFrames[args.sprite][args.frame] then
				for j=0,args.frameImg.height-1 do
					for i=0,args.frameImg.width-1 do
						local ofs = 4 * (self.dst.x+i + img.width * (self.dst.y+j))
						if img.buffer[3 + ofs] < 127 then
							img.buffer[0 + ofs] = 0
							img.buffer[1 + ofs] = 255
							img.buffer[2 + ofs] = 255
							img.buffer[3 + ofs] = 255
						end
					end
				end
			end

			self.dst.x = self.dst.x + args.frameImg.width
		end

		function tilesImgWriter:endSprite()
			self.dst.x = self.dst.x + 8
		end
		function tilesImgWriter:done()
			self.img:save'all-field-tiles.png'
		end


		local sheetImgWriter = {}
		function sheetImgWriter:init()
			self.anims = table()
			self.outDir = path'npc_sprites'
			self.outDir:mkdir()
			self.img = Image(256, 256, 1, uint8_t)
			self.sheetIndex = 0
			self.dst = vec2i()
		end
		function sheetImgWriter:flushCharSheet()
			local basename = 'sheet'..self.sheetIndex
			self.img:save(self.outDir(basename..'.png'))
			self.img:clear()
			self.sheetIndex = self.sheetIndex + 1
			self.dst = vec2i()
			self.anims:insert'_new_sheet_\n'
		end
		function sheetImgWriter:writeSprite(args)
			-- filter out garbage/unused frames
			local frameImgs = table(args.frameImgs)
			local frameNums = range(0,#frameImgs-1)
			assert.eq(#frameImgs, #frameNums)
			for framePlus1=#frameImgs,1,-1 do
				if not spriteFrames[args.sprite][framePlus1-1] then
					frameImgs:remove(framePlus1)
					frameNums:remove(framePlus1)
				end
			end
			assert.eq(#frameImgs, #frameNums)
			-- simulate frame inc across all frames
			-- see if we are still in this sheet
			-- if not then advance early
			local newdst = self.dst:clone()
			local reset
			for _,frameImg in ipairs(frameImgs) do
				newdst, reset = self:dstinc(newdst, frameImg)
				if reset then break end
			end
			if reset then
				self:flushCharSheet()
			end

			--[[ proper
			self.anims:insert'do\n'
			self.anims:insert'\tlocal anim = {\n'
			self.anims:insert('\t\tname = '..tolua(spriteNames[args.sprite])..',\n')
			self.anims:insert'\t\tframes = {\n'
			--]]
			-- [[ concise
			self.anims:insert(spriteNames[args.sprite])
			--]]

			for i,frameImg in ipairs(frameImgs) do
				self:writeFrame{
					sprite = args.sprite,
					frame = frameNums[i],
					frameImg = frameImg,
				}
			end

			--[[ proper
			self.anims:insert'\t\t},\n'
			self.anims:insert'\t\tseqs = table.union({}, charSeqs),\n'
			self.anims:insert'\t}\n'
			self.anims:insert'\tanim.frame0 = anim.frames!.standd\n'
			self.anims:insert'\tanim.seq0 = anim.seqs!.stand\n'
			self.anims:insert'\ttable.insert(anims, anim)\n'
			self.anims:insert'end\n'
			--]]
			-- [[ concise
			self.anims:insert'\n'
			--]]
		end
		function sheetImgWriter:dstinc(dst, img)
			dst = dst:clone()
			local reset
			dst.x = dst.x + img.width
			if dst.x + img.width > self.img.width then
				dst.x = 0
				dst.y = dst.y + img.height
				if dst.y + img.height > self.img.height then
					dst.y = 0
					reset = true
				end
			end
			return dst, reset
		end
		function sheetImgWriter:writeFrame(args)
			local frameImg = args.frameImg
			self.img.palette = frameImg.palette
			self.img:pasteInto{
				image = frameImg,
				x = self.dst.x,
				y = self.dst.y,
			}
			local frameName = frameNames[args.frame]
			if args.sprite >= spriteIndexes.ramuh then
				-- starting with ramuh ... the 1st frame is no longer stepping-down but is standing looking down...
				if frameName == 'special_anim_1' then
					frameName = 'stand1'
				elseif frameName == 'special_anim_2' then
					frameName = 'stand2'
				elseif frameName == 'special_anim_3' then
					frameName = 'stand3'
				elseif frameName == 'special_anim_4' then
					frameName = 'stand4'
				elseif frameName == 'walkd1' then
					frameName = 'stand1'
				else
					frameName = 'stand2'
				end
			end
			--[[ proper
			self.anims:insert('\t\t\t'..frameName..' = {\t-- '..args.frame..'\n')
			self.anims:insert("\t\t\t\tspriteIndex = "..(self.dst.x / 8)
				.." | ("..(self.dst.y / 8).." << 5)"
				.." | (("..(self.sheetIndex+1).." + sheetBlobIndexForName!['Characters #1']) << 10),\n")
			self.anims:insert('\t\t\t\tspriteWidth = '..(frameImg.width / 8)..',\n')
			self.anims:insert('\t\t\t\tspriteHeight = '..(frameImg.height / 8)..',\n')
			self.anims:insert'\t\t\t},\n'
			--]]
			-- [[ concise
			if frameImg.width == 16 and frameImg.height == 24 then
				self.anims:insert(' '..frameName)
			elseif frameImg.width == 16 and frameImg.height == 16 then
				self.anims:insert(' +'..frameName)
			elseif frameImg.width == 32 and frameImg.height == 32 then
				self.anims:insert(' *'..frameName)
			else
				error("idk how to classify this frame size")
			end
			--]]

			local reset
			self.dst, reset = self:dstinc(self.dst, frameImg)
			if reset then
				self:flushCharSheet()
			end
		end
		function sheetImgWriter:done()
			self:flushCharSheet()
			self.outDir('anims.lua'):write(self.anims:concat())
		end

		local writers = table{tilesImgWriter, sheetImgWriter}
		local function writeCall(fk)
			return function(...)
				for _,wr in ipairs(writers) do
					local f = wr[fk]
					if f then f(wr, ...) end
				end
			end
		end

		writeCall'init'()

		for sprite=0,game.numCharacterSprites-1 do
			local palIndexes = palettesForSprites[spriteNames[sprite]]
			local palIndex = palIndexes and palIndexes:last()
-- see all?
--print('sprite', spriteNames[sprite], 'using palettes', palIndexes and palIndexes:concat', ')
			palIndex = bit.band(palIndex or 0, 0x1f)
			local palette = makePalette(game, game.characterPalettes + palIndex, 4, 16)

			-- output all frames but assume all are 2x3
			-- reveals a few extra singing frames that I didn't output before
			local maxFrames = game.countof(game.characterFrameTileOffsets) / 6
			-- TODO who determines this? and who determines where the offset info is?
			-- ... turns out the NPC data does. great.
			local spriteTileCount = 6
			-- max tile placement, used for bounds in the sheet
			local frameTilesWide = 2
			local frameTilesHigh = 3

			-- get the x y to place the tile at
			local function getTileXY(tileIndex)
				local x = tileIndex % frameTilesWide
				local y = (tileIndex - x) / frameTilesWide
				return x, y
			end

			-- get the offset from charBaseAddr to the 8x8x4bpp tile data
			local function getFrameTileOffset(frame, tileIndex)
				-- TODO sometimes this is characterFrameTileOffsets, sometimes I bet it is what's next ...
				return game.characterFrameTileOffsets[tileIndex + frame * spriteTileCount]
			end

			local special = spriteSpecial[spriteNames[sprite]]
			if special then
				if special == true then
					spriteTileCount = 4
					maxFrames = 1
				elseif special == '32x32' then
					spriteTileCount = 16
					maxFrames = 1
					frameTilesWide = 4
					frameTilesHigh = 4
				elseif special == '2 frames' then
					spriteTileCount = 4
					maxFrames = 2
				elseif special ~= nil then
					error'here'
				end
				-- enable our frames
				for i=0,maxFrames-1 do
					spriteFrames[sprite][i] = true
				end
				-- change our frame tile offset getter to linear?
				getFrameTileOffset = function(frame, tileIndex)
					return 0x20 * (tileIndex + spriteTileCount * frame)
				end
			end

			local frameImgs = table()

			local charBaseAddr = getTileOffsetForSprite(sprite)

-- tiles are 8x8x4bpp = 32 bytes = 0x20 bytes ...
-- for a 16x16 that is 0x80 bytes
local tileDataSize = (getTileOffsetForSprite(sprite+1) or 0x183000) - charBaseAddr
print(
	'sprite', sprite, spriteNames[sprite],
	--'tile ofs', ('%x'):format(charBaseAddr),
	--'size', ('%x'):format(tileDataSize),
	(tileDataSize/0x20)..' tiles'
)

			for frame=0,maxFrames-1 do
				local frameImg = Image(frameTilesWide*tileWidth, frameTilesHigh*tileHeight, 1, uint8_t):clear()
				frameImgs:insert(frameImg)

				frameImg.palette = palette

				-- blit to our 8x8 with palette set up
				for spriteTileIndex=0,spriteTileCount-1 do
					local x, y = getTileXY(spriteTileIndex)

					local tile = rom + charBaseAddr + getFrameTileOffset(frame, spriteTileIndex)

					-- tile to our frame...
					readTile(frameImg, 8*x, 8*y, tile, bpp)
				end
			end
			writeCall'writeSprite'{
				sprite = sprite,
				frameImgs = frameImgs,
			}
		end

		writeCall'done'()
	end


-----------------------------------------------------------------------


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
