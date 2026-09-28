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
		[0] = 'edgar_sabin_celes',
		[1] = 'locke',
		[2] = 'terra',
		[3] = 'strago_relm_gau_gogo',
		[4] = 'cyan_shadow_setzer',
		[5] = 'mog_umaro',
		[6] = 'rainbow',
		[7] = 'vehicle',
		[8] = 'esper_terra',
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
		[18] = 'esper_terra',
		[19] = 'merchant',
		[20] = 'ghost',
		[21] = 'kefka',
		[22] = 'gestahl',
		[23] = 'old_man',
		[24] = 'man',
		[25] = 'dog',
		[26] = 'celes_dress',
		[27] = 'rich_man',
		[28] = 'draco',
		[29] = 'arvis',
		[30] = 'pilot',
		[31] = 'ultros',
		[32] = 'spiffy_gau',
		[33] = 'hooker',
		[34] = 'chancellor',
		[35] = 'clyde',
		[36] = 'old_woman',
		[37] = 'woman',
		[38] = 'boy',
		[39] = 'girl',
		[40] = 'bird',
		[41] = 'rachel',
		[42] = 'katarin',
		[43] = 'impresario',
		[44] = 'esper_elder',
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
		{'celes_dress', 'edgar_sabin_celes'},
		{'celes_dress', 'vehicle'},
		{'chadarnook_1', 'rainbow'},
		{'chadarnook_2', 'rainbow'},
		{'chadarnook_3', 'rainbow'},
		{'chancellor', 'terra'},
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
		{'esper_elder', 'cyan_shadow_setzer'},
		{'esper_terra', 'rainbow'},
		{'esper_terra', 'vehicle'},
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
		{'impresario', 'cyan_shadow_setzer'},
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
		{'old_man', 'cyan_shadow_setzer'},
		{'old_man', 'edgar_sabin_celes'},
		{'old_man', 'locke'},
		{'old_man', 'strago_relm_gau_gogo'},
		{'old_woman', 'cyan_shadow_setzer'},
		{'old_woman', 'edgar_sabin_celes'},
		{'old_woman', 'strago_relm_gau_gogo'},
		{'owzer_1', 'strago_relm_gau_gogo'},
		{'owzer_2', 'strago_relm_gau_gogo'},
		{'phantom', 'cyan_shadow_setzer'},
		{'pilot', 'locke'},
		{'pilot', 'strago_relm_gau_gogo'},
		{'plant', 'edgar_sabin_celes'},
		{'poltergeist_1', 'vehicle'},
		{'question_mark', 'rainbow'},
		{'rachel', 'edgar_sabin_celes'},
		{'ramuh', 'cyan_shadow_setzer'},
		{'rat', 'strago_relm_gau_gogo'},
		{'rich_man', 'cyan_shadow_setzer'},
		{'rich_man', 'edgar_sabin_celes'},
		{'rich_man', 'locke'},
		{'rich_man', 'rainbow'},
		{'rich_man', 'terra'},
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
		{'spiffy_gau', 'strago_relm_gau_gogo'},
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
	addSpritePal('celes_dress', 0)
	addSpritePal('pilot', 1)
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


	-- sprite frames ...
	local frameNames = {
		[0x00] = 'walking_down_1',
		[0x01] = 'walking_down_2',
		[0x02] = 'walking_down_3',
		[0x03] = 'walking_up_1',
		[0x04] = 'walking_up_2',
		[0x05] = 'walking_up_3',
		[0x06] = 'walking_left_1',
		[0x07] = 'walking_left_2',
		[0x08] = 'walking_left_3',
		[0x09] = 'near_fatal',
		[0x0a] = 'ready',
		[0x0b] = 'hit',
		[0x0c] = 'attacking_1',
		[0x0d] = 'attacking_2',
		[0x0e] = 'attacking_3',
		[0x0f] = 'jumping',
		[0x10] = 'casting_1',
		[0x11] = 'casting_2',
		[0x12] = 'dead_vert',
		[0x13] = 'eyes_closed_down',
		[0x14] = 'winking_down',
		[0x15] = 'eyes_closed_left',
		[0x16] = 'arms_up_down',
		[0x17] = 'arms_up_up',
		[0x18] = 'angry',
		[0x19] = 'waving_1_down',
		[0x1a] = 'waving_2_down',
		[0x1b] = 'waving_1_up',
		[0x1c] = 'waving_2_up',
		[0x1d] = 'laughing_1',
		[0x1e] = 'laughing_2',
		[0x1f] = 'surprised',
		[0x20] = 'head_down_down',
		[0x21] = 'head_down_up',
		[0x22] = 'head_down_left',
		[0x23] = 'head_turned',
		[0x24] = 'wagging_finger_1',
		[0x25] = 'wagging_finger_2',
		[0x26] = 'special',
		[0x27] = 'tent',
		[0x28] = 'dead_horz',
		[0x29] = 'npc_special_1',
		[0x2a] = 'npc_waving_1',
		[0x2b] = 'npc_waving_2',
		[0x2c] = 'npc_head_down_down',
		[0x2d] = 'npc_special_2',
		[0x2e] = 'riding_left_1',
		[0x2f] = 'riding_left_2',
		[0x30] = 'ramuh_staff_raised',
		[0x31] = 'ramuh_eyes_closed',
		[0x32] = 'special_anim_1',
		[0x33] = 'special_anim_2',
		[0x34] = 'special_anim_3',
		[0x35] = 'special_anim_4',
		[0x36] = 'opera_singer_mouth_open',
		[0x37] = 'opera_singer_mouth_closed',
		[0x38] = 'opera_singer_unused',
		[0x39] = 'map_sprite_frame_57',
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
	-- terra-imp have only up to wagging_finger_2, then tent, then two riding
	for sprite=spriteIndexes.terra,spriteIndexes.kefka do	-- terra - imp
		for frame=0,frameIndexes.wagging_finger_2 do
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
		if sprite <= spriteIndexes.old_man then
			spriteFrames[sprite][frameIndexes.npc_special_1] = true
		end
		if sprite <= spriteIndexes.man then
			spriteFrames[sprite][frameIndexes.npc_waving_1] = true
			spriteFrames[sprite][frameIndexes.npc_waving_2] = true
			spriteFrames[sprite][frameIndexes.npc_head_down_down] = true
		end
	end
	spriteFrames[spriteIndexes.dog][frameIndexes.attacking_3] = true
	spriteFrames[spriteIndexes.dog][frameIndexes.npc_head_down_down] = true
	for sprite=spriteIndexes.celes_dress,spriteIndexes.draco do
		spriteFrames[sprite][frameIndexes.opera_singer_mouth_open] = true
		spriteFrames[sprite][frameIndexes.opera_singer_mouth_closed] = true
		spriteFrames[sprite][frameIndexes.opera_singer_unused] = true
	end
	for sprite=spriteIndexes.celes_dress,spriteIndexes.maduin do
		if sprite ~= spriteIndexes.spiffy_gau
		and sprite ~= spriteIndexes.esper_elder
		and sprite ~= spriteIndexes.cid
		then
			spriteFrames[sprite][frameIndexes.npc_special_2] = true
		end
	end
	spriteFrames[spriteIndexes.figaro_guard][frameIndexes.riding_left_1] = true
	spriteFrames[spriteIndexes.figaro_guard][frameIndexes.riding_left_2] = true
	-- the rest are single-frame
	for sprite=spriteIndexes.ramuh,164 do
		spriteFrames[sprite][0] = true
	end
	spriteFrames[spriteIndexes.ramuh][frameIndexes.ramuh_staff_raised] = true
	spriteFrames[spriteIndexes.ramuh][frameIndexes.ramuh_eyes_closed] = true
	-- or it's just a coincicdence that this matches figaro_guard_dead
	--spriteFrames[spriteIndexes.figaro_guard_riding][frameIndexes.arms_up_up] = true
	spriteFrames[spriteIndexes.flying_bird_1][1] = true
	for frame=1,6 do
		spriteFrames[spriteIndexes.big_sparkle][frame] = true
	end
	for frame=1,2 do
		spriteFrames[spriteIndexes.multi_sparkles][frame] = true
	end
	spriteFrames[spriteIndexes.coin][1] = true
	spriteFrames[spriteIndexes.rat][1] = true
	spriteFrames[spriteIndexes.turtle][1] = true
	spriteFrames[spriteIndexes.small_bird_up][1] = true
	for frame=1,3 do
		spriteFrames[spriteIndexes.save_point][frame] = true
	end
	for frame=1,3 do
		spriteFrames[spriteIndexes.flame][frame] = true
	end
	for frame=1,2 do
		spriteFrames[spriteIndexes.explosion][frame] = true
	end
	for frame=1,3 do
		spriteFrames[spriteIndexes.tentacle_1][frame] = true
		spriteFrames[spriteIndexes.tentacle_2][frame] = true
	end

	do
		--4bpp means 32 bytes per 8x8 tile ...
		--  0x150000 - 0x185000 has 6784 8x8 tiles = 1696 16x16 tiles
		local ptr = game.fieldSpriteGraphics
		--[[
		local tilesWide = 41*2+1
		local tilesHigh = 108
		--]]
		-- [[
		local tilesWide = 58*2+1
		local tilesHigh = 165*3+1
		--]]

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

			--[[
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
			--]]
			-- [[ just output all frames but assume all are 2x3
			-- reveals a few extra singing frames that I didn't output before
			local maxFrames = game.countof(game.characterFrameTileOffsets) / 6
			-- TODO who determines this? and who determines where the offset info is?
			local spriteTileCount = 6
			--]]

			-- why are these all offset by 1 tile?
			if sprite >= spriteIndexes.small_statue
			and sprite < spriteIndexes.big_switch
			then
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
	--'tile ofs', ('%x'):format(charBaseOffset),
	--'size', ('%x'):format(tileDataSize),
	(tileDataSize/0x20)..' tiles'
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

					if not spriteFrames[sprite][frame] then
						for j=0,7 do
							for i=0,7 do
								local ofs = 4 * (tiledstx+i + tilesImg.width * (tiledsty+j))
								if tilesImg.buffer[3 + ofs] < 127 then
									tilesImg.buffer[0 + ofs] = 0
									tilesImg.buffer[1 + ofs] = 255
									tilesImg.buffer[2 + ofs] = 255
									tilesImg.buffer[3 + ofs] = 255
								end
							end
						end
					end
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
