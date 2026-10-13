--[[
for npc / character sprites
--]]
local ffi = require 'ffi'
local table = require 'ext.table'
local Image = require 'image'

local uint8_t = ffi.typeof'uint8_t'

-- add sprite stuff to 'game'
return function(game)
	local rom = game.rom

	local tileWidth = require 'ff6.graphics'.tileWidth
	local tileHeight = require 'ff6.graphics'.tileHeight
	local readTile = require 'ff6.graphics'.readTile

	-- from everything8215/ff6/src/gfx/map_sprite_pal.inc:
	game.spritePaletteNames = {
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

	local spritePaletteIndexes = table.map(game.spritePaletteNames, function(name,index) return index, name end):setmetatable(nil)
	spritePaletteIndexes.edgar = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.sabin = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.celes = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.imp = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.leo = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.ghost = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.green_soldier = spritePaletteIndexes.edgar_sabin_celes
	spritePaletteIndexes.merchant = spritePaletteIndexes.locke
	spritePaletteIndexes.brown_soldier = spritePaletteIndexes.locke
	spritePaletteIndexes.strago = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.relm = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.gau = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.gogo = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.banon = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.kefka = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.gestahl = spritePaletteIndexes.strago_relm_gau_gogo
	spritePaletteIndexes.cyan = spritePaletteIndexes.cyan_shadow_setzer
	spritePaletteIndexes.shadow = spritePaletteIndexes.cyan_shadow_setzer
	spritePaletteIndexes.setzer = spritePaletteIndexes.cyan_shadow_setzer
	spritePaletteIndexes.mog = spritePaletteIndexes.mog_umaro
	spritePaletteIndexes.umaro = spritePaletteIndexes.mog_umaro
	game.spritePaletteIndexes = spritePaletteIndexes

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
	game.spriteNames = spriteNames
	local spriteIndexes = table.map(spriteNames, function(name,index) return index, name end):setmetatable(nil)
	game.spriteIndexes = spriteIndexes

	-- grep'ing everything8215/ff6 and getting all npc_gfx sprite + palette instances...
	-- then remove "nothing" sprites and remove no explicit palette npcs
	-- then sort and remove duplicates
	game.palettesForSprites = {}
	local function addSpritePal(spriteName, paletteIndex)
		if not paletteIndex then return end
		game.palettesForSprites[spriteName] = game.palettesForSprites[spriteName] or table()
		game.palettesForSprites[spriteName]:removeObject(paletteIndex)
		game.palettesForSprites[spriteName]:insert(paletteIndex)
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
		addSpritePal(spriteName, spritePaletteIndexes[paletteName])
	end
--]]
	-- if any palette indexes have matching names with sprite names then set those
	-- do this last to override defaults
	for name,index in pairs(spritePaletteIndexes) do
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
	addSpritePal('elevator', spritePaletteIndexes.vehicle)
	addSpritePal('falcon_1', spritePaletteIndexes.vehicle)
	addSpritePal('falcon_2', spritePaletteIndexes.vehicle)
	addSpritePal('falcon_3', spritePaletteIndexes.vehicle)

	-- sprite frames ... my legacy names vs everything8215/ff6's names
	game.spriteFrameNames = {
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
	local spriteFrameIndexes = table.map(game.spriteFrameNames, function(name,index) return index, name end):setmetatable(nil)
	game.spriteFrameIndexes = spriteFrameIndexes


	--[[
	alright now to track what sprites use what frames
	since it doesn't look too obvious...
	--]]
	local spriteFrames = {}
	game.spriteFrames = spriteFrames
	for i=0,game.numCharacterSprites-1 do
		spriteFrames[i] = {}
	end
	-- terra-imp have only up to finger2, then tent, then two riding
	for sprite=spriteIndexes.terra,spriteIndexes.kefka do	-- terra - imp
		for frame=0,spriteFrameIndexes.finger2 do
			spriteFrames[sprite][frame] = true
		end
		spriteFrames[sprite][spriteFrameIndexes.tent] = true
		-- these also have dead_horz but it's a copy of dead...
		-- leo-kefka same but no riding
		if sprite <= spriteIndexes.imp then
			spriteFrames[sprite][spriteFrameIndexes.riding_left_1] = true
			spriteFrames[sprite][spriteFrameIndexes.riding_left_2] = true
		end
	end
	-- terra alone has special
	spriteFrames[spriteIndexes.terra][spriteFrameIndexes.special] = true
	-- these only have 9 walking frames, then some extras
	for sprite=spriteIndexes.gestahl,spriteIndexes.emperor_servant do
		for frame=0,8 do
			spriteFrames[sprite][frame] = true
		end
		-- gestahl and old man have this:
		if sprite <= spriteIndexes.elder then
			spriteFrames[sprite][spriteFrameIndexes.npc_special_1] = true
		end
		if sprite <= spriteIndexes.man then
			spriteFrames[sprite][spriteFrameIndexes.npc_waving_1] = true
			spriteFrames[sprite][spriteFrameIndexes.npc_waving_2] = true
			spriteFrames[sprite][spriteFrameIndexes.npc_head_down_down] = true
		end
	end
	spriteFrames[spriteIndexes.dog][spriteFrameIndexes.handsupl1] = true
	spriteFrames[spriteIndexes.dog][spriteFrameIndexes.npc_head_down_down] = true
	for sprite=spriteIndexes.celes_in_dress,spriteIndexes.draco do
		spriteFrames[sprite][spriteFrameIndexes.opera_singer_mouth_open] = true
		spriteFrames[sprite][spriteFrameIndexes.opera_singer_mouth_closed] = true
		spriteFrames[sprite][spriteFrameIndexes.opera_singer_unused] = true
	end
	for sprite=spriteIndexes.celes_in_dress,spriteIndexes.maduin do
		if sprite ~= spriteIndexes.gau_dressed_up
		and sprite ~= spriteIndexes.elder_esper
		and sprite ~= spriteIndexes.cid
		then
			spriteFrames[sprite][spriteFrameIndexes.npc_special_2] = true
		end
	end
	spriteFrames[spriteIndexes.figaro_guard][spriteFrameIndexes.riding_left_1] = true
	spriteFrames[spriteIndexes.figaro_guard][spriteFrameIndexes.riding_left_2] = true
	-- the rest are single-frame
	for sprite=spriteIndexes.ramuh,spriteIndexes.atma do
		spriteFrames[sprite][0] = true
	end
	spriteFrames[spriteIndexes.ramuh][spriteFrameIndexes.ramuh_staff_raised] = true
	spriteFrames[spriteIndexes.ramuh][spriteFrameIndexes.ramuh_eyes_closed] = true
	-- or it's just a coincicdence that this matches figaro_guard_dead
	--spriteFrames[spriteIndexes.figaro_guard_riding][spriteFrameIndexes.handsupu] = true

	-- animated don't use 0, but do use special thru special + #frames-1 enabled
	-- find these by searching everything8215/ff6/src/event/npc_prop.asm
	--  and looking for all npcs flagged with npc_anim, then note the npc_gfx, and note the animation count and the frame name
	game.animationFrameCount = {
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
		local frameCount = game.animationFrameCount[spriteNames[sprite]] or 1
		for i=0,frameCount-1 do
			spriteFrames[sprite][spriteFrameIndexes.special_anim_1+i] = true
		end
	end

	-- ok now the only way to find if a sprite is 16x24, 16x16, or 32x32 is to look at all the NPCs that use it
	-- these are everything8215/ff6/src/event/npc_prop.asm flagged as 'special_npc_prop'
	-- kind of like how to tell what palettes it works with
	-- true means 16x16
	-- 32x32 means 32x32
	-- and '2 frames' is me noticing they are set to a npc_anim, but i'm lazy about getting into that just yet
	game.spriteSpecial = {
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

	function game.getSpriteFrameCount(spriteIndex)
		local maxFrames = game.countof(game.characterFrameTileOffsets) / 6
		local special = game.spriteSpecial[spriteNames[spriteIndex]]
		if special then
			if special == true then
				maxFrames = 1
			elseif special == '32x32' then
				maxFrames = 1
			elseif special == '2 frames' then
				maxFrames = 2
			end
		end
		return maxFrames
	end

	for spriteIndex=0,game.numCharacterSprites-1 do
		local maxFrames = game.getSpriteFrameCount(spriteIndex)
		local special = game.spriteSpecial[spriteNames[spriteIndex]]
		if special then
			-- enable our frames
			for i=0,maxFrames-1 do
				spriteFrames[spriteIndex][i] = true
			end
		end
	end

	function game.getSpriteFrameImage(spriteIndex, frameIndex)
		if not frameIndex then
			frameIndex = table.keys(spriteFrames[spriteIndex]):sort()[1]
print('for sprite', spriteIndex, 'getting frame', frameIndex)
		end

		-- unique per sprite

		local maxFrames = game.getSpriteFrameCount(spriteIndex)

		local palIndexes = game.palettesForSprites[spriteNames[spriteIndex]]
		local palIndex = palIndexes and palIndexes:last()
-- see all?
--print('sprite', spriteNames[spriteIndex], 'using palettes', palIndexes and palIndexes:concat', ')
		palIndex = bit.band(palIndex or 0, 0x1f)
		local palette = game.getCharPal(palIndex)

		-- output all frames but assume all are 2x3
		-- TODO who determines this? and who determines where the offset info is?
		-- ... turns out the NPC data does. great.
		local spriteTileCount = 6
		-- max tile placement, used for bounds in the sheet
		local frameTilesWide = 2
		local frameTilesHigh = 3

		-- get the offset from charBaseAddr to the 8x8x4bpp tile data
		local function getFrameTileOffset(frameIndex, tileIndex)
			-- TODO sometimes this is characterFrameTileOffsets, sometimes I bet it is what's next ...
			return game.characterFrameTileOffsets[tileIndex + frameIndex * spriteTileCount]
		end

		local special = game.spriteSpecial[spriteNames[spriteIndex]]
		if special then
			if special == true then
				spriteTileCount = 4
				frameTilesWide = 2
				frameTilesHigh = 2
			elseif special == '32x32' then
				spriteTileCount = 16
				frameTilesWide = 4
				frameTilesHigh = 4
			elseif special == '2 frames' then
				spriteTileCount = 4
				frameTilesWide = 2
				frameTilesHigh = 2
			elseif special ~= nil then
				error'here'
			end

			-- change our frame tile offset getter to linear?
			getFrameTileOffset = function(frameIndex, tileIndex)
				return 0x20 * (tileIndex + spriteTileCount * frameIndex)
			end
		end

		local charBaseAddr = getTileOffsetForSprite(spriteIndex)

--[[
-- tiles are 8x8x4bpp = 32 bytes = 0x20 bytes ...
-- for a 16x16 that is 0x80 bytes
local tileDataSize = (getTileOffsetForSprite(spriteIndex+1) or 0x183000) - charBaseAddr
print(
	'sprite', spriteIndex, spriteNames[spriteIndex],
	--'tile ofs', ('%x'):format(charBaseAddr),
	--'size', ('%x'):format(tileDataSize),
	(tileDataSize/0x20)..' tiles'
)
--]]

		-- unique per frame
		local bpp = 4

		local frameImg = Image(frameTilesWide*tileWidth, frameTilesHigh*tileHeight, 1, uint8_t):clear()
		frameImg.palette = palette

		-- blit to our 8x8 with palette set up
		for spriteTileIndex=0,spriteTileCount-1 do
			local x = spriteTileIndex % frameTilesWide
			local y = (spriteTileIndex - x) / frameTilesWide

			local tile = rom + charBaseAddr + getFrameTileOffset(frameIndex, spriteTileIndex)

			-- tile to our frame...
			readTile(frameImg, 8*x, 8*y, tile, bpp)
		end

		return frameImg
	end
end
