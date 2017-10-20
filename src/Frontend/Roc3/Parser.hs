{-# OPTIONS_GHC -w #-}
-- if any change is made to the parser, type at the terminal
-- happy Parser.y
-- to replace Parser.hs file. The cabal file should generate this automatically,
-- but it doesn't; this happened after introducing multiple directories.
module Frontend.Roc3.Parser where
import Lexer
import Ast
import Data.List
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35

action_0 (61) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (7) = happyGoto action_2
action_0 _ = happyReduce_6

action_1 (61) = happyShift action_3
action_1 (7) = happyGoto action_2
action_1 _ = happyFail

action_2 (64) = happyShift action_10
action_2 (65) = happyShift action_11
action_2 (12) = happyGoto action_6
action_2 (13) = happyGoto action_7
action_2 (14) = happyGoto action_8
action_2 (15) = happyGoto action_9
action_2 _ = happyFail

action_3 (57) = happyShift action_5
action_3 _ = happyFail

action_4 (81) = happyAccept
action_4 _ = happyFail

action_5 (62) = happyShift action_17
action_5 (63) = happyShift action_18
action_5 (8) = happyGoto action_15
action_5 (9) = happyGoto action_16
action_5 _ = happyFail

action_6 (64) = happyShift action_10
action_6 (65) = happyShift action_11
action_6 (13) = happyGoto action_14
action_6 (14) = happyGoto action_8
action_6 (15) = happyGoto action_9
action_6 _ = happyReduce_1

action_7 _ = happyReduce_17

action_8 _ = happyReduce_20

action_9 _ = happyReduce_19

action_10 (80) = happyShift action_13
action_10 _ = happyFail

action_11 (64) = happyShift action_12
action_11 _ = happyFail

action_12 (80) = happyShift action_28
action_12 _ = happyFail

action_13 (36) = happyShift action_26
action_13 (40) = happyShift action_27
action_13 (23) = happyGoto action_25
action_13 _ = happyFail

action_14 _ = happyReduce_18

action_15 (44) = happyShift action_23
action_15 (45) = happyShift action_24
action_15 _ = happyFail

action_16 _ = happyReduce_8

action_17 (36) = happyShift action_21
action_17 (80) = happyShift action_22
action_17 _ = happyFail

action_18 (36) = happyShift action_19
action_18 (80) = happyShift action_20
action_18 _ = happyFail

action_19 (80) = happyShift action_38
action_19 (10) = happyGoto action_40
action_19 (11) = happyGoto action_37
action_19 _ = happyFail

action_20 (36) = happyShift action_39
action_20 _ = happyFail

action_21 (80) = happyShift action_38
action_21 (10) = happyGoto action_36
action_21 (11) = happyGoto action_37
action_21 _ = happyFail

action_22 (36) = happyShift action_35
action_22 _ = happyFail

action_23 _ = happyReduce_7

action_24 (62) = happyShift action_17
action_24 (63) = happyShift action_18
action_24 (9) = happyGoto action_34
action_24 _ = happyFail

action_25 (36) = happyShift action_33
action_25 _ = happyFail

action_26 (66) = happyShift action_32
action_26 (16) = happyGoto action_31
action_26 _ = happyFail

action_27 (79) = happyShift action_30
action_27 _ = happyFail

action_28 (40) = happyShift action_27
action_28 (23) = happyGoto action_29
action_28 _ = happyFail

action_29 (36) = happyShift action_58
action_29 _ = happyFail

action_30 (41) = happyShift action_57
action_30 _ = happyFail

action_31 (40) = happyShift action_52
action_31 (44) = happyReduce_27
action_31 (45) = happyReduce_27
action_31 (67) = happyShift action_53
action_31 (68) = happyShift action_54
action_31 (69) = happyShift action_55
action_31 (80) = happyShift action_56
action_31 (17) = happyGoto action_48
action_31 (18) = happyGoto action_49
action_31 (19) = happyGoto action_50
action_31 (21) = happyGoto action_51
action_31 _ = happyReduce_25

action_32 (57) = happyShift action_47
action_32 _ = happyFail

action_33 (66) = happyShift action_32
action_33 (16) = happyGoto action_46
action_33 _ = happyFail

action_34 _ = happyReduce_9

action_35 (80) = happyShift action_38
action_35 (10) = happyGoto action_45
action_35 (11) = happyGoto action_37
action_35 _ = happyFail

action_36 (37) = happyShift action_44
action_36 (45) = happyShift action_42
action_36 _ = happyFail

action_37 _ = happyReduce_14

action_38 _ = happyReduce_16

action_39 (80) = happyShift action_38
action_39 (10) = happyGoto action_43
action_39 (11) = happyGoto action_37
action_39 _ = happyFail

action_40 (37) = happyShift action_41
action_40 (45) = happyShift action_42
action_40 _ = happyFail

action_41 _ = happyReduce_13

action_42 (80) = happyShift action_38
action_42 (11) = happyGoto action_79
action_42 _ = happyFail

action_43 (37) = happyShift action_78
action_43 (45) = happyShift action_42
action_43 _ = happyFail

action_44 _ = happyReduce_12

action_45 (37) = happyShift action_77
action_45 (45) = happyShift action_42
action_45 _ = happyFail

action_46 (40) = happyShift action_52
action_46 (44) = happyReduce_27
action_46 (45) = happyReduce_27
action_46 (67) = happyShift action_53
action_46 (68) = happyShift action_54
action_46 (69) = happyShift action_55
action_46 (80) = happyShift action_56
action_46 (17) = happyGoto action_76
action_46 (18) = happyGoto action_49
action_46 (19) = happyGoto action_50
action_46 (21) = happyGoto action_51
action_46 _ = happyReduce_25

action_47 (80) = happyShift action_75
action_47 _ = happyFail

action_48 (38) = happyShift action_74
action_48 (24) = happyGoto action_72
action_48 (25) = happyGoto action_73
action_48 _ = happyFail

action_49 (44) = happyShift action_70
action_49 (45) = happyShift action_71
action_49 _ = happyFail

action_50 _ = happyReduce_29

action_51 (38) = happyShift action_69
action_51 (20) = happyGoto action_68
action_51 _ = happyReduce_31

action_52 (79) = happyShift action_66
action_52 (80) = happyShift action_67
action_52 _ = happyFail

action_53 (80) = happyShift action_65
action_53 _ = happyFail

action_54 (40) = happyShift action_64
action_54 _ = happyFail

action_55 (40) = happyShift action_63
action_55 (23) = happyGoto action_62
action_55 _ = happyFail

action_56 (36) = happyShift action_60
action_56 (80) = happyShift action_61
action_56 _ = happyFail

action_57 _ = happyReduce_45

action_58 (66) = happyShift action_32
action_58 (16) = happyGoto action_59
action_58 _ = happyFail

action_59 (40) = happyShift action_52
action_59 (44) = happyReduce_27
action_59 (45) = happyReduce_27
action_59 (67) = happyShift action_53
action_59 (68) = happyShift action_54
action_59 (69) = happyShift action_55
action_59 (80) = happyShift action_56
action_59 (17) = happyGoto action_95
action_59 (18) = happyGoto action_49
action_59 (19) = happyGoto action_50
action_59 (21) = happyGoto action_51
action_59 _ = happyReduce_25

action_60 (80) = happyShift action_94
action_60 (22) = happyGoto action_93
action_60 _ = happyReduce_42

action_61 _ = happyReduce_37

action_62 (40) = happyShift action_52
action_62 (67) = happyShift action_53
action_62 (68) = happyShift action_54
action_62 (69) = happyShift action_55
action_62 (80) = happyShift action_56
action_62 (21) = happyGoto action_92
action_62 _ = happyFail

action_63 (79) = happyShift action_30
action_63 (80) = happyShift action_91
action_63 _ = happyFail

action_64 (79) = happyShift action_90
action_64 _ = happyFail

action_65 _ = happyReduce_34

action_66 (41) = happyShift action_89
action_66 _ = happyFail

action_67 (41) = happyShift action_88
action_67 _ = happyFail

action_68 _ = happyReduce_30

action_69 (79) = happyShift action_86
action_69 (80) = happyShift action_87
action_69 _ = happyFail

action_70 _ = happyReduce_26

action_71 (40) = happyShift action_52
action_71 (67) = happyShift action_53
action_71 (68) = happyShift action_54
action_71 (69) = happyShift action_55
action_71 (80) = happyShift action_56
action_71 (19) = happyGoto action_85
action_71 (21) = happyGoto action_51
action_71 _ = happyFail

action_72 (37) = happyShift action_84
action_72 (38) = happyShift action_74
action_72 (25) = happyGoto action_83
action_72 _ = happyFail

action_73 _ = happyReduce_46

action_74 (80) = happyShift action_82
action_74 _ = happyFail

action_75 (44) = happyShift action_81
action_75 _ = happyFail

action_76 (38) = happyShift action_74
action_76 (24) = happyGoto action_80
action_76 (25) = happyGoto action_73
action_76 _ = happyFail

action_77 _ = happyReduce_10

action_78 _ = happyReduce_11

action_79 _ = happyReduce_15

action_80 (37) = happyShift action_106
action_80 (38) = happyShift action_74
action_80 (25) = happyGoto action_83
action_80 _ = happyFail

action_81 _ = happyReduce_24

action_82 (45) = happyShift action_105
action_82 _ = happyFail

action_83 _ = happyReduce_47

action_84 _ = happyReduce_21

action_85 _ = happyReduce_28

action_86 (39) = happyShift action_104
action_86 _ = happyFail

action_87 (39) = happyShift action_103
action_87 _ = happyFail

action_88 (40) = happyShift action_52
action_88 (67) = happyShift action_53
action_88 (68) = happyShift action_54
action_88 (69) = happyShift action_55
action_88 (80) = happyShift action_56
action_88 (21) = happyGoto action_102
action_88 _ = happyFail

action_89 (40) = happyShift action_52
action_89 (67) = happyShift action_53
action_89 (68) = happyShift action_54
action_89 (69) = happyShift action_55
action_89 (80) = happyShift action_56
action_89 (21) = happyGoto action_101
action_89 _ = happyFail

action_90 (58) = happyShift action_100
action_90 _ = happyFail

action_91 (41) = happyShift action_99
action_91 _ = happyFail

action_92 _ = happyReduce_40

action_93 (37) = happyShift action_97
action_93 (45) = happyShift action_98
action_93 _ = happyFail

action_94 _ = happyReduce_44

action_95 (38) = happyShift action_74
action_95 (24) = happyGoto action_96
action_95 (25) = happyGoto action_73
action_95 _ = happyFail

action_96 (37) = happyShift action_119
action_96 (38) = happyShift action_74
action_96 (25) = happyGoto action_83
action_96 _ = happyFail

action_97 _ = happyReduce_36

action_98 (80) = happyShift action_118
action_98 _ = happyFail

action_99 (40) = happyShift action_52
action_99 (67) = happyShift action_53
action_99 (68) = happyShift action_54
action_99 (69) = happyShift action_55
action_99 (80) = happyShift action_56
action_99 (21) = happyGoto action_117
action_99 _ = happyFail

action_100 (58) = happyShift action_116
action_100 _ = happyFail

action_101 _ = happyReduce_38

action_102 _ = happyReduce_39

action_103 _ = happyReduce_32

action_104 _ = happyReduce_33

action_105 (38) = happyShift action_109
action_105 (52) = happyShift action_110
action_105 (55) = happyShift action_111
action_105 (71) = happyShift action_112
action_105 (77) = happyShift action_113
action_105 (78) = happyShift action_114
action_105 (80) = happyShift action_115
action_105 (26) = happyGoto action_107
action_105 (34) = happyGoto action_108
action_105 _ = happyFail

action_106 _ = happyReduce_23

action_107 (39) = happyShift action_132
action_107 (45) = happyShift action_133
action_107 (49) = happyShift action_134
action_107 (50) = happyShift action_135
action_107 _ = happyFail

action_108 (42) = happyShift action_127
action_108 (43) = happyShift action_128
action_108 (47) = happyShift action_129
action_108 (48) = happyShift action_130
action_108 (51) = happyShift action_131
action_108 _ = happyFail

action_109 (38) = happyShift action_109
action_109 (52) = happyShift action_110
action_109 (55) = happyShift action_111
action_109 (71) = happyShift action_112
action_109 (77) = happyShift action_113
action_109 (78) = happyShift action_114
action_109 (80) = happyShift action_115
action_109 (26) = happyGoto action_126
action_109 (34) = happyGoto action_108
action_109 _ = happyFail

action_110 (38) = happyShift action_109
action_110 (52) = happyShift action_110
action_110 (55) = happyShift action_111
action_110 (71) = happyShift action_112
action_110 (77) = happyShift action_113
action_110 (78) = happyShift action_114
action_110 (80) = happyShift action_115
action_110 (26) = happyGoto action_125
action_110 (34) = happyGoto action_108
action_110 _ = happyFail

action_111 (80) = happyShift action_124
action_111 _ = happyFail

action_112 (51) = happyShift action_123
action_112 _ = happyReduce_93

action_113 _ = happyReduce_96

action_114 _ = happyReduce_97

action_115 (40) = happyShift action_121
action_115 (58) = happyShift action_122
action_115 _ = happyReduce_94

action_116 (79) = happyShift action_120
action_116 _ = happyFail

action_117 _ = happyReduce_41

action_118 _ = happyReduce_43

action_119 _ = happyReduce_22

action_120 (41) = happyShift action_159
action_120 _ = happyFail

action_121 (79) = happyShift action_158
action_121 _ = happyFail

action_122 (75) = happyShift action_156
action_122 (76) = happyShift action_157
action_122 _ = happyFail

action_123 (80) = happyShift action_141
action_123 (27) = happyGoto action_155
action_123 _ = happyFail

action_124 _ = happyReduce_54

action_125 (49) = happyShift action_134
action_125 (50) = happyShift action_135
action_125 _ = happyReduce_63

action_126 (39) = happyShift action_154
action_126 (49) = happyShift action_134
action_126 (50) = happyShift action_135
action_126 _ = happyFail

action_127 (38) = happyShift action_144
action_127 (71) = happyShift action_145
action_127 (77) = happyShift action_113
action_127 (78) = happyShift action_114
action_127 (79) = happyShift action_146
action_127 (80) = happyShift action_147
action_127 (34) = happyGoto action_152
action_127 (35) = happyGoto action_153
action_127 _ = happyFail

action_128 (38) = happyShift action_144
action_128 (71) = happyShift action_145
action_128 (77) = happyShift action_113
action_128 (78) = happyShift action_114
action_128 (79) = happyShift action_146
action_128 (80) = happyShift action_147
action_128 (34) = happyGoto action_150
action_128 (35) = happyGoto action_151
action_128 _ = happyFail

action_129 (38) = happyShift action_144
action_129 (71) = happyShift action_145
action_129 (77) = happyShift action_113
action_129 (78) = happyShift action_114
action_129 (79) = happyShift action_146
action_129 (80) = happyShift action_147
action_129 (34) = happyGoto action_148
action_129 (35) = happyGoto action_149
action_129 _ = happyFail

action_130 (38) = happyShift action_144
action_130 (71) = happyShift action_145
action_130 (77) = happyShift action_113
action_130 (78) = happyShift action_114
action_130 (79) = happyShift action_146
action_130 (80) = happyShift action_147
action_130 (34) = happyGoto action_142
action_130 (35) = happyGoto action_143
action_130 _ = happyFail

action_131 (80) = happyShift action_141
action_131 (27) = happyGoto action_140
action_131 _ = happyFail

action_132 (36) = happyShift action_139
action_132 _ = happyFail

action_133 (80) = happyShift action_138
action_133 _ = happyFail

action_134 (38) = happyShift action_109
action_134 (52) = happyShift action_110
action_134 (55) = happyShift action_111
action_134 (71) = happyShift action_112
action_134 (77) = happyShift action_113
action_134 (78) = happyShift action_114
action_134 (80) = happyShift action_115
action_134 (26) = happyGoto action_137
action_134 (34) = happyGoto action_108
action_134 _ = happyFail

action_135 (38) = happyShift action_109
action_135 (52) = happyShift action_110
action_135 (55) = happyShift action_111
action_135 (71) = happyShift action_112
action_135 (77) = happyShift action_113
action_135 (78) = happyShift action_114
action_135 (80) = happyShift action_115
action_135 (26) = happyGoto action_136
action_135 (34) = happyGoto action_108
action_135 _ = happyFail

action_136 (49) = happyShift action_134
action_136 (50) = happyShift action_135
action_136 _ = happyReduce_65

action_137 (49) = happyShift action_134
action_137 (50) = happyShift action_135
action_137 _ = happyReduce_64

action_138 (39) = happyShift action_174
action_138 _ = happyFail

action_139 (30) = happyGoto action_173
action_139 _ = happyReduce_76

action_140 (59) = happyShift action_172
action_140 _ = happyReduce_53

action_141 (42) = happyShift action_171
action_141 _ = happyReduce_69

action_142 (39) = happyReduce_104
action_142 (45) = happyReduce_104
action_142 (49) = happyReduce_104
action_142 (50) = happyReduce_104
action_142 _ = happyReduce_104

action_143 (53) = happyShift action_164
action_143 (54) = happyShift action_165
action_143 (55) = happyShift action_166
action_143 (56) = happyShift action_167
action_143 _ = happyReduce_62

action_144 (38) = happyShift action_144
action_144 (71) = happyShift action_145
action_144 (77) = happyShift action_113
action_144 (78) = happyShift action_114
action_144 (79) = happyShift action_146
action_144 (80) = happyShift action_147
action_144 (34) = happyGoto action_169
action_144 (35) = happyGoto action_170
action_144 _ = happyFail

action_145 _ = happyReduce_93

action_146 _ = happyReduce_103

action_147 (40) = happyShift action_121
action_147 (58) = happyShift action_168
action_147 _ = happyReduce_94

action_148 (39) = happyReduce_104
action_148 (45) = happyReduce_104
action_148 (49) = happyReduce_104
action_148 (50) = happyReduce_104
action_148 _ = happyReduce_104

action_149 (53) = happyShift action_164
action_149 (54) = happyShift action_165
action_149 (55) = happyShift action_166
action_149 (56) = happyShift action_167
action_149 _ = happyReduce_56

action_150 (39) = happyReduce_104
action_150 (45) = happyReduce_104
action_150 (49) = happyReduce_104
action_150 (50) = happyReduce_104
action_150 _ = happyReduce_104

action_151 (53) = happyShift action_164
action_151 (54) = happyShift action_165
action_151 (55) = happyShift action_166
action_151 (56) = happyShift action_167
action_151 _ = happyReduce_58

action_152 (39) = happyReduce_104
action_152 (45) = happyReduce_104
action_152 (49) = happyReduce_104
action_152 (50) = happyReduce_104
action_152 _ = happyReduce_104

action_153 (53) = happyShift action_164
action_153 (54) = happyShift action_165
action_153 (55) = happyShift action_166
action_153 (56) = happyShift action_167
action_153 _ = happyReduce_60

action_154 _ = happyReduce_66

action_155 (59) = happyShift action_163
action_155 _ = happyReduce_51

action_156 (38) = happyShift action_162
action_156 _ = happyFail

action_157 _ = happyReduce_95

action_158 (41) = happyShift action_161
action_158 _ = happyFail

action_159 (80) = happyShift action_160
action_159 _ = happyFail

action_160 _ = happyReduce_35

action_161 _ = happyReduce_92

action_162 (71) = happyShift action_145
action_162 (77) = happyShift action_113
action_162 (78) = happyShift action_114
action_162 (79) = happyShift action_196
action_162 (80) = happyShift action_147
action_162 (34) = happyGoto action_195
action_162 _ = happyFail

action_163 (80) = happyShift action_38
action_163 (11) = happyGoto action_194
action_163 _ = happyFail

action_164 (38) = happyShift action_144
action_164 (71) = happyShift action_145
action_164 (77) = happyShift action_113
action_164 (78) = happyShift action_114
action_164 (79) = happyShift action_146
action_164 (80) = happyShift action_147
action_164 (34) = happyGoto action_169
action_164 (35) = happyGoto action_193
action_164 _ = happyFail

action_165 (38) = happyShift action_144
action_165 (71) = happyShift action_145
action_165 (77) = happyShift action_113
action_165 (78) = happyShift action_114
action_165 (79) = happyShift action_146
action_165 (80) = happyShift action_147
action_165 (34) = happyGoto action_169
action_165 (35) = happyGoto action_192
action_165 _ = happyFail

action_166 (38) = happyShift action_144
action_166 (71) = happyShift action_145
action_166 (77) = happyShift action_113
action_166 (78) = happyShift action_114
action_166 (79) = happyShift action_146
action_166 (80) = happyShift action_147
action_166 (34) = happyGoto action_169
action_166 (35) = happyGoto action_191
action_166 _ = happyFail

action_167 (38) = happyShift action_144
action_167 (71) = happyShift action_145
action_167 (77) = happyShift action_113
action_167 (78) = happyShift action_114
action_167 (79) = happyShift action_146
action_167 (80) = happyShift action_147
action_167 (34) = happyGoto action_169
action_167 (35) = happyGoto action_190
action_167 _ = happyFail

action_168 (76) = happyShift action_157
action_168 _ = happyFail

action_169 _ = happyReduce_104

action_170 (39) = happyShift action_189
action_170 (53) = happyShift action_164
action_170 (54) = happyShift action_165
action_170 (55) = happyShift action_166
action_170 (56) = happyShift action_167
action_170 _ = happyFail

action_171 (40) = happyShift action_52
action_171 (67) = happyShift action_53
action_171 (68) = happyShift action_54
action_171 (69) = happyShift action_55
action_171 (80) = happyShift action_56
action_171 (21) = happyGoto action_186
action_171 (28) = happyGoto action_187
action_171 (29) = happyGoto action_188
action_171 _ = happyReduce_71

action_172 (80) = happyShift action_38
action_172 (11) = happyGoto action_185
action_172 _ = happyFail

action_173 (37) = happyShift action_181
action_173 (40) = happyShift action_52
action_173 (67) = happyShift action_53
action_173 (68) = happyShift action_54
action_173 (69) = happyShift action_55
action_173 (70) = happyShift action_182
action_173 (71) = happyShift action_145
action_173 (72) = happyShift action_183
action_173 (77) = happyShift action_113
action_173 (78) = happyShift action_114
action_173 (80) = happyShift action_184
action_173 (21) = happyGoto action_176
action_173 (31) = happyGoto action_177
action_173 (32) = happyGoto action_178
action_173 (33) = happyGoto action_179
action_173 (34) = happyGoto action_180
action_173 _ = happyFail

action_174 (36) = happyShift action_175
action_174 _ = happyFail

action_175 (30) = happyGoto action_208
action_175 _ = happyReduce_76

action_176 (46) = happyShift action_207
action_176 _ = happyFail

action_177 _ = happyReduce_77

action_178 (44) = happyShift action_206
action_178 _ = happyFail

action_179 _ = happyReduce_80

action_180 (46) = happyShift action_204
action_180 (52) = happyShift action_205
action_180 _ = happyFail

action_181 _ = happyReduce_48

action_182 (80) = happyShift action_203
action_182 _ = happyFail

action_183 _ = happyReduce_86

action_184 (36) = happyShift action_60
action_184 (40) = happyShift action_121
action_184 (46) = happyReduce_94
action_184 (52) = happyReduce_94
action_184 (58) = happyShift action_202
action_184 (80) = happyShift action_61
action_184 _ = happyReduce_81

action_185 _ = happyReduce_52

action_186 (46) = happyShift action_201
action_186 _ = happyReduce_75

action_187 (43) = happyShift action_199
action_187 (45) = happyShift action_200
action_187 _ = happyFail

action_188 _ = happyReduce_73

action_189 _ = happyReduce_100

action_190 _ = happyReduce_102

action_191 (56) = happyShift action_167
action_191 _ = happyReduce_101

action_192 (55) = happyShift action_166
action_192 (56) = happyShift action_167
action_192 _ = happyReduce_99

action_193 (55) = happyShift action_166
action_193 (56) = happyShift action_167
action_193 _ = happyReduce_98

action_194 _ = happyReduce_50

action_195 (39) = happyShift action_198
action_195 _ = happyFail

action_196 (39) = happyShift action_197
action_196 _ = happyFail

action_197 _ = happyReduce_68

action_198 _ = happyReduce_67

action_199 _ = happyReduce_70

action_200 (40) = happyShift action_52
action_200 (67) = happyShift action_53
action_200 (68) = happyShift action_54
action_200 (69) = happyShift action_55
action_200 (80) = happyShift action_56
action_200 (21) = happyGoto action_186
action_200 (29) = happyGoto action_218
action_200 _ = happyFail

action_201 (71) = happyShift action_145
action_201 (77) = happyShift action_113
action_201 (78) = happyShift action_114
action_201 (80) = happyShift action_147
action_201 (34) = happyGoto action_217
action_201 _ = happyFail

action_202 (73) = happyShift action_215
action_202 (74) = happyShift action_216
action_202 (76) = happyShift action_157
action_202 _ = happyFail

action_203 _ = happyReduce_87

action_204 (38) = happyShift action_144
action_204 (71) = happyShift action_145
action_204 (77) = happyShift action_113
action_204 (78) = happyShift action_114
action_204 (79) = happyShift action_146
action_204 (80) = happyShift action_147
action_204 (34) = happyGoto action_213
action_204 (35) = happyGoto action_214
action_204 _ = happyFail

action_205 (80) = happyShift action_141
action_205 (27) = happyGoto action_212
action_205 _ = happyFail

action_206 _ = happyReduce_78

action_207 (38) = happyShift action_144
action_207 (71) = happyShift action_145
action_207 (77) = happyShift action_113
action_207 (78) = happyShift action_114
action_207 (79) = happyShift action_146
action_207 (80) = happyShift action_147
action_207 (34) = happyGoto action_210
action_207 (35) = happyGoto action_211
action_207 _ = happyFail

action_208 (37) = happyShift action_209
action_208 (40) = happyShift action_52
action_208 (67) = happyShift action_53
action_208 (68) = happyShift action_54
action_208 (69) = happyShift action_55
action_208 (70) = happyShift action_182
action_208 (71) = happyShift action_145
action_208 (72) = happyShift action_183
action_208 (77) = happyShift action_113
action_208 (78) = happyShift action_114
action_208 (80) = happyShift action_184
action_208 (21) = happyGoto action_176
action_208 (31) = happyGoto action_177
action_208 (32) = happyGoto action_178
action_208 (33) = happyGoto action_179
action_208 (34) = happyGoto action_180
action_208 _ = happyFail

action_209 _ = happyReduce_49

action_210 (44) = happyReduce_104
action_210 _ = happyReduce_104

action_211 (53) = happyShift action_164
action_211 (54) = happyShift action_165
action_211 (55) = happyShift action_166
action_211 (56) = happyShift action_167
action_211 _ = happyReduce_91

action_212 (59) = happyShift action_221
action_212 _ = happyFail

action_213 (44) = happyReduce_104
action_213 _ = happyReduce_104

action_214 (53) = happyShift action_164
action_214 (54) = happyShift action_165
action_214 (55) = happyShift action_166
action_214 (56) = happyShift action_167
action_214 _ = happyReduce_89

action_215 (38) = happyShift action_220
action_215 _ = happyFail

action_216 (38) = happyShift action_219
action_216 _ = happyFail

action_217 _ = happyReduce_74

action_218 _ = happyReduce_72

action_219 (71) = happyShift action_145
action_219 (77) = happyShift action_113
action_219 (78) = happyShift action_114
action_219 (79) = happyShift action_226
action_219 (80) = happyShift action_147
action_219 (34) = happyGoto action_225
action_219 _ = happyFail

action_220 (71) = happyShift action_145
action_220 (77) = happyShift action_113
action_220 (78) = happyShift action_114
action_220 (79) = happyShift action_224
action_220 (80) = happyShift action_147
action_220 (34) = happyGoto action_223
action_220 _ = happyFail

action_221 (80) = happyShift action_38
action_221 (11) = happyGoto action_222
action_221 _ = happyFail

action_222 _ = happyReduce_79

action_223 (39) = happyShift action_230
action_223 _ = happyFail

action_224 (39) = happyShift action_229
action_224 _ = happyFail

action_225 (39) = happyShift action_228
action_225 _ = happyFail

action_226 (39) = happyShift action_227
action_226 _ = happyFail

action_227 _ = happyReduce_85

action_228 _ = happyReduce_83

action_229 _ = happyReduce_84

action_230 _ = happyReduce_82

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (Model happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_3 : happy_var_1
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : happy_var_1
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 5 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Network Ord happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Network Unord happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 9 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Network Ord "SingleNet" happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 9 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Network Unord "SingleNet" happy_var_3
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 ((happy_var_3) : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn11
		 (VC happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  12 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 7 14 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_6) `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Machine Nonsymmetric happy_var_2 1 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 9 14 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_8) `HappyStk`
	(HappyAbsSyn17  happy_var_7) `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_4) `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Machine Nonsymmetric happy_var_3 happy_var_4 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 8 15 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_7) `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Machine Symmetric happy_var_2 happy_var_3 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 16 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (State happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_0  17 happyReduction_25
happyReduction_25  =  HappyAbsSyn17
		 ([]
	)

happyReduce_26 = happySpecReduce_2  17 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  18 happyReduction_27
happyReduction_27  =  HappyAbsSyn18
		 ([]
	)

happyReduce_28 = happySpecReduce_3  18 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  19 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn19
		 (Field happy_var_1 happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  20 happyReduction_31
happyReduction_31  =  HappyAbsSyn20
		 (Nothing
	)

happyReduce_32 = happySpecReduce_3  20 happyReduction_32
happyReduction_32 _
	(HappyTerminal (TokenIdentifier happy_var_2))
	_
	 =  HappyAbsSyn20
		 (Just happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  20 happyReduction_33
happyReduction_33 _
	(HappyTerminal (TokenNum happy_var_2))
	_
	 =  HappyAbsSyn20
		 (Just (show happy_var_2)
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  21 happyReduction_34
happyReduction_34 (HappyTerminal (TokenIdentifier happy_var_2))
	_
	 =  HappyAbsSyn21
		 (Boolean happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 8 21 happyReduction_35
happyReduction_35 ((HappyTerminal (TokenIdentifier happy_var_8)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Integer happy_var_8 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 4 21 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Enum    happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  21 happyReduction_37
happyReduction_37 (HappyTerminal (TokenIdentifier happy_var_2))
	(HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn21
		 (Vertex    happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 4 21 happyReduction_38
happyReduction_38 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Array  (Left happy_var_2)  happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 21 happyReduction_39
happyReduction_39 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Array  (Right happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  21 happyReduction_40
happyReduction_40 (HappyAbsSyn21  happy_var_3)
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (Set  (Left happy_var_2) happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 5 21 happyReduction_41
happyReduction_41 ((HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Set (Right happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_0  22 happyReduction_42
happyReduction_42  =  HappyAbsSyn22
		 ([]
	)

happyReduce_43 = happySpecReduce_3  22 happyReduction_43
happyReduction_43 (HappyTerminal (TokenIdentifier happy_var_3))
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  22 happyReduction_44
happyReduction_44 (HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn22
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  23 happyReduction_45
happyReduction_45 _
	(HappyTerminal (TokenNum happy_var_2))
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  24 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  24 happyReduction_47
happyReduction_47 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 8 25 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((State happy_var_2, happy_var_4, Nothing, happy_var_7)
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 10 25 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((State happy_var_2, happy_var_4, Just (State happy_var_6), happy_var_9)
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 5 26 happyReduction_50
happyReduction_50 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (ReceiveFrom happy_var_3 (Nothing) (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_3  26 happyReduction_51
happyReduction_51 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn26
		 (ReceiveFrom happy_var_3 (Nothing) (Nothing)
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 5 26 happyReduction_52
happyReduction_52 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (ReceiveFrom happy_var_3 (Just happy_var_1) (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_3  26 happyReduction_53
happyReduction_53 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (ReceiveFrom happy_var_3 (Just happy_var_1) (Nothing)
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  26 happyReduction_54
happyReduction_54 (HappyTerminal (TokenIdentifier happy_var_2))
	_
	 =  HappyAbsSyn26
		 (Issue happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  26 happyReduction_55
happyReduction_55 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (Equals happy_var_1 (Left happy_var_3)
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  26 happyReduction_56
happyReduction_56 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (Equals happy_var_1 (Right happy_var_3)
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  26 happyReduction_57
happyReduction_57 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (Greater happy_var_1 (Left happy_var_3)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  26 happyReduction_58
happyReduction_58 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (Greater happy_var_1 (Right happy_var_3)
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  26 happyReduction_59
happyReduction_59 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (Less happy_var_1 (Left happy_var_3)
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  26 happyReduction_60
happyReduction_60 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (Less happy_var_1 (Right happy_var_3)
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  26 happyReduction_61
happyReduction_61 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (NotEq happy_var_1 (Left happy_var_3)
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  26 happyReduction_62
happyReduction_62 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn26
		 (NotEq happy_var_1 (Right happy_var_3)
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  26 happyReduction_63
happyReduction_63 (HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Not happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  26 happyReduction_64
happyReduction_64 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 :&: happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  26 happyReduction_65
happyReduction_65 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 :|: happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  26 happyReduction_66
happyReduction_66 _
	(HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happyReduce 6 26 happyReduction_67
happyReduction_67 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (IsIn happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_68 = happyReduce 6 26 happyReduction_68
happyReduction_68 (_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (IsIn happy_var_1 (VarOrVal (show happy_var_5))
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_1  27 happyReduction_69
happyReduction_69 (HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn27
		 (Msg happy_var_1 []
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happyReduce 4 27 happyReduction_70
happyReduction_70 (_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Msg happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_0  28 happyReduction_71
happyReduction_71  =  HappyAbsSyn28
		 ([]
	)

happyReduce_72 = happySpecReduce_3  28 happyReduction_72
happyReduction_72 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  28 happyReduction_73
happyReduction_73 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 ([happy_var_1]
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  29 happyReduction_74
happyReduction_74 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn29
		 (GuardAssign happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  29 happyReduction_75
happyReduction_75 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn29
		 (MsgArg happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_0  30 happyReduction_76
happyReduction_76  =  HappyAbsSyn30
		 ([]
	)

happyReduce_77 = happySpecReduce_2  30 happyReduction_77
happyReduction_77 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_2  31 happyReduction_78
happyReduction_78 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_78 _ _  = notHappyAtAll 

happyReduce_79 = happyReduce 5 32 happyReduction_79
happyReduction_79 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (Send happy_var_3 happy_var_1 (happy_var_5)
	) `HappyStk` happyRest

happyReduce_80 = happySpecReduce_1  32 happyReduction_80
happyReduction_80 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  32 happyReduction_81
happyReduction_81 (HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn32
		 (EmptyResp happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happyReduce 6 32 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (Add happy_var_1 (Left happy_var_5)
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 6 32 happyReduction_83
happyReduction_83 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (Del happy_var_1 (Left happy_var_5)
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 6 32 happyReduction_84
happyReduction_84 (_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (Add happy_var_1 (Right happy_var_5)
	) `HappyStk` happyRest

happyReduce_85 = happyReduce 6 32 happyReduction_85
happyReduction_85 (_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn32
		 (Del happy_var_1 (Right happy_var_5)
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_1  32 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn32
		 (Stall
	)

happyReduce_87 = happySpecReduce_2  32 happyReduction_87
happyReduction_87 (HappyTerminal (TokenIdentifier happy_var_2))
	_
	 =  HappyAbsSyn32
		 (Clear happy_var_2
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  33 happyReduction_88
happyReduction_88 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  33 happyReduction_89
happyReduction_89 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn33
		 (AssignNum happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  33 happyReduction_90
happyReduction_90 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn33
		 (AssignLocal happy_var_1 happy_var_3
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  33 happyReduction_91
happyReduction_91 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn33
		 (AssignLocalNum happy_var_1 happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happyReduce 4 34 happyReduction_92
happyReduction_92 (_ `HappyStk`
	(HappyTerminal (TokenNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (ArrayElem happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_93 = happySpecReduce_1  34 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn34
		 (VarOrVal "src"
	)

happyReduce_94 = happySpecReduce_1  34 happyReduction_94
happyReduction_94 (HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn34
		 (VarOrVal happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  34 happyReduction_95
happyReduction_95 _
	_
	(HappyTerminal (TokenIdentifier happy_var_1))
	 =  HappyAbsSyn34
		 (SetSize happy_var_1
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  34 happyReduction_96
happyReduction_96 _
	 =  HappyAbsSyn34
		 (VarOrVal "all"
	)

happyReduce_97 = happySpecReduce_1  34 happyReduction_97
happyReduction_97 _
	 =  HappyAbsSyn34
		 (VarOrVal "self"
	)

happyReduce_98 = happySpecReduce_3  35 happyReduction_98
happyReduction_98 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (Sum happy_var_1 happy_var_3
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  35 happyReduction_99
happyReduction_99 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  35 happyReduction_100
happyReduction_100 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn35
		 (Group happy_var_2
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  35 happyReduction_101
happyReduction_101 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (Times happy_var_1 happy_var_3
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  35 happyReduction_102
happyReduction_102 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn35
		 (Div happy_var_1 happy_var_3
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1  35 happyReduction_103
happyReduction_103 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn35
		 (Const happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  35 happyReduction_104
happyReduction_104 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn35
		 (IntVar happy_var_1
	)
happyReduction_104 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 81 81 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenOpCrlBracket -> cont 36;
	TokenClCrlBracket -> cont 37;
	TokenOpBracket -> cont 38;
	TokenClBracket -> cont 39;
	TokenOpSqBracket -> cont 40;
	TokenClSqBracket -> cont 41;
	TokenOpAngleBracket -> cont 42;
	TokenClAngleBracket -> cont 43;
	TokenSemiColon -> cont 44;
	TokenComa -> cont 45;
	TokenEq -> cont 46;
	TokenDoubleEq -> cont 47;
	TokenNotEq -> cont 48;
	TokenAnd -> cont 49;
	TokenOr -> cont 50;
	TokenQsMark -> cont 51;
	TokenExclMark -> cont 52;
	TokenPlus -> cont 53;
	TokenMinus -> cont 54;
	TokenStar -> cont 55;
	TokenSlash -> cont 56;
	TokenColon -> cont 57;
	TokenFullStop -> cont 58;
	TokenAt -> cont 59;
	TokenGlobal -> cont 60;
	TokenNetworks -> cont 61;
	TokenOrdered -> cont 62;
	TokenUnordered -> cont 63;
	TokenMachine -> cont 64;
	TokenNonsymmetric -> cont 65;
	TokenStartstate -> cont 66;
	TokenBoolean -> cont 67;
	TokenInt -> cont 68;
	TokenSet -> cont 69;
	TokenClear -> cont 70;
	TokenSrc -> cont 71;
	TokenStall -> cont 72;
	TokenAdd -> cont 73;
	TokenDel -> cont 74;
	TokenContains -> cont 75;
	TokenCount -> cont 76;
	TokenAll -> cont 77;
	TokenSelf -> cont 78;
	TokenNum happy_dollar_dollar -> cont 79;
	TokenIdentifier happy_dollar_dollar -> cont 80;
	_ -> happyError' (tk:tks)
	}

happyError_ 81 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

ast tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error "

parseAst :: String -> Ast

parseAst = ast . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-8.2.1/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc591_0/ghc_2.h" #-}








































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
