pragma Restrictions (No_Elaboration_Code);

private package Catalog.Quasars is

  type Object_List  is array (1 .. 338) of Object;

  Id : constant Object_List := (
      1 => Undefined,
      2 => Undefined,
      3 => Undefined,
      4 => Undefined,
      5 => Undefined,
      6 => Undefined,
      7 => Undefined,
      8 => Undefined,
      9 => Undefined,
     10 => Undefined,
     11 => Undefined,
     12 => Undefined,
     13 => Undefined,
     14 => Undefined,
     15 => Undefined,
     16 => Undefined,
     17 => Undefined,
     18 => Undefined,
     19 => Undefined,
     20 => Undefined,
     21 => Undefined,
     22 => Undefined,
     23 => Undefined,
     24 => Undefined,
     25 => Undefined,
     26 => Undefined,
     27 => Undefined,
     28 => Undefined,
     29 => Undefined,
     30 => Undefined,
     31 => Undefined,
     32 => Undefined,
     33 => Undefined,
     34 => Undefined,
     35 => Undefined,
     36 => Undefined,
     37 => Undefined,
     38 => Undefined,
     39 => Undefined,
     40 => Undefined,
     41 => Undefined,
     42 => Undefined,
     43 => Undefined,
     44 => Undefined,
     45 => Undefined,
     46 => Undefined,
     47 => Undefined,
     48 => Undefined,
     49 => Undefined,
     50 => Undefined,
     51 => Undefined,
     52 => Undefined,
     53 => Undefined,
     54 => Undefined,
     55 => Undefined,
     56 => Undefined,
     57 => Undefined,
     58 => Undefined,
     59 => Undefined,
     60 => Undefined,
     61 => Undefined,
     62 => Undefined,
     63 => Undefined,
     64 => Undefined,
     65 => Undefined,
     66 => Undefined,
     67 => Undefined,
     68 => Undefined,
     69 => Undefined,
     70 => Undefined,
     71 => Undefined,
     72 => Undefined,
     73 => Undefined,
     74 => Undefined,
     75 => Undefined,
     76 => Undefined,
     77 => Undefined,
     78 => Undefined,
     79 => Undefined,
     80 => Undefined,
     81 => Undefined,
     82 => Undefined,
     83 => Undefined,
     84 => Undefined,
     85 => Undefined,
     86 => Undefined,
     87 => Undefined,
     88 => Undefined,
     89 => Undefined,
     90 => Undefined,
     91 => Undefined,
     92 => Undefined,
     93 => Undefined,
     94 => Undefined,
     95 => Undefined,
     96 => Undefined,
     97 => Undefined,
     98 => Undefined,
     99 => Undefined,
    100 => Undefined,
    101 => Undefined,
    102 => Undefined,
    103 => Undefined,
    104 => Undefined,
    105 => Undefined,
    106 => Undefined,
    107 => Undefined,
    108 => Undefined,
    109 => Undefined,
    110 => Undefined,
    111 => Undefined,
    112 => Undefined,
    113 => Undefined,
    114 => Undefined,
    115 => Undefined,
    116 => Undefined,
    117 => Undefined,
    118 => Undefined,
    119 => Undefined,
    120 => Undefined,
    121 => Undefined,
    122 => Undefined,
    123 => Undefined,
    124 => Undefined,
    125 => Undefined,
    126 => Undefined,
    127 => Undefined,
    128 => Undefined,
    129 => Undefined,
    130 => Undefined,
    131 => Undefined,
    132 => Undefined,
    133 => Undefined,
    134 => Undefined,
    135 => Undefined,
    136 => Undefined,
    137 => Undefined,
    138 => Undefined,
    139 => Undefined,
    140 => Undefined,
    141 => Undefined,
    142 => Undefined,
    143 => Undefined,
    144 => Undefined,
    145 => Undefined,
    146 => Undefined,
    147 => Undefined,
    148 => Undefined,
    149 => Undefined,
    150 => Undefined,
    151 => Undefined,
    152 => Undefined,
    153 => Undefined,
    154 => Undefined,
    155 => Undefined,
    156 => Undefined,
    157 => Undefined,
    158 => Undefined,
    159 => Undefined,
    160 => Undefined,
    161 => Undefined,
    162 => Undefined,
    163 => Undefined,
    164 => Undefined,
    165 => Undefined,
    166 => Undefined,
    167 => Undefined,
    168 => Undefined,
    169 => Undefined,
    170 => Undefined,
    171 => Undefined,
    172 => Undefined,
    173 => Undefined,
    174 => Undefined,
    175 => Undefined,
    176 => Undefined,
    177 => Undefined,
    178 => Undefined,
    179 => Undefined,
    180 => Undefined,
    181 => Undefined,
    182 => Undefined,
    183 => Undefined,
    184 => Undefined,
    185 => Undefined,
    186 => Undefined,
    187 => Undefined,
    188 => Undefined,
    189 => Undefined,
    190 => Undefined,
    191 => Undefined,
    192 => Undefined,
    193 => Undefined,
    194 => Undefined,
    195 => Undefined,
    196 => Undefined,
    197 => Undefined,
    198 => Undefined,
    199 => Undefined,
    200 => Undefined,
    201 => Undefined,
    202 => Undefined,
    203 => Undefined,
    204 => Undefined,
    205 => Undefined,
    206 => Undefined,
    207 => Undefined,
    208 => Undefined,
    209 => Undefined,
    210 => Undefined,
    211 => Undefined,
    212 => Undefined,
    213 => Undefined,
    214 => Undefined,
    215 => Undefined,
    216 => Undefined,
    217 => Undefined,
    218 => Undefined,
    219 => Undefined,
    220 => Undefined,
    221 => Undefined,
    222 => Undefined,
    223 => Undefined,
    224 => Undefined,
    225 => Undefined,
    226 => Undefined,
    227 => Undefined,
    228 => Undefined,
    229 => Undefined,
    230 => Undefined,
    231 => Undefined,
    232 => Undefined,
    233 => Undefined,
    234 => Undefined,
    235 => Undefined,
    236 => Undefined,
    237 => Undefined,
    238 => Undefined,
    239 => Undefined,
    240 => Undefined,
    241 => Undefined,
    242 => Undefined,
    243 => Undefined,
    244 => Undefined,
    245 => Undefined,
    246 => Undefined,
    247 => Undefined,
    248 => Undefined,
    249 => Undefined,
    250 => Undefined,
    251 => Undefined,
    252 => Undefined,
    253 => Undefined,
    254 => Undefined,
    255 => Undefined,
    256 => Undefined,
    257 => Undefined,
    258 => Undefined,
    259 => Undefined,
    260 => Undefined,
    261 => Undefined,
    262 => Undefined,
    263 => Undefined,
    264 => Undefined,
    265 => Undefined,
    266 => Undefined,
    267 => Undefined,
    268 => Undefined,
    269 => Undefined,
    270 => Undefined,
    271 => Undefined,
    272 => Undefined,
    273 =>     16356,
    274 => Undefined,
    275 => Undefined,
    276 => Undefined,
    277 => Undefined,
    278 =>     16355,
    279 => Undefined,
    280 => Undefined,
    281 => Undefined,
    282 => Undefined,
    283 => Undefined,
    284 => Undefined,
    285 => Undefined,
    286 => Undefined,
    287 => Undefined,
    288 => Undefined,
    289 => Undefined,
    290 => Undefined,
    291 => Undefined,
    292 => Undefined,
    293 => Undefined,
    294 => Undefined,
    295 => Undefined,
    296 => Undefined,
    297 => Undefined,
    298 => Undefined,
    299 => Undefined,
    300 => Undefined,
    301 => Undefined,
    302 => Undefined,
    303 => Undefined,
    304 => Undefined,
    305 => Undefined,
    306 => Undefined,
    307 => Undefined,
    308 => Undefined,
    309 => Undefined,
    310 => Undefined,
    311 => Undefined,
    312 => Undefined,
    313 => Undefined,
    314 => Undefined,
    315 => Undefined,
    316 => Undefined,
    317 => Undefined,
    318 => Undefined,
    319 => Undefined,
    320 => Undefined,
    321 => Undefined,
    322 => Undefined,
    323 => Undefined,
    324 => Undefined,
    325 => Undefined,
    326 => Undefined,
    327 => Undefined,
    328 => Undefined,
    329 => Undefined,
    330 => Undefined,
    331 => Undefined,
    332 => Undefined,
    333 => Undefined,
    334 => Undefined,
    335 => Undefined,
    336 => Undefined,
    337 => Undefined,
    338 =>     14884);

  Count : constant Natural := 3;

end Catalog.Quasars;
