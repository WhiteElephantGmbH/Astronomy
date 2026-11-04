-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_Astronomy;

package body Name.Ocl is

  function Target_Name_Of (Number : Positive) return String is
  begin
    case Number is
    when 2 =>
      return "Cl Ruprecht 131";
    when 3 =>
      return "NGC 6558";
    when 4 =>
      return "Cl Ruprecht 134";
    when 5 =>
      return "Cr 351";
    when 6 =>
      return "Cl Ruprecht 133";
    when 7 =>
      return "Cl Ruprecht 168";
    when 8 =>
      return "Cl Czernik 37";
    when 9 =>
      return "Cl Trumpler 31";
    when 10 =>
      return "NGC 6520";
    when 11 =>
      return "NGC 6540";
    when 12 =>
      return "Cr 468";
    when 13 =>
      return "Cl Ruprecht 137";
    when 14 =>
      return "Cl Ruprecht 169";
    when 15 =>
      return "Cl Ruprecht 136";
    when 16 =>
      return "Cl Ruprecht 138";
    when 17 =>
      return "NGC 6544";
    when 18 =>
      return "NGC 6523";
    when 19 =>
      return "NGC 6530";
    when 20 =>
      return "Cl Ruprecht 139";
    when 21 =>
      return "NGC 6469";
    when 22 =>
      return "Cr 367";
    when 23 =>
      return "NGC 6514";
    when 24 =>
      return "NGC 6546";
    when 25 =>
      return "Cl Dolidze 27";
    when 26 =>
      return "NGC 6531";
    when 27 =>
      return "NGC 6583";
    when 28 =>
      return "NGC 6568";
    when 29 =>
      return "NGC 6642";
    when 30 =>
      return "NGC 6494";
    when 31 =>
      return "NGC 6595";
    when 32 =>
      return "NGC 6507";
    when 33 =>
      return "Cl Mrk 38";
    when 34 =>
      return "Cl Trumpler 33";
    when 35 =>
      return "Cr 469";
    when 36 =>
      return "NGC 6603";
    when 37 =>
      return "NGC 6717";
    when 38 =>
      return "IC 4725";
    when 39 =>
      return "ESO 592-4";
    when 40 =>
      return "NGC 6613";
    when 41 =>
      return "NGC 6596";
    when 42 =>
      return "Cl Collinder 394";
    when 43 =>
      return "Blanco";
    when 44 =>
      return "NGC 6618";
    when 45 =>
      return "NGC 6647";
    when 46 =>
      return "NGC 6716";
    when 47 =>
      return "NGC 6605";
    when 48 =>
      return "NGC 6645";
    when 49 =>
      return "Cl Ruprecht 135";
    when 50 =>
      return "Cl Ruprecht 171";
    when 51 =>
      return "IC 1257";
    when 52 =>
      return "Cl Ruprecht 145";
    when 53 =>
      return "Cl Dolidze 28";
    when 54 =>
      return "NGC 6611";
    when 55 =>
      return "Cl Trumpler 32";
    when 56 =>
      return "NGC 6604";
    when 57 =>
      return "NGC 6639";
    when 58 =>
      return "NGC 6625";
    when 59 =>
      return "NGC 6631";
    when 60 =>
      return "Cl Ruprecht 141";
    when 61 =>
      return "Cl Ruprecht 142";
    when 62 =>
      return "Cl Ruprecht 143";
    when 63 =>
      return "Cl Ruprecht 144";
    when 64 =>
      return "Cl Ruprecht 170";
    when 65 =>
      return "Cl Ruprecht 147";
    when 66 =>
      return "NGC 6649";
    when 67 =>
      return "NGC 6694";
    when 68 =>
      return "NGC 6664";
    when 69 =>
      return "Cl Trumpler 34";
    when 70 =>
      return "Cl Dolidze 29";
    when 71 =>
      return "Cl Dolidze 31";
    when 72 =>
      return "NGC 6712";
    when 73 =>
      return "C 1832-068";
    when 74 =>
      return "NGC 6683";
    when 75 =>
      return "Cr 350";
    when 76 =>
      return "NGC 6705";
    when 77 =>
      return "Cl Trumpler 35";
    when 78 =>
      return "Cl Dolidze 34";
    when 79 =>
      return "Cl Dolidze 33";
    when 80 =>
      return "Cl Dolidze 32";
    when 81 =>
      return "NGC 6426";
    when 82 =>
      return "NGC 6704";
    when 83 =>
      return "Cl Apriamaswili 1";
    when 84 =>
      return "Cl Melotte 186";
    when 85 =>
      return "IC 4665";
    when 86 =>
      return "Cl Berkeley 79";
    when 87 =>
      return "Cl Berkeley 80";
    when 88 =>
      return "Cl Berkeley 81";
    when 89 =>
      return "NGC 6994";
    when 90 =>
      return "NGC 6633";
    when 91 =>
      return "Cl Berkeley 42";
    when 92 =>
      return "NGC 6760";
    when 93 =>
      return "Cl Dolidze-D. 7";
    when 94 =>
      return "IC 4656";
    when 95 =>
      return "Cl Czernik 38";
    when 96 =>
      return "NGC 6755";
    when 97 =>
      return "Cl Czernik 39";
    when 98 =>
      return "Cr 401";
    when 99 =>
      return "NGC 6756";
    when 100 =>
      return "NGC 6709";
    when 101 =>
      return "NGC 6738";
    when 102 =>
      return "Cl Berkeley 43";
    when 103 =>
      return "Cl Berkeley 82";
    when 104 =>
      return "Cl Dolidze-D. 8";
    when 105 =>
      return "Cl Dolidze 35";
    when 106 =>
      return "Cl King 25";
    when 107 =>
      return "Cl Berkeley 45";
    when 108 =>
      return "Cl King 26";
    when 109 =>
      return "Cl Berkeley 47";
    when 110 =>
      return "Cl Berkeley 44";
    when 111 =>
      return "NGC 7492";
    when 112 =>
      return "Cl Roslund 1";
    when 113 =>
      return "Cr 399";
    when 114 =>
      return "NGC 6802";
    when 115 =>
      return "NGC 6793";
    when 116 =>
      return "Cl Collinder 408";
    when 117 =>
      return "NGC 6838";
    when 118 =>
      return "Cl Czernik 40";
    when 119 =>
      return "C 1806+315";
    when 120 =>
      return "Cl Berkeley 48";
    when 121 =>
      return "Cl Roslund 3";
    when 122 =>
      return "NGC 6820";
    when 123 =>
      return "NGC 6800";
    when 124 =>
      return "NGC 6823";
    when 125 =>
      return "NGC 6830";
    when 126 =>
      return "Cl Roslund 2";
    when 127 =>
      return "Cl Stock 1";
    when 128 =>
      return "Cl Dolidze-D. 5";
    when 129 =>
      return "Cl Dolidze-D. 6";
    when 130 =>
      return "Cl Czernik 41";
    when 131 =>
      return "NGC 6815";
    when 132 =>
      return "NGC 6885";
    when 133 =>
      return "NGC 6882";
    when 134 =>
      return "NGC 6834";
    when 135 =>
      return "Cl Berkeley 83";
    when 136 =>
      return "Cl Iskudarian";
    when 137 =>
      return "Cl Stephenson";
    when 138 =>
      return "Cl Roslund 4";
    when 139 =>
      return "NGC 6846";
    when 140 =>
      return "Cl Berkeley 52";
    when 141 =>
      return "NGC 6940";
    when 142 =>
      return "Cl Berkeley 46";
    when 143 =>
      return "Cl Berkeley 84";
    when 144 =>
      return "Cl Berkeley 49";
    when 145 =>
      return "Cl Roslund 5";
    when 146 =>
      return "Cl Berkeley 51";
    when 147 =>
      return "Cl Berkeley 50";
    when 148 =>
      return "NGC 6871";
    when 149 =>
      return "Biur 1";
    when 150 =>
      return "Biur 2";
    when 151 =>
      return "Cl Ruprecht 172";
    when 152 =>
      return "NGC 6883";
    when 153 =>
      return "Cl Dolidze 1";
    when 154 =>
      return "Cl Dolidze 37";
    when 155 =>
      return "NGC 6819";
    when 156 =>
      return "Cl Dolidze 3";
    when 157 =>
      return "Cl Dolidze 4";
    when 158 =>
      return "IC 4996";
    when 159 =>
      return "Cl Dolidze 39";
    when 160 =>
      return "Cl Berkeley 85";
    when 161 =>
      return "Cl Berkeley 87";
    when 162 =>
      return "Cl Dolidze 40";
    when 163 =>
      return "Cl Dolidze 41";
    when 164 =>
      return "Cl Dolidze 42";
    when 165 =>
      return "Cl Ruprecht 173";
    when 166 =>
      return "Cl Dolidze-D. 10";
    when 167 =>
      return "Cl Berkeley 86";
    when 168 =>
      return "NGC 6913";
    when 169 =>
      return "Cl Ruprecht 175";
    when 170 =>
      return "Cl Dolidze 38";
    when 171 =>
      return "Cl Dolidze 5";
    when 172 =>
      return "Cl Dolidze 36";
    when 173 =>
      return "Cl Trumpler 36";
    when 174 =>
      return "Cl Dolidze 2";
    when 175 =>
      return "Cl Dolidze 43";
    when 176 =>
      return "Cl Ruprecht 174";
    when 177 =>
      return "Cr 419";
    when 178 =>
      return "Cl Roslund 6";
    when 179 =>
      return "Cl Dolidze-D. 11";
    when 180 =>
      return "Cl Dolidze 10";
    when 181 =>
      return "NGC 6910";
    when 182 =>
      return "Cl Dolidze 6";
    when 183 =>
      return "NGC 6866";
    when 184 =>
      return "Cr 421";
    when 185 =>
      return "NGC 6811";
    when 186 =>
      return "Cl Dolidze 11";
    when 187 =>
      return "Cl Roslund 7";
    when 188 =>
      return "Cl Dolidze 9";
    when 189 =>
      return "Cl Dolidze 8";
    when 190 =>
      return "Cl Dolidze 44";
    when 191 =>
      return "Cl Dolidze 45";
    when 192 =>
      return "NGC 7063";
    when 193 =>
      return "Cl Berkeley 89";
    when 194 =>
      return "Cl Berkeley 54";
    when 195 =>
      return "Cl Berkeley 88";
    when 196 =>
      return "Cl Berkeley 90";
    when 197 =>
      return "NGC 6996";
    when 198 =>
      return "NGC 7044";
    when 199 =>
      return "Cl Berkeley 56";
    when 200 =>
      return "Cl Barhatova 1";
    when 201 =>
      return "Cr 428";
    when 202 =>
      return "NGC 6991";
    when 203 =>
      return "NGC 7039";
    when 204 =>
      return "IC 1369";
    when 205 =>
      return "NGC 7062";
    when 206 =>
      return "Cl Berkeley 91";
    when 207 =>
      return "Cl Berkeley 53";
    when 208 =>
      return "NGC 7067";
    when 209 =>
      return "NGC 7082";
    when 210 =>
      return "NGC 7031";
    when 211 =>
      return "NGC 7092";
    when 212 =>
      return "Cl Berkeley 55";
    when 213 =>
      return "IC 5146";
    when 214 =>
      return "NGC 7086";
    when 215 =>
      return "NGC 7209";
    when 216 =>
      return "Cl Barhatova 2";
    when 217 =>
      return "NGC 6939";
    when 218 =>
      return "NGC 7128";
    when 219 =>
      return "NGC 7127";
    when 220 =>
      return "Cl Berkeley 92";
    when 221 =>
      return "NGC 7243";
    when 222 =>
      return "Cl Trumpler 37";
    when 223 =>
      return "IC 1434";
    when 224 =>
      return "IC 1442";
    when 225 =>
      return "NGC 7245";
    when 226 =>
      return "NGC 7226";
    when 227 =>
      return "Cl King 9";
    when 228 =>
      return "NGC 7296";
    when 229 =>
      return "NGC 7235";
    when 230 =>
      return "NGC 7772";
    when 231 =>
      return "Cl Berkeley 94";
    when 232 =>
      return "Cl Berkeley 96";
    when 233 =>
      return "Cr 427";
    when 234 =>
      return "Cl Berkeley 98";
    when 235 =>
      return "NGC 7023";
    when 236 =>
      return "NGC 7160";
    when 237 =>
      return "NGC 7261";
    when 238 =>
      return "NGC 7281";
    when 239 =>
      return "Cl Berkeley 93";
    when 240 =>
      return "NGC 7129";
    when 241 =>
      return "NGC 7142";
    when 242 =>
      return "Cl Berkeley 95";
    when 243 =>
      return "Cl Berkeley 97";
    when 244 =>
      return "NGC 7380";
    when 245 =>
      return "Cl Czernik 42";
    when 246 =>
      return "Cl Berkeley 57";
    when 247 =>
      return "Cl King 18";
    when 248 =>
      return "Cl King 10";
    when 249 =>
      return "NGC 7429";
    when 250 =>
      return "NGC 7419";
    when 251 =>
      return "NGC 7686";
    when 252 =>
      return "Cr 285";
    when 253 =>
      return "Cl Dolidze 46";
    when 254 =>
      return "Cl King 19";
    when 255 =>
      return "Cr 471";
    when 256 =>
      return "NGC 7510";
    when 257 =>
      return "Cl Mrk 50";
    when 258 =>
      return "Cl Stock 12";
    when 259 =>
      return "Cl Stock 11";
    when 260 =>
      return "NGC 7654";
    when 261 =>
      return "Cl Czernik 43";
    when 262 =>
      return "Cl King 20";
    when 263 =>
      return "Cl Berkeley 102";
    when 264 =>
      return "Cl Berkeley 100";
    when 265 =>
      return "Cl Czernik 44";
    when 266 =>
      return "Cl Berkeley 103";
    when 267 =>
      return "Cl Berkeley 101";
    when 268 =>
      return "Cl Stock 17";
    when 269 =>
      return "NGC 7789";
    when 270 =>
      return "Cl King 21";
    when 271 =>
      return "Cl Berkeley 99";
    when 272 =>
      return "Cl King 12";
    when 273 =>
      return "C 2351+614";
    when 274 =>
      return "Cl Stock 19";
    when 275 =>
      return "NGC 7788";
    when 276 =>
      return "NGC 7790";
    when 277 =>
      return "Cl Berkeley 58";
    when 278 =>
      return "Cl Czernik 45";
    when 279 =>
      return "Cl King 11";
    when 280 =>
      return "NGC 7762";
    when 281 =>
      return "Cl Stock 18";
    when 282 =>
      return "Cl Berkeley 104";
    when 283 =>
      return "Cl Czernik 1";
    when 284 =>
      return "Cl Berkeley 1";
    when 285 =>
      return "Cl King 13";
    when 286 =>
      return "Cl Berkeley 59";
    when 287 =>
      return "Cl Dolidze-D. 1";
    when 288 =>
      return "Cl Berkeley 60";
    when 289 =>
      return "Cl Berkeley 2";
    when 290 =>
      return "Cl King 1";
    when 291 =>
      return "NGC 103";
    when 292 =>
      return "Cl Stock 20";
    when 293 =>
      return "Cl Stock 21";
    when 294 =>
      return "NGC 129";
    when 295 =>
      return "NGC 136";
    when 296 =>
      return "NGC 133";
    when 297 =>
      return "Alter";
    when 298 =>
      return "Cl King 15";
    when 299 =>
      return "NGC 146.SH";
    when 300 =>
      return "NGC 110";
    when 301 =>
      return "NGC 189";
    when 302 =>
      return "Cl Berkeley 3";
    when 303 =>
      return "Cl Dolidze 12";
    when 304 =>
      return "Cl Czernik 2";
    when 305 =>
      return "NGC 225";
    when 306 =>
      return "Cl King 16";
    when 307 =>
      return "Cl Berkeley 4";
    when 308 =>
      return "Cl Berkeley 61";
    when 309 =>
      return "NGC 188";
    when 310 =>
      return "Cl Dolidze 13";
    when 311 =>
      return "Cl King 2";
    when 312 =>
      return "NGC 272";
    when 313 =>
      return "NGC 281";
    when 314 =>
      return "Cl Berkeley 62";
    when 315 =>
      return "Cl Czernik 3";
    when 316 =>
      return "NGC 366";
    when 317 =>
      return "NGC 381";
    when 318 =>
      return "Cl Stock 3";
    when 319 =>
      return "Cl Stock 22";
    when 320 =>
      return "NGC 436";
    when 321 =>
      return "NGC 457";
    when 322 =>
      return "NGC 559";
    when 323 =>
      return "Cl Berkeley 8";
    when 324 =>
      return "Cr 463";
    when 325 =>
      return "Cl King 3";
    when 326 =>
      return "NGC 581";
    when 327 =>
      return "Cl Czernik 4";
    when 328 =>
      return "Cl Trumpler 1";
    when 329 =>
      return "NGC 637";
    when 330 =>
      return "NGC 654";
    when 331 =>
      return "Cl Berkeley 5";
    when 332 =>
      return "NGC 659";
    when 333 =>
      return "NGC 663";
    when 334 =>
      return "IC 166";
    when 335 =>
      return "Cl Berkeley 6";
    when 336 =>
      return "Cl Berkeley 7";
    when 337 =>
      return "NGC 657";
    when 338 =>
      return "Cl Czernik 5";
    when 339 =>
      return "Cl Stock 5";
    when 340 =>
      return "Cl Czernik 6";
    when 341 =>
      return "Cl Czernik 7";
    when 342 =>
      return "Cl Stock 4";
    when 343 =>
      return "NGC 743";
    when 344 =>
      return "Cl Berkeley 64";
    when 345 =>
      return "NGC 744";
    when 346 =>
      return "Cl Berkeley 63";
    when 347 =>
      return "Cl Stock 6";
    when 348 =>
      return "Cl Stock 2";
    when 349 =>
      return "Cl Tombaugh 4";
    when 350 =>
      return "NGC 869";
    when 351 =>
      return "Cl Mrk 6";
    when 352 =>
      return "IC 1805";
    when 353 =>
      return "NGC 884";
    when 354 =>
      return "Cl Czernik 10";
    when 355 =>
      return "Cl Czernik 9";
    when 356 =>
      return "Cl Czernik 13";
    when 357 =>
      return "NGC 1027";
    when 358 =>
      return "Cl Czernik 11";
    when 359 =>
      return "Cl Czernik 8";
    when 360 =>
      return "Cl Berkeley 65";
    when 361 =>
      return "Cl King 4";
    when 362 =>
      return "NGC 957";
    when 363 =>
      return "NGC 752";
    when 364 =>
      return "IC 1848";
    when 365 =>
      return "Cl Trumpler 2";
    when 366 =>
      return "Cl Trumpler 3";
    when 367 =>
      return "Cl Czernik 12";
    when 368 =>
      return "Cr 33";
    when 369 =>
      return "Cr 34";
    when 370 =>
      return "Cl Berkeley 10";
    when 371 =>
      return "Cr 21";
    when 372 =>
      return "Cr 464";
    when 373 =>
      return "Cl Berkeley 66";
    when 374 =>
      return "NGC 2314";
    when 375 =>
      return "Cl Stock 23";
    when 376 =>
      return "Cl Czernik 14";
    when 377 =>
      return "NGC 956";
    when 378 =>
      return "Cl Czernik 17";
    when 379 =>
      return "Upgren,rubin";
    when 380 =>
      return "NGC 1220";
    when 381 =>
      return "Cl King 6";
    when 382 =>
      return "NGC 1039";
    when 383 =>
      return "NGC 1502";
    when 384 =>
      return "Cl King 5";
    when 385 =>
      return "Cl Tombaugh 5";
    when 386 =>
      return "Cl Czernik 15";
    when 387 =>
      return "Cl Czernik 16";
    when 388 =>
      return "Cl Berkeley 9";
    when 389 =>
      return "NGC 1245";
    when 390 =>
      return "NGC 1193";
    when 391 =>
      return "NGC 1348";
    when 392 =>
      return "Cl Melotte 20";
    when 393 =>
      return "IC 361";
    when 394 =>
      return "NGC 1444";
    when 395 =>
      return "Cl King 7";
    when 396 =>
      return "NGC 1496";
    when 397 =>
      return "NGC 1528";
    when 398 =>
      return "NGC 1513";
    when 399 =>
      return "NGC 1545";
    when 400 =>
      return "Cl Berkeley 67";
    when 401 =>
      return "NGC 1342";
    when 402 =>
      return "Cl Berkeley 13";
    when 403 =>
      return "NGC 1624";
    when 404 =>
      return "Cl Berkeley11";
    when 405 =>
      return "NGC 1724";
    when 406 =>
      return "NGC 1605";
    when 407 =>
      return "NGC 1582";
    when 408 =>
      return "Cl Ruprecht 148";
    when 409 =>
      return "Gingrich";
    when 410 =>
      return "Cl Berkeley 16";
    when 411 =>
      return "NGC 1664";
    when 412 =>
      return "Cl Berkeley 12";
    when 413 =>
      return "Cl Berkeley 68";
    when 414 =>
      return "Cl Berkeley 15";
    when 415 =>
      return "NGC 1548";
    when 416 =>
      return "Cl Berkeley 14";
    when 417 =>
      return "NGC 1883";
    when 418 =>
      return "NGC 2126";
    when 419 =>
      return "Cl Berkeley 18";
    when 420 =>
      return "Cl King 22";
    when 421 =>
      return "M 45";
    when 422 =>
      return "Cl Berkeley 70";
    when 423 =>
      return "Cr 62";
    when 424 =>
      return "Cl King 17";
    when 425 =>
      return "Cl Dolidze 14";
    when 426 =>
      return "Cl Czernik 18";
    when 427 =>
      return "Cl Czernik 20";
    when 428 =>
      return "NGC 1857";
    when 429 =>
      return "NGC 1778";
    when 430 =>
      return "Cl Dolidze 15 170.2 -03.9 05 01.3 +34 46 O-1315 05 08 +36 310 115";
    when 431 =>
      return "Cl Stock 10";
    when 432 =>
      return "Cl Czernik 21";
    when 433 =>
      return "NGC 1912";
    when 434 =>
      return "NGC 1907";
    when 435 =>
      return "Cl Dolidze 16";
    when 436 =>
      return "Cl Stock 8";
    when 437 =>
      return "NGC 2192";
    when 438 =>
      return "IC 410";
    when 439 =>
      return "NGC 1893";
    when 440 =>
      return "Cl Dolidze 18";
    when 441 =>
      return "Cl Stock 9";
    when 442 =>
      return "Cl Dolidze 29";
    when 443 =>
      return "Cl Czernik 19";
    when 444 =>
      return "Cl Berkeley 69";
    when 445 =>
      return "NGC 1960";
    when 446 =>
      return "NGC 2281";
    when 447 =>
      return "Cl Berkeley 17";
    when 448 =>
      return "Cl King 8";
    when 449 =>
      return "Cl Berkeley 71";
    when 450 =>
      return "Cl Berkeley 19";
    when 451 =>
      return "NGC 2099";
    when 452 =>
      return "NGC 1746";
    when 453 =>
      return "NGC 1758";
    when 454 =>
      return "NGC 1750";
    when 455 =>
      return "Basel 4";
    when 456 =>
      return "B";
    when 457 =>
      return "NGC 1647";
    when 458 =>
      return "Cl Czernik 23";
    when 459 =>
      return "Cl Dolidze-D. 3";
    when 460 =>
      return "Cl Dolidze-D. 4";
    when 461 =>
      return "NGC 1663";
    when 462 =>
      return "NGC 1807";
    when 463 =>
      return "NGC 1817";
    when 464 =>
      return "Cl Berkeley 72";
    when 465 =>
      return "Cl Trumpler 4";
    when 466 =>
      return "NGC 2168";
    when 467 =>
      return "NGC 2129";
    when 468 =>
      return "NGC 2158";
    when 469 =>
      return "Cl Berkeley 21";
    when 470 =>
      return "NGC 1662";
    when 471 =>
      return "NGC 2266";
    when 472 =>
      return "Cl Czernik 24";
    when 473 =>
      return "Cr 89";
    when 474 =>
      return "Cr 65";
    when 475 =>
      return "NGC 2331";
    when 476 =>
      return "NGC 2175";
    when 477 =>
      return "Cl Dolidze-D. 2";
    when 478 =>
      return "Cl Berkeley 23";
    when 479 =>
      return "Cr 69";
    when 480 =>
      return "Cl Dolidze 19";
    when 481 =>
      return "NGC 2169";
    when 482 =>
      return "Cl Dolidze 17";
    when 483 =>
      return "Cl Dolidze 21";
    when 484 =>
      return "NGC 2304";
    when 485 =>
      return "NGC 2194";
    when 486 =>
      return "Cl Berkeley 29";
    when 487 =>
      return "NGC 2141";
    when 488 =>
      return "NGC 2420";
    when 489 =>
      return "Cr 74";
    when 490 =>
      return "Cl Berkeley 22";
    when 491 =>
      return "Cr 95";
    when 492 =>
      return "NGC 2259";
    when 493 =>
      return "Cl Czernik 25";
    when 494 =>
      return "Cl Trumpler 5";
    when 495 =>
      return "NGC 2264";
    when 496 =>
      return "NGC 2355";
    when 497 =>
      return "Cl Berkeley 20";
    when 498 =>
      return "NGC 2186";
    when 499 =>
      return "NGC 2251";
    when 500 =>
      return "NGC 2254";
    when 501 =>
      return "NGC 2236";
    when 502 =>
      return "NGC 2395";
    when 503 =>
      return "Cr 70";
    when 504 =>
      return "Cr 92";
    when 505 =>
      return "Cr 111";
    when 506 =>
      return "Cr 97";
    when 507 =>
      return "NGC 2632";
    when 508 =>
      return "Cl Dolidze 22";
    when 509 =>
      return "NGC 2112";
    when 510 =>
      return "Cr 106";
    when 511 =>
      return "NGC 2237";
    when 512 =>
      return "NGC 2239";
    when 513 =>
      return "Biur 7";
    when 514 =>
      return "NGC 2252";
    when 515 =>
      return "NGC 2244";
    when 516 =>
      return "Cl Dolidze 26";
    when 517 =>
      return "Cr 104";
    when 518 =>
      return "Cr 107";
    when 519 =>
      return "Cr 91";
    when 520 =>
      return "Biur 12";
    when 521 =>
      return "Biur 11";
    when 522 =>
      return "Biur 8";
    when 523 =>
      return "Cr 96";
    when 524 =>
      return "NGC 2269";
    when 525 =>
      return "NGC 1981";
    when 526 =>
      return "Cl Czernik 27";
    when 527 =>
      return "Van den Bergh";
    when 528 =>
      return "Cl Trapezium";
    when 529 =>
      return "NGC 1980";
    when 530 =>
      return "Cr 110";
    when 531 =>
      return "NGC 2262";
    when 532 =>
      return "Biur 10";
    when 533 =>
      return "Cl Dolidze 24";
    when 534 =>
      return "Biur 9";
    when 535 =>
      return "Cr 115";
    when 536 =>
      return "Cl Berkeley 78";
    when 537 =>
      return "Cl Dolidze 25";
    when 538 =>
      return "Cl Dolidze 23";
    when 539 =>
      return "Cl Berkeley 24";
    when 540 =>
      return "NGC 2301";
    when 541 =>
      return "Cl Berkeley 35";
    when 542 =>
      return "NGC 2324";
    when 543 =>
      return "Biur 13,";
    when 544 =>
      return "Cl Czernik 26";
    when 545 =>
      return "NGC 2232";
    when 546 =>
      return "Cl Berkeley 73";
    when 547 =>
      return "NGC 2250";
    when 548 =>
      return "NGC 2286";
    when 549 =>
      return "NGC 2682";
    when 550 =>
      return "NGC 2215";
    when 551 =>
      return "Cl Berkeley 37";
    when 552 =>
      return "Cl Czernik 28";
    when 553 =>
      return "NGC 2311";
    when 554 =>
      return "NGC 2302";
    when 555 =>
      return "Cl Berkeley 77";
    when 556 =>
      return "Cl Haffner 3";
    when 557 =>
      return "NGC2309";
    when 558 =>
      return "Cl Melotte 111";
    when 559 =>
      return "NGC 2323";
    when 560 =>
      return "Cl Ruprecht 4";
    when 561 =>
      return "Cl Berkeley 39";
    when 562 =>
      return "NGC 2335";
    when 563 =>
      return "Cl Ruprecht 1";
    when 564 =>
      return "Cr 465";
    when 565 =>
      return "NGC 2343";
    when 566 =>
      return "Cr 466";
    when 567 =>
      return "NGC 2353";
    when 568 =>
      return "Cl Berkeley 76";
    when 569 =>
      return "Cl Ruprecht 6";
    when 570 =>
      return "Cl Ruprecht 7";
    when 571 =>
      return "NGC2368";
    when 572 =>
      return "NGC 2204";
    when 573 =>
      return "Cl Ruprecht 8";
    when 574 =>
      return "Cl Czernik 30";
    when 575 =>
      return "NGC 2345";
    when 576 =>
      return "Cl Berkeley 25";
    when 577 =>
      return "Cl Berkeley 36";
    when 578 =>
      return "Cl Haffner 8";
    when 579 =>
      return "NGC 2396";
    when 580 =>
      return "Cr 467";
    when 581 =>
      return "Cl Melotte 72";
    when 582 =>
      return "Cl Haffner 6";
    when 583 =>
      return "Cl Haffner 4";
    when 584 =>
      return "NGC 2548";
    when 585 =>
      return "NGC 2374";
    when 586 =>
      return "Cl Ruprecht 24";
    when 587 =>
      return "Cl Melotte 71";
    when 588 =>
      return "NGC 2401";
    when 589 =>
      return "NGC 2360";
    when 590 =>
      return "Cl Haffner 23";
    when 591 =>
      return "Cl Ruprecht 5";
    when 592 =>
      return "NGC2423";
    when 593 =>
      return "NGC 2506";
    when 594 =>
      return "Cl Haffner 10";
    when 595 =>
      return "Cl Czernik 29";
    when 596 =>
      return "NGC 2422";
    when 597 =>
      return "NGC 2287";
    when 598 =>
      return "NGC 2414";
    when 599 =>
      return "Cl Haffner 12";
    when 600 =>
      return "Cl Haffner 9";
    when 601 =>
      return "NGC2437";
    when 602 =>
      return "Cl Ruprecht 26";
    when 603 =>
      return "Cl Tombaugh 1";
    when 604 =>
      return "Cl Ruprecht 10";
    when 605 =>
      return "Cl Tombaugh 2";
    when 606 =>
      return "Cl Ruprecht 151";
    when 607 =>
      return "Cl Ruprecht 11";
    when 608 =>
      return "Cl Haffner 24";
    when 609 =>
      return "Cl Ruprecht 15";
    when 610 =>
      return "Cl Ruprecht 9";
    when 611 =>
      return "NGC 2539";
    when 612 =>
      return "Cl Ruprecht 16";
    when 613 =>
      return "Cl Ruprecht 149";
    when 614 =>
      return "Cl Berkeley 75";
    when 615 =>
      return "Cl Ruprecht 37";
    when 616 =>
      return "NGC 2383";
    when 617 =>
      return "Cl Ruprecht 45";
    when 618 =>
      return "NGC 2384";
    when 619 =>
      return "Cr 121";
    when 620 =>
      return "NGC 2432";
    when 621 =>
      return "NGC2367";
    when 622 =>
      return "Cl Ruprecht 19";
    when 623 =>
      return "Cl Trumpler 8";
    when 624 =>
      return "Cl Haffner 5";
    when 625 =>
      return "Cl Czernik 31";
    when 626 =>
      return "NGC 2421";
    when 627 =>
      return "Cl Ruprecht 17";
    when 628 =>
      return "Cl Ruprecht 34";
    when 629 =>
      return "Cl Ruprecht 38";
    when 630 =>
      return "NGC 2509";
    when 631 =>
      return "Cl Ruprecht 13";
    when 632 =>
      return "Cl Ruprecht 23";
    when 633 =>
      return "NGC 2362";
    when 634 =>
      return "Cl Ruprecht 40";
    when 635 =>
      return "Cl Trumpler 7";
    when 636 =>
      return "NGC 2455";
    when 637 =>
      return "Cl Ruprecht 46";
    when 638 =>
      return "Cl Trumpler 6";
    when 639 =>
      return "NGC 2354";
    when 640 =>
      return "Cl Ruprecht 33";
    when 641 =>
      return "Ruprecht25";
    when 642 =>
      return "Cl Ruprecht 3";
    when 643 =>
      return "Cl Ruprecht 2";
    when 644 =>
      return "NGC 2243";
    when 645 =>
      return "Cl Ruprecht 39";
    when 646 =>
      return "Cl Ruprecht 18";
    when 647 =>
      return "Cl Ruprecht 12";
    when 648 =>
      return "Cl Ruprecht 150";
    when 649 =>
      return "NGC 2447";
    when 650 =>
      return "Cl Ruprecht 29";
    when 651 =>
      return "Cl Ruprecht 32";
    when 652 =>
      return "Cl Ruprecht 157";
    when 653 =>
      return "NGC 2482";
    when 654 =>
      return "Cl Ruprecht 27";
    when 655 =>
      return "Cl Haffner 16";
    when 656 =>
      return "Cl Haffner 25";
    when 657 =>
      return "Cl Haffner 11";
    when 658 =>
      return "Cl Ruprecht 62";
    when 659 =>
      return "Cl Ruprecht 20";
    when 660 =>
      return "Cl Ruprecht 36";
    when 661 =>
      return "Cl Haffner 7";
    when 662 =>
      return "Cl Haffner 19";
    when 663 =>
      return "Cl Trumpler 9";
    when 664 =>
      return "Cl Ruprecht 22";
    when 665 =>
      return "Cl Haffner 18a";
    when 666 =>
      return "Cl Haffner 18c";
    when 667 =>
      return "Cl Haffner 18b";
    when 668 =>
      return "NGC2467";
    when 669 =>
      return "Cl Ruprecht 42";
    when 670 =>
      return "NGC 2453";
    when 671 =>
      return "CR132";
    when 672 =>
      return "Cl Ruprecht 14";
    when 673 =>
      return "Cl Ruprecht 41";
    when 674 =>
      return "Cl Haffner14";
    when 675 =>
      return "Cl Ruprecht 21";
    when 676 =>
      return "Cl Ruprecht 49";
    when 677 =>
      return "Cl Haffner 21";
    when 678 =>
      return "Cl Haffner 13";
    when 679 =>
      return "Cr 140";
    when 680 =>
      return "Cl Ruprecht 28";
    when 681 =>
      return "Cl Ruprecht 44";
    when 682 =>
      return "Cl Ruprecht 53";
    when 683 =>
      return "Cl Czernik 32";
    when 684 =>
      return "Cl Ruprecht 43";
    when 685 =>
      return "NGC 2527";
    when 686 =>
      return "Cl Ruprecht 57";
    when 687 =>
      return "Cl Ruprecht 30";
    when 688 =>
      return "NGC 2439";
    when 689 =>
      return "Cl Ruprecht 35";
    when 690 =>
      return "NGC 2489";
    when 691 =>
      return "Cl Haffner 22";
    when 692 =>
      return "Cl Haffner 20";
    when 693 =>
      return "Cl Ruprecht 153";
    when 694 =>
      return "Cl Haffner 17";
    when 695 =>
      return "NGC2533";
    when 696 =>
      return "Cl Haffner 15";
    when 697 =>
      return "Cl Ruprecht 51";
    when 698 =>
      return "Cl Ruprecht 50";
    when 699 =>
      return "Cl Ruprecht 47";
    when 700 =>
      return "CR135";
    when 701 =>
      return "NGC 2571";
    when 702 =>
      return "Cl Ruprecht 48";
    when 703 =>
      return "Cl Ruprecht 155";
    when 704 =>
      return "Cr 187";
    when 705 =>
      return "Cl Ruprecht 52";
    when 706 =>
      return "NGC 2587";
    when 707 =>
      return "Cl Haffner 26";
    when 708 =>
      return "NGC 2567";
    when 709 =>
      return "NGC 2580";
    when 710 =>
      return "Cl Ruprecht 54";
    when 711 =>
      return "Cl Ruprecht 31";
    when 712 =>
      return "Cl Ruprecht 58";
    when 713 =>
      return "Cl Ruprecht 55";
    when 714 =>
      return "NGC 2627";
    when 715 =>
      return "NGC 2588";
    when 716 =>
      return "NGC2451";
    when 717 =>
      return "Cl Ruprecht 59";
    when 718 =>
      return "Cl Ruprecht 61";
    when 719 =>
      return "Cl Ruprecht 152";
    when 720 =>
      return "NGC 2477";
    when 721 =>
      return "Cr 196";
    when 722 =>
      return "Cr 198";
    when 723 =>
      return "NGC 2658";
    when 724 =>
      return "NGC 2579";
    when 725 =>
      return "Cr 185";
    when 726 =>
      return "NGC 2546";
    when 727 =>
      return "Cl Pishmish 1";
    when 728 =>
      return "NGC2635";
    when 729 =>
      return "Cl Ruprecht 68";
    when 730 =>
      return "Cl Ruprecht 56";
    when 731 =>
      return "Cl Pishmish 3";
    when 732 =>
      return "Cl Ruprecht 66";
    when 733 =>
      return "Cl Pishmish 2";
    when 734 =>
      return "Cl Pishmish 7";
    when 735 =>
      return "Cl Pishmish 5";
    when 736 =>
      return "Cl Ruprecht 158";
    when 737 =>
      return "Cl Ruprecht 72";
    when 738 =>
      return "Cl Ruprecht 154";
    when 739 =>
      return "Cl Melotte 66";
    when 740 =>
      return "Cl Ruprecht 64";
    when 741 =>
      return "CR173";
    when 742 =>
      return "Cr 197";
    when 743 =>
      return "NGC 2818";
    when 744 =>
      return "Cl Ruprecht 156";
    when 745 =>
      return "NGC 2671";
    when 746 =>
      return "Cl Pishmish 4";
    when 747 =>
      return "Cl Trumpler 10";
    when 748 =>
      return "Cl Ruprecht 67";
    when 749 =>
      return "Cl Ruprecht 74";
    when 750 =>
      return "Cl Ruprecht 65";
    when 751 =>
      return "Cl Ruprecht 60";
    when 752 =>
      return "Cl Pishmish 9";
    when 753 =>
      return "NGC 2547";
    when 754 =>
      return "Cl Pishmish 6";
    when 755 =>
      return "Cl Pishmish 8";
    when 756 =>
      return "NGC 2849";
    when 757 =>
      return "Cl Pishmish 10";
    when 758 =>
      return "Cl Ruprecht 63";
    when 759 =>
      return "NGC2660";
    when 760 =>
      return "Cl Ruprecht 70";
    when 761 =>
      return "Cl Ruprecht 71";
    when 762 =>
      return "Cl Ruprecht 69";
    when 763 =>
      return "IC2395";
    when 764 =>
      return "NGC 2670";
    when 765 =>
      return "Cl Pishmish 12";
    when 766 =>
      return "Cl Mrk 18";
    when 767 =>
      return "IC 2391";
    when 768 =>
      return "Cl Haffner 3";
    when 769 =>
      return "Cl Ruprecht 79";
    when 770 =>
      return "Cl Ruprecht 80";
    when 771 =>
      return "Cl Ruprecht 81";
    when 772 =>
      return "Cl Pishmish 11";
    when 773 =>
      return "Cl Pishmish 15";
    when 774 =>
      return "Cl Pishmish 13";
    when 775 =>
      return "Cl Ruprecht 76";
    when 776 =>
      return "NGC 2516";
    when 777 =>
      return "Cl Ruprecht 160";
    when 778 =>
      return "NGC 2972";
    when 779 =>
      return "Cl Hogg 1";
    when 780 =>
      return "Cl Pishmish 14";
    when 781 =>
      return "NGC 2910";
    when 782 =>
      return "Cl Ruprecht 78";
    when 783 =>
      return "NGC 2925";
    when 784 =>
      return "Cl Ruprecht 77";
    when 785 =>
      return "Cr 213";
    when 786 =>
      return "Cl Ruprecht 75";
    when 787 =>
      return "Cl Ruprecht 79";
    when 788 =>
      return "Cl Ruprecht 82";
    when 789 =>
      return "IC 2488";
    when 790 =>
      return "Cl Pishmish 16";
    when 791 =>
      return "Cl Ruprecht 83";
    when 792 =>
      return "Cl Ruprecht 87";
    when 793 =>
      return "Cl Hogg 4";
    when 794 =>
      return "Cl Hogg 3";
    when 795 =>
      return "Cl Ruprecht 159";
    when 796 =>
      return "NGC 3033";
    when 797 =>
      return "Cl Hogg 2";
    when 798 =>
      return "NGC 3105";
    when 799 =>
      return "Cl Ruprecht 85";
    when 800 =>
      return "NGC 3228";
    when 801 =>
      return "Cl Ruprecht 86";
    when 802 =>
      return "NGC 3114";
    when 803 =>
      return "Cl Trumpler 12";
    when 804 =>
      return "Cl Hogg 5";
    when 805 =>
      return "Cl Hogg 6";
    when 806 =>
      return "Cl Hogg 4";
    when 807 =>
      return "Cl Westerlund 2";
    when 808 =>
      return "Cl Trumpler 11";
    when 809 =>
      return "NGC 3247";
    when 810 =>
      return "Cl Ruprecht 161";
    when 811 =>
      return "IC 2581";
    when 812 =>
      return "";
    when 813 =>
      return "Cl Ruprecht 90";
    when 814 =>
      return "Cl Ruprecht 84";
    when 815 =>
      return "Cl Trumpler 13";
    when 816 =>
      return "NGC 3293";
    when 817 =>
      return "NGC 3255";
    when 818 =>
      return "Cr 223";
    when 819 =>
      return "NGC 3324";
    when 820 =>
      return "Cl Hogg 7";
    when 821 =>
      return "Cl Hogg 8";
    when 822 =>
      return "Cl Ruprecht 88";
    when 823 =>
      return "NGC 3680";
    when 824 =>
      return "Cl Ruprecht 91";
    when 825 =>
      return "Cl Trumpler 15";
    when 826 =>
      return "Cl Trumpler 14";
    when 827 =>
      return "Cr 232";
    when 828 =>
      return "Cr 228";
    when 829 =>
      return "Cl Trumpler 16";
    when 830 =>
      return "Cr 234";
    when 831 =>
      return "Cl Trumpler 17";
    when 832 =>
      return "Cl Hogg 9";
    when 833 =>
      return "Cl Pishmish 17";
    when 834 =>
      return "Cl Ruprecht 92";
    when 835 =>
      return "Cr 236";
    when 836 =>
      return "NGC 3496";
    when 837 =>
      return "Cl Sher 1";
    when 838 =>
      return "IC 2602";
    when 839 =>
      return "NGC 3532";
    when 840 =>
      return "Cl Ruprecht 162";
    when 841 =>
      return "Cl Melotte 101";
    when 842 =>
      return "Cl Feinstein";
    when 843 =>
      return "Cl Trumpler 19";
    when 844 =>
      return "Cl Ruprecht 93";
    when 845 =>
      return "Cl Stock 13";
    when 846 =>
      return "NGC 3572";
    when 847 =>
      return "Cl Hogg 10";
    when 848 =>
      return "Cr 240";
    when 849 =>
      return "Cl Hogg 11";
    when 850 =>
      return "Cl Trumpler 18";
    when 851 =>
      return "Cl Hogg 12";
    when 852 =>
      return "NGC 3590";
    when 853 =>
      return "Cl Hogg 13";
    when 854 =>
      return "NGC 3603";
    when 855 =>
      return "IC 2714";
    when 856 =>
      return "Cl Melotte 105";
    when 857 =>
      return "Cl Ruprecht 163";
    when 858 =>
      return "Cl Ruprecht 164";
    when 859 =>
      return "Cl Ruprecht 94";
    when 860 =>
      return "NGC 3766";
    when 861 =>
      return "NGC 3960";
    when 862 =>
      return "IC 2944";
    when 863 =>
      return "Cl Ruprecht 95";
    when 864 =>
      return "IC 2948";
    when 865 =>
      return "Cl Stock 14";
    when 866 =>
      return "Cl Ruprecht 96";
    when 867 =>
      return "Cl Ruprecht 97";
    when 868 =>
      return "Cl Ruprecht 98";
    when 869 =>
      return "Cl Stock 15";
    when 870 =>
      return "NGC 4052";
    when 871 =>
      return "NGC 4103";
    when 872 =>
      return "Cl Ruprecht 99";
    when 873 =>
      return "Cl Ruprecht 100";
    when 874 =>
      return "NGC 4230";
    when 875 =>
      return "Cl Ruprecht 101";
    when 876 =>
      return "Cl Ruprecht 103";
    when 877 =>
      return "Cl Ruprecht 102";
    when 878 =>
      return "NGC 4337";
    when 879 =>
      return "Cl Ruprecht 104";
    when 880 =>
      return "H 5";
    when 881 =>
      return "Cl Ruprecht 165";
    when 882 =>
      return "NGC 4349";
    when 883 =>
      return "Cl Hogg 14";
    when 884 =>
      return "NGC 4439";
    when 885 =>
      return "NGC4463";
    when 886 =>
      return "Cl Ruprecht 105";
    when 887 =>
      return "Cl Ruprecht 106";
    when 888 =>
      return "Cl Trumpler 20";
    when 889 =>
      return "H6";
    when 890 =>
      return "NGC 4609";
    when 891 =>
      return "Cl Hogg 15";
    when 892 =>
      return "NGC 4755";
    when 893 =>
      return "NGC 4815";
    when 894 =>
      return "NGC 4852";
    when 895 =>
      return "Cl Hogg 8";
    when 896 =>
      return "Cl Ruprecht 107";
    when 897 =>
      return "CR268";
    when 898 =>
      return "Cl Stock 16";
    when 899 =>
      return "Cl Ruprecht 166";
    when 900 =>
      return "Cr 271";
    when 901 =>
      return "Cl Hogg 16";
    when 902 =>
      return "NGC 5138";
    when 903 =>
      return "Cl Trumpler 21";
    when 904 =>
      return "Cr 272";
    when 905 =>
      return "NGC 5168";
    when 906 =>
      return "Cl Pishmish 18";
    when 907 =>
      return "Cl Ruprecht 108";
    when 908 =>
      return "Cr 275";
    when 909 =>
      return "Cr 277";
    when 910 =>
      return "NGC 5288";
    when 911 =>
      return "NGC 5281";
    when 912 =>
      return "Cl Ruprecht 110";
    when 913 =>
      return "NGC 5316";
    when 914 =>
      return "Cl Lynga 1";
    when 915 =>
      return "Cl Ruprecht 109";
    when 916 =>
      return "Cl Lynga 2";
    when 917 =>
      return "Cl Ruprecht 167";
    when 918 =>
      return "Cl Melotte 227";
    when 919 =>
      return "NGC 5617";
    when 920 =>
      return "Cl Trumpler 22";
    when 921 =>
      return "Cl Pishmish 19";
    when 922 =>
      return "NGC 5606";
    when 923 =>
      return "Cl Hogg 17";
    when 924 =>
      return "Cl Ruprecht 111";
    when 925 =>
      return "NGC 5460";
    when 926 =>
      return "NGC 5593";
    when 927 =>
      return "Cl Ruprecht 112";
    when 928 =>
      return "NGC 5662";
    when 929 =>
      return "NGC 5715";
    when 930 =>
      return "NGC 5749";
    when 931 =>
      return "Cl Pishmish 21";
    when 932 =>
      return "Cl Pishmish 20";
    when 933 =>
      return "Cl Hogg 18";
    when 934 =>
      return "NGC 5764";
    when 935 =>
      return "Cl Lynga 3";
    when 936 =>
      return "NGC 5823";
    when 937 =>
      return "NGC 5822";
    when 938 =>
      return "NGC 5925";
    when 939 =>
      return "NGC 6025";
    when 940 =>
      return "Cl Ruprecht 113";
    when 941 =>
      return "Cl Lynga 4";
    when 942 =>
      return "Cl Lynga 5";
    when 943 =>
      return "Cr 292";
    when 944 =>
      return "Cl Hogg 9";
    when 945 =>
      return "NGC 6005";
    when 946 =>
      return "NGC 5999";
    when 947 =>
      return "Cl Ruprecht 114";
    when 948 =>
      return "NGC 6087";
    when 949 =>
      return "Cl Lynga 7";
    when 950 =>
      return "Cl Trumpler 23";
    when 951 =>
      return "NGC 6031";
    when 952 =>
      return "Cr 299";
    when 953 =>
      return "NGC 6067";
    when 954 =>
      return "Cl Hogg 10";
    when 955 =>
      return "Cl Lynga 6";
    when 956 =>
      return "Cl Ruprecht 115";
    when 957 =>
      return "Cl Pishmish 22";
    when 958 =>
      return "Cl Ruprecht 116";
    when 959 =>
      return "Cl Ruprecht 117";
    when 960 =>
      return "Cl Ruprecht 118";
    when 961 =>
      return "NGC 6152";
    when 962 =>
      return "Cl Lynga 8";
    when 963 =>
      return "Cl Ruprecht 119";
    when 964 =>
      return "NGC 6208";
    when 965 =>
      return "Cr 307";
    when 966 =>
      return "Cl Lynga 9";
    when 967 =>
      return "Cl Pishmish 23";
    when 968 =>
      return "NGC 6134";
    when 969 =>
      return "Cl Hogg 19";
    when 970 =>
      return "NGC 5053";
    when 971 =>
      return "H11";
    when 972 =>
      return "NGC 6253";
    when 973 =>
      return "Cl Lynga 12";
    when 974 =>
      return "Cl Ruprecht 120";
    when 975 =>
      return "NGC 6193";
    when 976 =>
      return "Cl Hogg 20";
    when 977 =>
      return "Cl Hogg 21";
    when 978 =>
      return "NGC 6200";
    when 979 =>
      return "Cl Lynga 11";
    when 980 =>
      return "NGC 6178";
    when 981 =>
      return "Cl Hogg 22";
    when 982 =>
      return "NGC 6204";
    when 983 =>
      return "Cl Ruprecht 121";
    when 984 =>
      return "NGC 6169";
    when 985 =>
      return "Cl Westerlund 1";
    when 986 =>
      return "Cl Hogg 13";
    when 987 =>
      return "IC 4651";
    when 988 =>
      return "NGC 6192";
    when 989 =>
      return "NGC 6216";
    when 990 =>
      return "NGC 6124";
    when 991 =>
      return "NGC 6250";
    when 992 =>
      return "Cl Lynga 14";
    when 993 =>
      return "NGC 6352";
    when 994 =>
      return "NGC 6249";
    when 995 =>
      return "Cl Lynga 13";
    when 996 =>
      return "NGC 6259";
    when 997 =>
      return "NGC 6231";
    when 998 =>
      return "Cl Ruprecht 122";
    when 999 =>
      return "H 12";
    when 1000 =>
      return "NGC 6322";
    when 1001 =>
      return "NGC 6242";
    when 1002 =>
      return "NGC 6268";
    when 1003 =>
      return "NGC 6281";
    when 1004 =>
      return "NGC 6318";
    when 1005 =>
      return "Cl Ruprecht 124";
    when 1006 =>
      return "Cl Ruprecht 125";
    when 1007 =>
      return "Cl Trumpler 25";
    when 1008 =>
      return "Cl Ruprecht 123";
    when 1009 =>
      return "Cl Trumpler 29";
    when 1010 =>
      return "Cr 332";
    when 1011 =>
      return "Cr 302";
    when 1012 =>
      return "H 16";
    when 1013 =>
      return "Cr 338";
    when 1014 =>
      return "NGC 6400";
    when 1015 =>
      return "Cl Ruprecht 127";
    when 1016 =>
      return "Cl Pishmish 24";
    when 1017 =>
      return "Cr 333";
    when 1018 =>
      return "NGC 6396";
    when 1019 =>
      return "Cl Ruprecht 126";
    when 1020 =>
      return "Cl Ruprecht 128";
    when 1021 =>
      return "Cl Trumpler 27";
    when 1022 =>
      return "NGC 6374";
    when 1023 =>
      return "Cl Ruprecht 132";
    when 1024 =>
      return "NGC 6404";
    when 1025 =>
      return "Cl Trumpler 30";
    when 1026 =>
      return "NGC 6383";
    when 1027 =>
      return "Cr 345";
    when 1028 =>
      return "NGC 6475";
    when 1029 =>
      return "Cl Trumpler 28";
    when 1030 =>
      return "NGC 6405";
    when 1031 =>
      return "NGC 6416";
    when 1032 =>
      return "Cl Trumpler 26";
    when 1033 =>
      return "NGC 6425";
    when 1034 =>
      return "Cl Ruprecht 130";
    when 1035 =>
      return "NGC 6451";
    when 1036 =>
      return "NGC 6355";
    when 1037 =>
      return "Cl Ruprecht 129";
    when 1038 =>
      return "Cl Basel5";
    when 1039 =>
      return "Cr 347";
    when others =>
      return "";
    end case;
  end Target_Name_Of;

end Name.Ocl;
