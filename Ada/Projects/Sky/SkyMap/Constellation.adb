-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_White_Elephant;

with Ada.Containers.Indefinite_Ordered_Maps;
with Traces;

package body Constellation is

  package Log is new Traces ("Constellation");

  type Part is record
    From  : Star.Number;
    To    : Star.Number;
    Const : Item;
  end record;

  type Part_List is array (Positive range <>) of Part;

  Max_Parts_Per_Item : constant := 50;

  Data : constant Part_List := [
    (  15,  165, Anr), -- 21 alpha
    ( 165,  337, Anr), -- 31 delta
    ( 337,  603, Anr), -- 43 beta
    ( 226,  269, Anr), -- 35 nu
    ( 269,  337, Anr), -- 37 mu

    (7557, 7377, Aql), -- 53 alpha
    (7377, 7236, Aql), -- 30 delta
    (7236, 7193, Aql), -- 16 lambda
    (7710, 7602, Aql), -- 65 theta
    (7602, 7557, Aql), -- 60 beta
    (7557, 7525, Aql), -- 53 alpha
    (7525, 7235, Aql), -- 50 gamma

    (7950, 8093, Aqr), --  2 epsilon
    (8093, 8232, Aqr), -- 13 nu
    (8232, 8414, Aqr), -- 22 beta
    (8414, 8518, Aqr), -- 34 alpha
    (8518, 8539, Aqr), -- 48 gamma
    (8539, 8597, Aqr), -- 52 pi
    (8597, 8518, Aqr), -- 62 eta
    (8518, 8610, Aqr), -- 48 gamma
    (8610, 8698, Aqr), -- 63 kappa
    (8698, 8709, Aqr), -- 73 lambda
    (8709, 8812, Aqr), -- 76 delta
    (8812, 8892, Aqr), -- 88
    (8892, 8841, Aqr), -- 98
    (8841, 8698, Aqr), -- 91 psi^1

    (6510, 6461, Ara), --    alpha
    (6461, 6462, Ara), --    beta
    (6462, 6500, Ara), --    gamma
    (6229, 6285, Ara), --    eta
    (6285, 6461, Ara), --    zeta

    ( 553,  617, Ari), --  6 beta
    ( 617,  838, Ari), -- 13 alpha

    (1708, 2088, Aur), -- 13 alpha
    (2088, 2095, Aur), -- 34 beta
    (2095, 1791, Aur), -- 37 theta
    (1708, 1605, Aur), -- 13 alpha
    (1605, 1612, Aur), --  7 epsilon
    (1612, 1577, Aur), --  8 zeta
    (1577, 1791, Aur), --  3 iota

    (5478, 5340, Boo), -- 30 zeta
    (5340, 5506, Boo), -- 16 alpha
    (5506, 5681, Boo), -- 36 epsilon
    (5681, 5602, Boo), -- 49 delta
    (5602, 5435, Boo), -- 42 beta
    (5435, 5429, Boo), -- 27 gamma
    (5429, 5340, Boo), -- 25 rho
    (5340, 5235, Boo), -- 16 alpha
    (5235, 5200, Boo), --  8 eta

    (1603, 1155, Cam), -- 10 beta
    (1035, 1155, Cam), --
    (1155, 1686, Cam), --    BE

    (7747, 7776, Cap), --  5 alpha^1
    (7776, 8075, Cap), --  9 beta
    (8075, 8167, Cap), -- 23 theta
    (8167, 8278, Cap), -- 32 iota
    (8278, 8322, Cap), -- 40 gamma
    (8322, 8260, Cap), -- 49 delta
    (8260, 8213, Cap), -- 39 epsilon
    (8213, 8204, Cap), -- 36
    (8204, 8080, Cap), -- 34 zeta
    (8080, 7980, Cap), -- 24
    (7980, 7936, Cap), -- 18 omega
    (7936, 7776, Cap), -- 16 psi

    (2326, 2553, Car), --    alpha
    (2553, 2878, Car), -- up tau
    (2878, 3165, Car), -- up sigma
    (3685, 3890, Car), --    beta
    (3890, 3699, Car), --    upsilon
    (3699, 3485, Car), --    iota
    (3485, 3207, Car), -- el delta
    (3207, 2878, Car), -- el gamma^2
    (3699, 3307, Car), --    iota
    (3307, 3117, Car), --    epsilon
    (3117, 3207, Car), --    chi

    (  21,  168, Cas), -- 11 beta
    ( 168,  264, Cas), -- 18 alpha
    ( 264,  403, Cas), -- 27 gamma
    ( 403,  542, Cas), -- 37 delta

    (5459, 5267, Cen), --    alpha^1
    (5267, 5132, Cen), --    beta
    (5132, 5231, Cen), --    epsilon
    (5231, 5193, Cen), --    zeta
    (5193, 5190, Cen), --    mu
    (5190, 5288, Cen), --    nu
    (5288, 5378, Cen), --  5 theta
    (5378, 5440, Cen), --    v761
    (5028, 5089, Cen), --    iota
    (5089, 5190, Cen), --
    (4621, 4819, Cen), --    delta
    (4819, 5132, Cen), --    gamma

    (8974, 8238, Cep), -- 35 gamma
    (8238, 8162, Cep), --  8 beta
    (8162, 8571, Cep), --  5 alpha
    (8571, 8694, Cep), -- 27 delta
    (8694, 8238, Cep), -- 32 iota

    ( 804,  911, Cet), -- 86 gamma
    ( 911,  813, Cet), -- 92 alpha
    ( 813,  718, Cet), -- 87 mu
    ( 718,  754, Cet), -- 73 xi^2
    ( 754,  804, Cet), -- 78 nu
    ( 804,  779, Cet), -- 86 gamma
    ( 779,  681, Cet), -- 82 delta
    ( 681,  539, Cet), -- 68 o
    ( 539,  509, Cet), -- 55 zeta
    ( 509,  334, Cet), -- 52 tau
    ( 334,   74, Cet), -- 31 eta
    (  74,  188, Cet), --  8 iota
    ( 188,  334, Cet), -- 16 beta
    ( 334,  402, Cet), -- 31 eta
    ( 402,  539, Cet), -- 45 theta

    (5463, 5459, Cir), --    alpha

    (2282, 2294, Cma), --  1 zeta
    (2294, 2491, Cma), --  2 beta
    (2491, 2653, Cma), --  9 alpha
    (2653, 2693, Cma), -- 24 o^2
    (2693, 2827, Cma), -- 25 delta
    (2618, 2646, Cma), -- 21 epsilon
    (2646, 2693, Cma), -- 22 sigma

    (2943, 2845, Cmi), -- 10 alpha

    (3572, 3461, Cnc), -- 65 alpha
    (3461, 3449, Cnc), -- 47 delta
    (3449, 3475, Cnc), -- 43 gamma
    (3249, 3357, Cnc), -- 17 beta
    (3366, 3357, Cnc), -- 33 eta
    (3366, 3449, Cnc), -- 33 eta
    (3209, 3357, Cnc), -- 16 zeta
    (3357, 3461, Cnc), -- 31 theta

    (1862, 1956, Col), --    epsilon
    (1956, 2040, Col), --    alpha
    (2040, 2106, Col), --    beta
    (2106, 2296, Col), --    gamma

    (5971, 5947, Crb), -- 14 iota
    (5947, 5889, Crb), -- 13 epsilon
    (5889, 5849, Crb), -- 10 delta
    (5849, 5793, Crb), --  8 gamma
    (5793, 5747, Crb), --  5 alpha
    (5747, 5778, Crb), --  3 beta

    (4567, 4514, Crt), -- 30 eta
    (4514, 4405, Crt), -- 27 zeta
    (4405, 4343, Crt), -- 15 gamma
    (4343, 4287, Crt), -- 11 beta
    (4287, 4382, Crt), --  7 alpha
    (4382, 4405, Crt), -- 12 delta
    (4468, 4402, Crt), -- 21 theta
    (4402, 4382, Crt), -- 14 epsilon

    (4730, 4763, Cru), --    alpha^1
    (4853, 4656, Cru), --    beta

    (4623, 4630, Crv), --  1 alpha
    (4630, 4662, Crv), --  2 epsilon
    (4662, 4757, Crv), --  4 gamma
    (4757, 4786, Crv), --  7 delta
    (4786, 4630, Crv), --  9 beta

    (4915, 4785, Cvn), -- 12 alpha^2

    (7924, 7796, Cyg), -- 50 alpha
    (7796, 7615, Cyg), -- 37 gamma
    (7615, 7564, Cyg), -- 21 eta
    (7564, 7417, Cyg), --    chi
    (7528, 7796, Cyg), -- 18 delta
    (7796, 7949, Cyg), -- 37 gamma

    (7852, 7882, Del), --  2 epsilon
    (7882, 7906, Del), --  6 beta
    (7906, 7948, Del), --  9 alpha
    (7948, 7928, Del), -- 12 gamma^2
    (7928, 7882, Del), -- 11 delta

    (4434, 4787, Dra), --  1 lambda
    (4787, 5291, Dra), --  5 kappa
    (5291, 5744, Dra), -- 11 alpha
    (5744, 6132, Dra), -- 12 iota
    (6132, 6396, Dra), -- 14 eta
    (6396, 6927, Dra), -- 22 zeta
    (6927, 7310, Dra), -- 44 chi
    (7310, 6688, Dra), -- 57 delta
    (6688, 6705, Dra), -- 32 xi
    (6705, 6536, Dra), -- 33 gamma
    (6536, 6688, Dra), -- 23 beta

    (1465, 1922, Dor), --    alpha
    (1922, 2015, Dor), --    beta
    (1922, 1674, Dor), --    beta
    (1674, 1465, Dor), --    zeta

    (8131, 8123, Equ), --  8 alpha
    (8123, 8097, Equ), --  7 delta

    (1679, 1666, Eri), -- 69 lambda
    (1666, 1520, Eri), -- 67 beta
    (1520, 1463, Eri), -- 57 mu
    (1463, 1298, Eri), -- 48 nu
    (1298, 1231, Eri), -- 38 o^1
    (1231, 1136, Eri), -- 34 gamma
    (1136, 1084, Eri), -- 23 delta
    (1084,  874, Eri), -- 18 epsilon
    ( 874,  811, Eri), --  3 eta
    ( 818,  811, Eri), --  1 tau^1
    ( 818,  850, Eri), --  1 tau^1
    ( 850,  919, Eri), --  2 tau^2
    ( 919, 1003, Eri), -- 11 tau^3
    (1003, 1088, Eri), -- 16 tau^4
    (1088, 1173, Eri), -- 19 tau^5
    (1173, 1453, Eri), -- 27 tau^6
    (1453, 1464, Eri), -- 50 upsilon^
    (1464, 1393, Eri), -- 52 upsilon^
    (1393, 1347, Eri), -- 43
    (1347, 1195, Eri), -- 41
    (1195, 1106, Eri), --
    (1106, 1008, Eri), --
    (1008,  897, Eri), --
    ( 897,  794, Eri), --    theta^1
    ( 794,  721, Eri), --    iota
    ( 721,  674, Eri), --    kappa
    ( 674,  472, Eri), --    phi

    ( 963,  841, Frm), --    alpha

    (2891, 2990, Gem), -- 66 alpha^1
    (2990, 2777, Gem), -- 78 beta
    (2777, 2650, Gem), -- 55 delta
    (2650, 2421, Gem), -- 43 zeta
    (2421, 2343, Gem), -- 24 gamma
    (2343, 2286, Gem), -- 18 nu
    (2286, 2473, Gem), -- 13 mu
    (2473, 2891, Gem), -- 27 epsilon
    (2216, 2286, Gem), --  7 eta

    (8425, 8636, Gru), --    alpha
    (8636, 8820, Gru), --    beta
    (8820, 8787, Gru), --    iota
    (8353, 8411, Gru), --    gamma
    (8411, 8556, Gru), --    lambda
    (8556, 8636, Gru), --    delta^1
    (8636, 8675, Gru), --    beta
    (8675, 8747, Gru), --    epsilon

    ( 591,  570, Hyi), --    alpha
    ( 570,  705, Hyi), --    eta^2
    ( 705,  806, Hyi), --    delta
    ( 806, 1208, Hyi), --    epsilon
    (1208,   98, Hyi), --    gamma

    (6406, 6410, Her), -- 64 alpha^1
    (6410, 6623, Her), -- 65 delta
    (6623, 6703, Her), -- 86 mu
    (6703, 6779, Her), -- 92 xi
    (6695, 6485, Her), -- 91 theta
    (6485, 6418, Her), -- 75 phi
    (6418, 6324, Her), -- 67 pi
    (6324, 6410, Her), -- 58 epsilon
    (6588, 6418, Her), -- 85 iota
    (6418, 6220, Her), -- 67 pi
    (6220, 6212, Her), -- 44 eta
    (6212, 6324, Her), -- 40 zeta
    (6092, 6168, Her), -- 22 tau
    (6168, 6220, Her), -- 35 sigma
    (6095, 6148, Her), -- 20 gamma
    (6148, 6212, Her), -- 27 beta

    (3410, 3482, Hya), --  4 delta
    (3482, 3547, Hya), -- 11 epsilon
    (3547, 3665, Hya), -- 16 zeta
    (3665, 3748, Hya), -- 22 theta
    (3748, 3903, Hya), -- 30 alpha
    (3903, 4094, Hya), -- 39 upsilon^
    (4094, 4232, Hya), -- 42 mu
    (4232, 4314, Hya), --    nu
    (4314, 4450, Hya), --    chi^1
    (4450, 4552, Hya), --    xi
    (4552, 4839, Hya), --    beta
    (4839, 5020, Hya), --
    (5020, 5287, Hya), -- 46 gamma
    (5287, 5407, Hya), -- 49 pi
    (5407, 5516, Hya), -- 52

    (7869, 8140, Ind), --    alpha
    (8140, 8368, Ind), --    theta
    (8368, 8387, Ind), --    delta
    (7920, 8140, Ind), --    eta

    (8538, 8585, Lac), --  3 beta
    (8585, 8572, Lac), --  7 alpha
    (8572, 8498, Lac), --  5

    (3873, 3905, Leo), -- 17 epsilon
    (3905, 4031, Leo), -- 24 mu
    (4031, 4057, Leo), -- 36 zeta
    (4057, 4357, Leo), -- 41 gamma^1
    (4357, 4534, Leo), -- 68 delta
    (4534, 4359, Leo), -- 94 beta
    (4359, 3982, Leo), -- 70 theta
    (3982, 3975, Leo), -- 32 alpha
    (3975, 4057, Leo), -- 30 eta

    (2035, 1983, Lep), -- 15 delta
    (1983, 1829, Lep), -- 13 gamma
    (1829, 1654, Lep), --  9 beta
    (1654, 1702, Lep), --  2 epsilon
    (1702, 1865, Lep), --  5 mu
    (1865, 1829, Lep), -- 11 alpha
    (2085, 1998, Lep), -- 16 eta
    (1998, 1865, Lep), -- 14 zeta

    (5530, 5685, Lib), --  8 alpha^1
    (5685, 5787, Lib), -- 27 beta
    (5787, 5530, Lib), -- 38 gamma

    (4247, 4100, Lmi), -- 46
    (4100, 3974, Lmi), -- 31 beta
    (3974, 3800, Lmi), -- 21

    (5469, 5571, Lup), --    alpha
    (5571, 5705, Lup), --    beta
    (5705, 5776, Lup), --    phi^1
    (5948, 5776, Lup), --    eta
    (5776, 5708, Lup), --    gamma
    (5708, 5683, Lup), --    epsilon
    (5683, 5649, Lup), --    mu
    (5649, 5469, Lup), --    zeta

    (3705, 3690, Lyn), -- 40 alpha
    (3690, 3579, Lyn), -- 38
    (3579, 3275, Lyn), --
    (3275, 3173, Lyn), -- 31

    (7001, 7056, Lyr), --  3 alpha
    (7056, 7106, Lyr), --  6 zeta^1
    (7106, 7178, Lyr), -- 10 beta
    (7178, 7139, Lyr), -- 14 gamma
    (7139, 7056, Lyr), -- 12 delta^2

    (2970, 2714, Mon), -- 26 alpha
    (2714, 2503, Mon), -- 22 delta
    (2503, 2456, Mon), -- 17
    (2227, 2356, Mon), --  5 gamma
    (2356, 2714, Mon), -- 11 beta

    (8254, 5339, Oct), --    nu
    (5339, 8630, Oct), --    delta
    (8630, 8254, Oct), --    beta

    (6056, 6075, Oph), --  1 delta
    (6075, 6149, Oph), --  2 epsilon
    (6149, 6299, Oph), -- 10 lambda
    (6299, 6556, Oph), -- 27 kappa
    (6556, 6603, Oph), -- 55 alpha
    (6603, 6378, Oph), -- 60 beta
    (6378, 6075, Oph), -- 35 eta
    (6453, 6378, Oph), -- 42 theta

    (2061, 1948, Ori), -- 58 alpha
    (1948, 1903, Ori), -- 50 zeta
    (1903, 1852, Ori), -- 46 epsilon
    (1852, 1790, Ori), -- 34 delta
    (1790, 2061, Ori), -- 24 gamma
    (1948, 2004, Ori), -- 50 zeta
    (2004, 1713, Ori), -- 53 kappa
    (1713, 1735, Ori), -- 19 beta
    (1852, 1735, Ori), -- 34 delta

    (7790, 7913, Pav), --    alpha
    (7913, 7665, Pav), --    beta
    (7665, 7074, Pav), --    delta
    (7074, 6855, Pav), --    lambda
    (6855, 6745, Pav), --    xi
    (6745, 6582, Pav), --    pi
    (6582, 6982, Pav), --    eta
    (6982, 7590, Pav), --    zeta
    (7590, 7913, Pav), --    epsilon
    (7913, 8181, Pav), --    beta

    (8308, 8450, Peg), --  8 epsilon
    (8450, 8634, Peg), -- 26 theta
    (8634, 8665, Peg), -- 42 zeta
    (8665, 8781, Peg), -- 46 xi
    (8781,   39, Peg), -- 54 alpha
    (  39,   15, Peg), -- 88 gamma
    (8781, 8775, Peg), -- 54 alpha
    (8775, 8650, Peg), -- 53 beta
    (8650, 8684, Peg), -- 44 eta
    (8684, 8775, Peg), -- 48 mu
    (8775,   15, Peg), -- 53 beta

    ( 834,  915, Per), -- 15 eta
    ( 915, 1017, Per), -- 23 gamma
    (1017, 1122, Per), -- 33 alpha
    (1122, 1220, Per), -- 39 delta
    (1220, 1203, Per), -- 45 epsilon
    ( 936,  941, Per), -- 26 beta
    ( 941, 1017, Per), -- 27 kappa

    (  99,  322, Phe), --    alpha
    ( 322,  429, Phe), --    beta
    ( 429,   99, Phe), --    gamma

    (2550, 2042, Pic), --    alpha
    (2042, 2020, Pic), --    gamma

    (8305, 8431, Psa), --  9 iota
    (8431, 8576, Psa), -- 14 mu
    (8576, 8695, Psa), -- 17 beta
    (8695, 8720, Psa), -- 22 gamma
    (8720, 8728, Psa), -- 23 delta
    (8728, 8628, Psa), -- 24 alpha
    (8628, 8305, Psa), -- 18 epsilon

    ( 352,  383, Psc), -- 83 tau
    ( 383,  437, Psc), -- 90 upsilon
    ( 437,  510, Psc), -- 99 eta
    ( 510,  596, Psc), -- 10 o
    ( 596,  549, Psc), -- 13 alpha
    ( 549,  489, Psc), -- 11 xi
    ( 489,  361, Psc), -- 06 nu
    ( 361,  294, Psc), -- 86 zeta
    ( 294,  224, Psc), -- 71 epsilon
    ( 224, 9072, Psc), -- 63 delta
    (9072, 8969, Psc), -- 28 omega
    (8969, 8916, Psc), -- 17 iota
    (8916, 8878, Psc), -- 10 theta
    (8878, 8852, Psc), --  7
    (8852, 8911, Psc), --  6 gamma
    (8911, 8984, Psc), --  8 kappa
    (8984, 9004, Psc), -- 18 lambda
    (9004, 8969, Psc), -- 19

    (3185, 3045, Pup), -- 15 rho
    (3192, 3102, Pup), -- 16
    (3102, 3045, Pup), -- 11
    (3045, 2948, Pup), --  7 xi

    (3438, 3468, Pyx), --    beta
    (3468, 3518, Pyx), --    alpha

    (1336, 1175, Ret), --    alpha
    (1175, 1083, Ret), --    beta
    (1083, 1247, Ret), --    kappa
    (1247, 1336, Ret), --    delta

    (6527, 6580, Sco), -- 35 lambda
    (6580, 6553, Sco), --    kappa
    (6553, 6380, Sco), --    theta
    (6380, 6271, Sco), --    eta
    (6271, 6247, Sco), --    zeta^2
    (6247, 6241, Sco), --    mu^1
    (6241, 6165, Sco), -- 26 epsilon
    (6165, 6134, Sco), -- 23 tau
    (6134, 6084, Sco), -- 21 alpha
    (6084, 5944, Sco), -- 20 sigma
    (5953, 6084, Sco), --  7 delta
    (5984, 5997, Sco), --  8 beta^1
    (5997, 6084, Sco), -- 10 omega^2
    (6027, 6084, Sco), -- 14 nu
    (5928, 5944, Sco), --  5 rho
    (5944, 5953, Sco), --  6 pi
    (5953, 5984, Sco), --  7 delta
    (5984, 6027, Sco), --  8 beta^1

    (7635, 7536, Sge), -- 12 gamma
    (7536, 7488, Sge), --  7 delta
    (7479, 7536, Sge), --  5 alpha

    (6832, 6879, Sgr), --    eta
    (6879, 6859, Sgr), -- 20 epsilon
    (6859, 6913, Sgr), -- 19 delta
    (6913, 7039, Sgr), -- 22 lambda
    (7039, 7121, Sgr), -- 27 phi
    (7121, 7217, Sgr), -- 34 sigma
    (7217, 7264, Sgr), -- 39 o
    (7150, 7217, Sgr), -- 37 xi^2
    (7194, 7121, Sgr), -- 38 zeta
    (6746, 6859, Sgr), -- 10 gamma

    (5881, 5892, Srh), -- 32 mu (Ser - head)
    (5892, 5854, Srh), -- 37 epsilon
    (5854, 5789, Srh), -- 24 alpha
    (5789, 5867, Srh), -- 13 delta
    (5867, 5879, Srh), -- 28 beta
    (5879, 5933, Srh), -- 35 kappa
    (5933, 5867, Srh), -- 41 gamma

    (6561, 6698, Srt), -- 55 xi (Ser - tail)
    (6698, 6869, Srt), -- 64 nu
    (6869, 7141, Srt), -- 58 eta

    (1910, 1457, Tau), -- 23 zeta
    (1457, 1411, Tau), -- 87 alpha
    (1411, 1346, Tau), -- 77 theta^1
    (1346, 1239, Tau), -- 54 gamma
    (1791, 1409, Tau), -- 12 beta
    (1409, 1373, Tau), -- 74 epsilon
    (1373, 1346, Tau), -- 61 delta

    (6217, 5897, Tra), --    alpha
    (5897, 5771, Tra), --    beta
    (5771, 5671, Tra), --    epsilon
    (5671, 6217, Tra), --    gamma

    (5191, 5054, Uma), -- 85 eta
    (5054, 4905, Uma), -- 79 zeta
    (4905, 4660, Uma), -- 77 epsilon
    (4660, 4301, Uma), -- 69 delta
    (4301, 4295, Uma), -- 50 alpha
    (4295, 4554, Uma), -- 48 beta
    (4554, 4660, Uma), -- 64 gamma

    ( 424, 6789, Umi), --  1 alpha
    (6789, 6322, Umi), -- 23 delta
    (6322, 5903, Umi), -- 22 epsilon
    (5903, 5563, Umi), -- 16 zeta
    (5563, 5735, Umi), --  7 beta
    (5735, 6116, Umi), -- 13 gamma
    (6116, 5903, Umi), -- 21 eta

    (5056, 4963, Vir), -- 67 alpha
    (4963, 4825, Vir), -- 51 theta
    (4825, 4689, Vir), -- 29 gamma^1
    (4689, 4540, Vir), -- 15 eta
    (4932, 4910, Vir), -- 47 epsilon
    (4910, 4825, Vir), -- 43 delta
    (5511, 5107, Vir), -- 09
    (5107, 4910, Vir), -- 79 zeta

    (3615, 3347, Vol), --    alpha
    (3347, 3223, Vol), --    beta
    (3223, 2803, Vol), --    epsilon
    (3223, 2736, Vol), --    epsilon
    (3223, 3024, Vol)];--    epsilon


  The_Visible_Items : List(1 .. Item'pos(Item'last) + 1);
  Last_Visible_Item : Natural := 0;


  function Visible return List is
  begin
    return The_Visible_Items (1 .. Last_Visible_Item);
  end Visible;


  package Constellations is new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Item,
                                                                        Element_Type => Lines);
  The_Visible_Data : Constellations.Map;


  function Visible_Lines_Of (The_Item : Item) return Lines is
  begin
    return The_Visible_Data.Element (Key => The_Item);
  end Visible_Lines_Of;


  type Star_Set is array (Star.Number) of Boolean with Pack;

  The_Used_Stars : Star_Set;


  function Is_Used (Id : Star.Number) return Boolean is
  begin
    return The_Used_Stars (Id);
  end Is_Used;


  procedure Prepare (Margin : Angle.Degrees) is

    use type Angle.Value;

    Horizon_Margin : constant Angle.Value := Angle.Quadrant + Margin;

    The_Actual_Item : Item := Data(Data'first).Const;

    The_Lines : Lines(1 .. Max_Parts_Per_Item);
    Last_Line : Natural := 0;

    function Actual_Lines return Lines is
    begin
      return The_Lines(1 .. Last_Line);
    end Actual_Lines;

    procedure Insert_Visible_Actual is

      The_Visible_Lines : Lines(1 .. Max_Parts_Per_Item);
      Last_Visible_Line : Natural := 0;

      procedure Update (From_Alt : in out Angle.Value;
                        From_Az  : in out Angle.Value;
                        To_Alt   :        Angle.Value;
                        To_Az    :        Angle.Value) is

        use type Angle.Signed;
        use type Angle.Degrees;

        Delta_Az      : constant Angle.Degrees := +Angle.Signed'(To_Az - From_Az);
        Delta_Alt     : constant Angle.Degrees := +Angle.Signed'(From_Alt - To_Alt);
        New_Delta_Alt : constant Angle.Degrees := +Angle.Signed'(Angle.Quadrant - To_Alt);

        New_Delta_Az : Angle.Degrees;

      begin -- Update
        Log.Write ("  From_Alt : " & Angle.Image_Of (From_Alt));
        Log.Write ("  To_Alt   : " & Angle.Image_Of (To_Alt));
        Log.Write ("  From_Az  : " & Angle.Image_Of (From_Az));
        Log.Write ("  To_Az    : " & Angle.Image_Of (To_Az));
        New_Delta_Az := Delta_Az * New_Delta_Alt / Delta_Alt;
        From_Az := From_Az + New_Delta_Az;
        From_Alt := Horizon_Margin;
        Log.Write ("new From_Az: " & Angle.Image_Of (From_Az));
      end Update;

    begin -- Insert_Visible_Actual
      for The_Line of Actual_Lines loop
        if not Earth.Is_Below_Horizon (The_Line.From.Direction) or else
           not Earth.Is_Below_Horizon (The_Line.To.Direction)
        then
          declare
            From_Alt : Angle.Value := Angle.Quadrant - Earth.Alt_Of (The_Line.From.Direction); -- Zenit => 0
            To_Alt   : Angle.Value := Angle.Quadrant - Earth.Alt_Of (The_Line.To.Direction);
            From_Az  : Angle.Value := Earth.Az_Of (The_Line.From.Direction);
            To_Az    : Angle.Value := Earth.Az_Of (The_Line.To.Direction);
          begin
            if From_Alt < Horizon_Margin then
              The_Used_Stars (The_Line.From.Id) := True;
            else
              Log.Write ("From Update");
              Update (From_Alt => From_Alt,
                      From_Az  => From_Az,
                      To_Alt   => To_Alt,
                      To_Az    => To_Az);
            end if;
            if To_Alt < Horizon_Margin then
              The_Used_Stars (The_Line.To.Id) := True;
            else
              Log.Write ("To Update");
              Update (From_Alt => To_Alt,
                      From_Az  => To_Az,
                      To_Alt   => From_Alt,
                      To_Az    => From_Az);
            end if;
            From_Alt := Angle.Quadrant - From_Alt;
            To_Alt := Angle.Quadrant - To_Alt;
            Last_Visible_Line := @ + 1;
            The_Visible_Lines(Last_Visible_Line)
              := (From => (Direction => Earth.Direction_Of (Alt => From_Alt, Az  => From_Az),
                           Id        => The_Line.From.Id),
                  To   => (Direction => Earth.Direction_Of (Alt => To_Alt, Az  => To_Az),
                           Id        => The_Line.To.Id));
          end;
        end if;
      end loop;
      if Last_Visible_Line > 0 then
        The_Visible_Data.Insert (The_Actual_Item, The_Visible_Lines(1..Last_Visible_Line));
        Last_Visible_Item := @ + 1;
        The_Visible_Items(Last_Visible_Item) := The_Actual_Item;
      end if;
   end Insert_Visible_Actual;

  begin -- Prepare
    Log.Write ("Margin: " & Margin'image);
    Last_Visible_Item := 0;
    The_Visible_Data.Clear;
    The_Used_Stars := [others => False];
    for The_Part of Data loop
      if The_Part.Const /= The_Actual_Item then
        Insert_Visible_Actual;
        The_Actual_Item := The_Part.Const;
        Last_Line := 0;
      end if;
      Last_Line := @ + 1;
      The_Lines(Last_Line) := (From => (Id        => The_Part.From,
                                        Direction => Star.Location_Of (The_Part.From)),
                               To   => (Id        => The_Part.To,
                                        Direction => Star.Location_Of (The_Part.To)));
    end loop;
    Insert_Visible_Actual;
  end Prepare;


end Constellation;
