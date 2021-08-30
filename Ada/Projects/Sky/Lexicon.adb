-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Language;

package body Lexicon is

  type Name_Entry is array (Language.Kind) of access constant String;

  type Name_Table is array (Word) of Name_Entry;

  function E (English : String;
              French  : String;
              German  : String;
              Greek   : String;
              Italian : String;
              Spanish : String) return Name_Entry is
  begin
    return (Language.English => new String'(English),
            Language.French  => new String'(French),
            Language.German  => new String'(German),
            Language.Greek   => new String'(Greek),
            Language.Italian => new String'(Italian),
            Language.Spanish => new String'(Spanish));
  end E;

  pragma Style_Checks ("M300");

  Names : constant Name_Table := (
  --                         English             French                      German                Greek                      Italian                        Spanish
    Albireo           => E ("Albireo",           "Albireo",                  "Albireo",            "Αλμπίρεο",                "Albireo",                     "Albireo"),
    Aldebaran         => E ("Aldebaran",         "Aldébaran",                "Aldebaran",          "Αλδεβαράν",               "Aldebaran",                   "Aldebarán"),
    All_Objects       => E ("All",               "Tous",                     "Alle",               "όλα",                     "Tutti",                       "Todos"),
    Altair            => E ("Altair",            "Altaïr",                   "Altair",             "Αλτάιρ",                  "Altair",                      "Altair"),
    Andromeda_Galaxie => E ("Andromeda galaxy",  "Galaxie d'Andromède",      "Andromeda-Galaxie",  "Γαλαξίας της Ανδρομέδας", "Galassia di Andromeda",       "Galaxia de Andrómeda"),
    Arkturus          => E ("Arkturus",          "Arcturus",                 "Arktur",             "Αρκτούρος",               "Arturo",                      "Arturo"),
    Betelgeuse        => E ("Betelgeuse",        "Bételgeuse",               "Betelgeuze",         "Βετελγέζης",              "Betelgeuse",                  "Betelgeuse"),
    Camera            => E ("Camera",            "Caméra",                   "Kamera",             "κάμερα",                  "Telecamera",                  "Cámara"),
    Catalog           => E ("Catalog",           "Catalogue",                "Katalog",            "Κατάλογος",               "Catalogo",                    "Catálogo"),
    Clusters          => E ("Clusters",          "Amas globulaires",         "Sternhaufen",        "Αστρικά Σμήνη",           "Ammasso stellare",            "Cúmulos"),
    Deneb             => E ("Deneb",             "Deneb",                    "Deneb",              "Ντένεμπ",                 "Deneb",                       "Deneb"),
    East              => E ("E",                 "E",                        "O",                  "Α",                       "E",                           "E"),
    Eskimo_Nebula     => E ("Eskimo nebula",     "Nébuleuse Eskimo",         "Eskimonebel",        "Νεφέλωμα Εσκιμώος",       "Nebulosa Eskimo",             "Nebulosa del Esquimal"),
    Fans              => E ("Fans",              "Ventilateurs",             "Ventilatoren",       "Ανεμιστήρες",             "Fan",                         "Ventiladores"),
    Favorites         => E ("Favorites",         "Favoris",                  "Favoriten",          "Αγαπημένα",               "Preferiti",                   "Favoritos"),
    Galaxies          => E ("Galaxies",          "Galaxies",                 "Galaxien",           "Γαλαξίες",                "Galassie",                    "Galaxias"),
    Generate          => E ("Generate",          "Produire",                 "Generieren",         "Παράγω",                  "Creare",                      "Generar"),
    Hubbles_Nebula    => E ("Hubble's nebula",   "Nébuleuse de Hubble",      "Hubbles Nebel",      "Νεφέλωμα του Hubble",     "Nebulosa di Hubble",          "Nebulosa Hubble"),
    Jupiter           => E ("Jupiter",           "Jupiter",                  "Jupiter",            "Δίας",                    "Giove",                       "Júpiter"),
    Mars              => E ("Mars",              "Mars",                     "Mars",               "Άρης",                    "Marte",                       "Marte"),
    Mercury           => E ("Mercury",           "Mercure",                  "Merkur",             "Ερμής",                   "Mecurio",                     "Mecurio"),
    Mizar             => E ("Mizar",             "Mizar",                    "Mizar",              "Μιζάρ",                   "Mizar",                       "Mizar"),
    Moon              => E ("Moon",              "Lune",                     "Mond",               "Σελήνη",                  "Luna",                        "Luna"),
    Multiple_Stars    => E ("Multiple stars",    "Étoiles multiples",        "Mehrfachsterne",     "Πολλαπλά αστέρια",        "Stella multipla",             "Estrellas múltiples"),
    Nebulas           => E ("Nebulas",           "Nébuleuse",                "Nebel",              "Νεφέλωμα",                "Nebbia",                      "Nebulosas"),
    Neos              => E ("NEOs",              "Géocroiseurs",             "NEOs",               "Νέος",                    "Πλησίον της γής αντικείμενα", "Objetos próximos a La Tierra"),
    Neptune           => E ("Neptune",           "Neptune",                  "Neptun",             "Ποσειδώνας",              "Nettuno",                     "Neptuno"),
    North             => E ("N",                 "N",                        "N",                  "Β",                       "N",                           "N"),
    Ocular            => E ("Ocular",            "Oculaire",                 "Okular",             "οφθαλμικός",              "Oculare",                     "Ocular"),
    Off               => E ("Off",               "Off",                      "Aus",                "Off",                     "Off",                         "Off"),
    On                => E ("On",                "On",                       "Ein",                "On",                      "On",                          "On"),
    Open_Clusters     => E ("Open clusters",     "Amas ouverts",             "Offene Sternhaufen", "Ανοιχτά αστρικά σμήνη",   "Ammassi aperti",              "Cúmulos abiertos"),
    Optic             => E ("Optic",             "Optique",                  "Optik",              "Οπτικός",                 "Ottica",                      "Óptica"),
    Orion_Nebula      => E ("Orion nebula",      "Nébuleuse d'Orion",        "Orionnebel",         "Νεφέλωμα του Ορίωνα",     "Nebulosa di Orione",          "Nebulosa de Orión"),
    Park_Position     => E ("Park position",     "Position de repos",        "Parkposition",       "θέση ανάπαυσης",          "Posizione riposo",            "Posición de reposo"),
    Pluto             => E ("Pluto",             "Pluton",                   "Pluto",              "Πλούτωνας",               "Plutone",                     "Plutón"),
    Polaris           => E ("Polaris",           "Étoile Polaire",           "Polarstern",         "Πολικός Αστέρας",         "Stella Polare",               "Estrella Polar"),
    Pollux            => E ("Pollux",            "Pollux",                   "Pollux",             "Πολυδεύκης",              "Pollux",                      "Pollux"),
    Procyon           => E ("Procyon",           "Procyon",                  "Prokyon",            "Προκύων",                 "Procione",                    "Proción"),
    Quasars           => E ("Quasars",           "Quasars",                  "Quasare",            "Κβάζαρ",                  "Quasars",                     "Cuásares"),
    Regulus           => E ("Regulus",           "Regulus",                  "Regulus",            "Βασιλίσκος",              "Regolo",                      "Régulo"),
    Rigel             => E ("Rigel",             "Rigel",                    "Rigel",              "Ρίγκελ",                  "Rigel",                       "Rigel"),
    Ring_Nebula       => E ("Ring nebula",       "Nébuleuse de l'Anneau",    "Ringnebel",          "Δακτυλιοειδές νεφέλωμα",  "Nebulosa Anello",             "Nebulosa del anillo"),
    Road_Sign         => E ("Road sign",         "Panneau de signalisation", "Wegweiser",          "Οδική σήμανση",           "Segnale Stradale",            "Señal de tráfico"),
    Saturn            => E ("Saturn",            "Saturne",                  "Saturn",             "Κρόνος",                  "Saturno",                     "Saturno"),
    Saturn_Nebula     => E ("Saturn nebula",     "Nébuleuse de Saturne",     "Saturnnebel",        "Νεφέλωμα Κρόνου",         "Nebulosa Saturno",            "Nebulosa Saturno"),
    Selection         => E ("Selection",         "Sélection",                "Auswahl",            "Επιλογή",                 "Selezione",                   "Selección"),
    Sirius            => E ("Sirius",            "Sirius",                   "Sirius",             "Σείριος",                 "Sirius",                      "Sirio"),
    Solar_System      => E ("Solar system",      "Systèm solaire",           "Sonnensystem",       "Ηλιακό σύστημα",          "Sistema solare",              "Sistema solar"),
    South             => E ("S",                 "S",                        "S",                  "Ν",                       "S",                           "S"),
    Stars             => E ("Stars",             "Étoiles",                  "Sterne",             "Αστέρια",                 "Stelle",                      "Estrellas"),
    Sun               => E ("Sun",               "Soleil",                   "Sonne",              "Ἥλιος",                   "Sole",                        "Sol"),
    Target            => E ("Target",            "Cible",                    "Ziel",               "Στόχος",                  "Bersaglio",                   "Objetivo"),
    Uranus            => E ("Uranus",            "Uranus",                   "Uranus",             "Ουρανός",                 "Urano",                       "Urano"),
    Vega              => E ("Vega",              "Véga",                     "Vega",               "Βέγας",                   "Vega",                        "Vega"),
    Veil_Nebula       => E ("Veil nebula",       "Nébuleuse de Voile",       "Schleiernebel",      "Νεφέλωμα Βέλο",           "Nebulosa Velo",               "Nebulosa del Velo"),
    Venus             => E ("Venus",             "Vénus",                    "Venus",              "Αφροδίτη",                "Venere",                      "Venus"),
    West              => E ("W",                 "O",                        "W",                  "Δ",                       "O",                           "O"),
    Whirlpool_Galaxy  => E ("Whirlpool galaxy",  "Galaxy du Tourbillon",     "Strudelgalaxie",     "Γαλαξίας της Δίνης",      "Galassia Trottola",           "Galaxia del Remolino"),
    Wild_Duck_Cluster => E ("Wild Duck cluster", "Amas du Canard Sauvage",   "Wildenten-Haufen",   "Σμήνος Αγριόπαπιας",      "Ammasso Anatra Selvatica",    "Cúmulo del Pato Salvaje"));

    pragma Style_Checks ("M120");


  function Image_Of (Item : Word) return String is
  begin
    return Names(Item)(Language.Actual).all;
  end Image_Of;


  function Found (Name     : String;
                  For_Word : Word) return Boolean is
  begin
    for The_Name of Names(For_Word) loop
      if The_Name.all = Name then
        return True;
      end if;
    end loop;
    return False;
  end Found;


  function Word_Of (Name : String) return Word is
  begin
    for The_Word in Word loop
      if Found (Name, The_Word) then
        return The_Word;
      end if;
    end loop;
    raise Not_Found;
  end Word_Of;

end Lexicon;
