-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-- Generated Moon feature information, obtained from the web page https://planetarynames.wr.usgs.gov

pragma Style_White_Elephant;

pragma Restrictions (No_Elaboration_Code);

package Database.Moon is

  type Feature_Name is (
    Abbot,
    Abel,
    Abenezra,
    Abulfeda,
    Acosta,
    Adams,
    Aepinus,
    Agatharchides,
    Agrippa,
    Airy,
    Al_Bakri,
    Al_Biruni,
    Albategnius,
    Alexander,
    Alfraganus,
    Alhazen,
    Aliacensis,
    Almanon,
    Alpetragius,
    Alphonsus,
    Amundsen,
    Anaxagoras,
    Anaximander,
    Anaximenes,
    Andel,
    Andersson,
    Ansgarius,
    Anville,
    Apianus,
    Apollonius,
    Arago,
    Aratus,
    Archimedes,
    Archytas,
    Argelander,
    Ariadaeus,
    Aristarchus,
    Aristillus,
    Aristoteles,
    Arnold,
    Arrhenius,
    Aryabhata,
    Arzachel,
    Asada,
    Asclepi,
    Ashbrook,
    Aston,
    Atlas,
    Atwood,
    Autolycus,
    Auwers,
    Auzout,
    Avery,
    Avicenna,
    Azophi,
    Baade,
    Babbage,
    Babcock,
    Back,
    Baco,
    Baillaud,
    Bailly,
    Baily,
    Balboa,
    Ball,
    Balmer,
    Banachiewicz,
    Bancroft,
    Barkla,
    Barnard,
    Barocius,
    Barrow,
    Bartels,
    Bayer,
    Beals,
    Beaumont,
    Behaim,
    Bel_Kovich,
    Bela,
    Bell,
    Bellot,
    Bernoulli,
    Berosus,
    Berzelius,
    Bessel,
    Bettinus,
    Bianchini,
    Biela,
    Bilharz,
    Billy,
    Biot,
    Birmingham,
    Birt,
    Black,
    Blancanus,
    Blanchard,
    Blanchinus,
    Bliss,
    Bode,
    Boethius,
    Boguslawsky,
    Bohnenberger,
    Bohr,
    Boltzmann,
    Bonpland,
    Boole,
    Borda,
    Born,
    Boscovich,
    Boss,
    Bouguer,
    Boussingault,
    Brayley,
    Breislak,
    Brenner,
    Brianchon,
    Briggs,
    Brisbane,
    Brown,
    Brunner,
    Buch,
    Buerg,
    Buesching,
    Bullialdus,
    Bunsen,
    Burckhardt,
    Burnham,
    Byrd,
    Byrgius,
    C_Herschel,
    C_Mayer,
    Cabeus,
    Calippus,
    Cameron,
    Campanus,
    Cannizzaro,
    Cannon,
    Capella,
    Capuanus,
    Cardanus,
    Carlini,
    Carmichael,
    Carpenter,
    Carrel,
    Carrillo,
    Carrington,
    Cartan,
    Casatus,
    Cassini,
    Catalan,
    Catena_Abulfeda,
    Catena_Davy,
    Catena_Humboldt,
    Catena_Krafft,
    Catena_Littrow,
    Catena_Sylvester,
    Catena_Taruntius,
    Catena_Timocharis,
    Catharina,
    Cauchy,
    Cavalerius,
    Cavendish,
    Cayley,
    Celsius,
    Cepheus,
    Chacornac,
    Challis,
    Chamberlin,
    Chappe,
    Chevallier,
    Chladni,
    Cichus,
    Clairaut,
    Clausius,
    Clavius,
    Cleomedes,
    Cleostratus,
    Colombo,
    Condon,
    Condorcet,
    Conon,
    Cook,
    Copernicus,
    Couder,
    Cremona,
    Crozier,
    Crueger,
    Curie,
    Curtius,
    Cusanus,
    Cuvier,
    Cyrillus,
    Cysatus,
    D_Arrest,
    Da_Vinci,
    Daguerre,
    Dale,
    Dalton,
    Daly,
    Damoiseau,
    Daniell,
    Darney,
    Darwin,
    Daubree,
    Davy,
    Dawes,
    De_Gasparis,
    De_Gerlache,
    De_La_Rue,
    De_Roy,
    De_Sitter,
    De_Vico,
    Debes,
    Debus,
    Dechen,
    Delambre,
    Delaunay,
    Delisle,
    Delmotte,
    Deluc,
    Dembowski,
    Democritus,
    Demonax,
    Desargues,
    Descartes,
    Deslandres,
    Dionysius,
    Diophantus,
    Dollond,
    Donati,
    Donner,
    Doppelmayer,
    Dove,
    Drebbel,
    Dreyer,
    Drude,
    Drygalski,
    Dubyago,
    Dunthorne,
    Dziewulski,
    Eddington,
    Edison,
    Egede,
    Eichstadt,
    Eimmart,
    Einstein,
    Elger,
    Elmer,
    Encke,
    Endymion,
    Epigenes,
    Epimenides,
    Eratosthenes,
    Erlanger,
    Erro,
    Esclangon,
    Euclides,
    Euctemon,
    Eudoxus,
    Euler,
    Fabbroni,
    Fabricius,
    Faraday,
    Faustini,
    Fauth,
    Faye,
    Fermat,
    Fernelius,
    Fibiger,
    Firmicus,
    Flammarion,
    Flamsteed,
    Florey,
    Focas,
    Fontana,
    Fontenelle,
    Foucault,
    Fourier,
    Fox,
    Fra_Mauro,
    Fracastorius,
    Franck,
    Franklin,
    Franz,
    Fraunhofer,
    Fredholm,
    Froelich,
    Furnerius,
    G_Bond,
    Gaertner,
    Galilaei,
    Galle,
    Galvani,
    Gambart,
    Ganskiy,
    Gardner,
    Gassendi,
    Gaudibert,
    Gauricus,
    Gauss,
    Gay_Lussac,
    Geber,
    Geissler,
    Geminus,
    Gemma_Frisius,
    Gerard,
    Gernsback,
    Gibbs,
    Gilbert,
    Gill,
    Ginzel,
    Gioja,
    Glaisher,
    Glushko,
    Goclenius,
    Goddard,
    Godin,
    Goldschmidt,
    Goodacre,
    Gould,
    Graff,
    Greaves,
    Grignard,
    Grimaldi,
    Grove,
    Gruemberger,
    Gruithuisen,
    Guericke,
    Gum,
    Gutenberg,
    Guthnick,
    Gylden,
    Haber,
    Hagecius,
    Hahn,
    Haidinger,
    Hainzel,
    Haldane,
    Hale,
    Hall,
    Halley,
    Hamilton,
    Hanno,
    Hansen,
    Hansteen,
    Harding,
    Hargreaves,
    Harkhebi,
    Harlan,
    Harpalus,
    Hartwig,
    Hase,
    Hausen,
    Haworth,
    Hayn,
    Hecataeus,
    Hedervari,
    Hedin,
    Heinsius,
    Heis,
    Helicon,
    Hell,
    Helmert,
    Helmholtz,
    Henry,
    Henry_Freres,
    Heraclitus,
    Hercules,
    Herigonius,
    Hermann,
    Hermite,
    Herodotus,
    Herschel,
    Hesiodus,
    Hevelius,
    Heyrovsky,
    Hill,
    Hind,
    Hinshelwood,
    Hippalus,
    Hipparchus,
    Hirayama,
    Hohmann,
    Holden,
    Homeward,
    Hommel,
    Hooke,
    Horrebow,
    Horrocks,
    Hortensius,
    Houssay,
    Houtermans,
    Hubble,
    Huggins,
    Humboldt,
    Hume,
    Hypatia,
    Ibn_Bajja,
    Ibn_Battuta,
    Ibn_Rushd,
    Ibn_Yunus,
    Ideler,
    Il_In,
    Inghirami,
    Isidorus,
    J_Herschel,
    Jacobi,
    Jansen,
    Jansky,
    Janssen,
    Jeans,
    Jenkins,
    Jenner,
    Joliot,
    Julius_Caesar,
    Kaestner,
    Kaiser,
    Kane,
    Kant,
    Kao,
    Kapteyn,
    Keldysh,
    Kepler,
    Kies,
    Kiess,
    Kinau,
    Kirch,
    Kircher,
    Kirchhoff,
    Klaproth,
    Klein,
    Knox_Shaw,
    Koenig,
    Kopff,
    Krafft,
    Kramarov,
    Krasnov,
    Kreiken,
    Krieger,
    Krogh,
    Krusenstern,
    Kundt,
    Kunowsky,
    La_Caille,
    La_Condamine,
    La_Perouse,
    Lacroix,
    Lacus_Aestatis,
    Lacus_Autumni,
    Lacus_Bonitatis,
    Lacus_Doloris,
    Lacus_Excellentiae,
    Lacus_Felicitatis,
    Lacus_Gaudii,
    Lacus_Hiemalis,
    Lacus_Lenitatis,
    Lacus_Mortis,
    Lacus_Odii,
    Lacus_Perseverantiae,
    Lacus_Somniorum,
    Lacus_Spei,
    Lacus_Temporis,
    Lacus_Timoris,
    Lacus_Veris,
    Lade,
    Lagalla,
    Lagrange,
    Lalande,
    Lallemand,
    Lamarck,
    Lambert,
    Lame,
    Lamech,
    Lamont,
    Langley,
    Langrenus,
    Lansberg,
    Lassell,
    Laue,
    Lauritsen,
    Lavoisier,
    Lawrence,
    Le_Gentil,
    Le_Monnier,
    Le_Verrier,
    Leakey,
    Lebesgue,
    Lee,
    Legendre,
    Lehmann,
    Lenard,
    Lepaute,
    Letronne,
    Lexell,
    Licetus,
    Lichtenberg,
    Lick,
    Liebig,
    Lilius,
    Lindbergh,
    Lindblad,
    Lindenau,
    Lindsay,
    Liouville,
    Littrow,
    Lockyer,
    Loewy,
    Lohrmann,
    Lohse,
    Lomonosov,
    Longomontanus,
    Lorentz,
    Louville,
    Lovelace,
    Lubbock,
    Lubiniezky,
    Ludwig,
    Lyapunov,
    Lyell,
    Lyot,
    Maclaurin,
    Maclear,
    Macrobius,
    Maedler,
    Magelhaens,
    Maginus,
    Main,
    Mairan,
    Malapert,
    Mallet,
    Manilius,
    Manners,
    Manzinus,
    Maraldi,
    Marco_Polo,
    Mare_Anguis,
    Mare_Australe,
    Mare_Cognitum,
    Mare_Crisium,
    Mare_Fecunditatis,
    Mare_Frigoris,
    Mare_Humboldtianum,
    Mare_Humorum,
    Mare_Imbrium,
    Mare_Insularum,
    Mare_Marginis,
    Mare_Nectaris,
    Mare_Nubium,
    Mare_Orientale,
    Mare_Serenitatis,
    Mare_Smythii,
    Mare_Spumans,
    Mare_Tranquillitatis,
    Mare_Undarum,
    Mare_Vaporum,
    Marinus,
    Marius,
    Markov,
    Maskelyne,
    Mason,
    Maunder,
    Maupertuis,
    Maurolycus,
    Maury,
    Maxwell,
    Mcadie,
    Mcclure,
    Mclaughlin,
    Mee,
    Mees,
    Menelaus,
    Mercator,
    Mercurius,
    Merrill,
    Mersenius,
    Messala,
    Messier,
    Metius,
    Meton,
    Milichius,
    Miller,
    Mitchell,
    Moesting,
    Moigno,
    Monge,
    Mons_Ampere,
    Mons_Argaeus,
    Mons_Bradley,
    Mons_Delisle,
    Mons_Gruithuisen_Delta,
    Mons_Gruithuisen_Gamma,
    Mons_Hadley,
    Mons_Hadley_Delta,
    Mons_Hansteen,
    Mons_Huygens,
    Mons_La_Hire,
    Mons_Maraldi,
    Mons_Moro,
    Mons_Penck,
    Mons_Pico,
    Mons_Piton,
    Mons_Ruemker,
    Mons_Usov,
    Mons_Vinogradov,
    Mons_Vitruvius,
    Mons_Wolff,
    Mont_Blanc,
    Montanari,
    Montes_Agricola,
    Montes_Alpes,
    Montes_Apenninus,
    Montes_Archimedes,
    Montes_Carpatus,
    Montes_Caucasus,
    Montes_Cordillera,
    Montes_Haemus,
    Montes_Harbinger,
    Montes_Jura,
    Montes_Pyrenaeus,
    Montes_Recti,
    Montes_Riphaeus,
    Montes_Rook,
    Montes_Secchi,
    Montes_Spitzbergen,
    Montes_Taurus,
    Montes_Teneriffe,
    Moretus,
    Morley,
    Moseley,
    Mouchez,
    Moulton,
    Mueller,
    Murchison,
    Mutus,
    Nansen,
    Naonobu,
    Nasireddin,
    Nasmyth,
    Natasha,
    Neander,
    Nearch,
    Neison,
    Neper,
    Nernst,
    Neumayer,
    Newcomb,
    Newton,
    Nicholson,
    Nicolai,
    Nicollet,
    Nobile,
    Nobili,
    Noeggerath,
    Nonius,
    Nunn,
    Oceanus_Procellarum,
    Oenopides,
    Oersted,
    Oken,
    Olbers,
    Opelt,
    Oppolzer,
    Orontius,
    Palisa,
    Palitzsch,
    Pallas,
    Palmieri,
    Palus_Epidemiarum,
    Palus_Putredinis,
    Palus_Somni,
    Paneth,
    Parrot,
    Parry,
    Pascal,
    Patricia,
    Peary,
    Peek,
    Peirce,
    Peirescius,
    Pentland,
    Petavius,
    Petermann,
    Peters,
    Petrov,
    Pettit,
    Phillips,
    Philolaus,
    Phocylides,
    Piazzi,
    Piazzi_Smyth,
    Picard,
    Piccolomini,
    Pickering,
    Pictet,
    Pilatre,
    Pingre,
    Pitatus,
    Pitiscus,
    Plana,
    Plato,
    Playfair,
    Plinius,
    Plutarch,
    Poczobutt,
    Poisson,
    Polybius,
    Pomortsev,
    Poncelet,
    Pons,
    Pontanus,
    Pontecoulant,
    Popov,
    Porter,
    Posidonius,
    Prinz,
    Proclus,
    Proctor,
    Promontorium_Agarum,
    Promontorium_Agassiz,
    Promontorium_Archerusia,
    Promontorium_Deville,
    Promontorium_Fresnel,
    Promontorium_Heraclides,
    Promontorium_Kelvin,
    Promontorium_Laplace,
    Promontorium_Taenarium,
    Protagoras,
    Ptolemaeus,
    Puiseux,
    Purbach,
    Purkyne,
    Pythagoras,
    Pytheas,
    Rabbi_Levi,
    Raman,
    Ramsden,
    Rankine,
    Rayleigh,
    Reaumur,
    Regiomontanus,
    Regnault,
    Reichenbach,
    Reimarus,
    Reiner,
    Reiner_Gamma,
    Reinhold,
    Repsold,
    Respighi,
    Rhaeticus,
    Rheita,
    Riccioli,
    Riccius,
    Richardson,
    Riemann,
    Rima_Agatharchides,
    Rima_Agricola,
    Rima_Archytas,
    Rima_Ariadaeus,
    Rima_Artsimovich,
    Rima_Billy,
    Rima_Birt,
    Rima_Bradley,
    Rima_Brayley,
    Rima_Calippus,
    Rima_Cardanus,
    Rima_Carmen,
    Rima_Cauchy,
    Rima_Cleomedes,
    Rima_Cleopatra,
    Rima_Conon,
    Rima_Dawes,
    Rima_Delisle,
    Rima_Diophantus,
    Rima_Draper,
    Rima_Euler,
    Rima_Flammarion,
    Rima_Furnerius,
    Rima_G_Bond,
    Rima_Gaertner,
    Rima_Galilaei,
    Rima_Gay_Lussac,
    Rima_Hadley,
    Rima_Hansteen,
    Rima_Hesiodus,
    Rima_Hyginus,
    Rima_Jansen,
    Rima_Krieger,
    Rima_Mairan,
    Rima_Marius,
    Rima_Messier,
    Rima_Milichius,
    Rima_Oppolzer,
    Rima_Reaumur,
    Rima_Schroeter,
    Rima_Sharp,
    Rima_Sheepshanks,
    Rima_Suess,
    Rima_T_Mayer,
    Rima_Vladimir,
    Rima_Wan_Yu,
    Rima_Yangel,
    Rima_Zahia,
    Rimae_Alphonsus,
    Rimae_Apollonius,
    Rimae_Archimedes,
    Rimae_Aristarchus,
    Rimae_Arzachel,
    Rimae_Atlas,
    Rimae_Bode,
    Rimae_Boscovich,
    Rimae_Buerg,
    Rimae_Chacornac,
    Rimae_Daniell,
    Rimae_Darwin,
    Rimae_De_Gasparis,
    Rimae_Focas,
    Rimae_Fresnel,
    Rimae_Gassendi,
    Rimae_Gerard,
    Rimae_Goclenius,
    Rimae_Grimaldi,
    Rimae_Gutenberg,
    Rimae_Hase,
    Rimae_Herigonius,
    Rimae_Hevelius,
    Rimae_Hippalus,
    Rimae_Hypatia,
    Rimae_Janssen,
    Rimae_Kopff,
    Rimae_Littrow,
    Rimae_Maclear,
    Rimae_Maestlin,
    Rimae_Maupertuis,
    Rimae_Menelaus,
    Rimae_Mersenius,
    Rimae_Opelt,
    Rimae_Palmieri,
    Rimae_Parry,
    Rimae_Petavius,
    Rimae_Pettit,
    Rimae_Pitatus,
    Rimae_Plato,
    Rimae_Plinius,
    Rimae_Posidonius,
    Rimae_Prinz,
    Rimae_Ramsden,
    Rimae_Repsold,
    Rimae_Riccioli,
    Rimae_Ritter,
    Rimae_Roemer,
    Rimae_Secchi,
    Rimae_Sirsalis,
    Rimae_Sosigenes,
    Rimae_Sulpicius_Gallus,
    Rimae_Taruntius,
    Rimae_Theaetetus,
    Rimae_Triesnecker,
    Rimae_Vasco_Da_Gama,
    Rimae_Zupus,
    Ritchey,
    Ritter,
    Ritz,
    Robinson,
    Rocca,
    Roemer,
    Roentgen,
    Rosenberger,
    Ross,
    Rosse,
    Rost,
    Rothmann,
    Runge,
    Rupes_Altai,
    Rupes_Cauchy,
    Rupes_Kelvin,
    Rupes_Liebig,
    Rupes_Mercator,
    Rupes_Recta,
    Rupes_Toscanelli,
    Russell,
    Rutherfurd,
    Rydberg,
    Sabine,
    Sacrobosco,
    Santbech,
    Sasserides,
    Saunder,
    Saussure,
    Scheiner,
    Schiaparelli,
    Schickard,
    Schiller,
    Schlueter,
    Schmidt,
    Schoenfeld,
    Schomberger,
    Schorr,
    Schroeter,
    Schubert,
    Schumacher,
    Schwabe,
    Scoresby,
    Scott,
    Secchi,
    Segner,
    Seleucus,
    Seneca,
    Shaler,
    Shapley,
    Sharp,
    Sheepshanks,
    Shoemaker,
    Short,
    Shuckburgh,
    Shuleykin,
    Silberschlag,
    Simpelius,
    Sinas,
    Sinus_Aestuum,
    Sinus_Amoris,
    Sinus_Asperitatis,
    Sinus_Concordiae,
    Sinus_Fidei,
    Sinus_Honoris,
    Sinus_Iridum,
    Sinus_Lunicus,
    Sinus_Medii,
    Sinus_Roris,
    Sinus_Successus,
    Sirsalis,
    Sklodowska,
    Slocum,
    Smoluchowski,
    Snellius,
    Soemmering,
    Somerville,
    Sosigenes,
    South_Crater,
    Spallanzani,
    Spoerer,
    Spurr,
    Stadius,
    Steinheil,
    Stevinus,
    Stewart,
    Stiborius,
    Stoefler,
    Stokes,
    Strabo,
    Street,
    Struve,
    Sulpicius_Gallus,
    Sundman,
    Svedberg,
    Swasey,
    Swift,
    Sylvester,
    T_Mayer,
    Tacchini,
    Tacitus,
    Talbot,
    Tannerus,
    Taruntius,
    Taylor,
    Tebbutt,
    Tempel,
    Thales,
    Theaetetus,
    Thebit,
    Theon_Junior,
    Theon_Senior,
    Theophilus,
    Timaeus,
    Timocharis,
    Tisserand,
    Tolansky,
    Torricelli,
    Townley,
    Tralles,
    Triesnecker,
    Turner,
    Tycho,
    Ukert,
    Ulugh_Beigh,
    Urey,
    Vallis_Alpes,
    Vallis_Baade,
    Vallis_Bohr,
    Vallis_Bouvard,
    Vallis_Capella,
    Vallis_Inghirami,
    Vallis_Palitzsch,
    Vallis_Rheita,
    Vallis_Schroeteri,
    Vallis_Snellius,
    Van_Albada,
    Van_Vleck,
    Vasco_Da_Gama,
    Vashakidze,
    Vega,
    Vendelinus,
    Vestine,
    Vieta,
    Virchow,
    Vitello,
    Vitruvius,
    Vlacq,
    Vogel,
    Volta,
    Von_Baeyer,
    Von_Behring,
    Von_Braun,
    Voskresenskiy,
    W_Bond,
    Wallace,
    Walther,
    Wapowski,
    Wargentin,
    Warner,
    Watt,
    Watts,
    Webb,
    Weierstrass,
    Weigel,
    Weinek,
    Weiss,
    Werner,
    Wexler,
    Whewell,
    Widmannstaetten,
    Wildt,
    Wilhelm,
    Wilkins,
    Williams,
    Wilson,
    Winthrop,
    Woehler,
    Wolf,
    Wright,
    Wrottesley,
    Wurzelbauer,
    Wyld,
    Xenophanes,
    Yakovkin,
    Yerkes,
    Young,
    Zach,
    Zaehringer,
    Zagut,
    Zasyadko,
    Zeno,
    Zoellner,
    Zucchius,
    Zupus);

  type Feature is record
    Kind      : Moon_Feature_Type;
    Latitude  : Feature_Latitude;
    Longitude : Feature_Longitude;
    Size      : Feature_Size;
  end record;

  type Features is array (Feature_Name) of Feature;

  type Data is array (Feature_Name) of Feature;

  List : constant Data := [
  --   Kind           Latitude    Longitude      Size
    (Crater,         5.5561620,  54.7430590,   10.404),  -- Abbot
    (Crater,       -34.6257000,  85.7762190,  137.349),  -- Abel
    (Crater,       -20.9935640,  11.8906570,   43.189),  -- Abenezra
    (Crater,       -13.8650870,  13.9102980,   62.235),  -- Abulfeda
    (Crater,        -5.6453050,  60.1394730,   13.062),  -- Acosta
    (Crater,       -31.8916340,  68.3869020,   63.270),  -- Adams
    (Crater,        87.9581990, 250.3130530,   16.745),  -- Aepinus
    (Crater,       -19.8537010, 328.8853810,   51.983),  -- Agatharchides
    (Crater,         4.0963990,  10.4741350,   43.747),  -- Agrippa
    (Crater,       -18.1416280,   5.6148580,   38.900),  -- Airy
    (Crater,        14.3353920,  20.2451110,   12.212),  -- Al-Bakri
    (Crater,        18.0712940,  92.6161760,   80.410),  -- Al-Biruni
    (Crater,       -11.2400260,   4.0091970,  130.840),  -- Albategnius
    (Crater,        40.2515210,  13.6931540,   94.803),  -- Alexander
    (Crater,        -5.4216610,  18.9706170,   20.521),  -- Alfraganus
    (Crater,        15.9147830,  71.8278980,   34.648),  -- Alhazen
    (Crater,       -30.5951930,   5.1307670,   79.647),  -- Aliacensis
    (Crater,       -16.8450280,  15.1350760,   47.758),  -- Almanon
    (Crater,       -16.0479720, 355.4896010,   40.023),  -- Alpetragius
    (Crater,       -13.3879370, 357.1537280,  110.540),  -- Alphonsus
    (Crater,       -84.4397950,  83.0691500,  103.395),  -- Amundsen
    (Crater,        73.4809990, 349.8287510,   51.896),  -- Anaxagoras
    (Crater,        66.9670760, 308.5583170,   68.711),  -- Anaximander
    (Crater,        72.4850220, 315.0174980,   81.117),  -- Anaximenes
    (Crater,       -10.4141280,  12.3834540,   32.931),  -- Andel
    (Crater,       -49.9497540, 264.5372440,   13.420),  -- Andersson
    (Crater,       -12.9235070,  79.7231040,   91.419),  -- Ansgarius
    (Crater,         1.8428020,  49.5079960,   10.257),  -- Anville
    (Crater,       -26.9625260,   7.8682330,   63.440),  -- Apianus
    (Crater,         4.5053070,  60.9648050,   50.661),  -- Apollonius
    (Crater,         6.1487060,  21.4261040,   25.509),  -- Arago
    (Crater,        23.5793130,   4.5051920,   10.233),  -- Aratus
    (Crater,        29.7172310, 356.0069060,   81.042),  -- Archimedes
    (Crater,        58.8662010,   4.9924820,   31.950),  -- Archytas
    (Crater,       -16.5492900,   5.8011040,   33.716),  -- Argelander
    (Crater,         4.5549400,  17.2762780,   10.401),  -- Ariadaeus
    (Crater,        23.7298820, 312.5098750,   39.995),  -- Aristarchus
    (Crater,        33.8808310,   1.2075470,   54.371),  -- Aristillus
    (Crater,        50.2430500,  17.3200320,   87.567),  -- Aristoteles
    (Crater,        66.9846240,  35.8286290,   93.133),  -- Arnold
    (Crater,       -55.5836940, 268.5463400,   40.930),  -- Arrhenius
    (Crater,         6.2028000,  35.1732610,   21.890),  -- Aryabhata
    (Crater,       -18.2643220, 358.0696360,   96.988),  -- Arzachel
    (Crater,         7.2466460,  49.9017610,   12.373),  -- Asada
    (Crater,       -55.1899190,  25.5231240,   40.563),  -- Asclepi
    (Crater,       -81.1042780, 249.4249970,  157.680),  -- Ashbrook
    (Crater,        32.7658420, 272.3151790,   44.479),  -- Aston
    (Crater,        46.7402740,  44.3816030,   88.117),  -- Atlas
    (Crater,        -5.8770230,  57.7750180,   28.644),  -- Atwood
    (Crater,        30.6756790,   1.4858340,   38.879),  -- Autolycus
    (Crater,        14.9967570,  17.1184060,   19.642),  -- Auwers
    (Crater,        10.2078410,  64.0091760,   32.921),  -- Auzout
    (Crater,        -1.3171560,  81.3666780,   10.730),  -- Avery
    (Crater,        39.6254340, 262.7231850,   72.988),  -- Avicenna
    (Crater,       -22.1930270,  12.6957790,   47.541),  -- Azophi
    (Crater,       -44.7495820, 277.9654310,   57.851),  -- Baade
    (Crater,        59.5623300, 302.6169380,  146.558),  -- Babbage
    (Crater,         4.1343360,  94.1356610,   95.277),  -- Babcock
    (Crater,         1.1964060,  80.6680490,   34.633),  -- Back
    (Crater,       -51.0441210,  19.0964450,   65.308),  -- Baco
    (Crater,        74.6078180,  37.3517430,   89.439),  -- Baillaud
    (Crater,       -66.8214680, 291.1046380,  300.565),  -- Bailly
    (Crater,        49.7798000,  30.5582290,   25.676),  -- Baily
    (Crater,        19.2366800, 276.6920370,   69.195),  -- Balboa
    (Crater,       -35.9227880, 351.6057940,   40.311),  -- Ball
    (Crater,       -20.2714540,  70.2247060,  136.296),  -- Balmer
    (Crater,         5.2769730,  80.0096750,   99.094),  -- Banachiewicz
    (Crater,        28.0659090, 353.5660520,   12.498),  -- Bancroft
    (Crater,       -10.6679770,  67.2187390,   40.898),  -- Barkla
    (Crater,       -29.7882940,  85.9488810,  115.735),  -- Barnard
    (Crater,       -44.9790550,  16.8058220,   82.724),  -- Barocius
    (Crater,        71.2844040,   7.5860170,   93.821),  -- Barrow
    (Crater,        24.5064980, 270.1547890,   54.951),  -- Bartels
    (Crater,       -51.6207020, 324.8552810,   48.514),  -- Bayer
    (Crater,        37.1089900,  86.5785300,   52.609),  -- Beals
    (Crater,       -18.0806980,  28.8228540,   50.686),  -- Beaumont
    (Crater,       -16.6056270,  79.4050680,   56.211),  -- Behaim
    (Crater,        61.5277680,  90.1470230,  215.080),  -- Bel'kovich
    (Crater,        24.6650420,   2.2692870,   10.074),  -- Béla
    (Crater,        21.9781520, 263.4681780,   86.334),  -- Bell
    (Crater,       -12.4759820,  48.1945000,   17.496),  -- Bellot
    (Crater,        34.9295120,  60.6124010,   47.300),  -- Bernoulli
    (Crater,        33.4955590,  69.9921610,   75.237),  -- Berosus
    (Crater,        36.5518210,  50.9547770,   48.530),  -- Berzelius
    (Crater,        21.7342310,  17.9203540,   15.563),  -- Bessel
    (Crater,       -63.3968850, 314.8397690,   71.784),  -- Bettinus
    (Crater,        48.7814090, 325.6260550,   37.589),  -- Bianchini
    (Crater,       -54.9914740,  51.6285460,   77.029),  -- Biela
    (Crater,        -5.8278160,  56.3438020,   44.554),  -- Bilharz
    (Crater,       -13.8323990, 309.7618390,   45.571),  -- Billy
    (Crater,       -22.7002640,  51.0798320,   13.009),  -- Biot
    (Crater,        65.1237700, 349.3021300,   89.917),  -- Birmingham
    (Crater,       -22.3635830, 351.4077440,   15.807),  -- Birt
    (Crater,        -9.1959290,  80.3948730,   19.462),  -- Black
    (Crater,       -63.7697390, 338.3700750,  105.822),  -- Blancanus
    (Crater,       -58.2479790, 266.5041620,   37.459),  -- Blanchard
    (Crater,       -25.3156150,   2.4438580,   59.901),  -- Blanchinus
    (Crater,        53.0433450, 346.2206700,   22.850),  -- Bliss
    (Crater,         6.7100730, 357.5479130,   17.796),  -- Bode
    (Crater,         5.5689540,  72.3339960,   11.166),  -- Boethius
    (Crater,       -72.9012130,  43.2572240,   94.586),  -- Boguslawsky
    (Crater,       -16.2377170,  40.0555890,   31.739),  -- Bohnenberger
    (Crater,        12.7063100, 273.4759180,   70.071),  -- Bohr
    (Crater,       -74.8228570, 269.5931180,   72.314),  -- Boltzmann
    (Crater,        -8.3755490, 342.6658680,   59.245),  -- Bonpland
    (Crater,        63.7884670, 272.7146310,   61.337),  -- Boole
    (Crater,       -25.1951860,  46.5225780,   45.402),  -- Borda
    (Crater,        -6.0526080,  66.8273530,   15.067),  -- Born
    (Crater,         9.7109290,  11.0111040,   41.529),  -- Boscovich
    (Crater,        45.7479680,  88.6799250,   50.200),  -- Boss
    (Crater,        52.3224360, 324.1772150,   22.235),  -- Bouguer
    (Crater,       -70.2063340,  53.7341710,  127.609),  -- Boussingault
    (Crater,        20.8962810, 323.0627170,   14.167),  -- Brayley
    (Crater,       -48.3075290,  18.3096310,   48.639),  -- Breislak
    (Crater,       -39.0935800,  39.1078280,   90.011),  -- Brenner
    (Crater,        74.7511540, 271.6358370,  137.264),  -- Brianchon
    (Crater,        26.4527970, 290.8127340,   36.753),  -- Briggs
    (Crater,       -49.1997240,  68.7569420,   44.321),  -- Brisbane
    (Crater,       -46.5278950, 342.0118920,   34.032),  -- Brown
    (Crater,        -9.8573990,  90.9126410,   50.656),  -- Brunner
    (Crater,       -38.8990490,  17.6786180,   51.313),  -- Buch
    (Crater,        45.0733910,  28.2081340,   41.042),  -- Bürg
    (Crater,       -38.0424630,  19.9551110,   53.495),  -- Büsching
    (Crater,       -20.7477100, 337.7367870,   60.719),  -- Bullialdus
    (Crater,        41.4035840, 274.5403510,   55.224),  -- Bunsen
    (Crater,        31.1072340,  56.3876230,   54.365),  -- Burckhardt
    (Crater,       -13.9207620,   7.2512980,   24.092),  -- Burnham
    (Crater,        85.4283320,  10.0699240,   97.489),  -- Byrd
    (Crater,       -24.7310540, 294.6201250,   84.462),  -- Byrgius
    (Crater,        34.4815270, 328.7050500,   13.701),  -- C. Herschel
    (Crater,        63.2562310,  17.3072430,   37.540),  -- C. Mayer
    (Crater,       -85.3303040, 317.8673120,  100.582),  -- Cabeus
    (Crater,        38.9248910,  10.7175040,   34.033),  -- Calippus
    (Crater,         6.1862880,  45.9336790,   10.907),  -- Cameron
    (Crater,       -28.0425580, 332.1020890,   46.405),  -- Campanus
    (Crater,        55.5048600, 260.2705210,   54.514),  -- Cannizzaro
    (Crater,        19.8752400,  81.3641310,   57.576),  -- Cannon
    (Crater,        -7.6476050,  34.9164470,   48.130),  -- Capella
    (Crater,       -34.0926390, 333.2683760,   59.686),  -- Capuanus
    (Crater,        13.2715760, 287.5016660,   49.574),  -- Cardanus
    (Crater,        33.7499220, 335.8805580,   10.658),  -- Carlini
    (Crater,        19.5341870,  40.3618560,   19.731),  -- Carmichael
    (Crater,        69.5155500, 308.7663560,   59.061),  -- Carpenter
    (Crater,        10.6654780,  26.6795740,   15.588),  -- Carrel
    (Crater,        -2.1111070,  80.9606890,   17.852),  -- Carrillo
    (Crater,        43.9673620,  62.0366320,   27.773),  -- Carrington
    (Crater,         4.2416250,  59.2866520,   15.623),  -- Cartan
    (Crater,       -72.6963600, 329.2523900,  102.850),  -- Casatus
    (Crater,        40.2502620,   4.6436570,   56.877),  -- Cassini
    (Crater,       -45.6954510, 272.6284130,   26.774),  -- Catalán
    (Catena,       -16.5911580,  16.7001320,  209.972),  -- Catena Abulfeda
    (Catena,       -10.9847390, 353.7344240,   52.336),  -- Catena Davy
    (Catena,       -21.9809360,  84.6969890,  162.288),  -- Catena Humboldt
    (Catena,        14.9135540, 287.7497420,   55.105),  -- Catena Krafft
    (Catena,        22.2308060,  29.6113900,   10.300),  -- Catena Littrow
    (Catena,        79.9897850, 276.8822370,  139.000),  -- Catena Sylvester
    (Catena,         3.0430590,  48.7064220,   69.239),  -- Catena Taruntius
    (Catena,        29.0949240, 346.7940170,   48.369),  -- Catena Timocharis
    (Crater,       -17.9801520,  23.5521100,   98.772),  -- Catharina
    (Crater,         9.5561490,  38.6261250,   11.804),  -- Cauchy
    (Crater,         5.0972950, 293.0719550,   59.350),  -- Cavalerius
    (Crater,       -24.6250980, 306.2167130,   52.637),  -- Cavendish
    (Crater,         3.9378980,  15.0896030,   14.204),  -- Cayley
    (Crater,       -34.0979980,  20.0455530,   38.958),  -- Celsius
    (Crater,        40.6809510,  45.7806130,   39.434),  -- Cepheus
    (Crater,        29.8839860,  31.6669000,   50.444),  -- Chacornac
    (Crater,        79.5798770,   9.0855630,   53.210),  -- Challis
    (Crater,       -58.8338870,  96.0379380,   60.414),  -- Chamberlin
    (Crater,       -61.2912530, 268.7596770,   55.793),  -- Chappe
    (Crater,        45.0092530,  51.5726100,   51.832),  -- Chevallier
    (Crater,         3.9869200,   1.1171960,   13.072),  -- Chladni
    (Crater,       -33.2877160, 338.8174810,   39.185),  -- Cichus
    (Crater,       -47.8361020,  13.8617580,   76.895),  -- Clairaut
    (Crater,       -36.9030390, 316.0653050,   24.198),  -- Clausius
    (Crater,       -58.6228310, 345.2725100,  230.770),  -- Clavius
    (Crater,        27.6004840,  55.5005440,  130.771),  -- Cleomedes
    (Crater,        60.3195800, 282.6029630,   63.227),  -- Cleostratus
    (Crater,       -15.2550420,  46.0217800,   79.019),  -- Colombo
    (Crater,         1.8730780,  60.3601340,   34.855),  -- Condon
    (Crater,        12.0996080,  69.5831660,   74.853),  -- Condorcet
    (Crater,        21.6570580,   1.9503970,   20.965),  -- Conon
    (Crater,       -17.4974940,  48.8071570,   45.162),  -- Cook
    (Crater,         9.6209450, 339.9213790,   96.070),  -- Copernicus
    (Crater,        -4.9013410, 267.4175200,   18.558),  -- Couder
    (Crater,        67.2352170, 269.1428820,   85.125),  -- Cremona
    (Crater,       -13.5594810,  50.7176890,   22.514),  -- Crozier
    (Crater,       -16.6768600, 293.0397560,   45.945),  -- Crüger
    (Crater,       -23.0455420,  92.2792630,  138.869),  -- Curie
    (Crater,       -67.0793710,   4.4049400,   99.292),  -- Curtius
    (Crater,        71.8208060,  69.4003580,   60.870),  -- Cusanus
    (Crater,       -50.2926840,   9.6920750,   77.299),  -- Cuvier
    (Crater,       -13.2913290,  24.0654690,   98.086),  -- Cyrillus
    (Crater,       -66.2082050, 353.6617790,   47.773),  -- Cysatus
    (Crater,         2.2648090,  14.6041280,   29.668),  -- D'Arrest
    (Crater,         9.1048020,  44.9548500,   37.465),  -- da Vinci
    (Crater,       -11.9067170,  33.6134730,   45.787),  -- Daguerre
    (Crater,        -9.5580870,  82.9298880,   23.406),  -- Dale
    (Crater,        17.0654870, 275.5481950,   60.685),  -- Dalton
    (Crater,         5.7442080,  59.5003630,   14.963),  -- Daly
    (Crater,        -4.8513910, 298.7499780,   36.663),  -- Damoiseau
    (Crater,        35.4201100,  31.1644790,   28.204),  -- Daniell
    (Crater,       -14.6070870, 336.4317650,   14.812),  -- Darney
    (Crater,       -19.9278830, 290.7948720,  122.182),  -- Darwin
    (Crater,        15.7297910,  14.7463620,   14.667),  -- Daubrée
    (Crater,       -11.8470180, 351.8508810,   33.939),  -- Davy
    (Crater,        17.2081690,  26.3430770,   17.597),  -- Dawes
    (Crater,       -25.8330810, 309.1736090,   30.896),  -- de Gasparis
    (Crater,       -88.4849210, 271.6553140,   32.705),  -- de Gerlache
    (Crater,        59.0179050,  52.8408720,  135.221),  -- De La Rue
    (Crater,       -55.2350860, 261.0136630,   43.506),  -- De Roy
    (Crater,        79.8122910,  38.5700600,   63.786),  -- De Sitter
    (Crater,       -19.7114120, 299.6811410,   22.134),  -- De Vico
    (Crater,        29.4739120,  51.6239690,   31.924),  -- Debes
    (Crater,       -10.6828840,  99.6833450,   19.766),  -- Debus
    (Crater,        46.1204450, 291.8187340,   12.042),  -- Dechen
    (Crater,        -1.9391110,  17.3940400,   51.486),  -- Delambre
    (Crater,       -22.2577180,   2.6187160,   44.626),  -- Delaunay
    (Crater,        29.9831490, 325.3227980,   24.835),  -- Delisle
    (Crater,        27.1614570,  60.2047590,   32.156),  -- Delmotte
    (Crater,       -55.0164990, 357.0246320,   45.689),  -- Deluc
    (Crater,         2.8779190,   7.2743010,   26.108),  -- Dembowski
    (Crater,        62.3076280,  34.9905260,   37.782),  -- Democritus
    (Crater,       -78.0863660,  59.3552400,  121.931),  -- Demonax
    (Crater,        70.2481660, 286.5763580,   84.847),  -- Desargues
    (Crater,       -11.7428150,  15.6666800,   47.729),  -- Descartes
    (Crater,       -32.5540450, 354.4284450,  227.019),  -- Deslandres
    (Crater,         2.7732830,  17.2930230,   17.247),  -- Dionysius
    (Crater,        27.6193600, 325.7021290,   17.572),  -- Diophantus
    (Crater,       -10.4832600,  14.4129790,   11.041),  -- Dollond
    (Crater,       -20.6941110,   5.1006850,   35.843),  -- Donati
    (Crater,       -31.3545070,  97.9945250,   55.053),  -- Donner
    (Crater,       -28.4779170, 318.4929940,   65.078),  -- Doppelmayer
    (Crater,       -46.8321520,  31.4241960,   30.358),  -- Dove
    (Crater,       -40.9343180, 310.8780700,   30.233),  -- Drebbel
    (Crater,        10.2372350,  97.0948020,   63.843),  -- Dreyer
    (Crater,       -38.5644260, 268.1112040,   27.135),  -- Drude
    (Crater,       -79.5737830, 272.8230170,  162.489),  -- Drygalski
    (Crater,         4.3831410,  69.9490360,   48.121),  -- Dubyago
    (Crater,       -30.1188430, 328.2941640,   15.117),  -- Dunthorne
    (Crater,        20.9850180,  99.0150880,   68.905),  -- Dziewulski
    (Crater,        21.5025410, 287.9825270,  120.130),  -- Eddington
    (Crater,        24.8804940,  99.2680470,   62.722),  -- Edison
    (Crater,        48.7172960,  10.6359300,   34.182),  -- Egede
    (Crater,       -22.6281020, 281.5818090,   49.572),  -- Eichstadt
    (Crater,        23.9676450,  64.8039190,   44.990),  -- Eimmart
    (Crater,        16.6030430, 271.3455430,  181.473),  -- Einstein
    (Crater,       -35.4004450, 330.1890140,   21.515),  -- Elger
    (Crater,       -10.2318190,  84.1776710,   16.861),  -- Elmer
    (Crater,         4.5707030, 323.3170410,   28.266),  -- Encke
    (Crater,        53.6066800,  56.4832470,  122.101),  -- Endymion
    (Crater,        67.4973550, 355.3778820,   54.512),  -- Epigenes
    (Crater,       -40.9172340, 329.6688300,   22.561),  -- Epimenides
    (Crater,        14.4737160, 348.6838070,   58.768),  -- Eratosthenes
    (Crater,        86.9883130,  28.6166120,   10.937),  -- Erlanger
    (Crater,         5.6829270,  98.5399530,   63.751),  -- Erro
    (Crater,        21.4684510,  42.0618290,   15.292),  -- Esclangon
    (Crater,        -7.4008580, 330.4361330,   11.799),  -- Euclides
    (Crater,        76.2590320,  30.5690620,   62.697),  -- Euctemon
    (Crater,        44.2655670,  16.2256510,   70.157),  -- Eudoxus
    (Crater,        23.2626920, 330.8189700,   26.025),  -- Euler
    (Crater,        18.6578700,  29.2714610,   10.550),  -- Fabbroni
    (Crater,       -42.7509280,  41.8420820,   78.897),  -- Fabricius
    (Crater,       -42.4460710,   8.7474500,   69.027),  -- Faraday
    (Crater,       -87.1832460,  84.3099030,   42.485),  -- Faustini
    (Crater,         6.2314990, 339.8635700,   11.935),  -- Fauth
    (Crater,       -21.3938300,   3.8115960,   38.021),  -- Faye
    (Crater,       -22.7112870,  19.7923320,   37.774),  -- Fermat
    (Crater,       -38.1782700,   4.8587830,   68.417),  -- Fernelius
    (Crater,        86.1396280,  37.1330490,   21.097),  -- Fibiger
    (Crater,         7.2455050,  63.4259150,   56.807),  -- Firmicus
    (Crater,        -3.3285650, 356.2674650,   76.178),  -- Flammarion
    (Crater,        -4.4906050, 315.6601610,   19.343),  -- Flamsteed
    (Crater,        87.0371850, 340.2534060,   69.058),  -- Florey
    (Crater,       -33.7045670, 266.0889280,   22.042),  -- Focas
    (Crater,       -16.0393790, 303.2094610,   31.471),  -- Fontana
    (Crater,        63.4192280, 341.0439300,   37.682),  -- Fontenelle
    (Crater,        50.4589180, 320.1381910,   25.080),  -- Foucault
    (Crater,       -30.3131110, 306.8953620,   51.569),  -- Fourier
    (Crater,         0.4715180,  98.1443780,   23.968),  -- Fox
    (Crater,        -6.0610240, 343.0261980,   96.759),  -- Fra Mauro
    (Crater,       -21.3587480,  33.0703070,  120.575),  -- Fracastorius
    (Crater,        22.5870380,  35.5593670,   11.909),  -- Franck
    (Crater,        38.7287950,  47.6396830,   55.920),  -- Franklin
    (Crater,        16.5686500,  40.2445310,   25.483),  -- Franz
    (Crater,       -39.5154750,  59.0634460,   57.754),  -- Fraunhofer
    (Crater,        18.3660630,  46.5526320,   13.391),  -- Fredholm
    (Crater,        80.0014310, 248.3684070,   56.733),  -- Froelich
    (Crater,       -36.0037640,  60.5382800,  135.029),  -- Furnerius
    (Crater,        32.3873330,  36.3221180,   19.048),  -- G. Bond
    (Crater,        59.2432870,  34.7578650,  101.789),  -- Gärtner
    (Crater,        10.4804280, 297.1675440,   15.986),  -- Galilaei
    (Crater,        55.8732680,  22.3285850,   20.959),  -- Galle
    (Crater,        49.5121140, 275.4381690,   76.829),  -- Galvani
    (Crater,         0.9236050, 344.7610660,   24.675),  -- Gambart
    (Crater,        -9.6405770,  97.0029130,   42.106),  -- Ganskiy (Hansky)
    (Crater,        17.7474230,  33.8132690,   17.617),  -- Gardner
    (Crater,       -17.5545760, 320.0362730,  111.391),  -- Gassendi
    (Crater,       -10.9293590,  37.8231610,   33.138),  -- Gaudibert
    (Crater,       -33.9065610, 347.2579990,   79.640),  -- Gauricus
    (Crater,        36.0062850,  79.0766540,  170.723),  -- Gauss
    (Crater,        13.8750260, 339.2072060,   25.404),  -- Gay-Lussac
    (Crater,       -19.4614490,  13.8479060,   44.675),  -- Geber
    (Crater,        -2.5969330,  76.5051810,   17.390),  -- Geissler
    (Crater,        34.4197110,  56.6606650,   81.978),  -- Geminus
    (Crater,       -34.3284880,  13.3684530,   88.543),  -- Gemma Frisius
    (Crater,        44.5352940, 279.4937190,   98.781),  -- Gerard
    (Crater,       -36.5261450,  99.5260980,   47.205),  -- Gernsback
    (Crater,       -18.3659420,  84.2675460,   78.763),  -- Gibbs
    (Crater,        -3.1950290,  76.1598090,  100.247),  -- Gilbert
    (Crater,       -63.7652020,  75.9456570,   63.896),  -- Gill
    (Crater,        14.2495540,  97.4003320,   53.234),  -- Ginzel
    (Crater,        83.3469200,   1.7595040,   42.470),  -- Gioja
    (Crater,        13.1870280,  49.3405960,   15.916),  -- Glaisher
    (Crater,         8.1123560, 282.3322450,   40.099),  -- Glushko
    (Crater,       -10.0504320,  45.0310200,   73.039),  -- Goclenius
    (Crater,        15.1545050,  89.1274520,   93.185),  -- Goddard
    (Crater,         1.8235410,  10.1583340,   34.246),  -- Godin
    (Crater,        73.0367250, 356.6313700,  115.259),  -- Goldschmidt
    (Crater,       -32.6746300,  14.0754670,   44.094),  -- Goodacre
    (Crater,       -19.2580340, 342.7522380,   32.993),  -- Gould
    (Crater,       -42.3031720, 271.2902870,   36.165),  -- Graff
    (Crater,        13.1821460,  52.7913690,   14.269),  -- Greaves
    (Crater,        84.5352050, 284.1660050,   12.951),  -- Grignard
    (Crater,        -5.3750010, 291.6369730,  173.488),  -- Grimaldi
    (Crater,        40.3029880,  32.9834020,   28.546),  -- Grove
    (Crater,       -67.0412890, 349.6979180,   91.496),  -- Gruemberger
    (Crater,        32.8866710, 320.2208270,   14.976),  -- Gruithuisen
    (Crater,       -11.5663110, 345.8127810,   60.752),  -- Guericke
    (Crater,       -40.3369740,  88.9088250,   54.547),  -- Gum
    (Crater,        -8.6137190,  41.2489860,   70.650),  -- Gutenberg
    (Crater,       -47.7590270, 265.9643800,   37.026),  -- Guthnick
    (Crater,        -5.3736510,   0.2323260,   48.154),  -- Gyldén
    (Crater,        83.3997180, 265.3724450,   56.787),  -- Haber
    (Crater,       -59.9167750,  46.6292670,   79.549),  -- Hagecius
    (Crater,        31.2160230,  73.5490300,   87.494),  -- Hahn
    (Crater,       -39.1763410, 334.8610770,   21.326),  -- Haidinger
    (Crater,       -41.2315130, 326.4792660,   70.564),  -- Hainzel
    (Crater,        -1.6589560,  84.1053560,   40.255),  -- Haldane
    (Crater,       -74.1260690,  91.7125270,   84.417),  -- Hale
    (Crater,        33.8093330,  36.7501000,   31.769),  -- Hall
    (Crater,        -8.0491630,   5.7297850,   34.590),  -- Halley
    (Crater,       -42.7748280,  84.4127410,   57.454),  -- Hamilton
    (Crater,       -56.4637030,  71.3841440,   59.541),  -- Hanno
    (Crater,        14.0438470,  72.5423390,   41.180),  -- Hansen
    (Crater,       -11.5307060, 307.9408470,   44.991),  -- Hansteen
    (Crater,        43.5400640, 288.3438290,   22.574),  -- Harding
    (Crater,        -2.1783380,  64.0925570,   17.996),  -- Hargreaves
    (Crater,        40.8704800,  98.7448560,  337.136),  -- Harkhebi
    (Crater,       -38.3073320,  79.6475140,   63.452),  -- Harlan
    (Crater,        52.7316000, 316.5061990,   39.769),  -- Harpalus
    (Crater,        -6.1356030, 279.5305520,   78.456),  -- Hartwig
    (Crater,       -29.3707930,  62.6827520,   82.075),  -- Hase
    (Crater,       -65.1105460, 271.5094650,  163.237),  -- Hausen
    (Crater,       -87.4518380, 354.8307660,   51.423),  -- Haworth
    (Crater,        64.5576710,  83.8674670,   86.214),  -- Hayn
    (Crater,       -22.0612980,  79.6754470,  133.674),  -- Hecataeus
    (Crater,       -81.7719300,  85.6005420,   74.141),  -- Hédervári
    (Crater,         2.8745800, 283.4261660,  157.379),  -- Hedin
    (Crater,       -39.4809870, 342.1751400,   64.870),  -- Heinsius
    (Crater,        32.4731560, 328.0189100,   13.687),  -- Heis
    (Crater,        40.4306200, 336.8917510,   23.736),  -- Helicon
    (Crater,       -32.4149220, 352.2017940,   33.311),  -- Hell
    (Crater,        -7.5640450,  87.6719180,   26.718),  -- Helmert
    (Crater,       -68.6422700,  65.3421280,  110.164),  -- Helmholtz
    (Crater,       -23.9731550, 302.9868080,   39.061),  -- Henry
    (Crater,       -23.5221230, 300.9839510,   41.733),  -- Henry Frères
    (Crater,       -49.3051000,   6.4202420,   85.736),  -- Heraclitus
    (Crater,        46.8218970,  39.2134800,   68.315),  -- Hercules
    (Crater,       -13.3555380, 326.0286850,   14.858),  -- Herigonius
    (Crater,        -0.8745990, 302.5309110,   15.921),  -- Hermann
    (Crater,        86.1659440, 266.6848220,  108.638),  -- Hermite
    (Crater,        23.2536550, 310.1647170,   35.872),  -- Herodotus
    (Crater,        -5.6859880, 357.9145740,   39.089),  -- Herschel
    (Crater,       -29.4195420, 343.5815340,   43.237),  -- Hesiodus
    (Crater,         2.1952220, 292.5373680,  113.869),  -- Hevelius
    (Crater,       -39.5508480, 264.5757290,   16.655),  -- Heyrovsky
    (Crater,        20.9126060,  40.8133930,   15.860),  -- Hill
    (Crater,        -7.9179830,   7.3058110,   28.501),  -- Hind
    (Crater,        89.4091860, 308.1115730,   13.345),  -- Hinshelwood
    (Crater,       -24.9185820, 329.5827230,   57.359),  -- Hippalus
    (Crater,        -5.3564800,   4.9133070,  143.946),  -- Hipparchus
    (Crater,        -6.0127320,  93.6295400,  145.208),  -- Hirayama
    (Crater,       -17.9159370, 265.7356890,   16.807),  -- Hohmann
    (Crater,       -19.1873240,  62.5296760,   47.604),  -- Holden
    (Crater,       -12.0248550,  97.0931800,   12.524),  -- Homeward
    (Crater,       -54.7382830,  32.9253000,  113.597),  -- Hommel
    (Crater,        41.1372650,  54.8587550,   34.347),  -- Hooke
    (Crater,        58.8015720, 319.0687160,   24.977),  -- Horrebow
    (Crater,        -3.9887020,   5.8491010,   29.654),  -- Horrocks
    (Crater,         6.4670930, 332.0004680,   14.162),  -- Hortensius
    (Crater,        83.1147520,  98.5225990,   31.413),  -- Houssay
    (Crater,        -9.4193890,  87.3759600,   42.678),  -- Houtermans
    (Crater,        22.2895650,  86.9076230,   81.837),  -- Hubble
    (Crater,       -41.0686880, 358.4807880,   65.789),  -- Huggins
    (Crater,       -27.0212010,  80.9636420,  199.459),  -- Humboldt
    (Crater,        -4.6769480,  90.4736920,   22.295),  -- Hume
    (Crater,        -4.2502050,  22.5814020,   38.818),  -- Hypatia
    (Crater,       -86.2964100, 284.9638910,   11.977),  -- Ibn Bajja
    (Crater,        -6.9516450,  50.4370810,   11.513),  -- Ibn Battuta
    (Crater,       -11.6867600,  21.7132160,   31.076),  -- Ibn-Rushd
    (Crater,        14.1381900,  91.1361980,   59.687),  -- Ibn Yunus
    (Crater,       -49.3197150,  22.2429530,   37.775),  -- Ideler
    (Crater,       -17.8007770, 262.3231920,   12.423),  -- Il'in
    (Crater,       -47.4945810, 291.0529430,   94.598),  -- Inghirami
    (Crater,        -7.9629410,  33.5008490,   41.392),  -- Isidorus
    (Crater,        62.3138620, 318.1413510,  154.441),  -- J. Herschel
    (Crater,       -56.8152000,  11.3036890,   66.278),  -- Jacobi
    (Crater,        13.5521340,  28.6413520,   24.206),  -- Jansen
    (Crater,         8.6278760,  89.5046970,   73.775),  -- Jansky
    (Crater,       -44.9594810,  40.8220680,  200.651),  -- Janssen
    (Crater,       -55.5795270,  91.5133620,   81.130),  -- Jeans
    (Crater,         0.3716930,  78.0405400,   37.766),  -- Jenkins
    (Crater,       -42.0099710,  95.9837240,   73.664),  -- Jenner
    (Crater,        25.7883220,  93.3896050,  172.794),  -- Joliot
    (Crater,         9.1664620,  15.2105040,   84.716),  -- Julius Caesar
    (Crater,        -6.9018710,  78.9367180,  116.075),  -- Kästner
    (Crater,       -36.4852660,   6.4785650,   53.149),  -- Kaiser
    (Crater,        62.9949330,  25.8433200,   54.964),  -- Kane
    (Crater,       -10.6235040,  20.1979340,   30.851),  -- Kant
    (Crater,        -6.7121820,  87.8103150,   34.541),  -- Kao
    (Crater,       -10.7896230,  70.5943660,   48.654),  -- Kapteyn
    (Crater,        51.2253140,  43.6466390,   32.754),  -- Keldysh
    (Crater,         8.1209850, 321.9912750,   29.490),  -- Kepler
    (Crater,       -26.3071760, 337.3746550,   45.541),  -- Kies
    (Crater,        -6.4092410,  84.1108080,   67.787),  -- Kiess
    (Crater,       -60.7472510,  14.9353650,   41.871),  -- Kinau
    (Crater,        39.2651830, 354.3777580,   11.707),  -- Kirch
    (Crater,       -67.0075000, 314.5182020,   71.197),  -- Kircher
    (Crater,        30.3018470,  38.8444310,   24.380),  -- Kirchhoff
    (Crater,       -69.8528840, 333.7386360,  121.373),  -- Klaproth
    (Crater,       -11.9863980,   2.5264200,   43.474),  -- Klein
    (Crater,         5.3567270,  80.1936320,   13.440),  -- Knox-Shaw
    (Crater,       -24.2275890, 335.3207820,   22.856),  -- König
    (Crater,       -17.3902860, 270.3200380,   40.489),  -- Kopff
    (Crater,        16.5576310, 287.2784460,   51.146),  -- Krafft
    (Crater,        -2.2945520, 261.1082070,   21.048),  -- Kramarov
    (Crater,       -29.9334510, 280.1780480,   41.171),  -- Krasnov
    (Crater,        -9.0481070,  84.5521700,   29.365),  -- Kreiken
    (Crater,        29.0236130, 314.3909270,   22.869),  -- Krieger
    (Crater,         9.4127240,  65.6890930,   19.172),  -- Krogh
    (Crater,       -26.3000450,   5.7578040,   46.440),  -- Krusenstern
    (Crater,       -11.5669720, 348.4324820,   10.308),  -- Kundt
    (Crater,         3.2150910, 327.4720450,   18.267),  -- Kunowsky
    (Crater,       -23.6763360,   1.0836400,   67.223),  -- La Caille
    (Crater,        53.5370630, 331.7819680,   37.827),  -- La Condamine
    (Crater,       -10.6689540,  76.2808930,   80.402),  -- La Pérouse
    (Crater,       -37.9336790, 300.7999840,   36.074),  -- Lacroix
    (Lacus,        -14.8338250, 291.4303040,   86.387),  -- Lacus Aestatis
    (Lacus,        -11.8149860, 276.8276140,  195.645),  -- Lacus Autumni
    (Lacus,         23.1830110,  44.3158150,  122.103),  -- Lacus Bonitatis
    (Lacus,         16.8012690,   8.6149230,  102.902),  -- Lacus Doloris
    (Lacus,        -35.6548300, 316.4234360,  197.739),  -- Lacus Excellentiae
    (Lacus,         18.5199970,   5.3587740,   98.482),  -- Lacus Felicitatis
    (Lacus,         16.3319160,  12.2744900,   88.535),  -- Lacus Gaudii
    (Lacus,         15.0079630,  13.9743980,   48.044),  -- Lacus Hiemalis
    (Lacus,         14.3164790,  12.0548610,   78.251),  -- Lacus Lenitatis
    (Lacus,         45.1348230,  27.3190690,  158.782),  -- Lacus Mortis
    (Lacus,         19.2179740,   7.2729220,   72.678),  -- Lacus Odii
    (Lacus,          7.8377180,  61.9348630,   70.640),  -- Lacus Perseverantiae
    (Lacus,         37.5621630,  30.8046370,  424.759),  -- Lacus Somniorum
    (Lacus,         43.4609800,  65.2035120,   76.671),  -- Lacus Spei
    (Lacus,         46.7727940,  56.2052510,  205.297),  -- Lacus Temporis
    (Lacus,        -39.4215120, 332.0477600,  153.649),  -- Lacus Timoris
    (Lacus,        -16.4843880, 274.0913890,  382.882),  -- Lacus Veris
    (Crater,        -1.3261980,   9.9900720,   58.109),  -- Lade
    (Crater,       -44.4847390, 337.6400630,   88.776),  -- Lagalla
    (Crater,       -32.6025550, 288.5525550,  162.213),  -- Lagrange
    (Crater,        -4.4555850, 351.3532820,   23.541),  -- Lalande
    (Crater,       -14.4007460, 275.7937140,   16.686),  -- Lallemand
    (Crater,       -23.1204910, 289.9422690,  114.655),  -- Lamarck
    (Crater,        25.7717440, 339.0135610,   30.121),  -- Lambert
    (Crater,       -14.7552720,  64.5590150,   84.279),  -- Lamé
    (Crater,        42.7890190,  13.1507310,   12.410),  -- Lamèch
    (Crater,         5.1381380,  23.3164330,   83.234),  -- Lamont
    (Crater,        51.1662340, 273.9501260,   59.170),  -- Langley
    (Crater,        -8.8603870,  61.0380480,  131.977),  -- Langrenus
    (Crater,        -0.3118440, 333.3727110,   38.746),  -- Lansberg
    (Crater,       -15.4866320, 352.0965740,   21.821),  -- Lassell
    (Crater,        28.2904450, 262.9488100,   89.165),  -- Laue
    (Crater,       -27.5443140,  96.3089970,   51.243),  -- Lauritsen
    (Crater,        38.1693730, 278.7469670,   71.011),  -- Lavoisier
    (Crater,         7.3518750,  43.2965430,   24.022),  -- Lawrence
    (Crater,       -74.2738970, 283.9800520,  125.384),  -- Le Gentil
    (Crater,        26.6559840,  30.5015110,   68.395),  -- Le Monnier
    (Crater,        40.3266230, 339.3902020,   20.522),  -- Le Verrier
    (Crater,        -3.1895110,  37.4633070,   12.480),  -- Leakey
    (Crater,        -5.1359850,  88.9747060,   11.392),  -- Lebesgue
    (Crater,       -30.6574060, 319.2409660,   41.173),  -- Lee
    (Crater,       -28.9219710,  70.0192650,   78.082),  -- Legendre
    (Crater,       -39.9554730, 303.8326480,   53.848),  -- Lehmann
    (Crater,        85.1906800, 250.3059960,   47.652),  -- Lenard
    (Crater,       -33.3005650, 326.3074650,   16.358),  -- Lepaute
    (Crater,       -10.5006420, 317.5120910,  117.587),  -- Letronne
    (Crater,       -35.7752610, 355.7349400,   63.703),  -- Lexell
    (Crater,       -47.1896060,   6.5434720,   75.418),  -- Licetus
    (Crater,        31.8536750, 292.2838580,   19.529),  -- Lichtenberg
    (Crater,        12.3559250,  52.8434200,   31.627),  -- Lick
    (Crater,       -24.3541370, 311.7010110,   38.962),  -- Liebig
    (Crater,       -54.6017910,   6.0936160,   61.177),  -- Lilius
    (Crater,        -5.4123280,  52.9033250,   13.343),  -- Lindbergh
    (Crater,        70.0335900, 260.7775810,   59.366),  -- Lindblad
    (Crater,       -32.3487820,  24.7742260,   53.077),  -- Lindenau
    (Crater,        -6.9975120,  13.0101330,   32.230),  -- Lindsay
    (Crater,         2.7278330,  73.5658790,   16.121),  -- Liouville
    (Crater,        21.5037230,  31.3941110,   28.518),  -- Littrow
    (Crater,       -46.2732540,  36.5949870,   35.076),  -- Lockyer
    (Crater,       -22.6930530, 327.1477040,   22.453),  -- Loewy
    (Crater,        -0.4398510, 292.6174340,   31.253),  -- Lohrmann
    (Crater,       -13.7621010,  60.3078510,   43.341),  -- Lohse
    (Crater,        27.3500980,  98.2785760,   90.690),  -- Lomonosov
    (Crater,       -49.5516200, 338.1206610,  145.497),  -- Longomontanus
    (Crater,        34.5886140, 262.8130230,  378.420),  -- Lorentz
    (Crater,        44.1247720, 313.9604900,   34.813),  -- Louville
    (Crater,        82.0803600, 250.4903940,   57.063),  -- Lovelace
    (Crater,        -3.9859130,  41.7885590,   14.094),  -- Lubbock
    (Crater,       -17.8816310, 336.1059240,   42.971),  -- Lubiniezky
    (Crater,        -7.7216260,  97.4529440,   23.290),  -- Ludwig
    (Crater,        26.4290240,  89.3638640,   67.581),  -- Lyapunov
    (Crater,        13.6281730,  40.5564160,   31.167),  -- Lyell
    (Crater,       -50.4657800,  84.8040020,  150.600),  -- Lyot
    (Crater,        -1.9234510,  67.9894330,   54.332),  -- Maclaurin
    (Crater,        10.5221360,  20.1048570,   20.339),  -- Maclear
    (Crater,        21.2556490,  45.9702270,   62.790),  -- Macrobius
    (Crater,       -11.0414890,  29.7589560,   27.581),  -- Mädler
    (Crater,       -11.9757650,  44.0739850,   37.205),  -- Magelhaens
    (Crater,       -50.0336990, 354.0162820,  155.578),  -- Maginus
    (Crater,        80.8694080,  10.4085270,   47.425),  -- Main
    (Crater,        41.5965410, 316.5021180,   39.492),  -- Mairan
    (Crater,       -84.9997660,  11.4030740,   72.385),  -- Malapert
    (Crater,       -45.4128850,  54.0545800,   58.919),  -- Mallet
    (Crater,        14.4520300,   9.0736900,   38.339),  -- Manilius
    (Crater,         4.5701130,  19.9945830,   15.045),  -- Manners
    (Crater,       -67.5100440,  26.3735810,   95.971),  -- Manzinus
    (Crater,        19.3605510,  34.7971920,   39.618),  -- Maraldi
    (Crater,        15.5233540, 357.9515660,   28.252),  -- Marco Polo
    (Mare,          22.4267990,  67.5752700,  145.986),  -- Mare Anguis
    (Mare,         -47.7705290,  91.9853640,  996.840),  -- Mare Australe
    (Mare,         -10.5308170, 337.6856440,  350.009),  -- Mare Cognitum
    (Mare,          16.1773710,  59.1037470,  555.921),  -- Mare Crisium
    (Mare,          -7.8349740,  53.6691490,  840.354),  -- Mare Fecunditatis
    (Mare,          57.5923080, 359.9936190, 1446.410),  -- Mare Frigoris
    (Mare,          56.9220960,  81.5428340,  230.782),  -- Mare Humboldtianum
    (Mare,         -24.4784600, 321.4283950,  419.669),  -- Mare Humorum
    (Mare,          34.7244310, 345.0914020, 1145.530),  -- Mare Imbrium
    (Mare,           7.7919580, 329.3597880,  511.933),  -- Mare Insularum
    (Mare,          12.7018790,  86.5152940,  357.632),  -- Mare Marginis
    (Mare,         -15.1851680,  34.6020540,  339.387),  -- Mare Nectaris
    (Mare,         -20.5894420, 342.7127870,  714.499),  -- Mare Nubium
    (Mare,         -19.8655500, 265.3296820,  294.155),  -- Mare Orientale
    (Mare,          27.2878850,  18.3596440,  674.278),  -- Mare Serenitatis
    (Mare,          -1.7087550,  87.0494710,  373.965),  -- Mare Smythii
    (Mare,           1.3023050,  65.3032730,  143.132),  -- Mare Spumans
    (Mare,           8.3487160,  30.8346240,  875.749),  -- Mare Tranquillitatis
    (Mare,           7.4911940,  68.6589470,  244.843),  -- Mare Undarum
    (Mare,          13.1966740,   4.0861730,  242.458),  -- Mare Vaporum
    (Crater,       -39.3759240,  76.5738900,   56.554),  -- Marinus
    (Crater,        11.8967990, 309.1645220,   40.089),  -- Marius
    (Crater,        53.4332860, 297.1594910,   39.919),  -- Markov
    (Crater,         2.1557570,  30.0436890,   22.420),  -- Maskelyne
    (Crater,        42.6995940,  30.5086570,   33.333),  -- Mason
    (Crater,       -14.5249660, 266.1245410,   53.798),  -- Maunder
    (Crater,        49.6967540, 332.7170730,   45.486),  -- Maupertuis
    (Crater,       -41.7718670,  13.9200710,  115.350),  -- Maurolycus
    (Crater,        37.1087820,  39.6913050,   16.577),  -- Maury
    (Crater,        29.9048010,  98.5255610,  109.241),  -- Maxwell
    (Crater,         2.1096990,  92.2182180,   41.003),  -- McAdie
    (Crater,       -15.3192090,  50.2369210,   23.965),  -- McClure
    (Crater,        47.0081270, 267.1723070,   75.290),  -- McLaughlin
    (Crater,       -43.6253420, 324.8127720,  134.108),  -- Mee
    (Crater,        13.5700150, 263.8197340,   51.551),  -- Mees
    (Crater,        16.2593320,  15.9294050,   27.133),  -- Menelaus
    (Crater,       -29.2535020, 333.8895940,   46.322),  -- Mercator
    (Crater,        46.6634120,  66.0702940,   64.302),  -- Mercurius
    (Crater,        74.8300190, 242.5522210,   56.991),  -- Merrill
    (Crater,       -21.4927860, 310.6638040,   84.461),  -- Mersenius
    (Crater,        39.3078510,  60.0591460,  122.398),  -- Messala
    (Crater,        -1.9038500,  47.6532990,   13.796),  -- Messier
    (Crater,       -40.4244310,  43.3727450,   83.814),  -- Metius
    (Crater,        73.5673380,  19.6261910,  124.702),  -- Meton
    (Crater,        10.0058920, 329.7685860,   12.190),  -- Milichius
    (Crater,       -39.3719330,   0.7827320,   61.375),  -- Miller
    (Crater,        49.7695800,  20.1709950,   32.150),  -- Mitchell
    (Crater,        -0.7008540, 354.1191550,   24.377),  -- Mösting
    (Crater,        66.2666860,  28.7951520,   36.827),  -- Moigno
    (Crater,       -19.2409520,  47.5443430,   36.605),  -- Monge
    (Mons,          19.3238870, 356.2904780,   29.959),  -- Mons Ampère
    (Mons,          19.3308690,  29.0067840,   61.476),  -- Mons Argaeus
    (Mons,          21.7276310,   0.3813680,   76.492),  -- Mons Bradley
    (Mons,          29.4248950, 324.2116580,   32.422),  -- Mons Delisle
    (Mons,          36.0678170, 320.4101410,   27.243),  -- Mons Gruithuisen Delta
    (Mons,          36.5584840, 319.2787150,   19.653),  -- Mons Gruithuisen Gamma
    (Mons,          26.6948230,   4.1183280,   26.396),  -- Mons Hadley
    (Mons,          25.7230030,   3.7065090,   17.242),  -- Mons Hadley Delta
    (Mons,         -12.1948330, 309.7851980,   30.648),  -- Mons Hansteen
    (Mons,          19.9188480, 357.1429030,   41.968),  -- Mons Huygens
    (Mons,          27.6612480, 334.4946520,   21.706),  -- Mons La Hire
    (Mons,          20.3388020,  35.5012240,   15.896),  -- Mons Maraldi
    (Mons,         -11.8387070, 340.1562920,   13.681),  -- Mons Moro
    (Mons,         -10.0037830,  21.7448300,   37.590),  -- Mons Penck
    (Mons,          45.8247840, 351.1326980,   24.419),  -- Mons Pico
    (Mons,          40.7166770, 359.0772810,   22.502),  -- Mons Piton
    (Mons,          40.7607250, 301.6224570,   73.249),  -- Mons Rümker
    (Mons,          11.9080450,  63.2636310,   13.227),  -- Mons Usov
    (Mons,          22.3496690, 327.4810190,   28.734),  -- Mons Vinogradov
    (Mons,          19.3334580,  30.7352680,   44.278),  -- Mons Vitruvius
    (Mons,          16.8790640, 353.1986070,   32.867),  -- Mons Wolff
    (Mons,          45.4129950,   0.4402750,   21.568),  -- Mont Blanc
    (Crater,       -45.8282200, 339.2449310,   77.054),  -- Montanari
    (Mons,          29.0593010, 305.9284590,  159.760),  -- Montes Agricola
    (Mons,          48.3587730, 359.4243820,  334.479),  -- Montes Alpes
    (Mons,          19.8714310,   0.0253080,  599.675),  -- Montes Apenninus
    (Mons,          25.3938800, 354.7502140,  146.537),  -- Montes Archimedes
    (Mons,          14.5676730, 336.3755450,  333.592),  -- Montes Carpatus
    (Mons,          37.5188230,   9.9310530,  443.505),  -- Montes Caucasus
    (Mons,         -19.4358030, 265.0690350,  963.501),  -- Montes Cordillera
    (Mons,          17.1071470,  12.0307810,  384.663),  -- Montes Haemus
    (Mons,          26.8923340, 318.7098640,   92.696),  -- Montes Harbinger
    (Mons,          47.4935290, 323.8889960,  420.796),  -- Montes Jura
    (Mons,         -14.0479840,  41.5097020,  251.333),  -- Montes Pyrenaeus
    (Mons,          48.2956730, 340.2760840,   83.238),  -- Montes Recti
    (Mons,          -7.4828730, 332.3975320,  190.118),  -- Montes Riphaeus
    (Mons,         -19.4867750, 265.0523080,  682.275),  -- Montes Rook
    (Mons,           2.7171800,  43.1727590,   52.466),  -- Montes Secchi
    (Mons,          34.4723810, 354.7865230,   59.187),  -- Montes Spitzbergen
    (Mons,          27.3232370,  40.3354860,  166.163),  -- Montes Taurus
    (Mons,          47.8908000, 346.8128490,  111.983),  -- Montes Teneriffe
    (Crater,       -70.6346440, 354.0530020,  114.447),  -- Moretus
    (Crater,        -2.8200930,  64.6156960,   13.712),  -- Morley
    (Crater,        20.9543460, 269.7991820,   88.889),  -- Moseley
    (Crater,        78.3787240, 333.1754890,   82.781),  -- Mouchez
    (Crater,       -60.9081310,  97.5677580,   54.704),  -- Moulton
    (Crater,        -7.6391360,   2.0385570,   23.400),  -- Müller
    (Crater,         5.0668310, 359.7945940,   57.831),  -- Murchison
    (Crater,       -63.6479180,  29.9254100,   76.335),  -- Mutus
    (Crater,        81.1707580,  95.3805310,  116.900),  -- Nansen
    (Crater,        -4.7002120,  57.9303310,   32.960),  -- Naonobu
    (Crater,       -41.0386510,   0.1427480,   51.993),  -- Nasireddin
    (Crater,       -50.4863010, 303.6056630,   78.375),  -- Nasmyth
    (Crater,        19.9759450, 328.8368110,   10.985),  -- Natasha
    (Crater,       -31.3527300,  39.8849720,   49.220),  -- Neander
    (Crater,       -58.5806280,  39.0130200,   72.793),  -- Nearch
    (Crater,        68.2054650,  25.0232700,   51.027),  -- Neison
    (Crater,         8.7648860,  84.5820110,  144.318),  -- Neper
    (Crater,        35.5701870, 265.3638340,  121.523),  -- Nernst
    (Crater,       -71.1614100,  70.9231480,   84.381),  -- Neumayer
    (Crater,        29.7623430,  43.6665910,   39.803),  -- Newcomb
    (Crater,       -76.5206700, 342.5585900,   83.851),  -- Newton
    (Crater,       -26.2160150, 274.7893250,   38.082),  -- Nicholson
    (Crater,       -42.4710430,  25.8714880,   40.541),  -- Nicolai
    (Crater,       -21.9484440, 347.5042100,   14.726),  -- Nicollet
    (Crater,       -85.2751890,  53.2725210,   79.272),  -- Nobile
    (Crater,         0.1660670,  75.9486010,   41.790),  -- Nobili
    (Crater,       -48.8200040, 314.1581520,   32.118),  -- Nöggerath
    (Crater,       -34.9023970,   3.7889010,   70.610),  -- Nonius
    (Crater,         4.6150760,  91.1462620,   18.494),  -- Nunn
    (Oceanus,       20.6714220, 303.3226060, 2592.240),  -- Oceanus Procellarum
    (Crater,        57.1253420, 295.7971540,   73.469),  -- Oenopides
    (Crater,        43.0857220,  47.2546550,   42.278),  -- Oersted
    (Crater,       -43.7556120,  76.0913770,   78.667),  -- Oken
    (Crater,         7.2985750, 283.8567000,   73.017),  -- Olbers
    (Crater,       -16.3155410, 342.3599210,   48.732),  -- Opelt
    (Crater,        -1.5209540, 359.5478780,   40.866),  -- Oppolzer
    (Crater,       -40.3691260, 356.0401430,  121.019),  -- Orontius
    (Crater,        -9.4667830, 352.8114270,   33.474),  -- Palisa
    (Crater,       -28.0239430,  64.3895640,   41.868),  -- Palitzsch
    (Crater,         5.4763960, 358.3467140,   49.507),  -- Pallas
    (Crater,       -28.6407450, 312.2026460,   39.843),  -- Palmieri
    (Palus,        -31.9959170, 332.4567070,  300.384),  -- Palus Epidemiarum
    (Palus,         27.3560130,   0.0038600,  180.451),  -- Palus Putredinis
    (Palus,         13.6880660,  44.7246580,  163.455),  -- Palus Somni
    (Crater,        62.5963100, 265.3658330,   60.916),  -- Paneth
    (Crater,       -14.5731450,   3.2921150,   70.659),  -- Parrot
    (Crater,        -7.8834330, 344.2205940,   47.282),  -- Parry
    (Crater,        74.3570490, 289.3699160,  108.205),  -- Pascal
    (Crater,        24.9075360,   0.4984810,   10.081),  -- Patricia
    (Crater,        88.6250850,  24.4024020,   78.745),  -- Peary
    (Crater,         2.7596840,  86.9581650,   12.545),  -- Peek
    (Crater,        18.2641310,  53.3535920,   18.860),  -- Peirce
    (Crater,       -46.4049560,  67.7976040,   61.535),  -- Peirescius
    (Crater,       -64.5698710,  11.3425090,   56.448),  -- Pentland
    (Crater,       -25.3914320,  60.7775900,  184.062),  -- Petavius
    (Crater,        74.3468290,  67.8923030,   76.949),  -- Petermann
    (Crater,        68.0692960,  29.3921300,   14.668),  -- Peters
    (Crater,       -61.3618490,  88.1752030,   55.441),  -- Petrov
    (Crater,       -27.5205440, 273.2463680,   36.670),  -- Pettit
    (Crater,       -26.5745410,  75.6706260,  104.215),  -- Phillips
    (Crater,        72.2220680, 327.1155040,   71.435),  -- Philolaus
    (Crater,       -52.7917540, 302.6874690,  115.180),  -- Phocylides
    (Crater,       -36.1571480, 291.9912370,  102.574),  -- Piazzi
    (Crater,        41.9138460, 356.7568730,   12.958),  -- Piazzi Smyth
    (Crater,        14.5663070,  54.7240210,   22.348),  -- Picard
    (Crater,       -29.6998500,  32.1986360,   87.581),  -- Piccolomini
    (Crater,        -2.8764170,   6.9918070,   15.397),  -- Pickering
    (Crater,       -43.5636080, 352.5075810,   59.949),  -- Pictet
    (Crater,       -60.1701570, 273.3049310,   64.367),  -- Pilatre
    (Crater,       -58.6378190, 286.0478440,   88.431),  -- Pingré
    (Crater,       -29.8815810, 346.4730970,  100.628),  -- Pitatus
    (Crater,       -50.6111570,  30.5661330,   79.847),  -- Pitiscus
    (Crater,        42.2544240,  28.2214760,   42.972),  -- Plana
    (Crater,        51.6191940, 350.6174560,  100.684),  -- Plato
    (Crater,       -23.5604490,   8.4451550,   49.885),  -- Playfair
    (Crater,        15.3568510,  23.6067300,   41.313),  -- Plinius
    (Crater,        24.1768370,  79.0518660,   69.589),  -- Plutarch
    (Crater,        57.2677640, 260.7668310,  212.355),  -- Poczobutt
    (Crater,       -30.3393000,  10.5647830,   41.402),  -- Poisson
    (Crater,       -22.4633850,  25.6273960,   40.809),  -- Polybius
    (Crater,         0.7366490,  66.9105000,   25.508),  -- Pomortsev
    (Crater,        75.9093700, 305.4282260,   67.570),  -- Poncelet
    (Crater,       -25.4346730,  21.5520370,   39.697),  -- Pons
    (Crater,       -28.4222950,  14.3561710,   55.662),  -- Pontanus
    (Crater,       -58.7787940,  66.0673010,   91.405),  -- Pontécoulant
    (Crater,        16.9336220,  99.3759480,   71.395),  -- Popov
    (Crater,       -56.1468030, 349.8171360,   51.465),  -- Porter
    (Crater,        31.8783170,  29.9912770,   95.057),  -- Posidonius
    (Crater,        25.4878430, 315.8584560,   46.129),  -- Prinz
    (Crater,        16.0877530,  46.8943000,   26.908),  -- Proclus
    (Crater,       -46.4292140, 354.9559780,   47.638),  -- Proctor
    (Promontorium,  13.8716880,  65.7341650,   62.462),  -- Promontorium Agarum
    (Promontorium,  42.3979030,   1.7686610,   18.836),  -- Promontorium Agassiz
    (Promontorium,  16.8043390,  21.9434220,   11.213),  -- Promontorium Archerusia
    (Promontorium,  43.3087280,   1.1352310,   16.555),  -- Promontorium Deville
    (Promontorium,  28.6288470,   4.7473000,   20.000),  -- Promontorium Fresnel
    (Promontorium,  40.5986220, 325.9028090,   50.000),  -- Promontorium Heraclides
    (Promontorium, -26.9482080, 326.5546630,   45.006),  -- Promontorium Kelvin
    (Promontorium,  46.8449190, 334.4922810,   50.000),  -- Promontorium Laplace
    (Promontorium, -18.6301160, 352.6578200,   70.000),  -- Promontorium Taenarium
    (Crater,        56.0176650,   7.3406930,   21.053),  -- Protagoras
    (Crater,        -9.1604720, 358.1626530,  153.669),  -- Ptolemaeus
    (Crater,       -27.8179270, 320.8130150,   24.952),  -- Puiseux
    (Crater,       -25.5120500, 357.9696210,  114.973),  -- Purbach
    (Crater,        -1.5355010,  94.8606290,   50.295),  -- Purkyne
    (Crater,        63.6814240, 297.0241410,  144.552),  -- Pythagoras
    (Crater,        20.5669920, 339.4056720,   18.808),  -- Pytheas
    (Crater,       -34.7814660,  23.4565180,   82.438),  -- Rabbi Levi
    (Crater,        26.9586550, 304.8439520,   10.170),  -- Raman
    (Crater,       -32.9563960, 328.1250020,   25.105),  -- Ramsden
    (Crater,        -3.8686530,  71.4938560,   10.480),  -- Rankine
    (Crater,        29.1194490,  89.4467360,  113.769),  -- Rayleigh
    (Crater,        -2.4503670,   0.7330580,   51.254),  -- Réaumur
    (Crater,       -28.2794390, 358.9139470,  126.641),  -- Regiomontanus
    (Crater,        54.0438010, 272.1195330,   51.306),  -- Regnault
    (Crater,       -30.4772920,  47.9545030,   64.848),  -- Reichenbach
    (Crater,       -47.6917120,  60.4212700,   47.617),  -- Reimarus
    (Crater,         6.9200110, 305.0170370,   29.848),  -- Reiner
    (Swirl,          7.3852650, 301.0416630,   73.441),  -- Reiner Gamma
    (Crater,         3.2814720, 337.1375470,   43.285),  -- Reinhold
    (Crater,        51.3071800, 281.5897370,  108.654),  -- Repsold
    (Crater,         2.8611660,  71.8996530,   17.883),  -- Respighi
    (Crater,         0.0317830,   4.9241410,   44.427),  -- Rhaeticus
    (Crater,       -37.1000360,  47.1707680,   70.811),  -- Rheita
    (Crater,        -2.9003640, 285.5844780,  155.664),  -- Riccioli
    (Crater,       -37.0249940,  26.4254980,   71.789),  -- Riccius
    (Crater,        30.9256970,  99.8918790,  162.564),  -- Richardson
    (Crater,        39.3803630,  87.1776260,  117.854),  -- Riemann
    (Rima,         -20.3784250, 331.4356670,   54.251),  -- Rima Agatharchides
    (Rima,          29.2477350, 306.5795780,  125.078),  -- Rima Agricola
    (Rima,          53.6285760,   2.9985430,   90.182),  -- Rima Archytas
    (Rima,           6.4751810,  13.4381160,  247.449),  -- Rima Ariadaeus
    (Rima,          26.6631980, 321.3542990,   68.058),  -- Rima Artsimovich
    (Rima,         -14.7406650, 311.9593070,   69.821),  -- Rima Billy
    (Rima,         -21.4034040, 350.7189210,   54.183),  -- Rima Birt
    (Rima,          24.1735990, 359.3961450,  133.763),  -- Rima Bradley
    (Rima,          22.2978290, 323.6471510,  327.263),  -- Rima Brayley
    (Rima,          37.0315510,  12.6597320,   40.000),  -- Rima Calippus
    (Rima,          11.3175320, 288.8625430,  221.928),  -- Rima Cardanus
    (Rima,          19.9547960,  29.2979640,   15.020),  -- Rima Carmen
    (Rima,          10.4150880,  38.0746280,  167.000),  -- Rima Cauchy
    (Rima,          27.9776260,  56.5145130,   45.541),  -- Rima Cleomedes
    (Rima,          30.0314780, 306.1992620,   14.661),  -- Rima Cleopatra
    (Rima,          18.6920810,   1.8530200,   37.319),  -- Rima Conon
    (Rima,          17.5776100,  26.6303690,   15.000),  -- Rima Dawes
    (Rima,          30.8730290, 327.6518070,   57.598),  -- Rima Delisle
    (Rima,          28.7038250, 326.3265530,  201.505),  -- Rima Diophantus
    (Rima,          17.3691030, 334.6328220,  244.159),  -- Rima Draper
    (Rima,          21.0841820, 329.6878900,  104.972),  -- Rima Euler
    (Rima,          -2.3842250, 355.3268090,   49.748),  -- Rima Flammarion
    (Rima,         -35.2950310,  61.1716580,   65.854),  -- Rima Furnerius
    (Rima,          32.8578180,  35.2509430,  166.846),  -- Rima G. Bond
    (Rima,          58.8444960,  35.7654040,   42.727),  -- Rima Gärtner
    (Rima,          12.9081180, 300.7960960,  185.877),  -- Rima Galilaei
    (Rima,          13.1771950, 337.6726590,   40.037),  -- Rima Gay-Lussac
    (Rima,          25.7159180,   3.1467120,  116.086),  -- Rima Hadley
    (Rima,         -12.0881720, 307.0078250,   30.894),  -- Rima Hansteen
    (Rima,         -30.5395830, 338.1507000,  251.460),  -- Rima Hesiodus
    (Rima,           7.6163880,   6.7745590,  203.960),  -- Rima Hyginus
    (Rima,          14.4951360,  29.5115210,   45.123),  -- Rima Jansen
    (Rima,          29.2938430, 313.7373570,   22.623),  -- Rima Krieger
    (Rima,          38.2788920, 313.1726500,  120.511),  -- Rima Mairan
    (Rima,          16.3722590, 310.4582010,  283.540),  -- Rima Marius
    (Rima,          -0.7564730,  44.5454460,   74.218),  -- Rima Messier
    (Rima,           8.0293660, 327.1345660,  140.724),  -- Rima Milichius
    (Rima,          -1.5338390,   1.2757340,   94.200),  -- Rima Oppolzer
    (Rima,          -2.8382800,   2.4661860,   30.664),  -- Rima Réaumur
    (Rima,           1.2752110, 353.7538900,   27.245),  -- Rima Schröter
    (Rima,          46.0182140, 309.6387570,  276.668),  -- Rima Sharp
    (Rima,          58.2755100,  23.6884870,  157.492),  -- Rima Sheepshanks
    (Rima,           6.6219860, 312.8596330,  156.388),  -- Rima Suess
    (Rima,          13.2583580, 328.6290950,   67.807),  -- Rima T. Mayer
    (Rima,          25.1956680, 359.2460620,   10.505),  -- Rima Vladimir
    (Rima,          19.9816820, 328.5667850,   13.719),  -- Rima Wan-Yu
    (Rima,          16.6242290,   4.7934550,   30.395),  -- Rima Yangel'
    (Rima,          25.0208890, 329.5443630,   15.242),  -- Rima Zahia
    (Rima,         -13.4031840, 358.0572200,   87.000),  -- Rimae Alphonsus
    (Rima,           4.3880960,  54.3306310,   89.638),  -- Rimae Apollonius
    (Rima,          26.3359310, 355.4717820,  215.000),  -- Rimae Archimedes
    (Rima,          27.5232780, 312.7517290,  175.000),  -- Rimae Aristarchus
    (Rima,         -18.3077180, 358.6168110,   57.000),  -- Rimae Arzachel
    (Rima,          46.8189690,  44.4177100,   46.800),  -- Rimae Atlas
    (Rima,           9.5423170, 356.7780300,  233.000),  -- Rimae Bode
    (Rima,           9.8710650,  11.2652860,   32.000),  -- Rimae Boscovich
    (Rima,          44.7046770,  25.2742350,   98.000),  -- Rimae Bürg
    (Rima,          29.0099220,  31.2416600,  100.000),  -- Rimae Chacornac
    (Rima,          37.5292560,  24.3277350,  140.248),  -- Rimae Daniell
    (Rima,         -19.8415520, 293.3438100,  170.000),  -- Rimae Darwin
    (Rima,         -24.9874460, 309.6965760,   46.655),  -- Rimae de Gasparis
    (Rima,         -27.6803450, 262.4585450,   61.000),  -- Rimae Focas
    (Rima,          28.1116150,   3.7276510,   75.000),  -- Rimae Fresnel
    (Rima,         -17.4705760, 320.1293770,   70.000),  -- Rimae Gassendi
    (Rima,          45.5395590, 275.6389140,  110.000),  -- Rimae Gerard
    (Rima,          -7.8353390,  42.8793910,  190.000),  -- Rimae Goclenius
    (Rima,          -6.1817130, 296.1022340,  162.000),  -- Rimae Grimaldi
    (Rima,          -4.4221380,  36.4185920,  223.000),  -- Rimae Gutenberg
    (Rima,         -34.7084330,  67.7806250,  257.236),  -- Rimae Hase
    (Rima,         -13.9215560, 323.2470820,  180.000),  -- Rimae Herigonius
    (Rima,           0.8090510, 293.6233850,  180.000),  -- Rimae Hevelius
    (Rima,         -25.5975630, 330.6429030,  266.000),  -- Rimae Hippalus
    (Rima,          -0.3399220,  22.7769290,  200.000),  -- Rimae Hypatia
    (Rima,         -45.7987910,  39.2603660,  120.000),  -- Rimae Janssen
    (Rima,         -14.6813330, 271.8952080,  250.000),  -- Rimae Kopff
    (Rima,          22.4677930,  30.4736090,  165.000),  -- Rimae Littrow
    (Rima,          12.2307540,  19.8989850,   94.000),  -- Rimae Maclear
    (Rima,           2.8801890, 319.5172430,   71.000),  -- Rimae Maestlin
    (Rima,          51.2372430, 337.1790120,   50.000),  -- Rimae Maupertuis
    (Rima,          17.1046560,  17.7723640,   87.000),  -- Rimae Menelaus
    (Rima,         -20.6943720, 313.4712210,  240.000),  -- Rimae Mersenius
    (Rima,         -13.6431840, 341.8618490,   55.000),  -- Rimae Opelt
    (Rima,         -27.8346110, 312.8299570,   27.127),  -- Rimae Palmieri
    (Rima,          -8.0671460, 343.4819700,  210.000),  -- Rimae Parry
    (Rima,         -25.2340010,  60.4836890,  110.000),  -- Rimae Petavius
    (Rima,         -25.2189050, 266.3704250,  320.000),  -- Rimae Pettit
    (Rima,         -29.8352430, 346.3766450,   90.000),  -- Rimae Pitatus
    (Rima,          50.8791050, 356.9849110,  180.000),  -- Rimae Plato
    (Rima,          17.0508330,  23.1415320,  100.000),  -- Rimae Plinius
    (Rima,          32.0314460,  29.6055730,   78.000),  -- Rimae Posidonius
    (Rima,          27.0451690, 316.4941400,   10.949),  -- Rimae Prinz
    (Rima,         -32.9278710, 328.6781550,  100.000),  -- Rimae Ramsden
    (Rima,          50.7365480, 279.5413200,  152.054),  -- Rimae Repsold
    (Rima,          -1.5152270, 286.9289780,  250.000),  -- Rimae Riccioli
    (Rima,           3.5009860,  17.9677580,   75.000),  -- Rimae Ritter
    (Rima,          26.9834140,  34.8632780,  112.000),  -- Rimae Römer
    (Rima,           0.9926780,  44.0831750,   35.000),  -- Rimae Secchi
    (Rima,         -15.0052700, 298.6435650,  405.000),  -- Rimae Sirsalis
    (Rima,           8.0802980,  18.7179070,  130.000),  -- Rimae Sosigenes
    (Rima,          20.6540180,   9.9912740,   80.000),  -- Rimae Sulpicius Gallus
    (Rima,           5.8258360,  46.8276320,   35.000),  -- Rimae Taruntius
    (Rima,          33.0351070,   5.8665360,   53.000),  -- Rimae Theaetetus
    (Rima,           5.1042370,   4.8328570,  200.000),  -- Rimae Triesnecker
    (Rima,          11.5456880, 275.9666250,   10.394),  -- Rimae Vasco da Gama
    (Rima,         -15.4637190, 306.2391730,  130.000),  -- Rimae Zupus
    (Crater,       -11.1285050,   8.4767240,   24.070),  -- Ritchey
    (Crater,         1.9580330,  19.1713050,   29.525),  -- Ritter
    (Crater,       -15.3700890,  92.3847870,   53.767),  -- Ritz
    (Crater,        59.0645090, 313.9708120,   24.091),  -- Robinson
    (Crater,       -12.8876830, 287.1057070,   84.065),  -- Rocca
    (Crater,        25.4257580,  36.4050980,   43.705),  -- Römer
    (Crater,        32.8791470, 268.5755330,  128.424),  -- Röntgen
    (Crater,       -55.4924260,  43.1488860,   91.649),  -- Rosenberger
    (Crater,        11.6689790,  21.7374630,   24.485),  -- Ross
    (Crater,       -17.9479470,  34.9809910,   11.426),  -- Rosse
    (Crater,       -56.4236790, 326.1566120,   46.846),  -- Rost
    (Crater,       -30.8134350,  27.6981330,   41.667),  -- Rothmann
    (Crater,        -2.4289810,  86.8059920,   38.982),  -- Runge
    (Rupes,        -24.3175850,  23.1240380,  545.193),  -- Rupes Altai
    (Rupes,          9.3108750,  37.0758500,  169.845),  -- Rupes Cauchy
    (Rupes,        -28.0280970, 326.8261990,   85.915),  -- Rupes Kelvin
    (Rupes,        -25.1369770, 314.0767160,  144.783),  -- Rupes Liebig
    (Rupes,        -30.2075460, 337.1574590,  132.403),  -- Rupes Mercator
    (Rupes,        -21.6748040, 352.2981470,  115.948),  -- Rupes Recta
    (Rupes,         26.9708090, 312.4653760,   50.137),  -- Rupes Toscanelli
    (Crater,        26.5116480, 284.4521900,  103.366),  -- Russell
    (Crater,       -61.1458350, 347.7532020,   49.982),  -- Rutherfurd
    (Crater,       -46.4309500, 263.5730100,   48.030),  -- Rydberg
    (Crater,         1.3768910,  20.0660370,   29.751),  -- Sabine
    (Crater,       -23.7521690,  16.6400730,   97.669),  -- Sacrobosco
    (Crater,       -20.9882740,  44.0601740,   62.243),  -- Santbech
    (Crater,       -39.2775520, 350.5556210,   81.739),  -- Sasserides
    (Crater,        -4.2600950,   8.7178550,   44.385),  -- Saunder
    (Crater,       -43.3775780, 356.1150600,   54.557),  -- Saussure
    (Crater,       -60.3531530, 332.1908920,  110.069),  -- Scheiner
    (Crater,        23.3787750, 301.1792760,   24.200),  -- Schiaparelli
    (Crater,       -44.3793350, 304.8948270,  212.184),  -- Schickard
    (Crater,       -51.7243860, 320.2160180,  179.362),  -- Schiller
    (Crater,        -5.9334980, 276.6110500,   87.552),  -- Schlüter
    (Crater,         0.9563750,  18.7806370,   11.130),  -- Schmidt
    (Crater,        44.7601210, 261.9990930,   24.572),  -- Schönfeld
    (Crater,       -76.6392320,  24.6912150,   85.802),  -- Schomberger
    (Crater,       -19.4539400,  89.7961940,   52.094),  -- Schorr
    (Crater,         2.7384380, 353.0163450,   36.667),  -- Schröter
    (Crater,         2.7835850,  81.0142770,   51.945),  -- Schubert
    (Crater,        42.4209630,  60.8092350,   61.313),  -- Schumacher
    (Crater,        65.1032380,  45.4807730,   25.471),  -- Schwabe
    (Crater,        77.7298430,  14.1334440,   54.932),  -- Scoresby
    (Crater,       -82.3532130,  48.5153800,  107.823),  -- Scott
    (Crater,         2.4038620,  43.5585030,   22.135),  -- Secchi
    (Crater,       -58.9597120, 311.3247670,   67.841),  -- Segner
    (Crater,        21.0908410, 293.3392310,   45.014),  -- Seleucus
    (Crater,        26.7110970,  79.8062300,   47.572),  -- Seneca
    (Crater,       -32.8918550, 274.7294960,   48.473),  -- Shaler
    (Crater,         9.3543320,  56.8340550,   24.825),  -- Shapley
    (Crater,        45.7512970, 319.7834420,   37.609),  -- Sharp
    (Crater,        59.2418730,  17.0385970,   23.671),  -- Sheepshanks
    (Crater,       -88.1370170,  45.9109630,   51.816),  -- Shoemaker
    (Crater,       -74.5370220, 352.3160160,   67.942),  -- Short
    (Crater,        42.6480820,  52.7127450,   37.728),  -- Shuckburgh
    (Crater,       -27.1061980, 267.3256550,   14.668),  -- Shuleykin
    (Crater,         6.2065690,  12.5290980,   12.561),  -- Silberschlag
    (Crater,       -72.6125180,  14.7429900,   68.889),  -- Simpelius
    (Crater,         8.8511350,  31.6000910,   11.673),  -- Sinas
    (Sinus,         12.0976170, 351.6604490,  316.499),  -- Sinus Aestuum
    (Sinus,         19.9202870,  37.2893110,  189.102),  -- Sinus Amoris
    (Sinus,         -5.4118980,  27.4884610,  219.139),  -- Sinus Asperitatis
    (Sinus,         10.9789640,  42.4723390,  159.027),  -- Sinus Concordiae
    (Sinus,         17.9920640,   2.0379110,   70.699),  -- Sinus Fidei
    (Sinus,         11.7195480,  17.8657890,  111.609),  -- Sinus Honoris
    (Sinus,         45.0101000, 328.3347180,  249.290),  -- Sinus Iridum
    (Sinus,         32.3640550, 358.1488700,  119.175),  -- Sinus Lunicus
    (Sinus,          1.6336650,   1.0269170,  286.674),  -- Sinus Medii
    (Sinus,         50.2612820, 309.1412460,  195.042),  -- Sinus Roris
    (Sinus,          1.1242120,  58.5197630,  126.651),  -- Sinus Successus
    (Crater,       -12.4922250, 299.4934970,   44.173),  -- Sirsalis
    (Crater,       -18.0351700,  96.1496420,  125.551),  -- Sklodowska
    (Crater,        -3.1539800,  89.0746220,   12.152),  -- Slocum
    (Crater,        60.2218740, 263.0923920,   84.264),  -- Smoluchowski
    (Crater,       -29.3331540,  55.7048490,   85.984),  -- Snellius
    (Crater,         0.1926770, 352.4737290,   27.965),  -- Sömmering
    (Crater,        -8.3319020,  64.9610760,   17.083),  -- Somerville
    (Crater,         8.7000260,  17.5955360,   16.987),  -- Sosigenes
    (Crater,        57.5807380, 309.0585090,  119.036),  -- South_Crater
    (Crater,       -46.3834970,  24.7276510,   30.860),  -- Spallanzani
    (Crater,        -4.3025430, 358.2254580,   25.572),  -- Spörer
    (Crater,        27.9111540, 358.7304550,   13.206),  -- Spurr
    (Crater,        10.4757980, 346.2263450,   68.481),  -- Stadius
    (Crater,       -48.7090500,  46.6562380,   63.279),  -- Steinheil
    (Crater,       -32.4902170,  54.1371620,   71.538),  -- Stevinus
    (Crater,         2.1499910,  66.9786180,   13.768),  -- Stewart
    (Crater,       -34.4929650,  31.9908470,   43.757),  -- Stiborius
    (Crater,       -41.2416400,   5.9265100,  129.874),  -- Stöfler
    (Crater,        52.3638630, 271.8940600,   53.853),  -- Stokes
    (Crater,        61.9381790,  54.4249040,   54.722),  -- Strabo
    (Crater,       -46.5783700, 349.2636360,   58.517),  -- Street
    (Crater,        23.4131560, 283.3476740,  164.338),  -- Struve
    (Crater,        19.6261600,  11.6788490,   11.605),  -- Sulpicius Gallus
    (Crater,        10.7554140, 268.3066830,   41.038),  -- Sundman
    (Crater,       -81.6806050,  65.1504900,   15.340),  -- Svedberg
    (Crater,        -5.4574610,  89.6704060,   24.845),  -- Swasey
    (Crater,        19.3462580,  53.4447780,   10.065),  -- Swift
    (Crater,        82.6492380, 278.7818760,   59.277),  -- Sylvester
    (Crater,        15.5369960, 330.8330360,   33.150),  -- T. Mayer
    (Crater,         5.0842980,  85.8244650,   42.577),  -- Tacchini
    (Crater,       -16.2049460,  18.9494750,   39.806),  -- Tacitus
    (Crater,        -2.4744560,  85.2971130,   12.364),  -- Talbot
    (Crater,       -56.4375540,  21.9220360,   28.068),  -- Tannerus
    (Crater,         5.5021940,  46.5425560,   57.316),  -- Taruntius
    (Crater,        -5.2822740,  16.6479010,   36.353),  -- Taylor
    (Crater,         9.4590680,  53.5188510,   33.992),  -- Tebbutt
    (Crater,         3.7612030,  11.8573630,   43.191),  -- Tempel
    (Crater,        61.7351490,  50.2674560,   30.747),  -- Thales
    (Crater,        37.0106120,   6.0554050,   24.593),  -- Theaetetus
    (Crater,       -22.0111430, 355.9842630,   54.638),  -- Thebit
    (Crater,        -2.4094080,  15.7930340,   17.615),  -- Theon Junior
    (Crater,        -0.8143130,  15.4191970,   18.019),  -- Theon Senior
    (Crater,       -11.4524450,  26.2847150,   98.592),  -- Theophilus
    (Crater,        62.9129000, 359.4529570,   32.809),  -- Timaeus
    (Crater,        26.7172320, 346.8998170,   34.142),  -- Timocharis
    (Crater,        21.4131300,  48.1666490,   34.631),  -- Tisserand
    (Crater,        -9.5185620, 344.0177560,   13.028),  -- Tolansky
    (Crater,        -4.7196640,  28.4036550,   30.868),  -- Torricelli
    (Crater,         3.4243550,  63.1872170,   17.676),  -- Townley
    (Crater,        28.3180540,  52.8495660,   44.161),  -- Tralles
    (Crater,         4.1809640,   3.5987660,   24.965),  -- Triesnecker
    (Crater,        -1.3960980, 346.7592160,   11.217),  -- Turner
    (Crater,       -43.2958060, 348.7846510,   85.294),  -- Tycho
    (Crater,         7.7083610,   1.3660100,   21.707),  -- Ukert
    (Crater,        32.6682410, 278.0434630,   57.038),  -- Ulugh Beigh
    (Crater,        27.9335020,  87.4286020,   39.294),  -- Urey
    (Vallis,        49.2087800,   3.6313810,  155.417),  -- Vallis Alpes
    (Vallis,       -45.5533480, 282.7701730,  206.789),  -- Vallis Baade
    (Vallis,        10.2489440, 271.1381220,   95.322),  -- Vallis Bohr
    (Vallis,       -38.4496610, 277.6796150,  287.924),  -- Vallis Bouvard
    (Vallis,        -7.3917840,  35.0384720,  106.280),  -- Vallis Capella
    (Vallis,       -43.9470230, 287.4103480,  145.084),  -- Vallis Inghirami
    (Vallis,       -26.1634990,  64.6373900,  110.498),  -- Vallis Palitzsch
    (Vallis,       -42.5125640,  51.6540370,  509.071),  -- Vallis Rheita
    (Vallis,        26.1555260, 308.4194760,  185.323),  -- Vallis Schröteri
    (Vallis,       -30.9262100,  57.8381970,  640.000),  -- Vallis Snellius
    (Crater,         9.3606980,  64.3545050,   22.924),  -- van Albada
    (Crater,        -1.7738220,  78.2014790,   33.482),  -- Van Vleck
    (Crater,        13.7807200, 276.0613810,   93.517),  -- Vasco da Gama
    (Crater,        43.6533050,  93.0137280,   44.993),  -- Vashakidze
    (Crater,       -45.4060860,  63.2659410,   73.506),  -- Vega
    (Crater,       -16.4576310,  61.5456110,  141.211),  -- Vendelinus
    (Crater,        33.8709590,  93.6785120,   97.808),  -- Vestine
    (Crater,       -29.3106310, 303.4731460,   87.162),  -- Vieta
    (Crater,         9.8788530,  83.7658080,   18.828),  -- Virchow
    (Crater,       -30.4193520, 322.4493900,   42.506),  -- Vitello
    (Crater,        17.6626940,  31.2796800,   30.944),  -- Vitruvius
    (Crater,       -53.3893850,  38.6902230,   89.208),  -- Vlacq
    (Crater,       -15.1071320,   5.8306840,   26.305),  -- Vogel
    (Crater,        53.9004740, 275.2343780,  117.147),  -- Volta
    (Crater,       -81.8013370,  61.8738360,   13.457),  -- von Baeyer
    (Crater,        -7.7495640,  71.7222470,   37.646),  -- von Behring
    (Crater,        41.0431150, 281.9155260,   61.828),  -- von Braun
    (Crater,        27.9058600, 271.8798760,   49.374),  -- Voskresenskiy
    (Crater,        65.4051550,   3.5194750,  170.532),  -- W. Bond
    (Crater,        20.2570700, 351.2508520,   25.715),  -- Wallace
    (Crater,       -33.2484620,   0.6156200,  134.227),  -- Walther
    (Crater,       -83.0776500,  53.7893190,   11.450),  -- Wapowski
    (Crater,       -49.5313030, 299.5614000,   84.689),  -- Wargentin
    (Crater,        -3.9814450,  87.3483810,   34.508),  -- Warner
    (Crater,       -49.5972260,  48.4772690,   66.543),  -- Watt
    (Crater,         8.8442930,  46.3078980,   15.551),  -- Watts
    (Crater,        -0.9819060,  59.9993120,   21.412),  -- Webb
    (Crater,        -1.2579890,  77.1472090,   31.321),  -- Weierstrass
    (Crater,       -58.3850390, 320.6638590,   34.847),  -- Weigel
    (Crater,       -27.5650520,  37.0614320,   32.010),  -- Weinek
    (Crater,       -31.7618380, 340.4126910,   66.555),  -- Weiss
    (Crater,       -28.0262730,   3.2926860,   70.591),  -- Werner
    (Crater,       -68.8843090,  90.7106200,   52.444),  -- Wexler
    (Crater,         4.1603990,  13.7337540,   13.046),  -- Whewell
    (Crater,        -6.0928840,  85.4335060,   52.883),  -- Widmannstätten
    (Crater,         9.0240030,  75.8279840,   12.308),  -- Wildt
    (Crater,       -43.2126850, 339.0594150,  100.827),  -- Wilhelm
    (Crater,       -29.5801070,  19.5767470,   59.440),  -- Wilkins
    (Crater,        42.0203070,  37.3056000,   36.358),  -- Williams
    (Crater,       -69.3285600, 317.1743520,   66.573),  -- Wilson
    (Crater,       -10.7601030, 315.5400440,   17.251),  -- Winthrop
    (Crater,       -38.2518450,  31.3530580,   28.071),  -- Wöhler
    (Crater,       -22.7856450, 343.3741740,   25.745),  -- Wolf
    (Crater,       -31.5478540, 273.2565890,   40.165),  -- Wright
    (Crater,       -23.8964310,  56.6192770,   58.383),  -- Wrottesley
    (Crater,       -34.0408710, 343.9435760,   86.769),  -- Wurzelbauer
    (Crater,        -1.4160660,  98.1009350,  103.402),  -- Wyld
    (Crater,        57.4888970, 277.9941500,  117.572),  -- Xenophanes
    (Crater,       -54.4180130, 281.0711850,   35.922),  -- Yakovkin
    (Crater,        14.6039370,  51.6974780,   34.937),  -- Yerkes
    (Crater,       -41.5418860,  50.9762870,   71.445),  -- Young
    (Crater,       -60.9182580,   5.2454500,   68.539),  -- Zach
    (Crater,         5.5099330,  40.2137400,   11.189),  -- Zähringer
    (Crater,       -31.9411860,  21.8893260,   78.919),  -- Zagut
    (Crater,         3.9551860,  94.1882880,   10.267),  -- Zasyadko
    (Crater,        45.1487570,  72.9822080,   66.776),  -- Zeno
    (Crater,        -7.9667530,  18.8992760,   47.694),  -- Zöllner
    (Crater,       -61.3822220, 309.3516220,   63.180),  -- Zucchius
    (Crater,       -17.1782630, 307.6256900,   35.292)]; -- Zupus

end Database.Moon;
