module languages;

import std.conv: to;
import std.traits: isSomeString;
import enum_ex;

alias Lang = Enum!_Lang;

/** Language Code according to ISO 639-1 plus computer languages.
    See_Also: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
    See_Also: http://www.mathguide.de/info/tools/languagecode.html
    See_Also: http://msdn.microsoft.com/en-us/library/ms533052(v=vs.85).aspx
*/
private enum _Lang
{
    unknown,
    nullValue = unknown,

    en, english = en,            /// English
    en_US, american_english = en_US,    /// American. English
    en_GB, british_english = en_GB,     /// British English
    en_CA, canadian_english = en_CA,    /// Canadian English
    // ac,                       /// TODO?
    // ace,                      /// TODO?
    // ai,                       /// TODO?
    // ain,                       /// TODO?
    af, afrikaans = af, afr = afrikaans, /// Afrikaans
    ar, arabic = ar,                       /// Arabic
    // ary,                       /// TODO?
    // arc,                       /// TODO?
    ae, avestan = ae,                       /// Avestan Iran (extinct)
    ak, akan = ak,                       /// Akan
    // akk,                      /// TODO?
    an, aragonese = an,     /// Aragonese
    // ang,                       /// TODO?
    as, assamese = as,            /// Assamese
    // ase,                       /// TODO?
    // ast,                       /// TODO?
    // ax,                       /// TODO?
    az, azerbaijani = az, azeri = azerbaijani,                       /// Azerbaijani (Azeri)
    hy, armenian = hy,                       /// Armenian: Armenia
    eu, basque = eu,                       /// Basque: Spain, France
    ba, baskhir = ba,                       /// Baskhir: Volga, Urals, Central Asia
    // ban,                      /// TODO?
    be, belarusian = be,                       /// Belarusian
    // bj,                       /// TODO?
    bn, bengali = bn,                      /// Bengali (Bangla): Bangladesh, India
    br, breton = br,                      /// Breton: France
    bs, bosnian = bs,                      /// Bosnian
    bg, bulgarian = bg, bul = bulgarian,                      /// Bulgarian
    bo, tibetan = bo,                      /// Tibetan
    // bp,                       /// TODO?
    // bt,                       /// TODO?
    my, burmese = my,                      /// Burmese: Myanmar
    zh, chinese = zh,                      /// Chinese (Mandarin, Putonghua): China, Singapore
    crh, crimean = crh,                     /// Crimean Tatar
    hr, croatian = hr,                      /// Croatian: Croatia
    // cr,                       /// TODO?
    ca, catalan = ca, valencian = catalan, cat = valencian,                     /// Catalan/Valencian (Spain)
    cy, welch = cy, welsh = welch, cym = welsh,                     /// Welch: Wales, Heddwch, Tangnefedd
    cs, czech = cs, ces = czech,                      /// Czech: Czech Republic
    // csb,                      /// TODO?
    da, danish = da, dan = danish,                      /// Danish: Denmark, Greenland
    // ds,                       /// TODO?
    // dsb,                      /// TODO?
    nl, dutch = nl, flemish = dutch,                      /// Dutch (Flemish): Netherlands, Belgium
    eo, esperanto = eo,                      /// Esperanto
    et, estonian = et,                      /// Estonian
    fi, finnish = fi, fin = finnish,                      /// Finnish: Finland, Sweden, Russia
    fj, fijian = fj,                      /// Fijian: Fiji
    fo, faroese = fo,                      /// Faroese (Faeroese): Faroe Islands
    // fu,                       /// TODO?
    fr, french = fr, fra = french,                      /// French: France, Belgium, Canada, Caribbean, West Africa, Polynesia
    fr_ch, swiss_french,                    /// French (Switzerland)
    gl, galician = gl, gallegan = galician,                      /// Galician (Gallegan): Spain, Portugal
    gv, manx = gv,                      /// Manx: Isle of Man
    de, german = de, deu = german,                      /// German: Germany, Austria, Switzerland, Liechtenstein, Italy, Belgium
    el, greek = el, ell = greek,     /// Greek: Greece, Cyprus
    ha, hausa = ha,                      /// Hausa: Nigeria
    haw, hawaiian = haw,                     /// Hawaiian: Hawaii
    he, hebrew = he,                      /// Hebrew: Israel
    // hs,                       /// TODO?
    // hsb,                      /// TODO?
    hi, hindi = hi,                      /// Hindi: India, Nepal, Uganda, Suriname
    hu, hungarian = hu,                      /// Hungarian: Hungary, Romania, Slovakia
    is_, icelandic = is_,                     /// Icelandic
    io, ido = io,                      /// Ido: Nigeria
    id, indonesian = id,                      /// Indonesian (Bahasa): Indonesia
    ga, irish = ga,                      /// Irish: Ireland
    it, italian = it, ita = italian,                      /// Italian: Italy, Switzerland
    ja, japanese = ja,                      /// Japanese, 日本語: Japan
    ka, georgian = ka,                      /// Georgian: Georgia
    ku, kurdish = ku,                      /// Kurdish: Kurdistan (Turkey, Syria, Iran, Iraq)
    kn, kannada = kn,                      /// Kannada: India
    kk, kazakh = kk,                      /// Kazakh: Kazakhstan
    km, khmer = km,                      /// Khmer: Cambodia
    ko, korean = ko,                      /// Korean: Korea
    ky, kirghiz = ky, kyrgyz = kirghiz,                      /// Kirghiz (Kyrgyz): Kirghizstan, China
    lo, lao = lo,                      /// Lao: Laos
    la, latin = la, lat = latin,                      /// Latin: Rome (extinct)
    lt, lithuanian = lt,                      /// Lithuanian: Lithuania
    lv, latvian = lv,                      /// Latvian: Latvia
    jbo, /// lojban = jbo,                     /// Lojban
    mk, macedonian = mk,                      /// Macedonian: Macedonia
    nan, southern_min = nan, min_nan = southern_min,                    /// Min Nan: https://en.wikipedia.org/wiki/Southern_Min
    mg, malagasy = mg, malgache = malagasy,                    /// Malagasy (Malgache): Madagascar
    mn, mongolian = mn,                      /// Mongolian: Mongolia
    ms, malay = ms, zsm = malay,             /// Malay: Malaysia
    mt, maltese = mt,                      /// Maltese: Malta
    ne, nepali = ne,                      /// Nepali: Nepal
    no, norwegian = no, nob = norwegian,  /// Norwegian: Norway
    ps, pashto = ps,                      /// Pashto: Afghanistan, Iran, Pakistan
    fa, persian = fa, farsi = persian,                     /// Persian (Farsi): Iran, Iraq, Afghanistan, Pakistan, Azerbaijan
    oc, occitan = oc,                      /// Occitan (Provençal, Languedocian): France
    pl, polish = pl,                      /// Polish
    pt, portuguese = pt, por = portuguese,                     /// Portuguese: Portugal, Brazil, Angola, Mozambique, Cape Verde, Guinea-Bissau
    pt_BR, brazilian_portuguese = pt_BR,                    /// Brazilian Portuguese
    ro, romanian = ro,                      /// Romanian: Romania, Hungary
    ru, russian = ru, rus = russian,                      /// Russian
    sa, sanskrit = sa,                      /// Sanskrit: India (extinct, liturgical)
    // sc,                    /// TODO?
    // scn,                   /// TODO?
    si, sinhalese = si,                      /// Sinhalese: Sri Lanka
    sm, samoan = sm,                      /// Samoan: Samoa
    sco, scots = sco,                     /// Scots: Scotland
    sq, albanian = sq,                      /// Albanian: Albania, Kosovo
    // se,                       /// TODO?
    // sy,                       /// TODO?
    // syc,                       /// TODO?
    te, telugu = te,                      /// Telugu: India
    tl, tagalog = tl, pilipino = tagalog,                      /// Tagalog (Pilipino): Philippines
    // tp,                       /// TODO?
    // tpi,                       /// TODO?
    gd, scottish_gaelic = gd,                      /// Scottish Gaelic: Scotland
    sr, serbian = sr,                      /// Serbian: Serbia, Montenegro, Bosnia
    sk, slovak = sk, slk = slovak,         /// Slovak: Slovak Republic
    sl, slovene = sl, slovenian = slovene, slv = slovenian, /// Slovene, Slovenian: Slovenia, Austria, Italy
    es, spanish = es, spa = spanish,                      /// Spanish
    // es_ar, spanishArgentina = es_ar,
    // es_bo, spanishBolivia = es_bo,
    sw, swahili = sw, swa = swahili, /// Swahili: East Africa
    sv, swedish = sv, swe = swedish,                       /// Swedish
    asv, ancientSwedish = asv,                       /// Ancient Swedish
    tg, tajik = tg,                      /// Tajik: Tajikistan, Afghanistan
    ta, tamil = ta, tam = tamil,         /// Tamil: India
    th, thai = th, tha = thai,                      /// Thai: Thailand
    tr, turkish = tr, tur = turkish,                /// Turkish: Turkey, Cyprus
    tk, turkmen = tk,                      /// Turkmen: Turkmenistan, Afghanistan
    uk, ukrainian = uk, ukr = ukrainian,   /// Ukrainian
    ur, urdu = ur,                      /// Urdu: Pakistan, India, Central Asia
    uz, uzbek = uz,                      /// Uzbek: Uzbekistan, Central Asia, China
    vi, vietnamese = vi, vie = vietnamese, /// Vietnamese: Viet Nam
    vo, volapuk = vo,                      /// Volapük
    wa, waloon = wa, wln = waloon,         /// Waloon: Belgium
    yi, yiddish = yi, yid = yiddish, /// Yiddish: Israel, USA, Russia

    aa, afar = aa,/// Afar	Ethiopia, Eritrea, Djibouti	Salaamata
    ab, abkhaz = ab,/// Abkhaz	Caucasus	needed!
    afa, luganda = afa,/// Luganda	Uganda	Emirembe
    akk, akkadian = akk,/// Akkadian	Mesopotamia (extinct)	(Salmu)
    alg, abenaki = alg,/// Abenaki	North America	Okikiamgenoka, Kamignokawôgan
    am, amharic = am,/// Amharic	Ethopia, Egypt	ሰላም (salām)
    ang, /// Anglo Saxon (Old English)	England (extinct)	ᚠᚱᛁᚦ (friþ), Frið
    arw, arawak = arw,/// Arawak	Suriname, Guyana, Venezuela	needed!
    arc, aramaic = arc,/// Aramaic (Syriac, Assyrian)	Iraq, Iran, Syria, and liturgical	ܫܠܡܐ (shlamaa)
    arn, mapundungun = arn,/// Mapundungun	Chile	Uvchin, Tügkülen
    art, /// lojban = art,/// Lojban	 	panpi
    ast, asturian = ast,/// Asturian	Spain	Paz
    at, haiti_creole = at, kreyol = haiti_creole, /// Haiti / Guiana Creole (Kreyol)	Caribbean	Lapè
    aus, olkola = aus,/// Olkola	Australia	Erray
    /// aus, /// Pintupi-Luritja	Australia	Yatanpa
    /// aus, /// Wagiman	Australia	Nyimbur-ma
    /// aus, /// Warlpiri	Australia	Iawa-nyinami
    av, avar = av,/// Avar	Caucasus	Рекъел (reqel)
    ay, aymara = ay,/// Aymara	Bolivia	Hacaña
    bai, dschang = bai, yemba = dschang, /// Dschang (Yemba)	Cameroon	Mbwɛ´né
    bas, basaa = bas,/// Basaa	Cameroon	SàN
    bat, old_prussian = bat, sudovian = old_prussian, jatvingian = sudovian, /// Old Prussian, Sudovian (Jatvingian)	Baltic (extinct)	Pakajan
    btk, batak = btk,/// Batak	Indonesia	Pardamean
    bem, bemba = bem,/// Bemba	Zambia	Mutenden, Ukwikala mu
    bh, bihari = bh,/// Bihari	India	needed!
    bi, bislama = bi,/// Bislama	Vanuatu	Pís
    bla, blackfoot = bla,/// Blackfoot	North America	Innaihtsi'iyi
    bm, bambara = bm,/// Bambara	Mali	Here, Errébé
    cai, /// Ch'ol (Tumbalá)	Chiapas (Mexico)	Ñʌch'chocoya
    car, caribe = car,/// Caribe	Venezuala, Suriname	needed!
    ce, chechen = ce,/// Chechen	Chechnya (Caucasus)	Машар (mashar)
    ceb, cebuano = ceb,/// Cebuano	Philippines	Kalinaw, Kahusayan
    ch, chamorro = ch,/// Chamorro	Guam	Minaggen
    cho, choctaw = cho,/// Choctaw	North America	Achukma
    chk, chuuk = chk,/// Chuuk	Truk	Kunammwey
    chr, cherokee = chr,/// Cherokee (Tsalagi)	North America	ᏙᎯᏱ (dohiyi)
    chy, cheyenne = chy,/// Cheyenne	North America	Nanomonsetôtse
    co, corsican = co,/// Corsican	Corsica (France)	Pace
    cop, coptic = cop,/// Coptic	Egypt (extinct) and liturgical	Ϩιρηνη (hirīnī)
    cpe, /// Afro-Seminole	North America	needed!
    cpf, cajun_french = cpf, acadian = cajun_french, /// Cajun French (Acadian, Kreyol Lwiziyen)	Louisiana (USA)	needed!
    cr, cree = cr,/// Cree	North America	Wetaskiwin, Papayatik
    crp, fanagolo = crp,/// Fanagolo	South Africa	Kutula
    cu, /// Old Church Slavonic	Eastern Europe (liturgical)	Ми́ръ (mírə)
    cv, chuvash = cv,/// Chuvash	Russia	needed!
    del, lenape = del,/// Lenape (Delaware)	North America	Achwangundowagan
    dua, duala = dua,/// Duala	Cameroon	Musango
    dra, brahui = dra,/// Brahui	Pakistan, Afghanistan, Iran	(âsûdaî)
    dv, divehi = dv,/// Divehi (Maldivian)	Maldives	ސުޅަ (sulha)
    dz, dzongkha = dz,/// Dzongkha	Bhutan	གཞི་བདེ (gzhi-bde)
    ee, ewe = ee,/// Ewe (Évé)	West Africa	Ŋutifafa
    efi, efik = efi,/// Efik	Nigeria	Emem
    enm, middle_english = enm, /// Middle English	England (extinct)	Pes, Pise, Pees, ...
    ff, fula = ff, falani = fula, /// Fula (Falani)	West Africa	Jam
    fiu, karelian = fiu,/// Karelian	Russia, Finland	Rauhu, Vienosti
    fon, benin = fon,/// Fon	Benin	Fifâ
    fro, /// Old French	France	Pais
    fur, friulian = fur, frulan = friulian, /// Friulian (Frulan)	Friuli (Italy)	Pâs
    fy, frisian = fy,/// Frisian	Germany, Netherlands	Frede
    gez, /// Ge'ez	Ethiopia, Eritrea (liturgical)	ሰላም (salām)
    gmh, /// Middle High German	Central Europe (extinct)	Vride
    gn, guarani = gn,/// Guarani	Bolivia, Paraguay, Brazil	Apĭrĭvé, Ñerane'i, Py'aguapy
    goh, /// Old High German	Central Europe (extinct)	Fridu
    got, gothic = got,/// Gothic	Central Europe (extinct)
    grc, ancient_greek = grc,/// Ancient Greek	Miditerranean (extinct) and liturgical	Εἰρήνη (eirḗnē)
    gu, gujarati = gu,/// Gujarati	India, Pakistan	શાંતિ (śānti)
    hil, hiligaynon = hil,/// Hiligaynon (Ilongo)	Philippines	Paghidait
    hmn, hmong = hmn,/// Hmong	China, Viet Nam, Laos, Thailand	Kev tiaj tus
    ho, hiri_motu = ho,/// Hiri Motu	Papua New Guinea	Taim billong sikan
    hz, herero = hz,/// Herero	Southern Africa	needed!
    ia, interlingua = ia,/// Interlingua	 	Pace
    ie, interlingue = ie,/// Interlingue	 	Pace
    ig, igbo = ig,/// Igbo (Ibo)	Nigeria	Udo
    ii, sichuan_yi = ii,/// Sichuan Yi	China	ꄮꐽ (te-njo)
    in_,
    ik, /// Iñupiaq	Alaska	Kiñuiñak, Tutqiun
    ilo, ilocano = ilo,/// Ilocano	Philippines	Kappia
    inh, ingush = inh,/// Ingush	Ingutia (Caucasus)	Машар (mashar)
    iu, inuktitut = iu,/// Inuktitut	Canada	ᓴᐃᒻᒪᓯᒪᓂᖅ (saimmasimaniq)
    jpr, /// Judeo-Persian (Bukharic)	Mideast, Central Asia	needed!
    jrb, /// Judeo-Arabic	Mideast, North Africa	needed!
    jv, javanese = jv,/// Javanese	Indonesia	Rukun
    kab, kabyle = kab,/// Kabyle	Algeria	Lahna
    kg, kongo = kg,/// Kongo	Congo	Kikœndi
    ki, kikuyu = ki, gikuyu = kikuyu, /// Kikuyu (Gikuyu)	Kenya	Thayu
    kj, kwanyama = kj, kuanyama = kwanyama, /// Kwanyama (Kuanyama)	Angola, Namibia	needed!
    kl, greenlandic = kl, kalaallisut = greenlandic, /// Greenlandic (Kalaallisut)	Greenland	Irqigsiniq, Erĸigsineĸ
    kos, kosraean = kos,/// Kosraean	Micronesia	Mihs
    kr, kanuri = kr,/// Kanuri	Niger, Nigeria	needed!
    ks, kashmiri = ks,/// Kashmiri	Kashmir (India, Pakistan)	امن (amn)
    kv, komi = kv,/// Komi	Russian Arctic	needed!
    kw, cornish = kw,/// Cornish	Cornwall (extinct)	Cres
    lad, ladino = lad,/// Ladino (Judeo-Spanish)	Turkey, Israel, North Africa	Pas, פאש
    lb, luxemburgish = lb,/// Luxemburgish (Lëtzebuergesch)	Luxembourg	Fridd, Fridden
    lg, ganda = lg,/// Ganda	Niger, Congo	needed!
    li, limburgan = li,/// Limburgan (Limburgish)	Belgium	Vreij
    ln, lingala = ln,/// Lingala	Congo	Kímía
    lol, mongo_nkundu = lol,/// Mongo-Nkundu	Congo	Bóoto
    loz, lozi = loz,/// Lozi	Zambia	Nala
    lu, luba_katanga = lu,/// Luba-Katanga	Niger, Congo	needed!
    man, mandinka = man,/// Mandinka	Senegal, Gambia, Guinea-Bissau	Kayiroo
    map, bisaya = map,/// Bisaya	Brunei	Kalinaw
    mh, marshallese = mh,/// Marshallese	Marshall Islands	Ainemon
    mi, maori = mi,/// Maori	New Zealand	Rangima'arie, Nohopuku, Rongo
    mic, micmac = mic,/// Micmac	North America	Wôntôkóde
    mis, ainu = mis,/// Ainu	Japan	アプンノ, あぷんの (apunno)
    ml, malayalam = ml,/// Malayalam	India	സമാധാനം (samaadhaanam)
    mno, manobo = mno,/// Manobo	Philippines	Linew
    mo, moldavian = mo,/// Moldavian (Moldavan)	Moldava	Pace, Паче (pace)
    mos, mossi = mos,/// Mossi (Moré)	Burkina Faso	Lâfí
    mr, marathi = mr,/// Marathi	India	शांतता (śāntātā), शांती (śāntī)
    mus, muskogee = mus,/// Muskogee (Creek)	North America	Ittimokla
    myn, kekchi = myn,/// Kekchi	Guatemala, Belize	Tuktuquil usilal
    na, nauruan = na,/// Nauruan	Nauru	Iow
    nah, /// Náhuatl (Aztec)	Mexico, Guatemala	Tlamatcanemiliztli, Mocehuia
    nd, north_ndebele = nd,/// North Ndebele	Zimbabwe	needed!
    nds, old_saxon = nds, saxony = old_saxon, /// Old Saxon	Saxony (extinct)	Friðu
    ng, ndonga = ng,/// Ndonga	Angola, Namibia	needed!
    non, old_norse = non,/// Old Norse	Scandinavia (extinct)	ᚠᚱᛁᚦᚱ, Friðr
    nr, south_ndebele = nr,/// South Ndebele	Zimbabwe, Botswana	needed!
    nv, navajo = nv,/// Navajo (Navaho)	North America	K'é, Hozo
    ny, chichewa = ny,/// Chichewa (Chewa, Nyanja)	Malawi	M'tendere
    // om, oromo = om,/// Oromo	Ethiopia, Kenya	Nagaya, ነገየ (nagaya)
    or, oriya = or,/// Oriya	India	needed!
        // os, ossetic = os, ossetian = ossetic, /// Ossetic (Ossetian)	Georgia, Russia	needed!
    oto, otomi = oto,/// Otomi	Mexico	Hmetho
    pa, panjabi = pa, punjab = panjabi, /// Panjabi (Punjab)	Pakistan, India	ਸ਼ਾਂਤੀ (śānti)
    // paa, /// Ekari	Indonesia	Muka-Muka
    // pag, /// Pangasinan	Philippines	Kareenan
    // pam, /// Pampangan (Kapampangan)	Philippines	Kapayapan
    // pap, /// Papiamentu	Netherlands Antilles	Pas
    // pau, /// Palauan	Palau	Búdech
    // pi, /// Pali	India	Sāma, Santi
    // qu, /// Quechua	Peru	Anka Kay, Qasikay, Aligu, Sonqo Tiaykuy
    // rap, /// Rapanui	Chile	Kiba-kiba, Pava
    // rm, /// Raeto-Romance (Romansch)	Switzerland	Pasch
    // rn, /// Rundi (Kirundi, Urundi)	Burundi	Amahoro
    // rom, /// Romany (Gypsy, Tsigane)	Europe, South America, etc	Smirom
    // rw, /// Kinyarwanda (Rwanda, Ruanda)	Rwanda	Nimuhóre, Amahoro
    // sd, /// Sindhi	Pakistan, India	शांति, شانت (śanti)
    // sg, /// Sango	Central African Republic	needed!
    // sn, /// Shona	Zimbabwe	Runyaro, Dendemaro
    // so, /// Somali	Somalia, Djibouti, Ethiopia	Nabáda
    // ss, /// Swati (Siswati)	Swaziland	Kuthála
    // st, /// Southern Sotho (Sesotho, Sesuthu)	South Africa	Kgotso, Khotso
    // su, /// Sundanese	Indonesia	needed!
    // ti, /// Tigrinya (Tigrigna)	Eritrea	ሰላም (salām)
    // tli, /// Tlingit	North America	Li-k'ei
    // tn, /// Tswana (Setswana)	Botswana	Khotso, Kagiso
    // to, /// Tonga	Zambia	Malino, Melino
    // ts, /// Tsonga	South Africa	needed!
    // tt, /// Tatar	Russia	Тынычлык (tınıçlık)
    // tw, /// Twi	West Africa	Asomdwee
    // ty, /// Tahitian	Tahiti	Hau
    // ug, /// Uighur (Uygur)	China, Central Asia	تىنچلىق (tinçlik)
    // ve, /// Venda	South Africa	needed!
    // wen, /// Upper Sorbian (Wendish)	Germany	Pokoj
    // wo, /// Wolof	West Africa	Jàmm
    // xh, /// Xhosa	South Africa, Botswana	Uxolo
    // yo, /// Yoruba	Nigeria, Benin, Togo	Alaáfía
    // ypk, /// Yu'pik	Alaska, Canada	Kiñuiñak
    // za, /// Chuang (Zhuang)	China	needed!
    // zap, /// Zapotec	Mexico	Layeni, Binlo
    // zu, /// Zulu	Southern Africa	Isithangami, Ukuthula

    /** International Phonetic Alphabet (IPA)
        https:///en.wikipedia.org/wiki/International_Phonetic_Alphabet */
    ipa,

    /// Programming Languages.
    c, firstFormal = c,
    cxx,
    objectiveC,
    objectiveCxx,
    cSharp,
    d,
    go,
    java,
    ada,
    rust,
    swift,
    fortran,
    modelica,

    lastFormal = modelica,

    /// Academic Languages
    math,                /// "Mathematics is the only truly universal language"
    physics,

    regularExpression, regexp = regularExpression,
}

/** Return true if $(D lang) is case-sensitive. */
bool hasCase(Lang lang) @safe pure @nogc nothrow
{
    import std.algorithm.comparison: among;
    with (Lang)
        return cast(bool)lang.among!(bg, ada);
}
alias isCaseSensitive = hasCase;

/** Return true if $(D lang) is a formal (computer) language. */
bool isFormal(Lang lang) @safe pure @nogc nothrow
{
    pragma(inline, true);
    with (Lang)
        return (lang >= firstFormal &&
                lang <= lastFormal);
}

/** TODO Remove when `__traits(documentation)` is merged
 */
string toSpoken(Lang lang, Lang spokenLang = Lang.init) @safe pure nothrow // TODO @nogc
{
    with (Lang)
        switch (lang)
        {
        case unknown: return `??`;
        case en: return `English`; // 英語
        case en_US: return `American English`;
        case en_GB: return `British English`;
        case en_CA: return `Canadian English`;
        case af: return `Afrikaans`;
        case ar: return `Arabic`;
        case ae: return `Avestan`;
        case ak: return `Akan`;
        case an: return `Aragonese`;
        case as: return `Assamese`;
        case az: return `Azerbaijani`;
        case hy: return `Armenian`;
        case eu: return `Basque`;
        case ba: return `Baskhir`;
        case be: return `Belarusian`;
        case bn: return `Bengali`;
        case br: return `Breton`;
        case bs: return `Bosnian`;
        case bg: return `Bulgarian`;
        case bo: return `Tibetan`;
        case my: return `Burmese`;
        case zh: return `Chinese Mandarin`;
        case crh: return `Crimean Tatar`;
        case hr: return `Croatian`;
        case ca: return `Catalan`;
        case cy: return `Welch`;
        case cs: return `Czech`;
        case da: return `Danish`;
        case nl: return `Dutch`;
        case eo: return `Esperanto`;
        case et: return `Estonian`;
        case fi: return `Finnish`;
        case fj: return `Fiji`;
        case fo: return `Faroese`;
        case fr: return `French`;
        case fr_ch: return `French (Switzerland)`;
        case gl: return `Galician`;
        case gv: return `Manx`;
        case de: return `German`;
        case el: return `Greek`;
        case grc: return `Ancient Greek`;
        case ha: return `Hausa`;
        case he: return `Hebrew`;
        case hi: return `Hindi`;
        case hu: return `Hungarian`;
        case is_: return `Icelandic`;
        case io: return `Ido`;
        case id: return `Indonesian`;
        case ga: return `Irish`;
        case it: return `Italian`;
        case ja: return `Japanese`; // 日本語
        case ka: return `Georgian`;
        case ku: return `Kurdish`;
        case kn: return `Kannada`;
        case kk: return `Kazakh`;
        case km: return `Khmer`;
        case ko: return `Korean`;
        case ky: return `Kyrgyz`;
        case lo: return `Lao`;
        case la: return `Latin`;
        case lt: return `Lithuanian`;
        case lv: return `Latvian`;
        case jbo: return `Lojban`;
        case mk: return `Macedonian`;
        case nan: return `Min Nan`;
        case mg: return `Malagasy`;
        case mn: return `Mongolian`;
        case ms: return `Malay`;
        case mt: return `Maltese`;
        case ne: return `Nepali`;
        case no: return `Norwegian`;
        case ps: return `Pashto`;
        case fa: return `Persian`;
        case oc: return `Occitan`;
        case pl: return `Polish`;
        case pt: return `Portuguese`;
        case pt_BR: return `Brazilian Portuguese`;
        case ro: return `Romanian`;
        case ru: return `Russian`;
        case sa: return `Sanskrit`;
        case si: return `Sinhalese`;
        case sm: return `Samoan`;
        case sco: return `Scots`;
        case sq: return `Albanian`;
        case te: return `Tegulu`;
        case tl: return `Tagalog`;
        case gd: return `Scottish Gaelic`;
        case sr: return `Serbian`;
        case sk: return `Slovak`;
        case sl: return `Slovene`;
        case es: return `Spanish`;
        case sw: return `Swahili`;
        case sv: return `Swedish`;
        case asv: return `Ancient Swedish`;
        case tg: return `Tajik`;
        case ta: return `Tamil`;
        case th: return `Thai`;
        case tr: return `Turkish`;
        case tk: return `Turkmen`;
        case uk: return `Ukrainian`;
        case ur: return `Urdu`;
        case uz: return `Uzbek`;
        case vi: return `Vietnamese`;
        case vo: return `Volapük`;
        case wa: return `Waloon`;
        case yi: return `Yiddish`;
        case akk: return `Akkadian`;
        case ce: return `Chechen`;
        case co: return `Corsican`;
        case fur: return `Friulian`;
        case gu: return `Gujarati`;
        case hil: return `Hiligaynon`;
        case haw: return `Hawaiian`;

        case ipa: return `International Phonetic Alphabet`;

        case c: return `C`;
        case cxx: return `C++`;
        case cSharp: return `C#`;
        case objectiveC: return `Objective-C`;
        case objectiveCxx: return `Objective-C++`;
        case d: return `D`;
        case go: return `Go`;
        case java: return `Java`;
        case ada: return `Ada`;
        case rust: return `Rust`;
        case swift: return `Swift`;
        case fortran: return `Fortran`;
        case modelica: return `Modelica`;

        case math: return `mathematics`;
        case physics: return `physics`;
        case regularExpression: return `regular expression`;

        default:
            try
            {
                return lang.to!(typeof(return));
            }
            catch (Exception e)
            {
                return `__unconvertiable__`;
            }
        }
}

Lang decodeLang(S)(S lang) @safe pure nothrow // @nogc
if (isSomeString!S)
{
    switch  (lang)
    {
    case `is`: return Lang(Lang.is_);
    case `in`: return Lang(Lang.in_);
    default:
        import conv_ex : toDefaulted;
        return typeof(return)(lang.toDefaulted!_Lang(_Lang.unknown));
    }
}

@safe pure unittest
{
    assert(`sv`.decodeLang == Lang.sv);
}

Lang decodeLangDefaulted(S)(S lang, Lang defaultLang) @safe pure nothrow
if (isSomeString!S)
{
    try
    {
        return decodeLang(lang);
    }
    catch (Exception e)
    {
        return defaultLang;
    }
}

///
@safe pure nothrow unittest
{
    assert(`_`.decodeLangDefaulted(Lang(_Lang.unknown)) == Lang.unknown);
    assert(`sv`.decodeLangDefaulted(Lang(_Lang.unknown)) == Lang.sv);
}

///
@safe pure nothrow /*TODO @nogc*/ unittest
{
    assert(Lang(_Lang.unknown).toSpoken == `??`);
    assert(Lang(_Lang.c).toSpoken == `C`);
    assert(Lang(_Lang.cxx).toSpoken == `C++`);
    assert(Lang(_Lang.d).toSpoken == `D`);
    assert(Lang(_Lang.java).toSpoken == `Java`);
}

string toHTML(Lang lang) @safe pure nothrow /*TODO @nogc*/
{
    return lang.toSpoken;
}

///
string toMathML(Lang lang) @safe pure nothrow /*TODO @nogc*/
{
    return lang.toHTML;
}

Lang language(string name) @safe pure nothrow @nogc
{
    switch (name)
    {
        case `C`:
            return Lang(_Lang.c);
        case `C++`:
            return Lang(_Lang.cxx);
        case `Objective-C`:
            return Lang(_Lang.objectiveC);
        case `D`:
            return Lang(_Lang.d);
        case `Java`:
            return Lang(_Lang.java);
        default:
            return Lang(_Lang.unknown);
    }
}

/** Markup Lang */
enum MarkupLang:ubyte
{
    unknown,                    // Unknown: ?
    HTML,
    MathML
}

/** Languages that capitalize all their nouns, not only proper ones.
 */
bool capitalizesNoun(Lang lang) @safe pure nothrow @nogc
{
    import std.algorithm.comparison : among;
    return cast(bool)lang.among!(Lang.de);
}
