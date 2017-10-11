/** Grow size to next prime number stored in table.
 * See also: https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/
 */
module prime_growth;

@safe pure nothrow @nogc:

/// modulo constant prime
size_t mod0(in size_t) { return 0UL; }
size_t mod2(in size_t hash) { return hash % 2UL; }
size_t mod3(in size_t hash) { return hash % 3UL; }
size_t mod5(in size_t hash) { return hash % 5UL; }
size_t mod7(in size_t hash) { return hash % 7UL; }
size_t mod11(in size_t hash) { return hash % 11UL; }
size_t mod13(in size_t hash) { return hash % 13UL; }
size_t mod17(in size_t hash) { return hash % 17UL; }
size_t mod23(in size_t hash) { return hash % 23UL; }
size_t mod29(in size_t hash) { return hash % 29UL; }
size_t mod37(in size_t hash) { return hash % 37UL; }
size_t mod47(in size_t hash) { return hash % 47UL; }
size_t mod59(in size_t hash) { return hash % 59UL; }
size_t mod73(in size_t hash) { return hash % 73UL; }
size_t mod97(in size_t hash) { return hash % 97UL; }
size_t mod127(in size_t hash) { return hash % 127UL; }
size_t mod151(in size_t hash) { return hash % 151UL; }
size_t mod197(in size_t hash) { return hash % 197UL; }
size_t mod251(in size_t hash) { return hash % 251UL; }
size_t mod313(in size_t hash) { return hash % 313UL; }
size_t mod397(in size_t hash) { return hash % 397UL; }
size_t mod499(in size_t hash) { return hash % 499UL; }
size_t mod631(in size_t hash) { return hash % 631UL; }
size_t mod797(in size_t hash) { return hash % 797UL; }
size_t mod1009(in size_t hash) { return hash % 1009UL; }
size_t mod1259(in size_t hash) { return hash % 1259UL; }
size_t mod1597(in size_t hash) { return hash % 1597UL; }
size_t mod2011(in size_t hash) { return hash % 2011UL; }
size_t mod2539(in size_t hash) { return hash % 2539UL; }
size_t mod3203(in size_t hash) { return hash % 3203UL; }
size_t mod4027(in size_t hash) { return hash % 4027UL; }
size_t mod5087(in size_t hash) { return hash % 5087UL; }
size_t mod6421(in size_t hash) { return hash % 6421UL; }
size_t mod8089(in size_t hash) { return hash % 8089UL; }
size_t mod10193(in size_t hash) { return hash % 10193UL; }
size_t mod12853(in size_t hash) { return hash % 12853UL; }
size_t mod16193(in size_t hash) { return hash % 16193UL; }
size_t mod20399(in size_t hash) { return hash % 20399UL; }
size_t mod25717(in size_t hash) { return hash % 25717UL; }
size_t mod32401(in size_t hash) { return hash % 32401UL; }
size_t mod40823(in size_t hash) { return hash % 40823UL; }
size_t mod51437(in size_t hash) { return hash % 51437UL; }
size_t mod64811(in size_t hash) { return hash % 64811UL; }
size_t mod81649(in size_t hash) { return hash % 81649UL; }
size_t mod102877(in size_t hash) { return hash % 102877UL; }
size_t mod129607(in size_t hash) { return hash % 129607UL; }
size_t mod163307(in size_t hash) { return hash % 163307UL; }
size_t mod205759(in size_t hash) { return hash % 205759UL; }
size_t mod259229(in size_t hash) { return hash % 259229UL; }
size_t mod326617(in size_t hash) { return hash % 326617UL; }
size_t mod411527(in size_t hash) { return hash % 411527UL; }
size_t mod518509(in size_t hash) { return hash % 518509UL; }
size_t mod653267(in size_t hash) { return hash % 653267UL; }
size_t mod823117(in size_t hash) { return hash % 823117UL; }
size_t mod1037059(in size_t hash) { return hash % 1037059UL; }
size_t mod1306601(in size_t hash) { return hash % 1306601UL; }
size_t mod1646237(in size_t hash) { return hash % 1646237UL; }
size_t mod2074129(in size_t hash) { return hash % 2074129UL; }
size_t mod2613229(in size_t hash) { return hash % 2613229UL; }
size_t mod3292489(in size_t hash) { return hash % 3292489UL; }
size_t mod4148279(in size_t hash) { return hash % 4148279UL; }
size_t mod5226491(in size_t hash) { return hash % 5226491UL; }
size_t mod6584983(in size_t hash) { return hash % 6584983UL; }
size_t mod8296553(in size_t hash) { return hash % 8296553UL; }
size_t mod10453007(in size_t hash) { return hash % 10453007UL; }
size_t mod13169977(in size_t hash) { return hash % 13169977UL; }
size_t mod16593127(in size_t hash) { return hash % 16593127UL; }
size_t mod20906033(in size_t hash) { return hash % 20906033UL; }
size_t mod26339969(in size_t hash) { return hash % 26339969UL; }
size_t mod33186281(in size_t hash) { return hash % 33186281UL; }
size_t mod41812097(in size_t hash) { return hash % 41812097UL; }
size_t mod52679969(in size_t hash) { return hash % 52679969UL; }
size_t mod66372617(in size_t hash) { return hash % 66372617UL; }
size_t mod83624237(in size_t hash) { return hash % 83624237UL; }
size_t mod105359939(in size_t hash) { return hash % 105359939UL; }
size_t mod132745199(in size_t hash) { return hash % 132745199UL; }
size_t mod167248483(in size_t hash) { return hash % 167248483UL; }
size_t mod210719881(in size_t hash) { return hash % 210719881UL; }
size_t mod265490441(in size_t hash) { return hash % 265490441UL; }
size_t mod334496971(in size_t hash) { return hash % 334496971UL; }
size_t mod421439783(in size_t hash) { return hash % 421439783UL; }
size_t mod530980861(in size_t hash) { return hash % 530980861UL; }
size_t mod668993977(in size_t hash) { return hash % 668993977UL; }
size_t mod842879579(in size_t hash) { return hash % 842879579UL; }
size_t mod1061961721(in size_t hash) { return hash % 1061961721UL; }
size_t mod1337987929(in size_t hash) { return hash % 1337987929UL; }
size_t mod1685759167(in size_t hash) { return hash % 1685759167UL; }
size_t mod2123923447(in size_t hash) { return hash % 2123923447UL; }
size_t mod2675975881(in size_t hash) { return hash % 2675975881UL; }
size_t mod3371518343(in size_t hash) { return hash % 3371518343UL; }
size_t mod4247846927(in size_t hash) { return hash % 4247846927UL; }
size_t mod5351951779(in size_t hash) { return hash % 5351951779UL; }
size_t mod6743036717(in size_t hash) { return hash % 6743036717UL; }
size_t mod8495693897(in size_t hash) { return hash % 8495693897UL; }
size_t mod10703903591(in size_t hash) { return hash % 10703903591UL; }
size_t mod13486073473(in size_t hash) { return hash % 13486073473UL; }
size_t mod16991387857(in size_t hash) { return hash % 16991387857UL; }
size_t mod21407807219(in size_t hash) { return hash % 21407807219UL; }
size_t mod26972146961(in size_t hash) { return hash % 26972146961UL; }
size_t mod33982775741(in size_t hash) { return hash % 33982775741UL; }
size_t mod42815614441(in size_t hash) { return hash % 42815614441UL; }
size_t mod53944293929(in size_t hash) { return hash % 53944293929UL; }
size_t mod67965551447(in size_t hash) { return hash % 67965551447UL; }
size_t mod85631228929(in size_t hash) { return hash % 85631228929UL; }
size_t mod107888587883(in size_t hash) { return hash % 107888587883UL; }
size_t mod135931102921(in size_t hash) { return hash % 135931102921UL; }
size_t mod171262457903(in size_t hash) { return hash % 171262457903UL; }
size_t mod215777175787(in size_t hash) { return hash % 215777175787UL; }
size_t mod271862205833(in size_t hash) { return hash % 271862205833UL; }
size_t mod342524915839(in size_t hash) { return hash % 342524915839UL; }
size_t mod431554351609(in size_t hash) { return hash % 431554351609UL; }
size_t mod543724411781(in size_t hash) { return hash % 543724411781UL; }
size_t mod685049831731(in size_t hash) { return hash % 685049831731UL; }
size_t mod863108703229(in size_t hash) { return hash % 863108703229UL; }
size_t mod1087448823553(in size_t hash) { return hash % 1087448823553UL; }
size_t mod1370099663459(in size_t hash) { return hash % 1370099663459UL; }
size_t mod1726217406467(in size_t hash) { return hash % 1726217406467UL; }
size_t mod2174897647073(in size_t hash) { return hash % 2174897647073UL; }
size_t mod2740199326961(in size_t hash) { return hash % 2740199326961UL; }
size_t mod3452434812973(in size_t hash) { return hash % 3452434812973UL; }
size_t mod4349795294267(in size_t hash) { return hash % 4349795294267UL; }
size_t mod5480398654009(in size_t hash) { return hash % 5480398654009UL; }
size_t mod6904869625999(in size_t hash) { return hash % 6904869625999UL; }
size_t mod8699590588571(in size_t hash) { return hash % 8699590588571UL; }
size_t mod10960797308051(in size_t hash) { return hash % 10960797308051UL; }
size_t mod13809739252051(in size_t hash) { return hash % 13809739252051UL; }
size_t mod17399181177241(in size_t hash) { return hash % 17399181177241UL; }
size_t mod21921594616111(in size_t hash) { return hash % 21921594616111UL; }
size_t mod27619478504183(in size_t hash) { return hash % 27619478504183UL; }
size_t mod34798362354533(in size_t hash) { return hash % 34798362354533UL; }
size_t mod43843189232363(in size_t hash) { return hash % 43843189232363UL; }
size_t mod55238957008387(in size_t hash) { return hash % 55238957008387UL; }
size_t mod69596724709081(in size_t hash) { return hash % 69596724709081UL; }
size_t mod87686378464759(in size_t hash) { return hash % 87686378464759UL; }
size_t mod110477914016779(in size_t hash) { return hash % 110477914016779UL; }
size_t mod139193449418173(in size_t hash) { return hash % 139193449418173UL; }
size_t mod175372756929481(in size_t hash) { return hash % 175372756929481UL; }
size_t mod220955828033581(in size_t hash) { return hash % 220955828033581UL; }
size_t mod278386898836457(in size_t hash) { return hash % 278386898836457UL; }
size_t mod350745513859007(in size_t hash) { return hash % 350745513859007UL; }
size_t mod441911656067171(in size_t hash) { return hash % 441911656067171UL; }
size_t mod556773797672909(in size_t hash) { return hash % 556773797672909UL; }
size_t mod701491027718027(in size_t hash) { return hash % 701491027718027UL; }
size_t mod883823312134381(in size_t hash) { return hash % 883823312134381UL; }
size_t mod1113547595345903(in size_t hash) { return hash % 1113547595345903UL; }
size_t mod1402982055436147(in size_t hash) { return hash % 1402982055436147UL; }
size_t mod1767646624268779(in size_t hash) { return hash % 1767646624268779UL; }
size_t mod2227095190691797(in size_t hash) { return hash % 2227095190691797UL; }
size_t mod2805964110872297(in size_t hash) { return hash % 2805964110872297UL; }
size_t mod3535293248537579(in size_t hash) { return hash % 3535293248537579UL; }
size_t mod4454190381383713(in size_t hash) { return hash % 4454190381383713UL; }
size_t mod5611928221744609(in size_t hash) { return hash % 5611928221744609UL; }
size_t mod7070586497075177(in size_t hash) { return hash % 7070586497075177UL; }
size_t mod8908380762767489(in size_t hash) { return hash % 8908380762767489UL; }
size_t mod11223856443489329(in size_t hash) { return hash % 11223856443489329UL; }
size_t mod14141172994150357(in size_t hash) { return hash % 14141172994150357UL; }
size_t mod17816761525534927(in size_t hash) { return hash % 17816761525534927UL; }
size_t mod22447712886978529(in size_t hash) { return hash % 22447712886978529UL; }
size_t mod28282345988300791(in size_t hash) { return hash % 28282345988300791UL; }
size_t mod35633523051069991(in size_t hash) { return hash % 35633523051069991UL; }
size_t mod44895425773957261(in size_t hash) { return hash % 44895425773957261UL; }
size_t mod56564691976601587(in size_t hash) { return hash % 56564691976601587UL; }
size_t mod71267046102139967(in size_t hash) { return hash % 71267046102139967UL; }
size_t mod89790851547914507(in size_t hash) { return hash % 89790851547914507UL; }
size_t mod113129383953203213(in size_t hash) { return hash % 113129383953203213UL; }
size_t mod142534092204280003(in size_t hash) { return hash % 142534092204280003UL; }
size_t mod179581703095829107(in size_t hash) { return hash % 179581703095829107UL; }
size_t mod226258767906406483(in size_t hash) { return hash % 226258767906406483UL; }
size_t mod285068184408560057(in size_t hash) { return hash % 285068184408560057UL; }
size_t mod359163406191658253(in size_t hash) { return hash % 359163406191658253UL; }
size_t mod452517535812813007(in size_t hash) { return hash % 452517535812813007UL; }
size_t mod570136368817120201(in size_t hash) { return hash % 570136368817120201UL; }
size_t mod718326812383316683(in size_t hash) { return hash % 718326812383316683UL; }
size_t mod905035071625626043(in size_t hash) { return hash % 905035071625626043UL; }
size_t mod1140272737634240411(in size_t hash) { return hash % 1140272737634240411UL; }
size_t mod1436653624766633509(in size_t hash) { return hash % 1436653624766633509UL; }
size_t mod1810070143251252131(in size_t hash) { return hash % 1810070143251252131UL; }
size_t mod2280545475268481167(in size_t hash) { return hash % 2280545475268481167UL; }
size_t mod2873307249533267101(in size_t hash) { return hash % 2873307249533267101UL; }
size_t mod3620140286502504283(in size_t hash) { return hash % 3620140286502504283UL; }
size_t mod4561090950536962147(in size_t hash) { return hash % 4561090950536962147UL; }
size_t mod5746614499066534157(in size_t hash) { return hash % 5746614499066534157UL; }
size_t mod7240280573005008577(in size_t hash) { return hash % 7240280573005008577UL; }
size_t mod9122181901073924329(in size_t hash) { return hash % 9122181901073924329UL; }
size_t mod11493228998133068689(in size_t hash) { return hash % 11493228998133068689UL; }
size_t mod14480561146010017169(in size_t hash) { return hash % 14480561146010017169UL; }
size_t mod18446744073709551557(in size_t hash) { return hash % 18446744073709551557UL; }

static immutable primeModuloFunctions = [
    &mod0, &mod2, &mod3, &mod5, &mod7, &mod11, &mod13, &mod17, &mod23, &mod29, &mod37,
    &mod47, &mod59, &mod73, &mod97, &mod127, &mod151, &mod197, &mod251, &mod313, &mod397,
    &mod499, &mod631, &mod797, &mod1009, &mod1259, &mod1597, &mod2011, &mod2539, &mod3203,
    &mod4027, &mod5087, &mod6421, &mod8089, &mod10193, &mod12853, &mod16193, &mod20399,
    &mod25717, &mod32401, &mod40823, &mod51437, &mod64811, &mod81649, &mod102877,
    &mod129607, &mod163307, &mod205759, &mod259229, &mod326617, &mod411527, &mod518509,
    &mod653267, &mod823117, &mod1037059, &mod1306601, &mod1646237, &mod2074129,
    &mod2613229, &mod3292489, &mod4148279, &mod5226491, &mod6584983, &mod8296553,
    &mod10453007, &mod13169977, &mod16593127, &mod20906033, &mod26339969, &mod33186281,
    &mod41812097, &mod52679969, &mod66372617, &mod83624237, &mod105359939, &mod132745199,
    &mod167248483, &mod210719881, &mod265490441, &mod334496971, &mod421439783,
    &mod530980861, &mod668993977, &mod842879579, &mod1061961721, &mod1337987929,
    &mod1685759167, &mod2123923447, &mod2675975881, &mod3371518343, &mod4247846927,
    &mod5351951779, &mod6743036717, &mod8495693897, &mod10703903591, &mod13486073473,
    &mod16991387857, &mod21407807219, &mod26972146961, &mod33982775741, &mod42815614441,
    &mod53944293929, &mod67965551447, &mod85631228929, &mod107888587883, &mod135931102921,
    &mod171262457903, &mod215777175787, &mod271862205833, &mod342524915839,
    &mod431554351609, &mod543724411781, &mod685049831731, &mod863108703229,
    &mod1087448823553, &mod1370099663459, &mod1726217406467, &mod2174897647073,
    &mod2740199326961, &mod3452434812973, &mod4349795294267, &mod5480398654009,
    &mod6904869625999, &mod8699590588571, &mod10960797308051, &mod13809739252051,
    &mod17399181177241, &mod21921594616111, &mod27619478504183, &mod34798362354533,
    &mod43843189232363, &mod55238957008387, &mod69596724709081, &mod87686378464759,
    &mod110477914016779, &mod139193449418173, &mod175372756929481, &mod220955828033581,
    &mod278386898836457, &mod350745513859007, &mod441911656067171, &mod556773797672909,
    &mod701491027718027, &mod883823312134381, &mod1113547595345903, &mod1402982055436147,
    &mod1767646624268779, &mod2227095190691797, &mod2805964110872297, &mod3535293248537579,
    &mod4454190381383713, &mod5611928221744609, &mod7070586497075177, &mod8908380762767489,
    &mod11223856443489329, &mod14141172994150357, &mod17816761525534927,
    &mod22447712886978529, &mod28282345988300791, &mod35633523051069991,
    &mod44895425773957261, &mod56564691976601587, &mod71267046102139967,
    &mod89790851547914507, &mod113129383953203213, &mod142534092204280003,
    &mod179581703095829107, &mod226258767906406483, &mod285068184408560057,
    &mod359163406191658253, &mod452517535812813007, &mod570136368817120201,
    &mod718326812383316683, &mod905035071625626043, &mod1140272737634240411,
    &mod1436653624766633509, &mod1810070143251252131, &mod2280545475268481167,
    &mod2873307249533267101, &mod3620140286502504283, &mod4561090950536962147,
    &mod5746614499066534157, &mod7240280573005008577, &mod9122181901073924329,
    &mod11493228998133068689, &mod14480561146010017169, &mod18446744073709551557
    ];

size_t primeModuloHashToIndex(in size_t primeIndex,
                              in size_t hash)
{
    return primeModuloFunctions[primeIndex](hash);
}

unittest
{
    assert(primeModuloHashToIndex(3, 5) == 0); // modulo 5
    assert(primeModuloHashToIndex(4, 9) == 2); // modulo 7
}

// ubyte next_size_over(size_t & size) const
// {
//     // prime numbers generated by the following method:
//     // 1. start with a prime p = 2
//     // 2. go to wolfram alpha and get p = NextPrime(2 * p)
//     // 3. repeat 2. until you overflow 64 bits
//     // you now have large gaps which you would hit if somebody called reserve() with an unlucky number.
//     // 4. to fill the gaps for every prime p go to wolfram alpha and get ClosestPrime(p * 2^(1/3)) and ClosestPrime(p * 2^(2/3)) and put those in the gaps
//     // 5. get PrevPrime(2^64) and put it at the end
//     static immutable const size_t prime_list[] =
//     {
//         2UL, 3UL, 5UL, 7UL, 11UL, 13UL, 17UL, 23UL, 29UL, 37UL, 47UL,
//         59UL, 73UL, 97UL, 127UL, 151UL, 197UL, 251UL, 313UL, 397UL,
//         499UL, 631UL, 797UL, 1009UL, 1259UL, 1597UL, 2011UL, 2539UL,
//         3203UL, 4027UL, 5087UL, 6421UL, 8089UL, 10193UL, 12853UL, 16193UL,
//         20399UL, 25717UL, 32401UL, 40823UL, 51437UL, 64811UL, 81649UL,
//         102877UL, 129607UL, 163307UL, 205759UL, 259229UL, 326617UL,
//         411527UL, 518509UL, 653267UL, 823117UL, 1037059UL, 1306601UL,
//         1646237UL, 2074129UL, 2613229UL, 3292489UL, 4148279UL, 5226491UL,
//         6584983UL, 8296553UL, 10453007UL, 13169977UL, 16593127UL, 20906033UL,
//         26339969UL, 33186281UL, 41812097UL, 52679969UL, 66372617UL,
//         83624237UL, 105359939UL, 132745199UL, 167248483UL, 210719881UL,
//         265490441UL, 334496971UL, 421439783UL, 530980861UL, 668993977UL,
//         842879579UL, 1061961721UL, 1337987929UL, 1685759167UL, 2123923447UL,
//         2675975881UL, 3371518343UL, 4247846927UL, 5351951779UL, 6743036717UL,
//         8495693897UL, 10703903591UL, 13486073473UL, 16991387857UL,
//         21407807219UL, 26972146961UL, 33982775741UL, 42815614441UL,
//         53944293929UL, 67965551447UL, 85631228929UL, 107888587883UL,
//         135931102921UL, 171262457903UL, 215777175787UL, 271862205833UL,
//         342524915839UL, 431554351609UL, 543724411781UL, 685049831731UL,
//         863108703229UL, 1087448823553UL, 1370099663459UL, 1726217406467UL,
//         2174897647073UL, 2740199326961UL, 3452434812973UL, 4349795294267UL,
//         5480398654009UL, 6904869625999UL, 8699590588571UL, 10960797308051UL,
//         13809739252051UL, 17399181177241UL, 21921594616111UL, 27619478504183UL,
//         34798362354533UL, 43843189232363UL, 55238957008387UL, 69596724709081UL,
//         87686378464759UL, 110477914016779UL, 139193449418173UL,
//         175372756929481UL, 220955828033581UL, 278386898836457UL,
//         350745513859007UL, 441911656067171UL, 556773797672909UL,
//         701491027718027UL, 883823312134381UL, 1113547595345903UL,
//         1402982055436147UL, 1767646624268779UL, 2227095190691797UL,
//         2805964110872297UL, 3535293248537579UL, 4454190381383713UL,
//         5611928221744609UL, 7070586497075177UL, 8908380762767489UL,
//         11223856443489329UL, 14141172994150357UL, 17816761525534927UL,
//         22447712886978529UL, 28282345988300791UL, 35633523051069991UL,
//         44895425773957261UL, 56564691976601587UL, 71267046102139967UL,
//         89790851547914507UL, 113129383953203213UL, 142534092204280003UL,
//         179581703095829107UL, 226258767906406483UL, 285068184408560057UL,
//         359163406191658253UL, 452517535812813007UL, 570136368817120201UL,
//         718326812383316683UL, 905035071625626043UL, 1140272737634240411UL,
//         1436653624766633509UL, 1810070143251252131UL, 2280545475268481167UL,
//         2873307249533267101UL, 3620140286502504283UL, 4561090950536962147UL,
//         5746614499066534157UL, 7240280573005008577UL, 9122181901073924329UL,
//         11493228998133068689UL, 14480561146010017169UL, 18446744073709551557UL
//     };
//     const size_t * found = std::lower_bound(std::begin(prime_list), std::end(prime_list) - 1, size);
//     size = *found;
//     return cast(typeof(return))(1 + found - prime_list);
// }
// void commit(uint8_t new_prime_index)
// {
//     prime_index = new_prime_index;
// }
// void reset()
// {
//     prime_index = 0;
// }

// private:
// uint8_t prime_index = 0;
// };

// struct power_of_two_hash_policy
// {
//     size_t primeModuloHashToIndex(size_t hash, size_t num_slots_minus_one) const
//     {
//         return hash & num_slots_minus_one;
//     }
//     int8_t next_size_over(size_t & size) const
//     {
//         size = detailv3::next_power_of_two(size);
//         return 0;
//     }
//     void commit(int8_t)
//     {
//     }
//     void reset()
//     {
//     }
