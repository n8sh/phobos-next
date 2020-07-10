// version = usePhobos;

version(usePhobos)
{
    import std.typecons : Typedef;
}
else
{
    struct Typedef(T, T init = T.init, string cookie=null)
    {
        private T _payload = init;

        // https://issues.dlang.org/show_bug.cgi?id=18415
        // prevent default construction if original type does too.
        static if ((is(T == struct) ||
                    is(T == union)) &&
                   !is(typeof({T t;})))
        {
            @disable this();
        }

        this(T init)
        {
            _payload = init;
        }

        this(Typedef tdef)
        {
            this(tdef._payload);
        }
        alias _payload this;
    }
}

alias X1 = Typedef!(float, float.init, "X1");
alias X2 = Typedef!(float, float.init, "X2");
alias X3 = Typedef!(float, float.init, "X3");
alias X4 = Typedef!(float, float.init, "X4");
alias X5 = Typedef!(float, float.init, "X5");
alias X6 = Typedef!(float, float.init, "X6");
alias X7 = Typedef!(float, float.init, "X7");
alias X8 = Typedef!(float, float.init, "X8");
alias X9 = Typedef!(float, float.init, "X9");
alias X10 = Typedef!(float, float.init, "X10");
alias X11 = Typedef!(float, float.init, "X11");
alias X12 = Typedef!(float, float.init, "X12");
alias X13 = Typedef!(float, float.init, "X13");
alias X14 = Typedef!(float, float.init, "X14");
alias X15 = Typedef!(float, float.init, "X15");
alias X16 = Typedef!(float, float.init, "X16");
alias X17 = Typedef!(float, float.init, "X17");
alias X18 = Typedef!(float, float.init, "X18");
alias X19 = Typedef!(float, float.init, "X19");
alias X20 = Typedef!(float, float.init, "X20");
alias X21 = Typedef!(float, float.init, "X21");
alias X22 = Typedef!(float, float.init, "X22");
alias X23 = Typedef!(float, float.init, "X23");
alias X24 = Typedef!(float, float.init, "X24");
alias X25 = Typedef!(float, float.init, "X25");
alias X26 = Typedef!(float, float.init, "X26");
alias X27 = Typedef!(float, float.init, "X27");
alias X28 = Typedef!(float, float.init, "X28");
alias X29 = Typedef!(float, float.init, "X29");
alias X30 = Typedef!(float, float.init, "X30");
alias X31 = Typedef!(float, float.init, "X31");
alias X32 = Typedef!(float, float.init, "X32");
alias X33 = Typedef!(float, float.init, "X33");
alias X34 = Typedef!(float, float.init, "X34");
alias X35 = Typedef!(float, float.init, "X35");
alias X36 = Typedef!(float, float.init, "X36");
alias X37 = Typedef!(float, float.init, "X37");
alias X38 = Typedef!(float, float.init, "X38");
alias X39 = Typedef!(float, float.init, "X39");
alias X40 = Typedef!(float, float.init, "X40");
alias X41 = Typedef!(float, float.init, "X41");
alias X42 = Typedef!(float, float.init, "X42");
alias X43 = Typedef!(float, float.init, "X43");
alias X44 = Typedef!(float, float.init, "X44");
alias X45 = Typedef!(float, float.init, "X45");
alias X46 = Typedef!(float, float.init, "X46");
alias X47 = Typedef!(float, float.init, "X47");
alias X48 = Typedef!(float, float.init, "X48");
alias X49 = Typedef!(float, float.init, "X49");
alias X50 = Typedef!(float, float.init, "X50");
alias X51 = Typedef!(float, float.init, "X51");
alias X52 = Typedef!(float, float.init, "X52");
alias X53 = Typedef!(float, float.init, "X53");
alias X54 = Typedef!(float, float.init, "X54");
alias X55 = Typedef!(float, float.init, "X55");
alias X56 = Typedef!(float, float.init, "X56");
alias X57 = Typedef!(float, float.init, "X57");
alias X58 = Typedef!(float, float.init, "X58");
alias X59 = Typedef!(float, float.init, "X59");
alias X60 = Typedef!(float, float.init, "X60");
alias X61 = Typedef!(float, float.init, "X61");
alias X62 = Typedef!(float, float.init, "X62");
alias X63 = Typedef!(float, float.init, "X63");
alias X64 = Typedef!(float, float.init, "X64");
alias X65 = Typedef!(float, float.init, "X65");
alias X66 = Typedef!(float, float.init, "X66");
alias X67 = Typedef!(float, float.init, "X67");
alias X68 = Typedef!(float, float.init, "X68");
alias X69 = Typedef!(float, float.init, "X69");
alias X70 = Typedef!(float, float.init, "X70");
alias X71 = Typedef!(float, float.init, "X71");
alias X72 = Typedef!(float, float.init, "X72");
alias X73 = Typedef!(float, float.init, "X73");
alias X74 = Typedef!(float, float.init, "X74");
alias X75 = Typedef!(float, float.init, "X75");
alias X76 = Typedef!(float, float.init, "X76");
alias X77 = Typedef!(float, float.init, "X77");
alias X78 = Typedef!(float, float.init, "X78");
alias X79 = Typedef!(float, float.init, "X79");
alias X80 = Typedef!(float, float.init, "X80");
alias X81 = Typedef!(float, float.init, "X81");
alias X82 = Typedef!(float, float.init, "X82");
alias X83 = Typedef!(float, float.init, "X83");
alias X84 = Typedef!(float, float.init, "X84");
alias X85 = Typedef!(float, float.init, "X85");
alias X86 = Typedef!(float, float.init, "X86");
alias X87 = Typedef!(float, float.init, "X87");
alias X88 = Typedef!(float, float.init, "X88");
alias X89 = Typedef!(float, float.init, "X89");
alias X90 = Typedef!(float, float.init, "X90");
alias X91 = Typedef!(float, float.init, "X91");
alias X92 = Typedef!(float, float.init, "X92");
alias X93 = Typedef!(float, float.init, "X93");
alias X94 = Typedef!(float, float.init, "X94");
alias X95 = Typedef!(float, float.init, "X95");
alias X96 = Typedef!(float, float.init, "X96");
alias X97 = Typedef!(float, float.init, "X97");
alias X98 = Typedef!(float, float.init, "X98");
alias X99 = Typedef!(float, float.init, "X99");
alias X100 = Typedef!(float, float.init, "X100");
alias X101 = Typedef!(float, float.init, "X101");
alias X102 = Typedef!(float, float.init, "X102");
alias X103 = Typedef!(float, float.init, "X103");
alias X104 = Typedef!(float, float.init, "X104");
alias X105 = Typedef!(float, float.init, "X105");
alias X106 = Typedef!(float, float.init, "X106");
alias X107 = Typedef!(float, float.init, "X107");
alias X108 = Typedef!(float, float.init, "X108");
alias X109 = Typedef!(float, float.init, "X109");
alias X110 = Typedef!(float, float.init, "X110");
alias X111 = Typedef!(float, float.init, "X111");
alias X112 = Typedef!(float, float.init, "X112");
alias X113 = Typedef!(float, float.init, "X113");
alias X114 = Typedef!(float, float.init, "X114");
alias X115 = Typedef!(float, float.init, "X115");
alias X116 = Typedef!(float, float.init, "X116");
alias X117 = Typedef!(float, float.init, "X117");
alias X118 = Typedef!(float, float.init, "X118");
alias X119 = Typedef!(float, float.init, "X119");
alias X120 = Typedef!(float, float.init, "X120");
alias X121 = Typedef!(float, float.init, "X121");
alias X122 = Typedef!(float, float.init, "X122");
alias X123 = Typedef!(float, float.init, "X123");
alias X124 = Typedef!(float, float.init, "X124");
alias X125 = Typedef!(float, float.init, "X125");
alias X126 = Typedef!(float, float.init, "X126");
alias X127 = Typedef!(float, float.init, "X127");
alias X128 = Typedef!(float, float.init, "X128");
alias X129 = Typedef!(float, float.init, "X129");
alias X130 = Typedef!(float, float.init, "X130");
alias X131 = Typedef!(float, float.init, "X131");
alias X132 = Typedef!(float, float.init, "X132");
alias X133 = Typedef!(float, float.init, "X133");
alias X134 = Typedef!(float, float.init, "X134");
alias X135 = Typedef!(float, float.init, "X135");
alias X136 = Typedef!(float, float.init, "X136");
alias X137 = Typedef!(float, float.init, "X137");
alias X138 = Typedef!(float, float.init, "X138");
alias X139 = Typedef!(float, float.init, "X139");
alias X140 = Typedef!(float, float.init, "X140");
alias X141 = Typedef!(float, float.init, "X141");
alias X142 = Typedef!(float, float.init, "X142");
alias X143 = Typedef!(float, float.init, "X143");
alias X144 = Typedef!(float, float.init, "X144");
alias X145 = Typedef!(float, float.init, "X145");
alias X146 = Typedef!(float, float.init, "X146");
alias X147 = Typedef!(float, float.init, "X147");
alias X148 = Typedef!(float, float.init, "X148");
alias X149 = Typedef!(float, float.init, "X149");
alias X150 = Typedef!(float, float.init, "X150");
alias X151 = Typedef!(float, float.init, "X151");
alias X152 = Typedef!(float, float.init, "X152");
alias X153 = Typedef!(float, float.init, "X153");
alias X154 = Typedef!(float, float.init, "X154");
alias X155 = Typedef!(float, float.init, "X155");
alias X156 = Typedef!(float, float.init, "X156");
alias X157 = Typedef!(float, float.init, "X157");
alias X158 = Typedef!(float, float.init, "X158");
alias X159 = Typedef!(float, float.init, "X159");
alias X160 = Typedef!(float, float.init, "X160");
alias X161 = Typedef!(float, float.init, "X161");
alias X162 = Typedef!(float, float.init, "X162");
alias X163 = Typedef!(float, float.init, "X163");
alias X164 = Typedef!(float, float.init, "X164");
alias X165 = Typedef!(float, float.init, "X165");
alias X166 = Typedef!(float, float.init, "X166");
alias X167 = Typedef!(float, float.init, "X167");
alias X168 = Typedef!(float, float.init, "X168");
alias X169 = Typedef!(float, float.init, "X169");
alias X170 = Typedef!(float, float.init, "X170");
alias X171 = Typedef!(float, float.init, "X171");
alias X172 = Typedef!(float, float.init, "X172");
alias X173 = Typedef!(float, float.init, "X173");
alias X174 = Typedef!(float, float.init, "X174");
alias X175 = Typedef!(float, float.init, "X175");
alias X176 = Typedef!(float, float.init, "X176");
alias X177 = Typedef!(float, float.init, "X177");
alias X178 = Typedef!(float, float.init, "X178");
alias X179 = Typedef!(float, float.init, "X179");
alias X180 = Typedef!(float, float.init, "X180");
alias X181 = Typedef!(float, float.init, "X181");
alias X182 = Typedef!(float, float.init, "X182");
alias X183 = Typedef!(float, float.init, "X183");
alias X184 = Typedef!(float, float.init, "X184");
alias X185 = Typedef!(float, float.init, "X185");
alias X186 = Typedef!(float, float.init, "X186");
alias X187 = Typedef!(float, float.init, "X187");
alias X188 = Typedef!(float, float.init, "X188");
alias X189 = Typedef!(float, float.init, "X189");
alias X190 = Typedef!(float, float.init, "X190");
alias X191 = Typedef!(float, float.init, "X191");
alias X192 = Typedef!(float, float.init, "X192");
alias X193 = Typedef!(float, float.init, "X193");
alias X194 = Typedef!(float, float.init, "X194");
alias X195 = Typedef!(float, float.init, "X195");
alias X196 = Typedef!(float, float.init, "X196");
alias X197 = Typedef!(float, float.init, "X197");
alias X198 = Typedef!(float, float.init, "X198");
alias X199 = Typedef!(float, float.init, "X199");
alias X200 = Typedef!(float, float.init, "X200");
alias X201 = Typedef!(float, float.init, "X201");
alias X202 = Typedef!(float, float.init, "X202");
alias X203 = Typedef!(float, float.init, "X203");
alias X204 = Typedef!(float, float.init, "X204");
alias X205 = Typedef!(float, float.init, "X205");
alias X206 = Typedef!(float, float.init, "X206");
alias X207 = Typedef!(float, float.init, "X207");
alias X208 = Typedef!(float, float.init, "X208");
alias X209 = Typedef!(float, float.init, "X209");
alias X210 = Typedef!(float, float.init, "X210");
alias X211 = Typedef!(float, float.init, "X211");
alias X212 = Typedef!(float, float.init, "X212");
alias X213 = Typedef!(float, float.init, "X213");
alias X214 = Typedef!(float, float.init, "X214");
alias X215 = Typedef!(float, float.init, "X215");
alias X216 = Typedef!(float, float.init, "X216");
alias X217 = Typedef!(float, float.init, "X217");
alias X218 = Typedef!(float, float.init, "X218");
alias X219 = Typedef!(float, float.init, "X219");
alias X220 = Typedef!(float, float.init, "X220");
alias X221 = Typedef!(float, float.init, "X221");
alias X222 = Typedef!(float, float.init, "X222");
alias X223 = Typedef!(float, float.init, "X223");
alias X224 = Typedef!(float, float.init, "X224");
alias X225 = Typedef!(float, float.init, "X225");
alias X226 = Typedef!(float, float.init, "X226");
alias X227 = Typedef!(float, float.init, "X227");
alias X228 = Typedef!(float, float.init, "X228");
alias X229 = Typedef!(float, float.init, "X229");
alias X230 = Typedef!(float, float.init, "X230");
alias X231 = Typedef!(float, float.init, "X231");
alias X232 = Typedef!(float, float.init, "X232");
alias X233 = Typedef!(float, float.init, "X233");
alias X234 = Typedef!(float, float.init, "X234");
alias X235 = Typedef!(float, float.init, "X235");
alias X236 = Typedef!(float, float.init, "X236");
alias X237 = Typedef!(float, float.init, "X237");
alias X238 = Typedef!(float, float.init, "X238");
alias X239 = Typedef!(float, float.init, "X239");
alias X240 = Typedef!(float, float.init, "X240");
alias X241 = Typedef!(float, float.init, "X241");
alias X242 = Typedef!(float, float.init, "X242");
alias X243 = Typedef!(float, float.init, "X243");
alias X244 = Typedef!(float, float.init, "X244");
alias X245 = Typedef!(float, float.init, "X245");
alias X246 = Typedef!(float, float.init, "X246");
alias X247 = Typedef!(float, float.init, "X247");
alias X248 = Typedef!(float, float.init, "X248");
alias X249 = Typedef!(float, float.init, "X249");
alias X250 = Typedef!(float, float.init, "X250");
alias X251 = Typedef!(float, float.init, "X251");
alias X252 = Typedef!(float, float.init, "X252");
alias X253 = Typedef!(float, float.init, "X253");
alias X254 = Typedef!(float, float.init, "X254");
alias X255 = Typedef!(float, float.init, "X255");
alias X256 = Typedef!(float, float.init, "X256");
alias X257 = Typedef!(float, float.init, "X257");
alias X258 = Typedef!(float, float.init, "X258");
alias X259 = Typedef!(float, float.init, "X259");
alias X260 = Typedef!(float, float.init, "X260");
alias X261 = Typedef!(float, float.init, "X261");
alias X262 = Typedef!(float, float.init, "X262");
alias X263 = Typedef!(float, float.init, "X263");
alias X264 = Typedef!(float, float.init, "X264");
alias X265 = Typedef!(float, float.init, "X265");
alias X266 = Typedef!(float, float.init, "X266");
alias X267 = Typedef!(float, float.init, "X267");
alias X268 = Typedef!(float, float.init, "X268");
alias X269 = Typedef!(float, float.init, "X269");
alias X270 = Typedef!(float, float.init, "X270");
alias X271 = Typedef!(float, float.init, "X271");
alias X272 = Typedef!(float, float.init, "X272");
alias X273 = Typedef!(float, float.init, "X273");
alias X274 = Typedef!(float, float.init, "X274");
alias X275 = Typedef!(float, float.init, "X275");
alias X276 = Typedef!(float, float.init, "X276");
alias X277 = Typedef!(float, float.init, "X277");
alias X278 = Typedef!(float, float.init, "X278");
alias X279 = Typedef!(float, float.init, "X279");
alias X280 = Typedef!(float, float.init, "X280");
alias X281 = Typedef!(float, float.init, "X281");
alias X282 = Typedef!(float, float.init, "X282");
alias X283 = Typedef!(float, float.init, "X283");
alias X284 = Typedef!(float, float.init, "X284");
alias X285 = Typedef!(float, float.init, "X285");
alias X286 = Typedef!(float, float.init, "X286");
alias X287 = Typedef!(float, float.init, "X287");
alias X288 = Typedef!(float, float.init, "X288");
alias X289 = Typedef!(float, float.init, "X289");
alias X290 = Typedef!(float, float.init, "X290");
alias X291 = Typedef!(float, float.init, "X291");
alias X292 = Typedef!(float, float.init, "X292");
alias X293 = Typedef!(float, float.init, "X293");
alias X294 = Typedef!(float, float.init, "X294");
alias X295 = Typedef!(float, float.init, "X295");
alias X296 = Typedef!(float, float.init, "X296");
alias X297 = Typedef!(float, float.init, "X297");
alias X298 = Typedef!(float, float.init, "X298");
alias X299 = Typedef!(float, float.init, "X299");
alias X300 = Typedef!(float, float.init, "X300");
alias X301 = Typedef!(float, float.init, "X301");
alias X302 = Typedef!(float, float.init, "X302");
alias X303 = Typedef!(float, float.init, "X303");
alias X304 = Typedef!(float, float.init, "X304");
alias X305 = Typedef!(float, float.init, "X305");
alias X306 = Typedef!(float, float.init, "X306");
alias X307 = Typedef!(float, float.init, "X307");
alias X308 = Typedef!(float, float.init, "X308");
alias X309 = Typedef!(float, float.init, "X309");
alias X310 = Typedef!(float, float.init, "X310");
alias X311 = Typedef!(float, float.init, "X311");
alias X312 = Typedef!(float, float.init, "X312");
alias X313 = Typedef!(float, float.init, "X313");
alias X314 = Typedef!(float, float.init, "X314");
alias X315 = Typedef!(float, float.init, "X315");
alias X316 = Typedef!(float, float.init, "X316");
alias X317 = Typedef!(float, float.init, "X317");
alias X318 = Typedef!(float, float.init, "X318");
alias X319 = Typedef!(float, float.init, "X319");
alias X320 = Typedef!(float, float.init, "X320");
alias X321 = Typedef!(float, float.init, "X321");
alias X322 = Typedef!(float, float.init, "X322");
alias X323 = Typedef!(float, float.init, "X323");
alias X324 = Typedef!(float, float.init, "X324");
alias X325 = Typedef!(float, float.init, "X325");
alias X326 = Typedef!(float, float.init, "X326");
alias X327 = Typedef!(float, float.init, "X327");
alias X328 = Typedef!(float, float.init, "X328");
alias X329 = Typedef!(float, float.init, "X329");
alias X330 = Typedef!(float, float.init, "X330");
alias X331 = Typedef!(float, float.init, "X331");
alias X332 = Typedef!(float, float.init, "X332");
alias X333 = Typedef!(float, float.init, "X333");
alias X334 = Typedef!(float, float.init, "X334");
alias X335 = Typedef!(float, float.init, "X335");
alias X336 = Typedef!(float, float.init, "X336");
alias X337 = Typedef!(float, float.init, "X337");
alias X338 = Typedef!(float, float.init, "X338");
alias X339 = Typedef!(float, float.init, "X339");
alias X340 = Typedef!(float, float.init, "X340");
alias X341 = Typedef!(float, float.init, "X341");
alias X342 = Typedef!(float, float.init, "X342");
alias X343 = Typedef!(float, float.init, "X343");
alias X344 = Typedef!(float, float.init, "X344");
alias X345 = Typedef!(float, float.init, "X345");
alias X346 = Typedef!(float, float.init, "X346");
alias X347 = Typedef!(float, float.init, "X347");
alias X348 = Typedef!(float, float.init, "X348");
alias X349 = Typedef!(float, float.init, "X349");
alias X350 = Typedef!(float, float.init, "X350");
alias X351 = Typedef!(float, float.init, "X351");
alias X352 = Typedef!(float, float.init, "X352");
alias X353 = Typedef!(float, float.init, "X353");
alias X354 = Typedef!(float, float.init, "X354");
alias X355 = Typedef!(float, float.init, "X355");
alias X356 = Typedef!(float, float.init, "X356");
alias X357 = Typedef!(float, float.init, "X357");
alias X358 = Typedef!(float, float.init, "X358");
alias X359 = Typedef!(float, float.init, "X359");
alias X360 = Typedef!(float, float.init, "X360");
alias X361 = Typedef!(float, float.init, "X361");
alias X362 = Typedef!(float, float.init, "X362");
alias X363 = Typedef!(float, float.init, "X363");
alias X364 = Typedef!(float, float.init, "X364");
alias X365 = Typedef!(float, float.init, "X365");
alias X366 = Typedef!(float, float.init, "X366");
alias X367 = Typedef!(float, float.init, "X367");
alias X368 = Typedef!(float, float.init, "X368");
alias X369 = Typedef!(float, float.init, "X369");
alias X370 = Typedef!(float, float.init, "X370");
alias X371 = Typedef!(float, float.init, "X371");
alias X372 = Typedef!(float, float.init, "X372");
alias X373 = Typedef!(float, float.init, "X373");
alias X374 = Typedef!(float, float.init, "X374");
alias X375 = Typedef!(float, float.init, "X375");
alias X376 = Typedef!(float, float.init, "X376");
alias X377 = Typedef!(float, float.init, "X377");
alias X378 = Typedef!(float, float.init, "X378");
alias X379 = Typedef!(float, float.init, "X379");
alias X380 = Typedef!(float, float.init, "X380");
alias X381 = Typedef!(float, float.init, "X381");
alias X382 = Typedef!(float, float.init, "X382");
alias X383 = Typedef!(float, float.init, "X383");
alias X384 = Typedef!(float, float.init, "X384");
alias X385 = Typedef!(float, float.init, "X385");
alias X386 = Typedef!(float, float.init, "X386");
alias X387 = Typedef!(float, float.init, "X387");
alias X388 = Typedef!(float, float.init, "X388");
alias X389 = Typedef!(float, float.init, "X389");
alias X390 = Typedef!(float, float.init, "X390");
alias X391 = Typedef!(float, float.init, "X391");
alias X392 = Typedef!(float, float.init, "X392");
alias X393 = Typedef!(float, float.init, "X393");
alias X394 = Typedef!(float, float.init, "X394");
alias X395 = Typedef!(float, float.init, "X395");
alias X396 = Typedef!(float, float.init, "X396");
alias X397 = Typedef!(float, float.init, "X397");
alias X398 = Typedef!(float, float.init, "X398");
alias X399 = Typedef!(float, float.init, "X399");
alias X400 = Typedef!(float, float.init, "X400");
alias X401 = Typedef!(float, float.init, "X401");
alias X402 = Typedef!(float, float.init, "X402");
alias X403 = Typedef!(float, float.init, "X403");
alias X404 = Typedef!(float, float.init, "X404");
alias X405 = Typedef!(float, float.init, "X405");
alias X406 = Typedef!(float, float.init, "X406");
alias X407 = Typedef!(float, float.init, "X407");
alias X408 = Typedef!(float, float.init, "X408");
alias X409 = Typedef!(float, float.init, "X409");
alias X410 = Typedef!(float, float.init, "X410");
alias X411 = Typedef!(float, float.init, "X411");
alias X412 = Typedef!(float, float.init, "X412");
alias X413 = Typedef!(float, float.init, "X413");
alias X414 = Typedef!(float, float.init, "X414");
alias X415 = Typedef!(float, float.init, "X415");
alias X416 = Typedef!(float, float.init, "X416");
alias X417 = Typedef!(float, float.init, "X417");
alias X418 = Typedef!(float, float.init, "X418");
alias X419 = Typedef!(float, float.init, "X419");
alias X420 = Typedef!(float, float.init, "X420");
alias X421 = Typedef!(float, float.init, "X421");
alias X422 = Typedef!(float, float.init, "X422");
alias X423 = Typedef!(float, float.init, "X423");
alias X424 = Typedef!(float, float.init, "X424");
alias X425 = Typedef!(float, float.init, "X425");
alias X426 = Typedef!(float, float.init, "X426");
alias X427 = Typedef!(float, float.init, "X427");
alias X428 = Typedef!(float, float.init, "X428");
alias X429 = Typedef!(float, float.init, "X429");
alias X430 = Typedef!(float, float.init, "X430");
alias X431 = Typedef!(float, float.init, "X431");
alias X432 = Typedef!(float, float.init, "X432");
alias X433 = Typedef!(float, float.init, "X433");
alias X434 = Typedef!(float, float.init, "X434");
alias X435 = Typedef!(float, float.init, "X435");
alias X436 = Typedef!(float, float.init, "X436");
alias X437 = Typedef!(float, float.init, "X437");
alias X438 = Typedef!(float, float.init, "X438");
alias X439 = Typedef!(float, float.init, "X439");
alias X440 = Typedef!(float, float.init, "X440");
alias X441 = Typedef!(float, float.init, "X441");
alias X442 = Typedef!(float, float.init, "X442");
alias X443 = Typedef!(float, float.init, "X443");
alias X444 = Typedef!(float, float.init, "X444");
alias X445 = Typedef!(float, float.init, "X445");
alias X446 = Typedef!(float, float.init, "X446");
alias X447 = Typedef!(float, float.init, "X447");
alias X448 = Typedef!(float, float.init, "X448");
alias X449 = Typedef!(float, float.init, "X449");
alias X450 = Typedef!(float, float.init, "X450");
alias X451 = Typedef!(float, float.init, "X451");
alias X452 = Typedef!(float, float.init, "X452");
alias X453 = Typedef!(float, float.init, "X453");
alias X454 = Typedef!(float, float.init, "X454");
alias X455 = Typedef!(float, float.init, "X455");
alias X456 = Typedef!(float, float.init, "X456");
alias X457 = Typedef!(float, float.init, "X457");
alias X458 = Typedef!(float, float.init, "X458");
alias X459 = Typedef!(float, float.init, "X459");
alias X460 = Typedef!(float, float.init, "X460");
alias X461 = Typedef!(float, float.init, "X461");
alias X462 = Typedef!(float, float.init, "X462");
alias X463 = Typedef!(float, float.init, "X463");
alias X464 = Typedef!(float, float.init, "X464");
alias X465 = Typedef!(float, float.init, "X465");
alias X466 = Typedef!(float, float.init, "X466");
alias X467 = Typedef!(float, float.init, "X467");
alias X468 = Typedef!(float, float.init, "X468");
alias X469 = Typedef!(float, float.init, "X469");
alias X470 = Typedef!(float, float.init, "X470");
alias X471 = Typedef!(float, float.init, "X471");
alias X472 = Typedef!(float, float.init, "X472");
alias X473 = Typedef!(float, float.init, "X473");
alias X474 = Typedef!(float, float.init, "X474");
alias X475 = Typedef!(float, float.init, "X475");
alias X476 = Typedef!(float, float.init, "X476");
alias X477 = Typedef!(float, float.init, "X477");
alias X478 = Typedef!(float, float.init, "X478");
alias X479 = Typedef!(float, float.init, "X479");
alias X480 = Typedef!(float, float.init, "X480");
alias X481 = Typedef!(float, float.init, "X481");
alias X482 = Typedef!(float, float.init, "X482");
alias X483 = Typedef!(float, float.init, "X483");
alias X484 = Typedef!(float, float.init, "X484");
alias X485 = Typedef!(float, float.init, "X485");
alias X486 = Typedef!(float, float.init, "X486");
alias X487 = Typedef!(float, float.init, "X487");
alias X488 = Typedef!(float, float.init, "X488");
alias X489 = Typedef!(float, float.init, "X489");
alias X490 = Typedef!(float, float.init, "X490");
alias X491 = Typedef!(float, float.init, "X491");
alias X492 = Typedef!(float, float.init, "X492");
alias X493 = Typedef!(float, float.init, "X493");
alias X494 = Typedef!(float, float.init, "X494");
alias X495 = Typedef!(float, float.init, "X495");
alias X496 = Typedef!(float, float.init, "X496");
alias X497 = Typedef!(float, float.init, "X497");
alias X498 = Typedef!(float, float.init, "X498");
alias X499 = Typedef!(float, float.init, "X499");
alias X500 = Typedef!(float, float.init, "X500");
