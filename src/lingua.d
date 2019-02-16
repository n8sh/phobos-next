/** Generic Language Constructs.
    See_Also: https://en.wikipedia.org/wiki/Predicate_(grammar)

    Note that ! and ? are more definite sentence enders than .

    TODO `isSomeString` => `isStringLike`

    TODO Use static foreach to add declarations for all isX, for each X

    See_Also: http://forum.dlang.org/thread/mgdtuxkuswfxxoithwxh@forum.dlang.org
*/
module lingua;

import std.traits: isSomeChar, isSomeString;
import std.algorithm.comparison: among;
import std.algorithm: uniq, map, find, canFind, startsWith, endsWith, among;
import std.array: array;
import std.conv;

// TODO add overload to std.algorithm.among that takes an immutable array as
// argument to prevent calls to aliasSeqOf
import std.meta : aliasSeqOf;

import languages: Lang;

@safe pure:

/** Computer Token Usage. */
enum Usage
{
    unknown,
    definition,
    reference,
    call
}

/// ================ English Articles

/** English indefinite articles. */
immutable englishIndefiniteArticles = [`a`, `an`];

/** English definite articles. */
immutable englishDefiniteArticles = [`the`];

/** English definite articles. */
immutable englishArticles = englishIndefiniteArticles ~ englishDefiniteArticles;

bool isEnglishIndefiniteArticle(S)(S s)
{
    return cast(bool)s.among!(aliasSeqOf!englishIndefiniteArticles);
}

bool isEnglishDefiniteArticle(S)(S s)
if (isSomeString!S)
{
    return cast(bool)s.among!(aliasSeqOf!englishDefiniteArticles);
}

bool isEnglishArticle(S)(S s)
if (isSomeString!S)
{
    return cast(bool)s.among!(aliasSeqOf!englishArticles);
}

/// ================ German Articles

/** German indefinite articles. */
immutable germanIndefiniteArticles = [`ein`, `eine`, `einer`, `einen`, `einem`, `eines`];

/** German definite articles. */
immutable germanDefiniteArticles = [`der`, `die`, `das`, `den`, `dem`, `des`];

/** German definite articles. */
immutable germanArticles = germanIndefiniteArticles ~ germanDefiniteArticles;

/** Check if $(D s) is a Vowel. */
bool isGermanIndefiniteArticle(S)(S s)
if (isSomeString!S)
{
    return cast(bool)s.among!(aliasSeqOf!germanIndefiniteArticles);
}

/** Check if $(D s) is a Vowel. */
bool isGermanDefiniteArticle(S)(S s)
if (isSomeString!S)
{
    return cast(bool)s.among!(aliasSeqOf!germanDefiniteArticles);
}

/** Check if $(D s) is a Vowel. */
bool isGermanArticle(S)(S s)
if (isSomeString!C)
{
    return cast(bool)s.among!(aliasSeqOf!germanArticles);
}

/// ================ Vowels

/** English vowel type.
    See_Also: https://simple.wikipedia.org/wiki/Vowel
*/
enum EnglishVowel { a, o, u, e, i, y,
                    A, O, U, E, I, Y }

/** English Vowels. */
immutable dchar[] englishVowels = ['a', 'o', 'u', 'e', 'i', 'y',
                                   'A', 'O', 'U', 'E', 'I', 'Y'];

/** Check if $(D c) is a Vowel. */
bool isEnglishVowel(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!englishVowels);
}

/** English Accented Vowels. */
immutable dchar[] englishAccentedVowels = ['é'];

/** Check if $(D c) is an Accented Vowel. */
bool isEnglishAccentedVowel(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!englishAccentedVowels);
}

nothrow @nogc unittest
{
    assert('é'.isEnglishAccentedVowel);
}

/** Swedish Hard Vowels. */
immutable swedishHardVowels = ['a', 'o', 'u', 'å',
                               'A', 'O', 'U', 'Å'];

/** Swedish Soft Vowels. */
immutable swedishSoftVowels = ['e', 'i', 'y', 'ä', 'ö',
                               'E', 'I', 'Y', 'Ä', 'Ö'];

/** Swedish Vowels. */
immutable swedishVowels = swedishHardVowels ~ swedishSoftVowels;

/** Check if $(D c) is a Swedish Vowel. */
bool isSwedishVowel(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!swedishVowels);
}

/** Check if $(D c) is a Swedish hard vowel. */
bool isSwedishHardVowel(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!swedishHardVowels);
}

/** Check if $(D c) is a Swedish soft vowel. */
bool isSwedishSoftVowel(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!swedishSoftVowels);
}

/** Spanish Accented Vowels. */
enum spanishAccentedVowels = ['á', 'é', 'í', 'ó', 'ú',
                              'Á', 'É', 'Í', 'Ó', 'Ú'];

/** Check if $(D c) is a Spanish Accented Vowel. */
bool isSpanishAccentedVowel(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!spanishAccentedVowels);
}

/** Check if $(D c) is a Spanish Vowel. */
bool isSpanishVowel(C)(C c)
if (isSomeChar!C)
{
    return (c.isEnglishVowel ||
            c.isSpanishAccentedVowel);
}

nothrow @nogc unittest
{
    assert('é'.isSpanishVowel);
}

/** Check if $(D c) is a Vowel in language $(D lang). */
bool isVowel(C)(C c, Lang lang)
if (isSomeChar!C)
{
    switch (lang)
    {
    case Lang.en: return c.isEnglishVowel;
    case Lang.sv: return c.isSwedishVowel;
    default: return c.isEnglishVowel;
    }
}

nothrow @nogc unittest
{
    assert(!'k'.isSwedishVowel);
    assert('å'.isSwedishVowel);
}

/** English consonant type.
    See_Also: https://simple.wikipedia.org/wiki/Consonant
*/
enum EnglishConsonant { b, c, d, f, g, h, j, k, l, m, n, p, q, r, s, t, v, w, x }

/** English lowercase consontant characters. */
immutable dchar[] englishLowerConsonants = ['b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x'];

/** English uppercase consontant characters. */
immutable dchar[] englishUpperConsonants = ['B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X'];

/** English consontant characters. */
immutable dchar[] englishConsonants = englishLowerConsonants ~ englishUpperConsonants;

/** Check if $(D c) is a Consonant. */
bool isEnglishConsonant(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!englishConsonants);
}
alias isSwedishConsonant = isEnglishConsonant;

nothrow @nogc unittest
{
    assert('k'.isEnglishConsonant);
    assert(!'å'.isEnglishConsonant);
}

/** English letters. */
immutable dchar[] englishLetters = englishVowels ~ englishConsonants;

/** Check if $(D c) is a letter. */
bool isEnglishLetter(C)(C c)
if (isSomeChar!C)
{
    return cast(bool)c.among!(aliasSeqOf!englishLetters);
}
alias isEnglish = isEnglishLetter;

nothrow @nogc unittest
{
    assert('k'.isEnglishLetter);
    assert(!'å'.isEnglishLetter);
}

static immutable englishDoubleConsonants = [`bb`, `dd`, `ff`, `gg`, `mm`, `nn`, `pp`, `rr`, `tt`, `ck`, `ft`];

/** Check if $(D c) is an English Double consonant. */
bool isEnglishDoubleConsonant(S)(S s)
if (isSomeString!S)
{
    return cast(bool)c.among!(`bb`, `dd`, `ff`, `gg`, `mm`, `nn`, `pp`, `rr`, `tt`, `ck`, `ft`);
}

/** Computer token. */
enum TokenId
{
    unknown,

    keyword,
    type,
    constant,
    comment,
    variableName,
    functionName,
    builtinName,
    templateName,
    macroName,
    aliasName,
    enumeration,
    enumerator,
    constructor,
    destructors,
    operator,
}

/** Swedish Verb Inflection (conjugation of a verb).

    See_Also: http://www.101languages.net/swedish/swedish-verb-conjugator/
    See_Also: http://www.verbix.com/webverbix/Swedish/springa.html
 */
enum SwedishVerbInflection
{
    unknown,
}

/** Verb Form.

    See_Also: http://verb.woxikon.se/sv/springa
 */
enum VerbForm
{
    unknown,

    imperative,                 // Swedish example: spring

    infinitive,                 // sv:infinitiv,grundform. Swedish example: springa
    base = infinitive,

    presentIndicative,          // sv:presens. Swedish example: springer

    presentParticiple,          // sv:presens particip. Swedish example: springande
    gerund = presentParticiple, // Form that functions as a noun. Source: https://en.wikipedia.org/wiki/Gerund

    pastIndicative,             // sv:imperfekt. Swedish example: sprang
    preteritum = pastIndicative,

    supinum,                    // Swedish example: sprungit
    pastParticiple = supinum,

    perfekt,                    // sv:perfekt. Swedish example: har sprungit

    perfektParticiple,          // sv:perfekt particip. Swedish example: sprungen

    pluskvamperfekt,            // sv:pluskvamperfekt. Swedish example: hade sprungit

    futurum,                    // Swedish example:ska springa

    futurumExaktum,             // Swedish example:ska ha sprungit
    futurumPerfect = futurumExaktum,

    konditionalisI,             // Swedish example:skulle springa

    conditionalPerfect,         // Swedish example:skulle ha sprungit
    konditionalisII = conditionalPerfect,
}

/** Verb Instance. */
struct Verb(S)
if (isSomeString!S)
{
    S expr;
    VerbForm form;
    alias expr this;
}

/** Subject Count. */
enum Count
{
    unknown,
    singular,
    plural,
    uncountable
}

struct Noun(S)
if (isSomeString!S)
{
    S expr;
    Count count;
    alias expr this;
}

/** Comparation.
    See_Also: https://en.wikipedia.org/wiki/Comparison_(grammar)
*/
enum Comparation
{
    unknown,
    positive,
    comparative,
    superlative,
    elative,
    exzessive
}

struct Adjective(S)
if (isSomeString!S)
{
    S expr;
    Comparation comparation;
    alias expr this;
}

/** English Tense.

    Same as "tempus" in Swedish.

    See_Also: http://www.ego4u.com/en/cram-up/grammar/tenses-graphic
    See_Also: http://www.ego4u.com/en/cram-up/grammar/tenses-examples
*/
enum Tense
{
    unknown,

    present, presens = present, // sv:nutid
    past, preteritum = past, imperfekt = past, // sv:dåtid, https://en.wikipedia.org/wiki/Past_tense
    future, futurum = future, // framtid, https://en.wikipedia.org/wiki/Future_tense

    pastMoment,
    presentMoment, // sv:plays
    futureMoment, // [will|is going to|intends to] play

    pastPeriod,
    presentPeriod,
    futurePeriod,

    pastResult,
    presentResult,
    futureResult,

    pastDuration,
    presentDuration,
    futureDuration,
}
alias Tempus = Tense;

nothrow @nogc
{
    bool isPast(Tense tense)
    {
        with (Tense)
            return cast(bool)tense.among!(past, pastMoment, pastPeriod, pastResult, pastDuration);
    }

    bool isPresent(Tense tense)
    {
        with (Tense)
            return cast(bool)tense.among!(present, presentMoment, presentPeriod, presentResult, presentDuration);
    }

    bool isFuture(Tense tense)
    {
        with (Tense)
            return cast(bool)tense.among!(future, futureMoment, futurePeriod, futureResult, futureDuration);
    }
}

/** Part of a Sentence. */
enum SentencePart
{
    unknown,
    subject,
    predicate,
    adverbial,
    object,
}

enum Adverbial
{
    unknown,

    manner,          // they were playing `happily` (sätts-adverbial in Swedish)

    place,                      // we met in `London`, `at the beach`
    space = place,

    time,                       // they start work `at six thirty`

    probability,                // `perhaps` the weather will be fine

    direction, // superman flew `in`, the car drove `out` (förändring av tillstånd in Swedish)
    location,  // are you `in`?, the ball is `out` (oföränderligt tillstånd in Swedish)

    quantifier,                 // he weighs `63 kilograms` (måtts-adverbial in Swedish)

    comparation,                // (grads-adverbial in Swedish)

    cause,                      // (orsaks-adverbial in Swedish)

    circumstance,               // (omständighets-adverbial in Swedish)
}

class Part
{
}

// class Predicate : Part
// {
// }

// TODO: Conversion to Sense
enum Article
{
    unknown,
    indefinite,
    definite,
    partitive
}

class Subject : Part
{
    Article article;
}

static immutable implies = [`in order to`];

/** Subject Person. */
enum Person
{
    unknown,
    first,
    second,
    third
}

/** Grammatical Gender.
 *
 * Called genus in Swedish.
 *
 * See_Also: https://en.wikipedia.org/wiki/Grammatical_gender
 * See_Also: https://sv.wikipedia.org/wiki/Genus_(k%C3%B6nsbegrepp)
 */
enum Gender
{
    unknown,

    male, masculine = male,    // maskulinum

    female, feminine = female, // femininum

    neutral, neuter = neutral, neutrum = neuter, // non-alive. for example: "något"

    common, utrum = common, reale = utrum, // Present in Swedish. real/alive. for example: "någon"
}

/** (Grammatical) Mood.
 *
 * Sometimes also called mode.
 *
 * Named modus in Swedish.
 *
 * See_Also: https://en.wikipedia.org/wiki/Grammatical_mood
 * See_Also: https://www.cse.unsw.edu.au/~billw/nlpdict.html#mood
 */
enum Mood
{
    unknown,

    indicative, // indikativ in Swedish. Example: I eat pizza.

    /// See_Also: https://www.cse.unsw.edu.au/~billw/nlpdict.html#subjunctive
    subjunctive,          // Example: if I were to eat more pizza, I would be sick.
    conjunctive = subjunctive, // konjunktiv in Swedish

    conditional,
    optative,

    /// See_Also: https://www.cse.unsw.edu.au/~billw/nlpdict.html#imperative
    imperative, // imperativ in Swedish. Example: eat the pizza!

    jussive,
    potential,
    inferential,
    interrogative,

    /// See_Also: https://www.cse.unsw.edu.au/~billw/nlpdict.html#wh-question
    whQuestion, // Example: who is eating pizza?

    /// See_Also: https://www.cse.unsw.edu.au/~billw/nlpdict.html#yn-question
    ynQuestion, // Example: did you eat pizza?
}

/** Check if $(D mood) is a Realis Mood.
    See_Also: https://en.wikipedia.org/wiki/Grammatical_mood#Realis_moods
 */
bool isRealis(Mood mood) @nogc nothrow
{
    with (Mood)
    {
        return cast(bool)mood.among!(indicative);
    }
}

enum realisMoods = [Mood.indicative];

/** Check if $(D mood) is a Irrealis Mood.
    See_Also: https://en.wikipedia.org/wiki/Grammatical_mood#Irrealis_moods
*/
bool isIrrealis(Mood mood) @nogc nothrow
{
    with (Mood)
    {
        return cast(bool)mood.among!(subjunctive,
                                     conditional,
                                     optative,
                                     imperative,
                                     jussive,
                                     potential,
                                     inferential);
    }
}

enum irrealisMoods = [Mood.subjunctive,
                      Mood.conditional,
                      Mood.optative,
                      Mood.imperative,
                      Mood.jussive,
                      Mood.potential,
                      Mood.inferential];

/** English Negation Prefixes.
    See_Also: http://www.english-for-students.com/Negative-Prefixes.html
*/
static immutable englishNegationPrefixes = [ `un`, `non`, `dis`, `im`, `in`, `il`, `ir`, ];

static immutable swedishNegationPrefixes = [ `icke`, `o`, ];

/** English Noun Suffixes.
    See_Also: http://www.english-for-students.com/Noun-Suffixes.html
 */
static immutable adjectiveNounSuffixes = [ `ness`, `ity`, `ment`, `ance` ];
static immutable verbNounSuffixes = [ `tion`, `sion`, `ment`, `ence` ];
static immutable nounNounSuffixes = [ `ship`, `hood` ];
static immutable allNounSuffixes = (adjectiveNounSuffixes ~
                                    verbNounSuffixes ~
                                    nounNounSuffixes ~
                                    [ `s`, `ses`, `xes`, `zes`, `ches`, `shes`, `men`, `ies`, ]);

/** English Verb Suffixes. */
static immutable verbSuffixes = [ `s`, `ies`, `es`, `es`, `ed`, `ed`, `ing`, `ing`, ];

/** English Adjective Suffixes. */
static immutable adjectiveSuffixes = [ `er`, `est`, `er`, `est` ];

/** English Job/Professin Title Suffixes.
    Typically built from noun or verb bases.
    See_Also: http://www.english-for-students.com/Job-Title-Suffixes.html
*/
static immutable jobTitleSuffixes = [ `or`, // traitor
                                      `er`, // builder
                                      `ist`, // typist
                                      `an`, // technician
                                      `man`, // dustman, barman
                                      `woman`, // policewoman
                                      `ian`, // optician
                                      `person`, // chairperson
                                      `sperson`, // spokesperson
                                      `ess`, // waitress
                                      `ive` // representative
    ];

/** English Linking Verbs in Nominative Form.
 */
static immutable englishLinkingVerbs = [`is`, `seem`, `look`, `appear to be`, `could be`];
static immutable swedishLinkingVerbs = [`är`, `verkar`, `ser`, `kan vara`];

/** English Word Suffixes. */
static immutable wordSuffixes = [ allNounSuffixes ~ verbSuffixes ~ adjectiveSuffixes ].uniq.array;

/** Return string $(D word) in plural optionally in $(D count). */
string inPlural(string word, int count = 2,
                string pluralWord = null)
{
    if (count == 1 || word.length == 0)
    {
        return word; // it isn't actually inPlural
    }
    if (pluralWord !is null)
    {
        return pluralWord;
    }
    switch (word[$ - 1])
    {
        case 's':
        case 'a', 'e', 'i', 'o', 'u':
            return word ~ `es`;
        case 'f':
            return word[0 .. $-1] ~ `ves`;
        case 'y':
            return word[0 .. $-1] ~ `ies`;
        default:
            return word ~ `s`;
    }
}

/** Return $(D s) lemmatized (normalized).
    See_Also: https://en.wikipedia.org/wiki/Lemmatisation
 */
S lemmatized(S)(S s) nothrow
if (isSomeString!S)
{
    if      (s.among!(`be`, `is`, `am`, `are`)) return `be`;
    else if (s.among!(`do`, `does`))            return `do`;
    else return s;
}

/**
   TODO Reuse knet translation query instead.
 */
string negationIn(Lang lang) nothrow @nogc
{
    switch (lang) with (Lang)
    {
    case en: return `not`;
    case sv: return `inte`;
    case de: return `nicht`;
    default: return `not`;
    }
}

enum Manner
{
    // TODO add unknown
    formal,
    informal,
    slang,
    rude,
}

/** Grammatical Case.
    See_Also: https://en.wikipedia.org/wiki/Grammatical_case
*/
enum Case
{
    unknown,
    nominative,
    genitive,
    dative,
    accusative,
    ablative
}

/** English Subject Pronouns.
   See_Also: https://en.wikipedia.org/wiki/Subject_pronoun
*/
enum englishSubjectPronouns = [`I`, // 1st-person singular
                               `you`, // 2nd-person singular
                               `he`, `she`, `it`, // 3rd-person singular
                               `we`,              // 1st-person plural
                               `they`,            // 2nd-person plural
                               `what`,            // interrogate singular (Object)
                               `who`];            // interrogate singular

/** Swedish Subject Pronouns.
    See_Also: https://en.wikipedia.org/wiki/Subject_pronoun
*/
enum swedishSubjectPronouns = [`jag`, // 1st-person singular
                               `du`,  // 2nd-person singular
                               `han`, `hon`, `den`, `det`, // 3rd-person singular
                               `vi`,                       // 1st-person plural
                               `de`,                       // 2nd-person plural
                               `vad`,                      // interrogate singular (Object)
                               `vem`,                      // interrogate singular
                               `vilka`];                   // interrogate plural

/** English Object Pronouns.
    See_Also: https://en.wikipedia.org/wiki/Object_pronoun
*/
enum englishObjectPronouns = [`me`, // 1st-person singular
                              `you`, // 2nd-person singular
                              `him,`, `her`, // 3rd-person singular
                              `us`,          // 1st-person plural
                              `them`,        // 2nd-person plural
                              `whom`];       // interrogate singular

/** Swedish Object Pronouns.
    See_Also: https://en.wikipedia.org/wiki/Object_pronoun
*/
enum swedishObjectPronouns = [`mig`, `dig`,
                              `honom,`, `henne`,
                              `oss`,
                              `dem`];

enum Casing
{
    unknown,
    lower,
    upper,
    capitalized,
    camel
}
