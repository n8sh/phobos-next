/** International Phonetic Alphabet.
 */
module ipa;

// enum Phoneme : string
// {
//     `æ`,
//     `ə`,
//     `ʌ`,
//     `ɜ`,
//     `eə`,
//     `ɜr`,
//     `ɑː`,
//     `aɪ`,
//     `ɑr`,
//     `aʊ`,
//     `b`,
//     `d`,
//     `ð`,
//     `dʒ`,
//     `ɛ`,
//     `eɪ`,
//     `f`,
//     `ɡ`,
//     `h`,
//     `hw`,
//     `iː`,
//     `ɪ`,
//     `j`,
//     `k`,
//     `l`,
//     `m`,
//     `n`,
//     `ŋ`,
//     `ɔː`,
//     `ɔɪ`,
//     `oʊ`,
//     `p`,
//     `r`,
//     `s`,
//     `ʃ`,
//     `t`,
//     `θ`,
//     `tʃ`,
//     `uː`,
//     `ʊ`,
//     `v`,
//     `w`,
//     `z`,
//     `ʒ`,
//     }

// size_t countSyllables(Phoneme s) @safe pure nothrow @nogc
// {
//     return 0;
// }

import std.traits : isSomeString;

bool isIPAVowelPhoneme(S)(S s)
    if (isSomeString!S)
{
    import std.algorithm.comparison : among;
    return s.among!(`æ`, `ə`, `ʌ`, `ɜ`, `eə`, `ɜr`, `ɑː`, `aɪ`, `ɑr`, `aʊ`, `ɛ`, `eɪ`, `iː`, `ɪ`, `ɔː`, `ɔɪ`, `oʊ`, `uː`, `ʊ`) != 0;
}

bool isIPAConsonantPhoneme(S)(S s)
    if (isSomeString!S)
{
    import std.algorithm.comparison : among;
    return s.among!(`b`, `d`, `ð`, `dʒ`, `f`, `ɡ`, `h`, `hw`, `j`, `k`, `l`, `m`, `n`, `ŋ`, `p`, `r`, `s`, `ʃ`, `t`, `θ`, `tʃ`, `tʃ`, `w`, `z`, `ʒ`);
}
