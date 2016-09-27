/** SUO-KIF File Format.

    See: https://en.wikipedia.org/wiki/Knowledge_Interchange_Format
    See: http://sigmakee.cvs.sourceforge.net/viewvc/sigmakee/sigma/suo-kif.pdf
*/
module suokif;

// import std.range : isInputRange;
import dbgio : dln;
import array_ex : Array;

/** SUO-KIF Token. */
enum Token
{
    leftParen,
    rightParen,
    symbol,
    stringLiteral,
    oneDirInference,                  // one-directional inference
    biDirInference,                 // bi-directional inference
    equivalence,
    and_,
    or_,
    exists_,
    not_,
    variable,
    whitespace,
    number,
    comment,
}

/** Parse SUO-KIF from `src`. */
Array!Token lexSUOKIF(string src) @safe pure
{
    import std.range : empty, front, popFront;
    import std.uni : isWhite, isAlpha;
    import std.ascii : isDigit;
    import std.algorithm : among, skipOver;

    typeof(return) tokens;

    src.skipOver(x"EFBBBF");    // skip magic? header for some files

    /// Skip comment.
    static void skipComment(ref string src)
    {
        while (!src.empty && !src.front.among('\r', '\n')) // until end of line
        {
            src.popFront();
        }
    }

    static string skipN(ref string src, size_t n)
    {
        const part = src[0 .. n];
        src = src[n .. $];
        return part;
    }

    /// Get symbol.
    static string getSymbol(ref string src)
    {
        size_t i = 0;
        while (i != src.length && src[i].isAlpha) { ++i; }
        return skipN(src, i);
    }

    /// Get numeric literal (number).
    static string getNumber(ref string src)
    {
        size_t i = 0;
        while (i != src.length && src[i].isDigit) { ++i; }
        return skipN(src, i);
    }

    /// Get string literal.
    static string getStringLiteral(ref string src)
    {
        src.popFront();         // pop leading double quote
        size_t i = 0;
        while (i != src.length && src[i] != '"') { ++i; }
        const literal = src[0 .. i]; src = src[i .. $]; // TODO functionize
        src.popFront();         // pop ending double quote
        return literal;
    }

    /// Skip whitespace.
    static string getWhitespace(ref string src)
    {
        size_t i = 0;
        while (i != src.length && src[i].isWhite) { ++i; }
        return skipN(src, i);
    }

    while (!src.empty)
    {
        // dlnl(`front:'`, src.front, `'`);
        switch (src.front)
        {
        case ';':
            skipComment(src);
            tokens ~= Token.comment;
            break;
        case '(':
            tokens ~= Token.leftParen;
            src.popFront();
            break;
        case ')':
            tokens ~= Token.rightParen;
            src.popFront();
            break;
        case '"':
            const stringLiteral = getStringLiteral(src); // TODO tokenize
            tokens ~= Token.stringLiteral;
            break;
        case '=':
            src.popFront();
            if (src.front == '>')
            {
                tokens ~= Token.oneDirInference;
                src.popFront();
            }
            else
            {
                tokens ~= Token.equivalence;
            }
            break;
        case '<':
            src.popFront();
            if (src.front == '=')
            {
                src.popFront();
                if (src.front == '>')
                {
                    tokens ~= Token.biDirInference;
                    src.popFront();
                }
            }
            break;
        case '?':
            src.popFront();
            const variableSymbol = getSymbol(src); // TODO tokenize
            tokens ~= Token.variable;
            break;
        default:
            // keywords
            if      (src.skipOver(`and`)) { tokens ~= Token.and_; }
            else if (src.skipOver(`or`)) { tokens ~= Token.or_; }
            else if (src.skipOver(`exists`)) { tokens ~= Token.exists_; }
            else if (src.skipOver(`not`)) { tokens ~= Token.not_; }
            // other
            else if (src.front.isWhite)
            {
                getWhitespace(src);
                tokens ~= Token.whitespace;
            }
            else if (src.front.isAlpha)
            {
                const symbol = getSymbol(src); // TODO tokenize
                tokens ~= Token.symbol;
            }
            else if (src.front.isDigit)
            {
                const number = getNumber(src); // TODO tokenize
                tokens ~= Token.number;
            }
            else
            {
                dln(`Cannot handle character '`, src.front, `'`);
                assert(false);
            }
            break;
        }
    }

    return tokens;
}

unittest
{
    import std.path : expandTilde;
    const rootDirPath = `~/Work/justd/sumo`;
    import std.file: dirEntries, SpanMode;
    auto entries = dirEntries(rootDirPath.expandTilde, SpanMode.shallow, false); // false: skip symlinks
    foreach (dent; entries)
    {
        const filePath = dent.name;
        import std.algorithm : endsWith;
        if (filePath.endsWith(`.kif`))
        {
            import std.file : readText;
            // file.readText.lexSUOKIF2();
            dln(`Lexing SUO-KIF file `, filePath);
            const tokens = filePath.readText.lexSUOKIF();
        }
    }

    // const file = `~/Work/justd/phobos-next/src/emotion.kif`.expandTilde;
    // dln(tokens[]);
}

// void lexSUOKIF2(R)(R src)
// {
//     import std.experimental.lexer;

//     static immutable TokOperators = [ `(`, `)`, `=>` ];
//     static immutable TokDynamic = [ `stringLiteral`, `comment`, `identifier`, `numberLiteral`, `whitespace` ];
//     static immutable TokKeywords = [ `and`, `exists`, `or`, `not` ];
//     import std.meta : AliasSeq;

//     alias Toks = AliasSeq!(TokOperators, TokDynamic, TokKeywords);
//     alias TokID = TokenIdType!Toks;
//     alias tokToString = tokenStringRepresentation!(TokID, Toks);
//     alias tok(string symbol) = TokenId!(TokID, LuaTokens, symbol);

//     static immutable tokenHandlers = [
//         "\"", "lexStringLiteral",
//         ";", "lexComment",
//         " ",  "lexWhitespace",
//         "\t", "lexWhitespace",
//         "\r", "lexWhitespace",
//         "\n", "lexWhitespace",
//         ];
// }
