/** SUO-KIF File Format. */
module suokif;

// import std.range : isInputRange;

enum Token
{
    leftParen,
    rightParen,
    symbol,
    stringLiteral,
}

import array_ex : Array;

/** Parse SUO-KIF from `src`. */
Array!Token parseSUOKIF(string whole) @safe pure
{
    import std.range : empty, front, popFront;
    import std.uni : isWhite, isAlpha;
    import std.algorithm : among, skipOver;
    import dbgio : dln;

    typeof(return) tokens;

    auto rest = whole;

    rest.skipOver(x"EFBBBF");    // skip magic? header for some files

    /// Skip comment.
    void skipComment()
    {
        while (!rest.empty && !rest.front.among('\r', '\n')) // until end of line
        {
            rest.popFront();
        }
    }

    /// Get Symbol.
    string getSymbol()
    {
        size_t i = 0;
        while (i != rest.length && rest[i].isAlpha) { ++i; }
        const symbol = rest[0 .. i];
        rest = rest[i .. $];
        return symbol;
    }

    /// Get string literal.
    string getStringLiteral()
    {
        rest.popFront();         // pop leading double quote

        size_t i = 0;
        while (i != rest.length && rest[i] != '"') { ++i; }
        const literal = rest[0 .. i];
        rest = rest[i .. $];

        rest.popFront();         // pop ending double quote

        return literal;
    }

    /// Skip whitespace.
    void skipWhite()
    {
        while (!rest.empty && rest.front.isWhite)
        {
            rest.popFront();
        }
    }

    while (!rest.empty)
    {
        dln("front:'", rest.front, "'");
        if      (rest.front.isWhite) { skipWhite(); }
        else if (rest.front == ';') { skipComment(); }
        else if (rest.front == '(')
        {
            tokens ~= Token.leftParen;
            rest.popFront();
        }
        else if (rest.front == ')')
        {
            tokens ~= Token.rightParen;
            rest.popFront();
        }
        else if (rest.front.isAlpha)
        {
            const symbol = getSymbol();
            dln("symbol:", symbol[]);
        }
        else if (rest.front == '"')
        {
            const stringLiteral = getStringLiteral();
            dln("stringLiteral:", stringLiteral[]);
        }
        else
        {
            assert(false);
        }
    }

    return tokens;
}

void lexSUOKIF(R)(R src)
{
    import std.experimental.lexer;

    static immutable TokOperators = [ "(", ")", "=>" ];
    static immutable TokDynamic = [ "stringLiteral", "comment", "identifier", "numberLiteral", "whitespace" ];
    static immutable TokKeywords = [ "and", "exists", "or", "not" ];
    import std.meta : AliasSeq;

    alias Toks = AliasSeq!(TokOperators, TokDynamic, TokKeywords);
    alias TokID = TokenIdType!Toks;
    alias tokToString = tokenStringRepresentation!(TokID, Toks);
    alias tok(string symbol) = TokenId!(TokID, LuaTokens, symbol);

    static immutable tokenHandlers = [
        "\"", "lexStringLiteral",
        ";", "lexComment",
        " ",  "lexWhitespace",
        "\t", "lexWhitespace",
        "\r", "lexWhitespace",
        "\n", "lexWhitespace",
        ];
}

unittest
{
    import std.path : expandTilde;
    const file = "~/Work/justd/phobos-next/src/emotion.kif".expandTilde;

    import std.file : readText;
    // file.readText.lexSUOKIF();
    file.readText.parseSUOKIF();
}
