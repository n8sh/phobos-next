/** SUO-KIF File Format. */
module suokif;

import std.range : isInputRange;

enum Token
{
    leftParen,
    rightParen,
    symbol,
    stringLiteral,
}

import array_ex : Array;

/** Parse SUO-KIF from `src`. */
Array!Token parseSUOKIF(R)(R whole) @safe pure
    if (isInputRange!R)
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
    Array!dchar getSymbol()
    {
        typeof(return) symbol;
        while (!rest.empty && rest.front.isAlpha)
        {
            symbol ~= rest.front;
            rest.popFront();
        }
        return symbol;
    }

    /// Get string literal.
    Array!dchar getStringLiteral()
    {
        rest.popFront();         // pop leading double quote

        typeof(return) stringLiteral;
        while (!rest.empty && rest.front != '"')
        {
            stringLiteral ~= rest.front;
            rest.popFront();
        }

        rest.popFront();         // pop ending double quote

        return stringLiteral;
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
        if (rest.front.isWhite)
        {
            skipWhite();
        }
        else if (rest.front == ';')
        {
            skipComment();
        }
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

    enum tokenHandlers = [
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
