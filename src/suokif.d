/** SUO-KIF File Format. */
module suokif;

import std.range : isInputRange;

/** Parse SUO-KIF from `src`. */
void parseSUOKIF(R)(R src)
    if (isInputRange!R)
{
    import std.range : empty, front, popFront;
    import std.uni : isWhite, isAlpha;
    import std.algorithm : among, skipOver;
    import array_ex : Array;
    import dbgio : dln;

    enum Token
    {
        leftParen,
        rightParen,
        symbol,
        stringLiteral,
    }
    Array!Token tokens;

    src.skipOver(x"EFBBBF");    // skip magic? header for some files

    /// Skip comment.
    void skipComment()
    {
        while (!src.empty && !src.front.among('\r', '\n')) // until end of line
        {
            src.popFront();
        }
    }

    /// Get Symbol.
    Array!dchar getSymbol()
    {
        typeof(return) symbol;
        while (!src.empty && src.front.isAlpha)
        {
            symbol ~= src.front;
            src.popFront();
        }
        return symbol;
    }

    /// Get string literal.
    Array!dchar getStringLiteral()
    {
        src.popFront();         // pop leading double quote

        typeof(return) stringLiteral;
        while (!src.empty && src.front != '"')
        {
            stringLiteral ~= src.front;
            src.popFront();
        }

        src.popFront();         // pop ending double quote

        return stringLiteral;
    }

    /// Skip whitespace.
    void skipWhite()
    {
        while (!src.empty && src.front.isWhite)
        {
            src.popFront();
        }
    }

    while (!src.empty)
    {
        dln("front:'", src.front, "'");
        if (src.front.isWhite)
        {
            skipWhite();
        }
        else if (src.front == ';')
        {
            skipComment();
        }
        else if (src.front == '(')
        {
            tokens ~= Token.leftParen;
            src.popFront();
        }
        else if (src.front == ')')
        {
            tokens ~= Token.rightParen;
            src.popFront();
        }
        else if (src.front.isAlpha)
        {
            const symbol = getSymbol();
            dln("symbol:", symbol[]);
        }
        else if (src.front == '"')
        {
            const stringLiteral = getStringLiteral();
            dln("stringLiteral:", stringLiteral[]);
        }
        else
        {
            assert(false);
        }
    }
}

void lexSUOKIF(R)(R src)
{
    import std.experimental.lexer;

    enum TokOperators = [ "(", ")", "=>" ];
    enum TokDynamic = [ "stringLiteral", "comment", "identifier", "numberLiteral", "whitespace" ];
    enum TokKeywords = [ "and", "exists", "or", "not" ];
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
    file.readText.lexSUOKIF();
    // file.readText.parseSUOKIF();
}
