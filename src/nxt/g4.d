/** Lexer and parser for G4 grammars.
 *
 * See_Also: https://github.com/antlr/grammars-v4
 */
module nxt.g4;

import std.conv : to;
import std.stdio : writeln;

///< Token kind.
enum TOK
{
    unknown,                    ///< Unknown.

    GRAMMAR,                    ///< Keyword `grammar`.
    IMPORT,                     ///< Keyword `import`.

    symbol,                     ///< Symbol.

    blockComment,               ///< Block comment.
    lineComment,                ///< Line comment.

    leftParen,                  ///< Left parenthesis.
    rightParen,                 ///< Right parenthesis.

    characterLiteral,           ///< Character literal.
    stringLiteral,              ///< String literal. TODO: needed?
}

/// G4 rule.
struct Token
{
    TOK tt;
    const(char)[] input;
}

/// G4 lexer.
struct G4Lexer
{
    import std.algorithm.comparison : among;

    alias Input = const(char)[];

@safe pure:

    this(Input input,
         bool includeComments = false) @trusted
    {
        _input = input;

        import std.exception : enforce;
        import nxt.parsing : isNullTerminated;
        enforce(_input.isNullTerminated, "Input isn't null-terminated"); // input cannot be trusted

        _includeComments = includeComments;

        nextFront();
    }

    @disable this(this);

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _endOfFile;
    }

    Token front() const scope return
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _token;
    }

    void popFront()
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        nextFront();
    }

    import std.meta : AliasSeq;

    // from std.ascii.isWhite
    alias endOfLineChars = AliasSeq!('\n', // (0x0a)
                                     '\r', // (0x0c)
        );
    alias whiteChars = AliasSeq!(' ', // 0x20
                                 '\t', // (0x09)
                                 '\n', // (0x0a)
                                 '\v', // (0x0b)
                                 '\r', // (0x0c)
                                 '\f' // (0x0d)
        );
    alias digitChars = AliasSeq!('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

private:

    /// Get next `char` in input.
    dchar peekNext() const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset];    // TODO: .ptr. TODO: decode `dchar`
    }

    /// Get next `char` in input.
    dchar peekNextNth(size_t n) const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset + n]; // TODO: .ptr. TODO: decode `dchar`
    }

    /// Get next n `chars` in input.
    Input peekNextsN(size_t n) const return scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset .. _offset + n]; // TODO: .ptr
    }

    /// Drop next byte in input.
    void dropFront() nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        _offset += 1;
    }

    /// Drop next `n` bytes in input.
    void dropFrontN(size_t n) nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        _offset += n;
    }

    /// Skip over `n` bytes in input.
    Input skipOverN(size_t n) return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        const part = _input[_offset .. _offset + n]; // TODO: .ptr
        dropFrontN(n);
        return part;
    }

    /// Skip line comment.
    void skipLineComment() scope nothrow @nogc
    {
        while (!peekNext().among!('\0', endOfLineChars))
        {
            _offset += 1;
        }
    }

    /// Skip block comment.
    void skipBlockComment() scope nothrow
    {
        while (!peekNext().among!('\0', '*'))
        {
            if (peekNext == '*' &&
                peekNextNth(1) == '/')
            {
                _offset += 2;
                return;
            }
            _offset += 1;
        }
        assert(false,
               `Non-terminated block comment '` ~ peekNext.to!string ~
               `' at charater offset:` ~ _offset.to!string);
    }

    /// Get symbol.
    Input getSymbol() return nothrow @nogc
    {
        size_t i = 0;
        while ((!peekNextNth(i).among!('\0', '(', ')', '"', whiteChars))) // NOTE this is faster than !src[i].isWhite
            ++i;
        return skipOverN(i);
    }

    Input getWhitespace() return nothrow @nogc
    {
        size_t i = 0;
        while (peekNextNth(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
            ++i;
        return skipOverN(i);
    }

    /// Get string literal in input.
    Input getStringLiteral() return nothrow @nogc
    {
        dropFront();
        size_t i = 0;
        while (!peekNextNth(i).among!('\0', '"'))
        {
            if (peekNextNth(i) == '\\' &&
                peekNextNth(i + 1) == '"')
            {
                i += 2;         // skip \n
                continue;
            }
            ++i;
        }
        const literal = peekNextsN(i);
        dropFrontN(i);
        if (peekNext() == '"')
            dropFront();        // pop ending double singlequote
        return literal;
    }

    /// Get character literal in input.
    Input getCharacterLiteral() return nothrow @nogc
    {
        dropFront();
        size_t i = 0;
        while (!peekNextNth(i).among!('\0', '\''))
        {
            if (peekNextNth(i) == '\\' &&
                peekNextNth(i + 1) == '\'')
            {
                i += 2;         // skip \n
                continue;
            }
            ++i;
        }
        const literal = peekNextsN(i);
        dropFrontN(i);
        if (peekNext() == '\'')
            dropFront();        // pop ending double singlequote
        return literal;
    }

    void nextFront()
    {
        import std.range.primitives : empty, front, popFront, popFrontN;
        import std.uni : isWhite, isAlpha;
        import std.ascii : isDigit;

        while (true)
        {
            switch (peekNext)
            {
            case '/':
                if (peekNextNth(1) == '/') // `//`
                {
                    _offset += 2;
                    skipLineComment();
                    if (_includeComments)
                    {
                        // TODO: store comment
                    }
                }
                else if (peekNextNth(1) == '*') // `/*`
                {
                    _offset += 2;
                    skipBlockComment();
                    if (_includeComments)
                    {
                        // TODO: store comment
                    }
                }
                else
                    assert(false);
                break;
            case '(':
                _token = Token(TOK.leftParen, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case ')':
                _token = Token(TOK.rightParen, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '"':
                const stringLiteral = getStringLiteral();
                _token = Token(TOK.stringLiteral, stringLiteral);
                break;
            case '\'':
                const characterLiteral = getCharacterLiteral();
                _token = Token(TOK.characterLiteral, characterLiteral);
                break;
            case '0':
                break;
            case ' ':
            case '\t':
            case '\n':
            case '\v':
            case '\r':
            case '\f':
                assert(peekNext.isWhite);
                getWhitespace();
                break;
            case '\0':
                _endOfFile = true;
                return;
            default:
                if (peekNext.isAlpha)
                {
                    const symbol = getSymbol();
                    _token = Token(TOK.characterLiteral, symbol);
                }
                else
                    // other
                    assert(false,
                           `Cannot handle character '` ~ peekNext.to!string ~
                           `' at charater offset:` ~ _offset.to!string);
            }
        }
    }

    public ptrdiff_t offsetTo(scope const char[] expr) const @trusted pure nothrow @nogc
    {
        return expr.ptr - _input.ptr;
    }

    import nxt.line_column : LineColumn, offsetLineColumn;

    public LineColumn offsetToLineColumn(size_t offset) const @trusted pure nothrow @nogc
    {
        return offsetLineColumn(_input, offset);
    }

    public LineColumn charsToLineColumn(scope const(char)[] chars) const @trusted pure nothrow @nogc
    {
        return offsetLineColumn(_input, offsetTo(chars));
    }

private:
    size_t _offset;             // current offset in `_input`
    const Input _input;         // input

    Token _token;
    bool _endOfFile;            // signals null terminator found
    bool _includeComments = false;
}

/// G4 parser.
struct G4Parser
{
    alias Input = const(char)[];
    this(Input input,
         bool includeComments = false) @trusted
    {
        _lexer = G4Lexer(input);
    }

private:
    G4Lexer _lexer;
}

/// G4 filer parser.
struct G4FileParser           // TODO: convert to `class`
{
@safe:
    this(const string filePath)
    {
        debug writeln(filePath);
        import std.path : expandTilde;
        import nxt.file_ex : rawReadPath;
        const path = filePath.expandTilde.rawReadPath();
        parser = G4Parser(cast(G4Parser.Input)path, false);
        lexer = G4Lexer(cast(G4Lexer.Input)path, false);
    }
    ~this() @nogc
    {
    }
    G4Lexer lexer;
    G4Parser parser;
    alias parser this;
}

///
unittest
{
    import std.file : dirEntries, SpanMode;
    import std.path : expandTilde;
    foreach (dirEntry; dirEntries("~/Work/grammars-v4/".expandTilde, SpanMode.breadth))
    {
        import nxt.array_algorithm : endsWith;
        if (dirEntry.name.endsWith(`g4`))
        {
            auto parser = G4FileParser(dirEntry.name);
            while (!parser.lexer.empty)
            {
                writeln(parser.lexer.front);
                parser.lexer.popFront();
            }
        }
    }
}
