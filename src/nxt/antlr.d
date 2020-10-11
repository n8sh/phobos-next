/** Lexer and parser for ANTLR (G,G2,G4) grammars.
 *
 * See_Also: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
 * See_Also: https://github.com/antlr/grammars-v4
 * See_Also: https://github.com/antlr/grammars-v4/blob/master/bnf/bnf.g4
 * See_Also: https://stackoverflow.com/questions/53245751/convert-a-form-of-bnf-grammar-to-g4-grammar
 * See_Also: https://bnfc.digitalgrammars.com/
 */
module nxt.antlr;

import std.conv : to;
import core.stdc.stdio : printf;
import std.stdio : writeln;

// `d-deps.el` requires these to be at the top:
import nxt.line_column : LineColumn, offsetLineColumn;
import nxt.file_ex : rawReadPath;

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

    leftBrace,                  ///< Left curly brace.
    rightBrace,                 ///< Right curly brace.

    alts,                       ///< Alternatives within '[' ... ']'

    textLiteralSingleQuoted,    ///< Text (string) literal, surrounded by single quotes.
    textLiteralDoubleQuoted,    ///< Text (string) literal, surrounded by double quotes.

    colon,                      ///< Colon `:`.
    semicolon,                  ///< Semicolon `;`.
    hash,                       ///< Hash `#`
    equal,                      ///< Hash `=`

    star,                       ///< `*`
    plus,                       ///< `+`
    pipe,                       ///< `|`
    tilde,                      ///< `~`
    qmark,                      ///< `?`
    lt,                         ///< `<`
    gt,                         ///< `>`
    comma,                      ///< `.`
    dot,                        ///< `.`
    dotdot,                     ///< `..`
    arrow,                      ///< `->`

    _error,                     ///< Error token.
}

/// G4 rule.
struct Token
{
@safe pure nothrow @nogc:
    this(TOK tt, const(char)[] input)
    {
        this.tt = tt;
        this.input = input;
        // debug writeln("tt:", tt, " input:", input);
    }
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
         string path = null,
         bool includeComments = false) @trusted
    {
        _input = input;
        _path = path;

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

    Token front() const scope return nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _token;
    }

    void popFront() nothrow
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

    /// Peek next `char` in input.
    dchar peekChar() const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset];    // TODO: .ptr. TODO: decode `dchar`
    }

    /// Peek n-th next `char` in input.
    dchar peekCharNth(size_t n) const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset + n]; // TODO: .ptr. TODO: decode `dchar`
    }

    /// Get next n `chars` in input as an array of `char`.
    Input peekStringN(size_t n) const return scope nothrow @nogc
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
        while (!peekChar().among!('\0', endOfLineChars))
        {
            _offset += 1;
        }
    }

    /// Skip block comment.
    void skipBlockComment() scope nothrow @nogc
    {
        while (!peekChar().among!('\0'))
        {
            if (peekChar == '*' &&
                peekCharNth(1) == '/')
            {
                _offset += 2;
                return;
            }
            _offset += 1;
        }
        error("unterminated block comment");
    }

    /// Get symbol.
    Input getSymbol() return nothrow @nogc
    {
        size_t i = 0;
        import std.uni : isAlpha, isAlphaNum;
        if (peekChar.isAlpha ||
            peekChar == '_' ||
            peekChar == '@')
            ++i;
        while (peekCharNth(i).isAlphaNum ||
               peekCharNth(i) == '_')
            ++i;
        return skipOverN(i);
    }

    Input getWhitespace() return nothrow @nogc
    {
        size_t i = 0;
        while (peekCharNth(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
            ++i;
        return skipOverN(i);
    }

    bool skipOverEsc(ref size_t i) nothrow @nogc
    {
        if (peekCharNth(i) == '\\')
        {
            ++i;
            if (peekCharNth(i) == 'n')
            {
                ++i;            // TODO: convert to "\r"
            }
            else if (peekCharNth(i) == 't')
            {
                ++i;            // TODO: convert to "\t"
            }
            else if (peekCharNth(i) == 'r')
            {
                ++i;            // TODO: convert to ASCII "\r"
            }
            else if (peekCharNth(i) == ']')
            {
                ++i;            // TODO: convert to ASCII "]"
            }
            else if (peekCharNth(i) == 'u')
            {
                ++i;
                import std.ascii : isDigit;
                while (peekCharNth(i).isDigit)
                    ++i;
                // TODO: convert to `dchar`
            }
            else if (peekCharNth(i) == '\0')
            {
                error("unterminated escape sequence at end of file");
                return false;
            }
            else
            {
                i += 1;
            }
            return true;
        }
        return false;
    }

    Input getTextLiteralDoubleQuoted() return nothrow @nogc
    {
        dropFront();
        size_t i = 0;
        while (!peekCharNth(i).among!('\0', '"'))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        const literal = peekStringN(i);
        dropFrontN(i);
        if (peekChar() == '"')
            dropFront();        // pop ending double singlequote
        return literal;
    }

    Input getTextLiteralSingleQuoted() return nothrow @nogc
    {
        dropFront();
        size_t i = 0;
        while (!peekCharNth(i).among!('\0', '\''))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        const literal = peekStringN(i);
        dropFrontN(i);
        if (peekChar() == '\'')
            dropFront();        // pop ending double singlequote
        return literal;
    }

    Input getAlts() return nothrow @nogc
    {
        size_t i = 0;
        while (!peekCharNth(i).among!('\0', ']'))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        if (peekCharNth(i + 1))
        {
            ++i;
        }
        return skipOverN(i);
    }

    void nextFront() nothrow @nogc
    {
        import std.uni : isWhite;

        while (true)
        {
            switch (peekChar)
            {
            case '/':
                if (peekCharNth(1) == '/') // `//`
                {
                    _offset += 2;
                    skipLineComment();
                    if (_includeComments)
                    {
                        // TODO: store comment
                    }
                }
                else if (peekCharNth(1) == '*') // `/*`
                {
                    _offset += 2;
                    skipBlockComment();
                    if (_includeComments)
                    {
                        // TODO: store comment
                    }
                }
                else
                    error("unexpected character");
                break;
            case '(':
                _token = Token(TOK.leftParen, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case ')':
                _token = Token(TOK.rightParen, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '{':
                _token = Token(TOK.leftBrace, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '}':
                _token = Token(TOK.rightBrace, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '[':
                const alts = getAlts();
                _token = Token(TOK.alts, alts);
                break;
            case '"':
                const literal = getTextLiteralDoubleQuoted();
                _token = Token(TOK.textLiteralDoubleQuoted, literal);
                break;
            case '\'':
                const literal = getTextLiteralSingleQuoted();
                _token = Token(TOK.textLiteralSingleQuoted, literal);
                break;
            case ':':
                _token = Token(TOK.colon, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case ';':
                _token = Token(TOK.semicolon, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '#':
                _token = Token(TOK.hash, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '=':
                _token = Token(TOK.equal, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '*':
                _token = Token(TOK.star, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '+':
                _token = Token(TOK.plus, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '|':
                _token = Token(TOK.pipe, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '~':
                _token = Token(TOK.tilde, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '?':
                _token = Token(TOK.qmark, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '<':
                _token = Token(TOK.lt, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '>':
                _token = Token(TOK.gt, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case ',':
                _token = Token(TOK.comma, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '.':
                if (peekCharNth(1) == '.') // `..`
                {
                    _token = Token(TOK.dotdot, _input[_offset .. _offset + 2]);
                    _offset += 2;
                }
                else
                {
                    _token = Token(TOK.dot, _input[_offset .. _offset + 1]);
                    _offset += 1;
                }
                break;
            case '-':
                if (peekCharNth(1) == '>') // `->`
                {
                    _token = Token(TOK.arrow, _input[_offset .. _offset + 1]);
                    _offset += 2;
                }
                else
                    error("unexpected character");
                break;
            case ' ':
            case '\t':
            case '\n':
            case '\v':
            case '\r':
            case '\f':
                assert(peekChar.isWhite);
                getWhitespace();
                break;
            case '\0':
                _endOfFile = true;
                return;
            default:
                import std.uni : isAlpha;
                if (peekChar.isAlpha ||
                    peekChar == '_' ||
                    peekChar == '@')
                {
                    const symbol = getSymbol();
                    _token = Token(TOK.textLiteralSingleQuoted, symbol);
                }
                else
                    error("unexpected character");
            }
        }
    }

    // TODO: into warning(const char* format...) like in `dmd` and put in `nxt.parsing` and reuse here and in lispy.d
    void error(const string msg) const @trusted nothrow @nogc scope
    {
        const lc = offsetToLineColumn();
        debug printf("%.*s(%u,%u): Error: %.*s at offset %llu being char `%c`\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     cast(int)msg.length, msg.ptr,
                     _offset,
                     peekChar);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    public ptrdiff_t offsetTo(scope const char[] expr) const @trusted nothrow @nogc
    {
        return expr.ptr - _input.ptr;
    }

    public LineColumn offsetToLineColumn() const @trusted nothrow @nogc
    {
        return offsetLineColumn(_input, _offset);
    }

    public LineColumn offsetToLineColumn(size_t offset) const @trusted nothrow @nogc
    {
        return offsetLineColumn(_input, offset);
    }

    public LineColumn charsToLineColumn(scope const(char)[] chars) const @trusted nothrow @nogc
    {
        return offsetLineColumn(_input, offsetTo(chars));
    }

private:
    size_t _offset;             // current offset in `_input`
    const Input _input;         ///< Input data.
    const string _path;         ///< Input file (or null if in-memory).

    Token _token;
    bool _endOfFile;            // signals null terminator found
    bool _includeComments = false;
}

/// G4 parser.
struct G4Parser
{
    alias Input = const(char)[];
    this(Input input,
         string path = null,
         bool includeComments = false) @trusted
    {
        _lexer = G4Lexer(input, path, includeComments);
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
        import std.path : expandTilde;
        const path = filePath.expandTilde;
        const data = cast(G4Parser.Input)rawReadPath(path); // cast to Input because we don't want to keep all file around:
        parser = G4Parser(data, filePath, false);
        lexer = G4Lexer(data, filePath, false);
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
        const filePath = dirEntry.name;

        // skip grammars with inline code. TODO: handle these by skipping over matching braces
        if (filePath.endsWith(`RexxLexer.g4`) ||
            filePath.endsWith(`V.g4`) ||
            filePath.endsWith(`PGN.g4`) ||
            filePath.endsWith(`ASN_3gpp.g4`) ||
            filePath.endsWith(`PlSqlLexer.g4`) ||
            filePath.endsWith(`PlSqlParser.g4`) ||
            filePath.endsWith(`SQLiteParser.g4`) ||
            filePath.endsWith(`Java6Lex.g`) ||
            filePath.endsWith(`WavefrontOBJ.g`) ||
            filePath.endsWith(`Sexpr.g`) ||
            filePath.endsWith(`simplecalc.g`) ||
            filePath.endsWith(`krl.g`) ||
            filePath.endsWith(`ASN.g`) ||
            filePath.endsWith(`FreeMPS.g`) ||
            filePath.endsWith(`C.g`) ||
            filePath.endsWith(`Antlr3.g`) ||
            filePath.endsWith(`Java9.g4`) ||
            filePath.endsWith(`JavadocLexer.g4`) ||
            filePath.endsWith(`LPC.g4`) ||
            filePath.endsWith(`XPath2.g`) ||
            filePath.endsWith(`XPath2.g`) ||
            filePath.endsWith(`ObjectiveC2ansi.g`) ||
            filePath.endsWith(`CPP14.g4`) ||
            filePath.endsWith(`Issue1567.g4`) ||
            filePath.endsWith(`Python2.g4`) ||
            filePath.endsWith(`PythonParser.g4`) ||
            filePath.endsWith(`Python3.g4`))
            continue; // so skip. TODO: replace with exception or error return `TOK._error` in iteration below

        if (filePath.endsWith(`.g`) ||
            filePath.endsWith(`.g2`) ||
            filePath.endsWith(`.g4`))
        {
            debug writeln("Scanning ", filePath);
            auto parser = G4FileParser(filePath);
            while (!parser.lexer.empty)
            {
                writeln(parser.lexer.front);
                parser.lexer.popFront();
            }
        }
    }
}
