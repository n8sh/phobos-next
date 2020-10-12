/** Lexer and parser for ANTLR (G,G2,G4) grammars.
 *
 * See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687036/ANTLR+Cheat+Sheet
 * See_Also: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
 * See_Also: https://github.com/antlr/grammars-v4
 * See_Also: https://github.com/antlr/grammars-v4/blob/master/bnf/bnf.g4
 * See_Also: https://stackoverflow.com/questions/53245751/convert-a-form-of-bnf-grammar-to-g4-grammar
 * See_Also: https://bnfc.digitalgrammars.com/
 */
module nxt.antlr;

import std.conv : to;
import std.stdio : writeln;

// `d-deps.el` requires these to be at the top:
import nxt.line_column : LineColumn, offsetLineColumn;
import nxt.file_ex : rawReadPath;

enum useKeywords = true;

///< Token kind. TODO: make this a string type like with std.experimental.lexer
enum TOK
{
    unknown,                    ///< Unknown.

    // Keywords:
    SCOPE,          ///< Dynamically scoped attribute.
    FRAGMENT,       ///< Lexer rule is a helper rule, not real token for parser.
    LEXER,          ///< Grammar type.
    TREE,           ///< Grammar type.
    PARSER,         ///< Grammar type
    GRAMMAR,        ///< Grammar header.
    RETURNS,        ///< Rule return value(s).
    THROWS,         ///< Rule throws exceptions(s).
    CATCH,          ///< Catch rule exceptions(s).
    FINALLY,        ///< Do this no matter what.
    OPTIONS,        ///< Grammar or rule options.
    TOKENS,         ///< Can add tokens with this; usually imaginary tokens.
    IMPORT,         ///< Import grammar(s).

    symbol,                     ///< Symbol.
    attributeSymbol,            ///< Attribute Symbol (starting with `$`).
    actionSymbol,               ///< Action Symbol (starting with `@`).

    number,                     ///< Number.

    lineComment,                ///< Single line comment.
    blockComment,               ///< Multi-line (block) comment.

    leftParen,                  ///< Left parenthesis.
    rightParen,                 ///< Right parenthesis.

    action,                  ///< Code block.

    alts,                       ///< Alternatives within '[' ... ']'

    textLiteralSingleQuoted,    ///< Text (string) literal, surrounded by single quotes.
    textLiteralDoubleQuoted,    ///< Text (string) literal, surrounded by double quotes.

    colon,                      ///< Colon `:`.
    semicolon,                  ///< Semicolon `;`.
    hash,                       ///< Hash `#`
    labelAssignment,            ///< Label assignment `=`
    listLabelAssignment,        ///< List label assignment `+=`

    zeroOrMore,                 ///< Zero or more (`*`)
    oneOrMore,                  ///< One or more (`+`)
    alternative,                ///< Alternative (`|`)
    negation,                   ///< Match negation (`~`)
    optOrSemPred,               ///< Optional or semantic predicate (`?`)
    lt,                         ///< `<`
    gt,                         ///< `>`
    comma,                      ///< `.`
    exclude,                    ///< Exclude from AST (`!`)
    rootNode,                   ///< Root node (`^`)
    wildcard,                   ///< `.`
    dotdot,                     ///< `..`
    rewrite,                    ///< Rewrite rule (`->`)
    alwaysIncludePredicate,     ///< Rewrite rule (`=>`)

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

static bool isSymbolStart(in dchar ch) pure nothrow @safe @nogc
{
    import std.uni : isAlpha;
    return (ch.isAlpha ||
            ch == '_' ||
            ch == '$' ||
            ch == '@');
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

    /// Skip line comment.
    Input getLineComment() return nothrow @nogc
    {
        size_t i;
        while (!peekCharNth(i).among!('\0', endOfLineChars))
        {
            ++i;
        }
        return skipOverN(i);
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
        import std.uni : isAlphaNum;
        size_t i;
        if (peekChar.isSymbolStart)
            ++i;
        while (peekCharNth(i).isAlphaNum ||
               peekCharNth(i) == '_')
            ++i;
        return skipOverN(i);
    }

    /// Get number.
    Input getNumber() return nothrow @nogc
    {
        import std.ascii : isDigit;
        size_t i;
        while (peekCharNth(i).isDigit)
            ++i;
        return skipOverN(i);
    }

    Input getWhitespace() return nothrow @nogc
    {
        size_t i;
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
        size_t i;
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
        size_t i;
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
        size_t i;
        while (!peekCharNth(i).among!('\0', ']'))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        if (peekCharNth(i + 1))
            ++i;
        return skipOverN(i);
    }

    Input getAction() return nothrow @nogc
    {
        size_t i;

        import nxt.dynamic_array : DynamicArray;
        DynamicArray!char ds;   // delimiter stack

        bool inBlockComment;
        bool inLineComment;
        bool inChar;
        bool inString;

        const infoFlag = false;

        while (!peekCharNth(i).among!('\0'))
        {
            // skip over all escape sequences in quoted
            if (inChar ||
                inString)
            {
                while (skipOverEsc(i)) {}
            }

            if (!inBlockComment &&
                !inLineComment &&
                !inChar &&
                !inString)
            {
                if (peekCharNth(i) == '/' &&
                    peekCharNth(i + 1) == '/')
                {
                    if (infoFlag) info("line comment start", i, ds[]);
                    inLineComment = true;
                    i += 2;
                    continue;
                }
                else if (peekCharNth(i) == '/' &&
                         peekCharNth(i + 1) == '*')
                {
                    if (infoFlag) info("block comment start", i, ds[]);
                    inBlockComment = true;
                    i += 2;
                    continue;
                }
                else if (peekCharNth(i) == '{')
                {
                    if (infoFlag) info("brace open", i, ds[]);
                    ds.insertBack('{');
                }
                else if (peekCharNth(i) == '}')
                {
                    if (infoFlag) info("brace close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '{')
                        error("unmatched", i);
                    ds.popBack();
                }
                else if (peekCharNth(i) == '[')
                {
                    if (infoFlag) info("hook open", i, ds[]);
                    ds.insertBack('[');
                }
                else if (peekCharNth(i) == ']')
                {
                    if (infoFlag) info("hook close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '[')
                        error("unmatched", i);
                    ds.popBack();
                }
                else if (peekCharNth(i) == '(')
                {
                    if (infoFlag) info("paren open", i, ds[]);
                    ds.insertBack('(');
                }
                else if (peekCharNth(i) == ')')
                {
                    if (infoFlag) info("paren close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '(')
                        error("unmatched", i);
                    ds.popBack();
                }
            }

            // block comment close
            if (inBlockComment &&
                peekCharNth(i) == '*' &&
                peekCharNth(i + 1) == '/')
            {
                if (infoFlag) info("block comment close", i, ds[]);
                inBlockComment = false;
                i += 2;
                continue;
            }

            // line comment close
            if (inLineComment &&
                (peekCharNth(i) == '\n' ||
                 peekCharNth(i) == '\r'))
            {
                if (infoFlag) info("line comment close", i, ds[]);
                inLineComment = false;
            }

            // single-quote open/close
            if (!inBlockComment &&
                !inLineComment &&
                !inString &&
                peekCharNth(i) == '\'')
            {
                if (!ds.empty &&
                    ds.back == '\'')
                {
                    if (infoFlag) info("single-quote close", i, ds[]);
                    ds.popBack();
                    inChar = false;
                }
                else
                {
                    if (infoFlag) info("single-quote open", i, ds[]);
                    ds.insertBack('\'');
                    inChar = true;
                }
            }

            // double-quote open/close
            if (!inBlockComment &&
                !inLineComment &&
                !inChar &&
                peekCharNth(i) == '"')
            {
                if (!ds.empty &&
                    ds.back == '"')
                {
                    if (infoFlag) info("double-quote close", i, ds[]);
                    ds.popBack();
                    inString = false;
                }
                else
                {
                    if (infoFlag) info("doubl-quote open", i, ds[]);
                    ds.insertBack('"');
                    inString = true;
                }
            }

            i += 1;

            if (ds.length == 0)
                break;
        }

        if (inBlockComment)
            error("unterminated block comment", i);
        if (ds.length != 0)
            error("unbalanced code block", i);

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
                _token = Token(TOK.action, getAction());
                break;
            case '[':
                _token = Token(TOK.alts, getAlts());
                break;
            case '"':
                _token = Token(TOK.textLiteralDoubleQuoted,
                               getTextLiteralDoubleQuoted());
                break;
            case '\'':
                _token = Token(TOK.textLiteralSingleQuoted,
                               getTextLiteralSingleQuoted());
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
                if (peekCharNth(1) == '>')
                {
                    _token = Token(TOK.alwaysIncludePredicate, _input[_offset .. _offset + 2]);
                    _offset += 2;
                }
                else
                {
                    _token = Token(TOK.labelAssignment, _input[_offset .. _offset + 1]);
                    _offset += 1;
                }
                break;
            case '*':
                _token = Token(TOK.zeroOrMore, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '+':
                if (peekCharNth(1) == '=')
                {
                    _token = Token(TOK.listLabelAssignment, _input[_offset .. _offset + 2]);
                    _offset += 2;
                }
                else
                {
                    _token = Token(TOK.oneOrMore, _input[_offset .. _offset + 1]);
                    _offset += 1;
                }
                break;
            case '|':
                _token = Token(TOK.alternative, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '~':
                _token = Token(TOK.negation, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '?':
                _token = Token(TOK.optOrSemPred, _input[_offset .. _offset + 1]);
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
            case '!':
                _token = Token(TOK.exclude, _input[_offset .. _offset + 1]);
                _offset += 1;
                break;
            case '^':
                _token = Token(TOK.rootNode, _input[_offset .. _offset + 1]);
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
                    _token = Token(TOK.wildcard, _input[_offset .. _offset + 1]);
                    _offset += 1;
                }
                break;
            case '-':
                if (peekCharNth(1) == '>') // `->`
                {
                    _token = Token(TOK.rewrite, _input[_offset .. _offset + 2]);
                    _offset += 2;
                }
                else
                    error("unexpected character");
                break;
            case '0':
                ..
            case '9':
                _token = Token(TOK.number, getNumber());
                break;
                // from std.ascii.isWhite
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
                if (peekChar.isSymbolStart)
                {
                    const symbol = getSymbol();
                    switch (symbol[0])
                    {
                    case '$':
                        _token = Token(TOK.attributeSymbol, symbol);
                        break;
                    case '@':
                        _token = Token(TOK.actionSymbol, symbol);
                        break;
                    default:
                        static if (useKeywords)
                        {
                            switch (symbol)
                            {
                            case "scope": _token = Token(TOK.SCOPE, symbol); break;
                            case "fragment": _token = Token(TOK.FRAGMENT, symbol); break;
                            case "lexer": _token = Token(TOK.LEXER, symbol); break;
                            case "tree": _token = Token(TOK.TREE, symbol); break;
                            case "parser": _token = Token(TOK.PARSER, symbol); break;
                            case "grammar": _token = Token(TOK.GRAMMAR, symbol); break;
                            case "returns": _token = Token(TOK.RETURNS, symbol); break;
                            case "throws": _token = Token(TOK.THROWS, symbol); break;
                            case "catch": _token = Token(TOK.CATCH, symbol); break;
                            case "finally": _token = Token(TOK.FINALLY, symbol); break;
                            case "options": _token = Token(TOK.OPTIONS, symbol); break;
                            case "tokens": _token = Token(TOK.TOKENS, symbol); break;
                            case "import": _token = Token(TOK.IMPORT, symbol); break;
                            default: _token = Token(TOK.symbol, symbol); break;
                            }
                        }
                        else
                        {
                            _token = Token(TOK.symbol, symbol);
                        }
                        break;
                    }
                }
                else
                    error("unexpected character");
            }
        }
    }

    // TODO: into warning(const char* format...) like in `dmd` and put in `nxt.parsing` and reuse here and in lispy.d
    void error(const string msg, size_t i = 0) const @trusted nothrow @nogc scope
    {
        message("Error", msg, i);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    void warning(const string msg, size_t i = 0) const @trusted nothrow @nogc scope
    {
        message("Warning", msg, i);
    }

    void info(const string msg, size_t i = 0, const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        message("Info", msg, i, ds);
    }

    void message(const string tag,
                 const string msg,
                 size_t i = 0,
                 const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        import core.stdc.stdio : printf;
        const lc = offsetLineColumn(_input, _offset + i);
        debug printf("%.*s(%u,%u): %s: %.*s at offset %llu being char `%c` ds:`%.*s`\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     tag.ptr,
                     cast(int)msg.length, msg.ptr,
                     _offset + i,
                     peekCharNth(i),
                     cast(int)ds.length, ds.ptr);
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
        // if (!filePath.endsWith(`links.g2`))
        //     continue;

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
