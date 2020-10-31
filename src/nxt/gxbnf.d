/** Lexer/Parser Generator for ANTLR (G, G2, G4) and (E)BNF grammars.
 *
 * See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687036/ANTLR+Cheat+Sheet
 * See_Also: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
 * See_Also: https://github.com/antlr/grammars-v4
 * See_Also: https://github.com/antlr/grammars-v4/blob/master/bnf/bnf.g4
 * See_Also: https://stackoverflow.com/questions/53245751/convert-a-form-of-bnf-grammar-to-g4-grammar
 * See_Also: https://bnfc.digitalgrammars.com/
 *
 * TODO:
 *
 * - Find top rule is the rule that isn't referenced anywhere else
 *
 * - Use `DETECT` upper-case lexer rules LexerRule
 *
 * - Is `RulesByName` needed?
 *
 * - Use `TOK.tokenSpecOptions` in parsing. Ignored for now.
 *
 * - Add properties for uint, uint lengthRange()
 * - Sort `AltM` subs by descending minimum length
 *
 * - handle all TODO's in `makeRule`
 *
 * - create index of symbols and link them in second pass
 *
 * - Detect indirect mutual left-recursion. How? Simple-way in generated parsers:
 *   enters a rule again without offset change.
 *
 * - non-pure diagnostics functions
 *
 * - Warn about `options{greedy=false;}:` and advice to replace with non-greedy variants
 * - Warn about `options{greedy=true;}:` being deprecated
 *
 * - Display column range for tokens in messages. Use `head.input.length`.
 *   Requires updating FlyCheck.
 *   See: `-fdiagnostics-print-source-range-info` at https://clang.llvm.org/docs/UsersManual.html.
 *   See: https://clang.llvm.org/diagnostics.html
 *   Use GNU-style formatting such as: fix-it:"test.c":{45:3-45:21}:"gtk_widget_show_all".
 *
 * - Use a region allocator on top of the GC to pre-allocate the
 *   nodes. Maybe one region for each file. Calculate the region size from lexer
 *   statistics (number of operators, symbols and literals).
 *
 * - Emacs click on link in `compilation-mode` doesn't navigate to correct offset on lines containing tabs before offset
 *
 * - If performance is needed:
 *   - Avoid casts and instead compare against `head.tok` for `isA!NodeType`
 *   - use `RuleAltN(uint n)` in `makeAlt`
 *   - use `SeqN(uint n)` in `makeSeq`
 */
module nxt.gxbnf;

version = show;
version = Do_Inline;

enum useStaticTempArrays = true; ///< Use fixed-size (statically allocated) sequence and alternative buffers.

import core.lifetime : move;
import core.stdc.stdio : putchar, printf;

import std.conv : to;
import std.algorithm.iteration : substitute;

// `d-deps.el` requires these to be at the top:
import nxt.line_column : offsetLineColumn;
import nxt.fixed_array : FixedArray;
import nxt.dynamic_array : DynamicArray;
import nxt.file_ex : rawReadPath;
import nxt.array_algorithm : startsWith, endsWith, endsWithEither, skipOver, skipOverBack;
import nxt.conv_ex : toDefaulted;

import std.stdio : stdout, write, writeln;

@safe:

alias Input = const(char)[];      ///< Grammar input source.
alias Output = DynamicArray!char; ///< Generated parser output source.

alias RulesByName = Rule[Input];

enum matcherFunctionNamePrefix = `m__`;

///< Token kind. TODO: make this a string type like with std.experimental.lexer
enum TOK
{
    none,

    unknown,                    ///< Unknown

    whitespace,                 ///< Whitespace

    symbol,                     ///< Symbol
    attributeSymbol,            ///< Attribute Symbol (starting with `$`)
    actionSymbol,               ///< Action Symbol (starting with `@`)

    number,                     ///< Number

    lineComment,                ///< Single line comment
    blockComment,               ///< Multi-line (block) comment

    leftParen,                  ///< Left parenthesis
    rightParen,                 ///< Right parenthesis

    action,                     ///< Code block

    brackets,                   ///< Alternatives within '[' ... ']'

    literal,                    ///< Text literal, single or double quoted

    colon,                      ///< Colon `:`
    semicolon,                  ///< Semicolon `;`
    hash,                       ///< Hash `#`
    labelAssignment,            ///< Label assignment `=`
    listLabelAssignment,        ///< List label assignment `+=`

    qmark,                      ///< Greedy optional or semantic predicate (`?`)
    qmarkQmark,                 ///< Non-Greedy optional (`??`)

    star,                       ///< Greedy zero or more (`*`)
    starQmark,                  ///< Non-Greedy Zero or more (`*?`)

    plus,                       ///< Greedy one or more (`+`)
    plusQmark,                  ///< Non-Greedy One or more (`+?`)

    pipe,                       ///< Alternative (`|`)
    tilde,                      ///< Match negation (`~`)
    lt,                         ///< `<`
    gt,                         ///< `>`
    comma,                      ///< `.`
    exclamation,                ///< Exclude from AST (`!`)
    rootNode,                   ///< Root node (`^`)
    wildcard,                   ///< `.`
    dotdot,                     ///< `..`
    rewrite,                    ///< Rewrite rule (`->`)
    rewriteSyntacticPredicate,  ///< Rewrite rule (`=>`)

    /** Token spec options:
        "<"
        id ASSIGN optionValue
        ( SEMI id ASSIGN optionValue )*
        ">"
        ;
    */
    tokenSpecOptions,

    _error,                     ///< Error token
}

/// Gx rule.
struct Token
{
@safe pure nothrow @nogc:
    this(in TOK tok, in const(char)[] input = null)
    {
        this.tok = tok;
        this.input = input;
    }
    const(char)[] input;
    TOK tok;
}

static bool isSymbolStart(in dchar ch) pure nothrow @safe @nogc
{
    import std.uni : isAlpha;
    return (ch.isAlpha ||
            ch == '_' ||
            ch == '$' ||
            ch == '@');
}

/** Gx lexer for all version ANTLR grammsrs (`.g`, `.g2`, `.g4`).
 *
 * See_Also: `ANTLRv4Lexer.g4`
 */
struct GxLexer
{
    import std.algorithm.comparison : among;

@safe pure:

    this(in Input input,
         const string path = null,
         in bool includeComments = false,
         in bool includeWhitespace = false)
    {
        _input = input;
        _path = path;

        import std.exception : enforce;
        import nxt.parsing : isNullTerminated;
        enforce(_input.isNullTerminated, "Input isn't null-terminated"); // input cannot be trusted

        _includeComments = includeComments;
        _includeWhitespace = includeWhitespace;

        nextFront();
    }

    @disable this(this);

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        return _endOfFile;
    }

    inout(Token) front() inout scope return nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        assert(!empty);
        return _token;
    }

    void popFront() scope nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        assert(!empty);
        nextFront();
    }

    void frontEnforce(in TOK tok, const scope string msg = "") nothrow
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        if (front.tok != tok)
            errorAtFront(msg ~ ", expected `TOK." ~ tok.toDefaulted!string(null) ~ "`");
    }

    void popFrontEnforce(in TOK tok, const scope string msg) nothrow
    {
        version(D_Coverage) {} else version(LDC) version(Do_Inline) pragma(inline, true);
        if (frontPop().tok != tok)
            errorAtFront(msg ~ ", expected `TOK." ~ tok.toDefaulted!string(null) ~ "`");
    }

    Token frontPopEnforce(in TOK tok, const scope string msg = "") nothrow
    {
        version(D_Coverage) {} else version(LDC) version(Do_Inline) pragma(inline, true);
        const result = frontPop();
        if (result.tok != tok)
            errorAtFront(msg ~ ", expected `TOK." ~ tok.toDefaulted!string(null) ~ "`");
        return result;
    }

    Token frontPop() scope return nothrow @nogc
    {
        version(D_Coverage) {} else version(LDC) version(Do_Inline) pragma(inline, true);
        const result = front;
        popFront();
        return result;
    }

    Token skipOverToken(in Token token) scope return nothrow
    {
        if (front == token)
            return frontPop();
        return typeof(return).init;
    }

    Token skipOverTOK(in TOK tok) scope return nothrow
    {
        if (front.tok == tok)
            return frontPop();
        return typeof(return).init;
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

private:

    /// Peek next `char` in input.
    dchar peek0() const scope nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        return _input[_offset]; // TODO: decode `dchar`
    }

    /// Peek next next `char` in input.
    dchar peek1() const scope nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        return _input[_offset + 1]; // TODO: decode `dchar`
    }

    /// Peek `n`-th next `char` in input.
    dchar peekN(in size_t n) const scope nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        return _input[_offset + n]; // TODO: decode `dchar`
    }

    /// Drop next byte in input.
    void drop1() nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        _offset += 1;
    }

    /// Drop next `n` bytes in input.
    void dropN(in size_t n) nothrow @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        _offset += n;           // TODO: decode `dchar`
    }

    /// Skip over `n` bytes in input.
    Input skipOverN(in size_t n) return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        const part = _input[_offset .. _offset + n]; // TODO: decode `dchar`
        dropN(n);
        return part;
    }

    /// Skip over next `char`.
    Input skipOver1() return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        return _input[_offset .. ++_offset]; // TODO: decode `dchar`
    }

    /// Skip over next two `char`s.
    Input skipOver2() return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        return _input[_offset .. (_offset += 2)]; // TODO: decode `dchar`
    }

    /// Skip line comment.
    void skipLineComment() scope nothrow @nogc
    {
        while (!peek0().among!('\0', endOfLineChars))
            _offset += 1;       // TODO: decode `dchar`
    }

    /// Skip line comment.
    Input getLineComment() return nothrow @nogc
    {
        size_t i;
        while (!peekN(i).among!('\0', endOfLineChars))
            i += 1;                // TODO: decode `dchar`
        return skipOverN(i);    // TODO: decode `dchar`
    }

    /// Skip block comment.
    void skipBlockComment() scope nothrow @nogc
    {
        while (!peek0().among!('\0'))
        {
            if (peek0() == '*' &&
                peek1() == '/')
            {
                _offset += 2;
                return;
            }
            _offset += 1;
        }
        errorAtFront("unterminated block comment");
    }

    /// Get symbol.
    Input getSymbol() return nothrow @nogc
    {
        import std.uni : isAlphaNum; // TODO: decode `dchar`
        size_t i;
        const bool attributeFlag = peek0() == '@';
        if (peek0().isSymbolStart)
            i += 1;
        while (peekN(i).isAlphaNum ||
               peekN(i) == '_' ||
               (attributeFlag && // attribute name
                peekN(i) == ':')) // may include colon qualifier
        {
            i += 1;
        }

        // skip optional whitespace before label assignment
        auto j = i;
        while (peekN(j).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
            j += 1;

        if (peekN(j) == '=')         // label assignment
            return skipOverN(j + 1);
        else if (peekN(j) == '+' &&
                 peekN(j + 1) == '=') // list label assignment
            return skipOverN(j + 2);
        else
            return skipOverN(i);
    }

    /// Get number.
    Input getNumber() return nothrow @nogc
    {
        import std.ascii : isDigit;
        size_t i;
        while (peekN(i).isDigit)
            i += 1;
        return skipOverN(i);
    }

    Input getWhitespace() return nothrow @nogc
    {
        size_t i;
        while (peekN(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
            i += 1;
        return skipOverN(i);
    }

    bool skipOverEsc(ref size_t i) nothrow @nogc
    {
        if (peekN(i) == '\\')   // TODO: decode `dchar`
        {
            i += 1;
            if (peekN(i) == 'n')
                i += 1;            // TODO: convert to "\r"
            else if (peekN(i) == 't')
                i += 1;            // TODO: convert to "\t"
            else if (peekN(i) == 'r')
                i += 1;            // TODO: convert to ASCII "\r"
            else if (peekN(i) == ']')
                i += 1;            // TODO: convert to ASCII "]"
            else if (peekN(i) == 'u')
            {
                i += 1;
                import std.ascii : isDigit;
                while (peekN(i).isDigit)
                    i += 1;
                // TODO: convert to `dchar`
            }
            else if (peekN(i) == '\0')
                errorAtIndex("unterminated escape sequence at end of file");
            else
                i += 1;
            return true;
        }
        return false;
    }

    Input getLiteral(dchar terminator)() return nothrow @nogc
    {
        size_t i = 1;
        while (!peekN(i).among!('\0', terminator))
            if (!skipOverEsc(i))
                i += 1;
        if (peekN(i) == '\0')
            errorAtIndex("unterminated string literal at end of file");
        return skipOverN(i + 1); // include terminator
    }

    Input getTokenSpecOptions() return nothrow @nogc
    {
        enum dchar terminator = '>';
        size_t i = 1;
        while (!peekN(i).among!('\0', terminator))
            i += 1;
        if (peekN(i) != terminator)
        {
            if (peekN(i) == '\0')
                errorAtIndex("unterminated string literal at end of file");
            else
                errorAtIndex("unterminated token spec option");
        }
        return skipOverN(i + 1); // include terminator '>'
    }

    Input getHooks() return nothrow @nogc
    {
        size_t i;
        while (!peekN(i).among!('\0', ']')) // may contain whitespace
            if (!skipOverEsc(i))
                i += 1;
        if (peekN(i) == ']') // skip ']'
            i += 1;
        return skipOverN(i);
    }

    Input getAction() return nothrow @nogc
    {
        size_t i;

        DynamicArray!char ds;   // delimiter stack

        bool inBlockComment;
        bool inLineComment;
        bool inChar;
        bool inString;

        const infoFlag = false;

        while (!peekN(i).among!('\0'))
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
                if (peekN(i) == '/' &&
                    peekN(i + 1) == '/')
                {
                    if (infoFlag) infoAtIndex("line comment start", i, ds[]);
                    inLineComment = true;
                    i += 2;
                    continue;
                }
                else if (peekN(i) == '/' &&
                         peekN(i + 1) == '*')
                {
                    if (infoFlag) infoAtIndex("block comment start", i, ds[]);
                    inBlockComment = true;
                    i += 2;
                    continue;
                }
                else if (peekN(i) == '{')
                {
                    if (infoFlag) infoAtIndex("brace open", i, ds[]);
                    ds.put('{');
                }
                else if (peekN(i) == '}')
                {
                    if (infoFlag) infoAtIndex("brace close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '{')
                        errorAtIndex("unmatched", i);
                    ds.popBack();
                }
                else if (peekN(i) == '[')
                {
                    if (infoFlag) infoAtIndex("hook open", i, ds[]);
                    ds.put('[');
                }
                else if (peekN(i) == ']')
                {
                    if (infoFlag) infoAtIndex("hook close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '[')
                        errorAtIndex("unmatched", i);
                    ds.popBack();
                }
                else if (peekN(i) == '(')
                {
                    if (infoFlag) infoAtIndex("paren open", i, ds[]);
                    ds.put('(');
                }
                else if (peekN(i) == ')')
                {
                    if (infoFlag) infoAtIndex("paren close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '(')
                        errorAtIndex("unmatched", i);
                    ds.popBack();
                }
            }

            // block comment close
            if (inBlockComment &&
                peekN(i) == '*' &&
                peekN(i + 1) == '/')
            {
                if (infoFlag) infoAtIndex("block comment close", i, ds[]);
                inBlockComment = false;
                i += 2;
                continue;
            }

            // line comment close
            if (inLineComment &&
                (peekN(i) == '\n' ||
                 peekN(i) == '\r'))
            {
                if (infoFlag) infoAtIndex("line comment close", i, ds[]);
                inLineComment = false;
            }

            // single-quote open/close
            if (!inBlockComment &&
                !inLineComment &&
                !inString &&
                peekN(i) == '\'')
            {
                if (!ds.empty &&
                    ds.back == '\'')
                {
                    if (infoFlag) infoAtIndex("single-quote close", i, ds[]);
                    ds.popBack();
                    inChar = false;
                }
                else
                {
                    if (infoFlag) infoAtIndex("single-quote open", i, ds[]);
                    ds.put('\'');
                    inChar = true;
                }
            }

            // double-quote open/close
            if (!inBlockComment &&
                !inLineComment &&
                !inChar &&
                peekN(i) == '"')
            {
                if (!ds.empty &&
                    ds.back == '"')
                {
                    if (infoFlag) infoAtIndex("double-quote close", i, ds[]);
                    ds.popBack();
                    inString = false;
                }
                else
                {
                    if (infoFlag) infoAtIndex("doubl-quote open", i, ds[]);
                    ds.put('"');
                    inString = true;
                }
            }

            i += 1;

            if (ds.length == 0)
                break;
        }

        if (inBlockComment)
            errorAtIndex("unterminated block comment", i);
        if (ds.length != 0)
            errorAtIndex("unbalanced code block", i);

        return skipOverN(i);
    }

    void nextFront() scope nothrow @nogc @trusted
    {
        switch (peek0())
        {
        case '/':
            if (peek1() == '/') // `//`
            {
                _offset += 2;
                skipLineComment();
                if (_includeComments)
                    _token = Token(TOK.lineComment);
                else
                    nextFront();
            }
            else if (peek1() == '*') // `/*`
            {
                _offset += 2;
                skipBlockComment();
                if (_includeComments)
                    _token = Token(TOK.blockComment);
                else
                    return nextFront();
            }
            else
                errorAtIndex("unexpected character");
            break;
        case '(':
            _token = Token(TOK.leftParen, skipOver1());
            break;
        case ')':
            _token = Token(TOK.rightParen, skipOver1());
            break;
        case '{':
            _token = Token(TOK.action, getAction());
            break;
        case '[':
            _token = Token(TOK.brackets, getHooks());
            break;
        case '"':
            _token = Token(TOK.literal, getLiteral!('"')());
            break;
        case '\'':
            _token = Token(TOK.literal, getLiteral!('\'')());
            break;
        case ':':
            _token = Token(TOK.colon, skipOver1());
            break;
        case ';':
            _token = Token(TOK.semicolon, skipOver1());
            break;
        case '#':
            _token = Token(TOK.hash, skipOver1());
            break;
        case '=':
            if (peek1() == '>')
                _token = Token(TOK.rewriteSyntacticPredicate, skipOver2());
            else
                errorAtFront("expected '>' after '='");
            break;
        case '?':
            if (peek1() == '?')
                _token = Token(TOK.qmarkQmark, skipOver2());
            else
                _token = Token(TOK.qmark, skipOver1());
            break;
        case '*':
            if (peek1() == '?')
                _token = Token(TOK.starQmark, skipOver2());
            else
                _token = Token(TOK.star, skipOver1());
            break;
        case '+':
            if (peek1() == '=')
                _token = Token(TOK.listLabelAssignment, skipOver2());
            else if (peek1() == '?')
                _token = Token(TOK.plusQmark, skipOver2());
            else
                _token = Token(TOK.plus, skipOver1());
            break;
        case '|':
            _token = Token(TOK.pipe, skipOver1());
            break;
        case '~':
            _token = Token(TOK.tilde, skipOver1());
            break;
        case '<':
            _token = Token(TOK.tokenSpecOptions, getTokenSpecOptions());
            break;
        case ',':
            _token = Token(TOK.comma, skipOver1());
            break;
        case '!':
            _token = Token(TOK.exclamation, skipOver1());
            break;
        case '^':
            _token = Token(TOK.rootNode, skipOver1());
            break;
        case '.':
            if (peek1() == '.') // `..`
                _token = Token(TOK.dotdot, skipOver2());
            else
                _token = Token(TOK.wildcard, skipOver1());
            break;
        case '-':
            if (peek1() == '>') // `->`
                _token = Token(TOK.rewrite, skipOver2());
            else
                errorAtIndex("unexpected character");
            break;
        case '0':
            ..
        case '9':
            _token = Token(TOK.number, getNumber());
            break;
        case ' ':
        case '\t':
        case '\n':
        case '\v':
        case '\r':
        case '\f':
            // TODO: extend to std.uni
            // import std.uni : isWhite;
            // assert(peek0().isWhite);
            const ws = getWhitespace();
            if (_includeWhitespace)
                _token = Token(TOK.whitespace, ws);
            else
                return nextFront();
            break;
        case '\0':
            _token = Token.init;
            _endOfFile = true;
            return;
        default:
            if (peek0().isSymbolStart)
            {
                const symbol = getSymbol();
                if (symbol.endsWith("+="))
                {
                    if (_includeListLabelAssignment)
                        _token = Token(TOK.listLabelAssignment, symbol);
                    else
                        return nextFront();
                }
                else if (symbol.endsWith('='))
                {
                    if (_includeLabelAssignment)
                        _token = Token(TOK.labelAssignment, symbol);
                    else
                        return nextFront();
                }
                else
                    switch (symbol[0])
                    {
                    case '$':
                        _token = Token(TOK.attributeSymbol, symbol);
                        break;
                    case '@':
                        _token = Token(TOK.actionSymbol, symbol);
                        break;
                    default:
                        _token = Token(TOK.symbol, symbol);
                        break;
                    }
            }
            else
            {
                _token = Token(TOK._error);
                errorAtIndex("unexpected character");
            }
        }
    }

    void infoAtFront(const scope string msg) const nothrow @nogc scope
    {
        messageAtToken(front, "Info", msg);
    }

    void warningAtFront(const scope string msg) const nothrow @nogc scope
    {
        messageAtToken(front, "Warning", msg);
    }

    void errorAtFront(const scope string msg) const nothrow @nogc scope
    {
        messageAtToken(front, "Error", msg);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    private void infoAtToken(in Token token,
                             const scope string msg) const nothrow @nogc scope
    {
        messageAtToken(token, "Info", msg);
    }

    private void warningAtToken(in Token token,
                                const scope string msg) const nothrow @nogc scope
    {
        messageAtToken(token, "Warning", msg);
    }

    private void errorAtToken(in Token token,
                              const scope string msg) const nothrow @nogc scope
    {
        messageAtToken(token, "Error", msg);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    private void messageAtToken(in Token token,
                                const scope string tag,
                                const scope string msg) const @trusted nothrow @nogc scope
    {
        const offset = token.input.ptr - _input.ptr; // unsafe
        const lc = offsetLineColumn(_input, offset);
        debug printf("%.*s(%u,%u): %s: %.*s, token `%.*s` at offset %llu\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     tag.ptr,
                     cast(int)msg.length, msg.ptr,
                     cast(int)token.input.length, token.input.ptr,
                     offset);
    }

    // TODO: into warning(const char* format...) like in `dmd` and put in `nxt.parsing` and reuse here and in lispy.d
    void errorAtIndex(const scope string msg,
                      in size_t i = 0) const nothrow @nogc scope
    {
        messageAtIndex("Error", msg, i);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    void warningAtIndex(const scope string msg,
                        in size_t i = 0) const nothrow @nogc scope
    {
        messageAtIndex("Warning", msg, i);
    }

    void infoAtIndex(const scope string msg,
                     in size_t i = 0, in const(char)[] ds = null) const nothrow @nogc scope
    {
        messageAtIndex("Info", msg, i, ds);
    }

    void messageAtIndex(const scope string tag,
                        const scope string msg,
                        in size_t i = 0,
                        in const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        const lc = offsetLineColumn(_input, _offset + i);
        // TODO: remove printf
        debug printf("%.*s(%u,%u): %s: %.*s at offset %llu being char `%c` ds:`%.*s`\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     tag.ptr,
                     cast(int)msg.length, msg.ptr,
                     _offset + i,
                     peekN(i),
                     cast(int)ds.length, ds.ptr);
    }

private:
    size_t _offset;             // current offset in `_input`
    const Input _input;         ///< Input data.
    const string _path;         ///< Input file (or null if in-memory).

    Token _token;
    bool _endOfFile;            // signals null terminator found
    bool _includeComments;
    bool _includeWhitespace;
    bool _includeLabelAssignment;
    bool _includeListLabelAssignment;
    bool _diagnoseLeftRecursion; ///< Diagnose left-recursion.
}

/// Node.
enum NODE
{
    grammar,                    ///< Grammar defintion (name).
    rule                        ///< Grammar rule.
}

/// Format when printing AST (nodes).
enum Layout : ubyte
{
    source,                     ///< Try to mimic original source.
    tree                        ///< Makes AST-structure clear.
}

enum indentStep = 4;        ///< Indentation size in number of spaces.

struct Format
{
    uint indentDepth;           ///< Indentation depth.
    Layout layout;
    void showIndent() @safe const nothrow @nogc
    {
        showNSpaces(indentDepth);
    }
}

void showNSpaces(uint indentDepth) @safe nothrow @nogc
{
    foreach (_; 0 .. indentDepth*indentStep)
        putchar(' ');
}

void showNSpaces(scope ref Output sink, uint indentDepth) @safe pure nothrow @nogc
{
    foreach (_; 0 .. indentDepth*indentStep)
        sink.put(" ");
}

private void showChars(in const(char)[] chars) @trusted
{
    printf("%.*s",
           cast(uint)chars.length,
           chars.ptr);
}

private void showToken(in Token token,
                       in Format fmt)
{
    fmt.showIndent();
    showChars(token.input);
}

/// AST node.
private abstract class Node
{
@safe:
    abstract void show(in Format fmt = Format.init) const;
pure nothrow:
    abstract bool equals(const Node o) const @nogc;
    abstract void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const @nogc;
    this() @nogc {}
}

alias NodeArray = DynamicArray!(Node, null, uint); // `uint` capacity is enough

bool equalsAll(const scope Node[] a,
               const scope Node[] b) pure nothrow @nogc
{
    if (a.length != b.length)
        return false;
    foreach (const i; 0 .. a.length)
        if (!a[i].equals(b[i])) // TODO: use `.ptr` if needed
            return false;
    return true;
}

/// N-ary expression.
abstract class NaryExpr : Node
{
@safe pure nothrow @nogc:
    this(NodeArray subs)
    {
        move(subs, this.subs);
    }
    override bool equals(const Node o) const
    {
        if (this is o)
            return true;
        if (const o_ = cast(const typeof(this))o)
            return equalsAll(this.subs[], o_.subs[]);
        return false;
    }
    this(uint n)(Node[n] subs)
    if (n >= 2)
    {
        foreach (sub; subs)
            this.subs.put(sub);
    }
    NodeArray subs;
}

/// Sequence.
final class SeqM : NaryExpr
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        fmt.showIndent();
        foreach (const i, const sub; subs)
        {
            if (i)
                putchar(' ');
            sub.show();
        }
    }
pure nothrow @nogc:
    this(NodeArray subs)
    {
        super(subs.move());
    }
    this(uint n)(Node[n] subs) if (n >= 2)
    {
        super(subs);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("seq(");
        foreach (const i, const sub; subs)
        {
            if (i)
                sink.put(", ");
            sub.toMatchCallSource(sink, rulesByName);
        }
        sink.put(")");
    }
}

Node makeSeq(NodeArray subs,
             const scope ref GxLexer lexer,
             in bool rewriteFlag = true) pure nothrow
{
    subs = flattenSubs!SeqM(subs.move());
    if (subs.empty)
        return null;            // TODO: use new Nothing => EmptySeq instead
    if (subs.length == 1)
        return subs[0];
    if (rewriteFlag)
    {
        foreach (const i, const sub; subs)
        {
            if (i + 1 == subs.length) // skip last
                break;
            if (const zom = cast(const GreedyZeroOrMore)subs[i + 1])
                if (zom.sub.equals(sub))
                    lexer.warningAtToken(zom.head, "replace with `X+`");
        }
    }
    return new SeqM(subs.move());
}

Node makeSeq(Node[] subs,
             const scope ref GxLexer lexer,
             in bool rewriteFlag = true) pure nothrow
{
    return makeSeq(NodeArray(subs), lexer, rewriteFlag);
}

NodeArray flattenSubs(BranchNode)(NodeArray subs) pure nothrow @nogc
if (is(BranchNode : SeqM) ||
    is(BranchNode : AltM))
{
    typeof(subs) subs_;
    foreach (sub; subs)
    {
        if (auto sub_ = cast(BranchNode)sub)
            subs_.insertBack(flattenSubs!(BranchNode)(sub_.subs.move()));
        else
            subs_.insertBack(sub);
    }
    return subs_.move();
}

/// Nothing.
final class Nothing : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
    }
@safe pure nothrow @nogc:
    this(Token head)
    {
        super(head);
    }
}

/// Rule.
class Rule : Node
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
        showChars(":\n");
        if (top)
            top.show(Format(fmt.indentDepth + 1));
        showChars(" ;\n");
    }
@safe pure nothrow @nogc:
    void diagnoseDirectLeftRecursion(const scope ref GxLexer lexer)
    {
        void checkLeft(const scope Node top) @safe pure nothrow @nogc
        {
            if (const alt = cast(const AltM)top) // common case
                foreach (const sub; alt.subs[]) // all alternatives
                    checkLeft(sub);
            else if (const seq = cast(const SeqM)top)
                return checkLeft(seq.subs[0]); // only first in sequence
            else if (const s = cast(const Symbol)top)
                if (head.input == s.head.input)
                    lexer.warningAtToken(s.head, "left-recursion");
        }
        checkLeft(top);
    }
    this(in Token head, Node top)
    {
        this.head = head;
        this.top = top;
    }
    override bool equals(const Node o) const
    {
        if (this is o)
            return true;
        if (const o_ = cast(const typeof(this))o)
            return head == o_.head && top.equals(o_.top);
        return false;
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const @nogc {} // dummy
    void toMatcherSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.showNSpaces(1); sink.put(`Match `);
        sink.put(matcherFunctionNamePrefix);
        sink.put(head.input); sink.put("()\n");
        sink.showNSpaces(1); sink.put("{\n");
        sink.showNSpaces(2); sink.put(`return`);
        if (top)
        {
            sink.put(` `);
            top.toMatchCallSource(sink, rulesByName);
        }
        sink.put(";\n");
        sink.showNSpaces(1); sink.put("}\n");
    }
    Token head;                 ///< Name.
    Node top;
}

final class FragmentRule : Rule
{
@safe pure nothrow @nogc:
    this(in Token head, Node top)
    {
        super(head, top);
    }
}

final class AltM : NaryExpr
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        const wrapFlag = needsWrapping(subs[]);
        if (wrapFlag)
            putchar('(');
        showSubs(fmt);
        if (wrapFlag)
            putchar(')');
    }
    final void showSubs(in Format fmt) const
    {
        foreach (const i, const sub; subs)
        {
            sub.show(fmt);
            if (i + 1 != subs.length)
            {
                if (fmt.indentDepth)
                    showChars(" |\n");
                else
                    showChars(" | ");
            }
        }
    }
pure nothrow @nogc:
    this(NodeArray subs)
    {
        super(subs.move());
    }
    this(uint n)(Node[n] subs) if (n >= 2)
    {
        super(subs);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("alt(");
        foreach (const i, const sub; subs)
        {
            if (i)
                sink.put(", ");
            sub.toMatchCallSource(sink, rulesByName);
        }
        sink.put(")");
    }
}

Node makeAlt(NodeArray subs,
             in bool rewriteFlag = true) pure nothrow
{
    subs = flattenSubs!AltM(subs.move());
    switch (subs.length)
    {
    case 0:
        return null;
    case 1:
        return subs[0];
    default:
        return new AltM(subs.move());
    }
}

Node makeAlt(Node[] subs,
             in bool rewriteFlag = true) pure nothrow
{
    return makeAlt(NodeArray(subs), rewriteFlag);
}

class TokenNode : Node
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
    }
pure nothrow @nogc:
    this(in Token head)
    {
        this.head = head;
    }
    override bool equals(const Node o) const @nogc
    {
        if (this is o)
            return true;
        if (const o_ = cast(const typeof(this))o)
            return head == o_.head;
        return false;
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const @trusted
    {
        sink.put(`tok(`);
        sink.put(head.input[]);
        sink.put(`)`);
    }
    Token head;
}

/// Unary match combinator.
abstract class UnExpr : Node
{
@safe:
    final override void show(in Format fmt = Format.init) const
    {
        putchar('(');
        sub.show(fmt);
        putchar(')');
        showToken(head, fmt);
    }
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super();
        this.head = head;
        this.sub = sub;
    }
    final override bool equals(const Node o) const
    {
        if (this is o)
            return true;
        if (const o_ = cast(const typeof(this))o)
            return (head == o_.head &&
                    this.sub.equals(o_.sub));
        return false;
    }
    Token head;
    Node sub;
}

/// Don't match an instance of type `sub`.
final class Not : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("not(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match (greedily) zero or one instances of type `sub`.
final class GreedyZeroOrOne : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("gzo(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match (greedily) zero or more instances of type `sub`.
final class GreedyZeroOrMore : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("gzm(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match (greedily) one or more instances of type `sub`.
final class GreedyOneOrMore : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("gom(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match (non-greedily) zero or one instances of type `sub`.
final class NonGreedyZeroOrOne : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("nzo(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match (non-greedily) zero or more instances of type `sub`.
final class NonGreedyZeroOrMore : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("nzm(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match (non-greedily) one or more instances of type `sub`.
final class NonGreedyOneOrMore : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("nom(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
}

/// Match `count` number of instances of type `sub`.
final class Several : UnExpr
{
@safe pure nothrow @nogc:
    this(in Token head, Node sub)
    {
        super(head, sub);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("cnt(");
        sub.toMatchCallSource(sink, rulesByName);
        sink.put(")");
    }
    ulong count;
}

final class Symbol : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
    }
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put(matcherFunctionNamePrefix);
        sink.put(head.input);
        sink.put(`()`);
        // if (const Rule* rulePtr = head.input in rulesByName)
        //     (*rulePtr).toMatchCallSource(sink, rulesByName);
    }
}

final class PipeSentinel : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class DotDotSentinel : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class Tilde : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class Wildcard : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const @trusted
    {
        sink.put(`any()`);
    }
}

/// Match literal `head.input`.
final class Literal : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const @trusted
    {
        if (head.input.length == 3)
        {
            sink.put(`ch('`);
            sink.put(head.input[1 .. $-1]);
            sink.put(`')`);
        }
        else
        {
            sink.put(`str("`);
            if (head.input.length >= 3)
                sink.put(head.input[1 .. $-1]);
            else
                sink.put(head.input[]);
            sink.put(`")`);
        }
    }
}

bool needsWrapping(const scope Node[] subs) @safe pure nothrow @nogc
{
    bool wrapFlag;
    foreach (const sub; subs)
        if (!cast(const TokenNode)sub)
            wrapFlag = true;
    return wrapFlag;
}

/// Binary match combinator.
abstract class BinExpr : Node
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        fmt.showIndent();
        subs[0].show(fmt);
        putchar(' ');
        showChars(head.input);
        putchar(' ');
        subs[1].show(fmt);
    }
@safe pure nothrow @nogc:
    this(in Token head, Node[2] subs)
    {
        assert(subs[0]);
        assert(subs[1]);
        super();
        this.subs = subs;
    }
    final override bool equals(const Node o) const
    {
        if (this is o)
            return true;
        if (const o_ = cast(const typeof(this))o)
            return (head == o_.head &&
                    equalsAll(this.subs[], o_.subs[]));
        return false;
    }
    Token head;
    Node[2] subs;
}

/// Match value range between `limits[0]` and `limits[1]`.
final class Range : BinExpr
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        const wrapFlag = needsWrapping(subs[]);
        fmt.showIndent();
        if (wrapFlag)
            putchar('(');
        subs[0].show(fmt);
        showChars(" .. ");
        subs[1].show(fmt);
        if (wrapFlag)
            putchar(')');
    }
pure nothrow @nogc:
    this(in Token head, Node[2] limits)
    {
        super(head, limits);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        sink.put("rng(");
        if (const lower = cast(const Literal)subs[0])
            sink.put(lower.head.input);
        else
        {
            debug writeln("handle sub[0] of type ", typeid(subs[0]).name);
            debug subs[0].show();
            assert(false);
        }
        sink.put(",");
        // sink.put(" <= s[offset] && s[offset] <= ");
        if (const upper = cast(const Literal)subs[1])
            sink.put(upper.head.input);
        else
            assert(false);
        sink.put(")");
    }
}

final class Hooks : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
    override void toMatchCallSource(scope ref Output sink, const scope ref RulesByName rulesByName) const
    {
        Input input = head.input;

        assert(input.skipOver('['));
        assert(input.skipOverBack(']'));

        sink.put("alts(");

        for (size_t i; i < input.length;)
        {
            if (i)
                sink.put(", ");
            sink.put('\'');
            if (input[i] == '\\')
            {
                sink.put('\\');
                i += 1;
                sink.put(input[i]);
                i += 1;
            }
            else
            {
                sink.put(input[i]);
                i += 1;
            }
            sink.put('\'');
        }
        sink.put(")");
    }
}

final class LineComment : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class BlockComment : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

/// Grammar named `name`.
final class Grammar : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
        putchar(' ');
        showChars(name);
        showChars(";\n");
    }
pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    final override bool equals(const Node o) const
    {
        if (this is o)
            return true;
        if (const o_ = cast(const typeof(this))o)
            return (head == o_.head &&
                    name == o_.name);
        return false;
    }
    Input name;
}

/// Lexer grammar named `name`.
final class LexerGrammar : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

/** Parser grammar named `name`.
 *
 * See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687210/Quick+Starter+on+Parser+Grammars+-+No+Past+Experience+Required
 */
final class ParserGrammar : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

/// Import of `modules`.
final class Import : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
        putchar(' ');
        foreach (const i, const m ; modules)
        {
            if (i)
                putchar(',');
            showChars(m);
        }
        putchar(';');
        putchar('\n');
    }
pure nothrow @nogc:
    this(in Token head, const Input[] modules)
    {
        super(head);
        this.modules = modules;
    }
    const Input[] modules;
}

final class Mode : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

final class Options : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, in Token code)
    {
        super(head);
        this.code = code;
    }
    Input name;
    Token code;
}

final class Header : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, in Token name, in Token code)
    {
        super(head);
        this.name = name;
        this.code = code;
    }
    Token name;
    Token code;
}

final class ScopeSymbolAction : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head,
         in Input name,
         in Token code)
    {
        super(head);
        this.name = name;
        this.code = code;
    }
    Input name;
    Token code;
}

final class ScopeSymbol : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head,
         in Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

final class ScopeAction : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
    }
pure nothrow @nogc:
    this(in Token head,
         in Token code)
    {
        super(head);
        this.code = code;
    }
    Token code;
}

final class AttributeSymbol : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
    }
pure nothrow @nogc:
    this(in Token head, in Token code)
    {
        super(head);
        this.code = code;
    }
    Token code;
}

final class Action : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class ActionSymbol : TokenNode
{
@safe:
    override void show(in Format fmt = Format.init) const
    {
        showToken(head, fmt);
    }
pure nothrow @nogc:
    this(in Token head, in Token code)
    {
        super(head);
        this.code = code;
    }
    Token code;
}

final class Channels : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, in Token code)
    {
        super(head);
        this.code = code;
    }
    Token code;
}

final class Tokens : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, in Token code)
    {
        super(head);
        this.code = code;
    }
    Token code;
}

final class Class : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head, Input name, Input baseName)
    {
        super(head);
        this.name = name;
        this.baseName = baseName;
    }
    Input name;
    Input baseName;             ///< Base class name.
}

final class AlwaysIncludePredicate : TokenNode
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

/** Gx parser.
 *
 * See: `ANTLRv4Parser.g4`
 */
struct GxParser
{
@safe pure:
    this(in Input input,
         const string path = null,
         in bool includeComments = false)
    {
        _lexer = GxLexer(input, path, includeComments);
        if (!_lexer.empty)
            _front = nextFront();
    }

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        return _front is null;
    }

    inout(Node) front() inout scope return
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        assert(!empty);
        return _front;
    }

    void popFront()
    {
        version(D_Coverage) {} else version(Do_Inline) pragma(inline, true);
        assert(!empty);
        if (_lexer.empty)
            _front = null;      // make `this` empty
        else
            _front = nextFront();
    }

    private Rule makeRule(in Token name,
                          in bool isFragment,
                          ActionSymbol actionSymbol = null,
                          Action action = null) @trusted
    {
        _lexer.popFrontEnforce(TOK.colon, "no colon");

        static if (false/*useStaticTempArrays*/)
            FixedArray!(Node, 100) alts;
        else
            NodeArray alts;

        while (_lexer.front.tok != TOK.semicolon)
        {
            size_t parentDepth = 0;

            static if (useStaticTempArrays)
                FixedArray!(Node, 70) seq; // doesn't speed up that much
            else
                NodeArray seq;

            void seqPutCheck(Node last)
            {
                if (last is null)
                    return _lexer.warningAtToken(name, "empty sequence");
                if (!_lexer.empty && _lexer.front.tok == TOK.dotdot)
                    return seq.put(last); // ... has higher prescedence
                if (!seq.empty)
                {
                    if (cast(PipeSentinel)seq.back) // binary operator. TODO: if skipOver!PipeSentinel
                    {
                        seq.popBack(); // pop `PipeSentinel`
                        return seqPutCheck(new AltM([seq.backPop(), last]));
                    }
                    if (auto dotdot = cast(DotDotSentinel)seq.back) // binary operator
                    {
                        seq.popBack(); // pop `DotDotSentinel`
                        return seqPutCheck(new Range(dotdot.head, [seq.backPop(), last]));
                    }
                    if (auto tilde = cast(Tilde)seq.back) // prefix unary operator
                    {
                        seq.popBack(); // pop `Tilde`
                        return seqPutCheck(new Not(tilde.head, last));
                    }
                }
                return seq.put(last);
            }

            while ((parentDepth != 0 ||
                    _lexer.front.tok != TOK.pipe) &&
                   _lexer.front.tok != TOK.semicolon)
            {
                // TODO: use static array with length being number of tokens till `TOK.pipe`
                switch (_lexer.front.tok)
                {
                case TOK.symbol:
                    if (_lexer.front.input == "options")
                        auto _ = makeRuleOptions(_lexer.frontPop(), true);
                    else
                    {
                        auto symbol = _lexer.frontPop();
                        if (_lexer.front.tok == TOK.colon)
                        {
                            _lexer.popFront();
                            continue; // skip element label: SYMBOL '.'. See_Also: https://www.antlr2.org/doc/metalang.html section "Element Labels"
                        }
                        seqPutCheck(new Symbol(symbol));
                    }
                    break;
                case TOK.literal:
                    seqPutCheck(new Literal(_lexer.frontPop()));
                    break;
                case TOK.qmark:
                    const head = _lexer.frontPop();
                    seq.put(new GreedyZeroOrOne(head, seq.backPop()));
                    break;
                case TOK.qmarkQmark:
                    const head = _lexer.frontPop();
                    seq.put(new NonGreedyZeroOrOne(head, seq.backPop()));
                    break;
                case TOK.star:
                    const head = _lexer.frontPop();
                    seq.put(new GreedyZeroOrMore(head, seq.backPop()));
                    break;
                case TOK.starQmark:
                    const head = _lexer.frontPop();
                    seq.put(new NonGreedyZeroOrMore(head, seq.backPop()));
                    break;
                case TOK.plus:
                    const head = _lexer.frontPop();
                    seq.put(new GreedyOneOrMore(head, seq.backPop()));
                    break;
                case TOK.plusQmark:
                    const head = _lexer.frontPop();
                    seq.put(new NonGreedyOneOrMore(head, seq.backPop()));
                    break;
                case TOK.tilde:
                    seq.put(new Tilde(_lexer.frontPop()));
                    break;
                case TOK.pipe:
                    if (const symbol = cast(Symbol)seq.back)
                    {
                        if (symbol.head.tok == TOK.leftParen)
                        {
                            // _lexer.warningAtFront("operator '|' without left-hand side argument has no effect");
                            _lexer.frontPop();
                            continue;
                        }
                    }
                    seq.put(new PipeSentinel(_lexer.frontPop()));
                    break;
                case TOK.dotdot:
                    seq.put(new DotDotSentinel(_lexer.frontPop()));
                    break;
                case TOK.wildcard:
                    seq.put(new Wildcard(_lexer.frontPop()));
                    break;
                case TOK.brackets:
                    seq.put(new Hooks(_lexer.frontPop()));
                    break;
                case TOK.hash:
                case TOK.rewrite:
                    while (_lexer.front.tok != TOK.pipe &&
                           _lexer.front.tok != TOK.semicolon)
                        _lexer.popFront(); // ignore for now
                    break;
                case TOK.leftParen:
                    parentDepth += 1;
                    seq.put(new Symbol(_lexer.frontPop()));
                    break;
                case TOK.rightParen:
                    parentDepth -= 1;
                    // find matching '(' if any
                    size_t li = seq.length; // left paren index
                    Symbol hs;              // left parent index Symbol
                    foreach_reverse (const i, const Node node; seq[])
                    {
                        auto symbol = cast(Symbol)node;        // TODO: avoid cast
                        if (symbol &&
                            symbol.head.tok == TOK.leftParen)
                        {
                            li = i;
                            hs = symbol;
                            break;
                        }
                    }

                    if (li + 3 <= seq.length) // normal case: ... ( X Y ... )
                    {
                        NodeArray subseq; // TODO: use stack for small arrays. TODO: use `Rule` as ElementType
                        foreach (node; seq[li + 1 .. $])
                            subseq.put(node);
                        seq.popBackN(seq.length - li);
                        seqPutCheck(makeSeq(subseq.move(), _lexer));
                    }
                    else if (li + 2 == seq.length) // single case: ... ( X )
                    {
                        // _lexer.warningAtFront("single element group has no use");
                        Node single = seq.backPop(); // pop X
                        seq.popBack(); // pop '('
                        seqPutCheck(single); // insert X
                    }
                    else if (li + 1 == seq.length) // empty case: ... ( )
                    {
                        auto nothing = new Nothing(hs.head);
                        seq.popBack(); // pop '('
                        seqPutCheck(nothing); // TODO: use GreedyZeroOrOne() instead
                    }
                    else if (li == seq.length) // unmatched case: ... )
                    {
                        _lexer.errorAtFront("no matching opening parenthesis found before this closing parenthesis");
                    }
                    _lexer.frontPop();
                    break;
                case TOK.action:
                    _lexer.frontPop(); // ignore action
                    _lexer.skipOverTOK(TOK.qmark); // TODO: handle in a more generic way
                    break;
                case TOK.labelAssignment:
                    // ignore for now: SYMBOL '='
                    if (!cast(Symbol)seq.back)
                        _lexer.errorAtFront("non-symbol before label assignment");
                    _lexer.frontPop();
                    seq.popBack(); // ignore
                    break;
                case TOK.tokenSpecOptions:
                    _lexer.frontPop(); // ignore
                    break;
                case TOK.colon:
                    _lexer.warningAtFront("ignoring colon with no effect");
                    _lexer.frontPop(); // ignore
                    continue;
                case TOK.rootNode:
                    /* AST root operator. When generating abstract syntax trees
                     * (ASTs), token references suffixed with the "^" root
                     * operator force AST nodes to be created and added as the
                     * root of the current tree. This symbol is only effective
                     * when the buildAST option is set. More information about
                     * ASTs is also available. */
                    _lexer.frontPop(); // ignore
                    break;
                case TOK.exclamation:
                    /* AST exclude operator. When generating abstract syntax
                     * trees, token references suffixed with the "!" exclude
                     * operator are not included in the AST constructed for that
                     * rule. Rule references can also be suffixed with the
                     * exclude operator, which implies that, while the tree for
                     * the referenced rule is constructed, it is not linked into
                     * the tree for the referencing rule. This symbol is only
                     * effective when the buildAST option is set. More
                     * information about ASTs is also available. */
                    _lexer.frontPop(); // ignore
                    break;
                case TOK.rewriteSyntacticPredicate:
                    if (seq.empty)
                        _lexer.errorAtFront("missing left-hand side of semantic predicate");
                    // TODO: pop seq.back `seq` and pair with symbol after or parens after
                    seqPutCheck(new AlwaysIncludePredicate(_lexer.frontPop()));
                    break;
                default:
                    _lexer.infoAtFront("TODO: unhandled token type" ~ _lexer.front.to!string);
                    seqPutCheck(new Symbol(_lexer.frontPop()));
                    break;
                }
            }
            if (!seq.length)
            {
                // `seq` may be empty
                // _lexer.infoAtFront("empty sequence");
            }
            static if (useStaticTempArrays)
            {
                alts.put(makeSeq(seq[], _lexer));
                seq.clear();
            }
            else
                alts.put(makeSeq(seq.move(), _lexer));
            if (_lexer.front.tok == TOK.pipe)
                _lexer.popFront(); // skip terminator
        }

        _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");

        // needed for ANTLRv2.g2:
        if (!_lexer.empty)
        {
            // if (_lexer.front == Token(TOK.symbol, "exception"))
            //     _lexer.popFront();
            // if (_lexer.front == Token(TOK.symbol, "catch"))
            //     _lexer.popFront();
            if (_lexer.front.tok == TOK.brackets)
                _lexer.popFront();
            if (_lexer.front.tok == TOK.action)
                _lexer.popFront();
        }

        static if (useStaticTempArrays)
        {
            Node top = alts.length == 1 ? alts.backPop() : makeAlt(alts[]);
            alts.clear();
        }
        else
            Node top = alts.length == 1 ? alts.backPop() : makeAlt(alts.move());

        Rule rule = (isFragment
                     ? new FragmentRule(name, top)
                     : new Rule(name, top));

        if (_lexer._diagnoseLeftRecursion)
            rule.diagnoseDirectLeftRecursion(_lexer);

        if (rulesByName.length == 0)
            firstRule = rule;
        rulesByName[rule.head.input] = rule;
        return rule;
    }

    Input[] makeArgs(in TOK separator,
                     in TOK terminator)
    {
        DynamicArray!(Input) result;
        while (true)
        {
            result.put(_lexer.frontPopEnforce(TOK.symbol).input);
            if (_lexer.front.tok != separator)
                break;
            _lexer.popFront();
        }
        _lexer.popFrontEnforce(terminator, "no terminating semicolon");
        return result[].dup;
    }

    AttributeSymbol makeAttributeSymbol(in Token head) nothrow
    {
        return new AttributeSymbol(head, _lexer.frontPopEnforce(TOK.action, "missing action"));
    }

    ActionSymbol makeActionSymbol(in Token head) nothrow
    {
        return new ActionSymbol(head, _lexer.frontPopEnforce(TOK.action, "missing action"));
    }

    TokenNode makeScope(in Token head)
    {
        if (_lexer.front.tok == TOK.symbol)
        {
            const symbol = _lexer.frontPop().input;
            if (_lexer.front.tok == TOK.action)
                return new ScopeSymbolAction(head, symbol,
                                             _lexer.frontPopEnforce(TOK.action, "missing action"));
            else
            {
                auto result = new ScopeSymbol(head, symbol);
                _lexer.frontPopEnforce(TOK.semicolon,
                                          "missing terminating semicolon");
                return result;
            }
        }
        else
        {
            return new ScopeAction(head,
                                   _lexer.frontPopEnforce(TOK.action, "missing action"));
        }
    }

    Import makeImport(in Token head)
    {
        auto import_ = new Import(head, makeArgs(TOK.comma, TOK.semicolon));
        this.imports.put(import_);
        return import_;
    }

    TokenNode makeClass(in Token head)
    {
        auto result = new Class(head,
                               _lexer.frontPopEnforce(TOK.symbol, "missing symbol").input,
                               _lexer.skipOverToken(Token(TOK.symbol, "extends")).input ?
                               _lexer.frontPop().input :
                               null);
        _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
        return result;
    }

    Symbol skipOverSymbol(in string symbolIdentifier) return
    {
        if (_lexer.front == Token(TOK.symbol, symbolIdentifier))
        {
            return new Symbol(_lexer.frontPop());
        }
        return null;
    }

    /// Skip over scope if any.
    TokenNode skipOverScope()
    {
        if (_lexer.front == Token(TOK.symbol, "scope"))
            return makeScope(_lexer.frontPop());
        return null;
    }

    Options makeRuleOptions(in Token head,
                            in bool skipOverColon = false) nothrow
    {
        version(Do_Inline) pragma(inline, true);
        const action = _lexer.frontPopEnforce(TOK.action, "missing action");
        if (skipOverColon)
            _lexer.skipOverTOK(TOK.colon);
        return new Options(head, action);
    }

    Options makeTopOptions(in Token head) nothrow
    {
        version(Do_Inline) pragma(inline, true);
        const action = _lexer.frontPopEnforce(TOK.action, "missing action");
        _lexer.skipOverTOK(TOK.colon); // optionally scoped. See_Also: https://stackoverflow.com/questions/64477446/meaning-of-colon-inside-parenthesises/64477817#64477817
        return new Options(head, action);
    }

    Channels makeChannels(in Token head) nothrow
    {
        version(Do_Inline) pragma(inline, true);
        return new Channels(head, _lexer.frontPopEnforce(TOK.action, "missing action"));
    }

    Tokens makeTokens(in Token head) nothrow
    {
        version(Do_Inline) pragma(inline, true);
        return new Tokens(head, _lexer.frontPopEnforce(TOK.action, "missing action"));
    }

    Header makeHeader(in Token head)
    {
        const name = (_lexer.front.tok == TOK.literal ?
                      _lexer.frontPop() :
                      Token.init);
        const action = _lexer.frontPopEnforce(TOK.action, "missing action");
        return new Header(head, name, action);
    }

    Mode makeMode(in Token head)
    {
        auto result = new Mode(head, _lexer.frontPop().input);
        _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
        return result;
    }

    Action makeAction(in Token head)
    {
        version(Do_Inline) pragma(inline, true);
        return new Action(head);
    }

    /// Skip over options if any.
    Options skipOverPreRuleOptions()
    {
        if (_lexer.front == Token(TOK.symbol, "options"))
            return makeRuleOptions(_lexer.frontPop());
        return null;
    }

    bool skipOverExclusion()
    {
        if (_lexer.front.tok == TOK.exclamation)
        {
            _lexer.frontPop();
            return true;
        }
        return false;
    }

    bool skipOverReturns()
    {
        if (_lexer.front == Token(TOK.symbol, "returns"))
        {
            _lexer.frontPop();
            return true;
        }
        return false;
    }

    bool skipOverHooks()
    {
        if (_lexer.front.tok == TOK.brackets)
        {
            // _lexer.infoAtFront("TODO: use TOK.brackets");
            _lexer.frontPop();
            return true;
        }
        return false;
    }

    Action skipOverAction()
    {
        if (_lexer.front.tok == TOK.action)
            return makeAction(_lexer.frontPop());
        return null;
    }

    ActionSymbol skipOverActionSymbol()
    {
        if (_lexer.front.tok == TOK.actionSymbol)
            return makeActionSymbol(_lexer.frontPop);
        return null;
    }

    Node makeRuleOrOther(in Token head)
    {
        if (_lexer.front.tok == TOK.colon) // normal case
            return makeRule(head, false);  // fast path

        if (head.input == "lexer" ||
            head.input == "parser" ||
            head.input == "grammar")
        {
            bool lexerFlag;
            bool parserFlag;
            if (head.input == "lexer")
            {
                lexerFlag = true;
                _lexer.popFrontEnforce(TOK.symbol, "expected `grammar` after `lexer`"); // TODO: enforce input grammar
            }
            else if (head.input == "parser")
            {
                parserFlag = true;
                _lexer.popFrontEnforce(TOK.symbol, "expected `grammar` after `parser`"); // TODO: enforce input grammar
            }

            if (lexerFlag)
            {
                auto lexerGrammar = new LexerGrammar(head, _lexer.frontPop().input);
                _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
                return this.grammar = lexerGrammar;
            }
            else if (parserFlag)
            {
                auto parserGrammar = new ParserGrammar(head, _lexer.frontPop().input);
                _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
                return this.grammar = parserGrammar;
            }
            else
            {
                if (_lexer.front.tok == TOK.colon)
                    return makeRule(head, false);
                else
                {
                    auto grammar = new Grammar(head, _lexer.frontPop().input);
                    _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
                    this.grammar = grammar;
                    return grammar;
                }
            }
        }

        switch (head.input)
        {
        case `private`:
            _lexer.frontEnforce(TOK.symbol, "expected symbol after `private`");
            return makeRuleOrOther(_lexer.frontPop); // TODO: set private qualifier
        case `protected`:
            _lexer.frontEnforce(TOK.symbol, "expected symbol after `protected`");
            return makeRuleOrOther(_lexer.frontPop); // TODO: set protected qualifier
        case `channels`:
            return makeChannels(head);
        case `tokens`:
            return makeTokens(head);
        case `options`:
            return makeTopOptions(head);
        case `header`:
            return makeHeader(head);
        case `mode`:
            return makeMode(head);
        case `class`:
            return makeClass(head);
        case `scope`:
            return makeScope(head);
        case `import`:
            return makeImport(head);
        case `fragment`: // lexer helper rule, not real token for parser.
            return makeRule(_lexer.frontPop(), true);
        default:
            while (_lexer.front.tok != TOK.colon)
            {
                // TODO: use switch
                if (skipOverExclusion()) // TODO: use
                    continue;
                if (skipOverReturns())  // TODO: use
                    continue;
                if (skipOverHooks())    // TODO: use
                    continue;
                if (const _ = skipOverSymbol("locals")) // TODO: use
                    continue;
                if (const _ = skipOverPreRuleOptions()) // TODO: use
                    continue;
                if (const _ = skipOverScope())     // TODO: use
                    continue;
                if (const _ = skipOverAction()) // TODO: use
                    continue;
                if (const _ = skipOverActionSymbol()) // TODO: use
                    continue;
                break;          // no progression so done
            }
            return makeRule(head, false);
        }
    }

    Node nextFront()
    {
        const head = _lexer.frontPop();
        switch (head.tok)
        {
        case TOK.attributeSymbol:
            return makeAttributeSymbol(head);
        case TOK.actionSymbol:
            return makeActionSymbol(head);
        case TOK.blockComment:
            return new BlockComment(head);
        case TOK.lineComment:
            return new LineComment(head);
        case TOK.action:
            return new Action(head);
        case TOK.symbol:
            return makeRuleOrOther(head);
        default:
            _lexer.errorAtFront("TODO: handle");
            assert(false);
        }
    }

    Node grammar;
    Node options;
    DynamicArray!(Import, null, uint) imports;
    // DynamicArray!(Rule, null, uint) rules;
    Rule firstRule;
    RulesByName rulesByName;
private:
    GxLexer _lexer;
    Node _front;
}

/// Gx filer parser.
struct GxFileParser           // TODO: convert to `class`
{
@safe:
    this(in string filePath)
    {
        import std.path : expandTilde;
        const path = filePath.expandTilde;
        const data = cast(Input)rawReadPath(path); // cast to Input because we don't want to keep all file around:
        parser = GxParser(data, filePath, false);
    }
    ~this() @nogc {}
    GxParser parser;
    alias parser this;
}

struct GxFileReader
{
@safe:
    this(in string filePath)
    {
        import std.path : baseName, stripExtension;
        const showFlag = filePath.endsWith("oncrpcv2.g4");
        Format fmt;
        auto gxp = GxFileParser(filePath);

        Output parserSource;

        while (!gxp.empty)
        {
            // gxp.front.show(fmt);
            gxp.popFront();
        }

        const moduleName = filePath.baseName.stripExtension ~ "_parser";

        parserSource.put(q{module } ~ moduleName ~ q{;

});

        parserSource.put(`alias Input = const(char)[];

struct Match
{
@safe pure nothrow @nogc:
    @property static Match none()
    {
        return typeof(return)(_length.max);
    }
    @property uint length()
    {
        return _length;
    }
    const uint _length;                // length == uint.max is no match
}

struct Parser
{
@safe nothrow @nogc:
    Input inp;
    size_t off;

    Match ch(dchar x)
    {
        pragma(inline, true);
        if (inp[off] == x)
        {
            off += 1;
            return Match(1);
        }
        return Match.none();
    }

    Match str(const scope string x)
    {
        pragma(inline, true);
        if (off + x.length <= inp.length &&
            inp[off .. off + x.length] == x) // inp[off .. $].startsWith(x)
        {
            off += x.length;
            return Match(x.length);
        }
        return Match.none();
    }

`);

        foreach (kv; gxp.rulesByName.byKeyValue)
        {
            Input name = kv.key;
            Rule rule = kv.value;
            rule.show(fmt);
            rule.toMatcherSource(parserSource, gxp.rulesByName);
        }

        parserSource.put(`} // struct Parser
`);

        const parserPath = filePath.stripExtension ~ "_parser.d";

        import std.file : write;
        write(parserPath, parserSource[]);
        debug writeln("Wrote parser ", parserPath);

        import std.process : execute;
        auto dmd = execute(["dmd", "-c", parserPath]);
        if (dmd.status == 0)
            writeln("Compilation of ", parserPath, " successful");
        else
            writeln("Compilation of ", parserPath, " failed:\n",
                    dmd.output);
    }

    ~this() @nogc {}
}

bool isGxFilename(const scope char[] name) @safe pure nothrow @nogc
{
    return name.endsWithEither([`.g`, `.g2`, `.g4`]);
}

///
version(show)
@trusted unittest
{
    import std.datetime.stopwatch : StopWatch;
    import std.file : dirEntries, SpanMode, getcwd;
    import std.path : expandTilde, relativePath;

    const root = "~/Work/grammars-v4/".expandTilde;
    const lexerFlag = false;
    const parserFlag = true;
    const showProgressFlag = true;

    auto of = stdout;           // output file

    string adjustPath(const return scope string path)
    {
        const cwd = getcwd();
        if (root.startsWith(cwd))
            return path.relativePath(cwd);
        return path;
    }

    if (lexerFlag)
    {
        scope StopWatch swAll;
        swAll.start();
        foreach (const e; dirEntries(root, SpanMode.breadth))
        {
            const fn = e.name;
            if (fn.isGxFilename)
            {
                if (showProgressFlag)
                    of.writeln("Lexing ", adjustPath(fn), " ...");  // TODO: read use curren directory
                const data = cast(Input)rawReadPath(fn); // exclude from benchmark
                scope StopWatch swOne;
                swOne.start();
                auto lexer = GxLexer(data, fn, false);
                while (!lexer.empty)
                    lexer.popFront();
                if (showProgressFlag)
                    of.writeln("Lexing ", adjustPath(fn), " took ", swOne.peek());
            }
        }
        of.writeln("Lexing all took ", swAll.peek());
    }

    if (parserFlag)
    {
        scope StopWatch swAll;
        swAll.start();
        foreach (const e; dirEntries(root, SpanMode.breadth))
        {
            const fn = e.name;
            if (fn.isGxFilename)
            {
                if (fn.endsWith(`Antlr3.g`) ||
                    fn.endsWith(`ANTLRv2.g2`)) // skip this crap
                    continue;
                if (!fn.endsWith(`pascal.g4`))
                    continue;
                if (showProgressFlag)
                    of.writeln("Reading ", adjustPath(fn), " ...");
                scope StopWatch swOne;
                swOne.start();
                auto reader = GxFileReader(fn);
                if (showProgressFlag)
                    of.writeln("Reading ", adjustPath(fn), " took ", swOne.peek());
            }
        }
        of.writeln("Reading all took ", swAll.peek());
    }
}
