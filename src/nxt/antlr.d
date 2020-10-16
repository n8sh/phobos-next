/** Lexer and parser for ANTLR (G,G2,G4) grammars.
 *
 * See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687036/ANTLR+Cheat+Sheet
 * See_Also: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
 * See_Also: https://github.com/antlr/grammars-v4
 * See_Also: https://github.com/antlr/grammars-v4/blob/master/bnf/bnf.g4
 * See_Also: https://stackoverflow.com/questions/53245751/convert-a-form-of-bnf-grammar-to-g4-grammar
 * See_Also: https://bnfc.digitalgrammars.com/
 *
 * TODO:
 * - make diagnostics functions non-pure
 * - parse postfix operators *, +, ?
 * - create index of symbols and link them in second pass
 * - gorup things into a `struct G4`
 */
module nxt.antlr;

import std.array : Appender;
import std.stdio : writeln;

// `d-deps.el` requires these to be at the top:
import nxt.line_column : offsetLineColumn;
import nxt.file_ex : rawReadPath;

@safe:

enum useKeywords = true;

alias Input = const(char)[];

///< Token kind. TODO: make this a string type like with std.experimental.lexer
enum TOK
{
    none,

    unknown,                    ///< Unknown.

    whitespace,

    // Keywords:
    LEXER,          ///< Grammar type.
    PARSER,         ///< Grammar type
    GRAMMAR,        ///< Grammar header.
    OPTIONS,        ///< Grammar or rule options.
    HEADER,
    TOKENS,         ///< Can add tokens with this; usually imaginary tokens.
    CHANNELS,       ///< `channels`.
    MODE,           ///< `mode`.
    PRIVATE,        ///< `private`.
    PROTECTED,      ///< `protected`.

    symbol,                     ///< Symbol.
    lexerRuleName,              ///< TODO: use instead of `symbol`
    parserRuleName,             ///< TODO: use instead of `symbol`
    attributeSymbol,            ///< Attribute Symbol (starting with `$`).
    actionSymbol,               ///< Action Symbol (starting with `@`).

    number,                     ///< Number.

    lineComment,                ///< Single line comment.
    blockComment,               ///< Multi-line (block) comment.

    leftParen,                  ///< Left parenthesis.
    rightParen,                 ///< Right parenthesis.

    action,                  ///< Code block.

    hooks,                       ///< Alternatives within '[' ... ']'

    textLiteralSingleQuoted,    ///< Text (string) literal, surrounded by single quotes.
    textLiteralDoubleQuoted,    ///< Text (string) literal, surrounded by double quotes.

    colon,                      ///< Colon `:`.
    semicolon,                  ///< Semicolon `;`.
    hash,                       ///< Hash `#`
    labelAssignment,            ///< Label assignment `=`
    listLabelAssignment,        ///< List label assignment `+=`

    optOrSemPred,               ///< Optional or semantic predicate (`?`)
    star,                       ///< Zero or more (`*`)
    plus,                       ///< One or more (`+`)
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
    alwaysIncludePredicate,     ///< Rewrite rule (`=>`)

    _error,                     ///< Error token.
}

/// G4 rule.
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

/** G4 lexer.
 *
 * See_Also: `ANTLRv4Lexer.g4`
 */
struct G4Lexer
{
    import std.algorithm.comparison : among;

    import nxt.conv_ex : toDefaulted;

@safe pure:

    this(in Input input,
         const scope string path = null,
         in bool includeComments = false,
         in bool includeWhitespace = false) @trusted
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
        version(D_Coverage) {} else pragma(inline, true);
        return _endOfFile;
    }

    inout(Token) front() inout scope return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _token;
    }

    void popFront() scope nothrow @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        nextFront();
    }

    void frontEnforceTOK(in TOK tok, const scope string msg = "") nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        if (front.tok != tok)
            errorAtFront(msg ~ ", expected `TOK." ~ tok.toDefaulted!string(null) ~ "`");
    }

    void popFrontEnforceTOK(in TOK tok, const scope string msg) nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        if (frontPop().tok != tok)
            errorAtFront(msg ~ ", expected `TOK." ~ tok.toDefaulted!string(null) ~ "`");
    }

    Token frontPopEnforceTOK(in TOK tok, const scope string msg = "") nothrow
    {
        version(D_Coverage) {} else version(LDC) pragma(inline, true);
        const result = frontPop();
        if (result.tok != tok)
            errorAtFront(msg ~ ", expected `TOK." ~ tok.toDefaulted!string(null) ~ "`");
        return result;
    }

    Token frontPop() scope return nothrow
    {
        version(D_Coverage) {} else version(LDC) pragma(inline, true);
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
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset]; // TODO: decode `dchar`
    }

    /// Peek next next `char` in input.
    dchar peek1() const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset + 1]; // TODO: decode `dchar`
    }

    /// Peek `n`-th next `char` in input.
    dchar peekN(in size_t n) const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset + n]; // TODO: decode `dchar`
    }

    /// Drop next byte in input.
    void drop1() nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        _offset += 1;
    }

    /// Drop next `n` bytes in input.
    void dropN(in size_t n) nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
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
            ++i;                // TODO: decode `dchar`
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
            ++i;
        while (peekN(i).isAlphaNum ||
               peekN(i) == '_' ||
               (attributeFlag && // attribute name
                peekN(i) == ':')) // may include colon qualifier
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get number.
    Input getNumber() return nothrow @nogc
    {
        import std.ascii : isDigit;
        size_t i;
        while (peekN(i).isDigit)
            ++i;
        return skipOverN(i);
    }

    Input getWhitespace() return nothrow @nogc
    {
        size_t i;
        while (peekN(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
            ++i;
        return skipOverN(i);
    }

    bool skipOverEsc(ref size_t i) nothrow @nogc
    {
        if (peekN(i) == '\\')   // TODO: decode `dchar`
        {
            ++i;
            if (peekN(i) == 'n')
                ++i;            // TODO: convert to "\r"
            else if (peekN(i) == 't')
                ++i;            // TODO: convert to "\t"
            else if (peekN(i) == 'r')
                ++i;            // TODO: convert to ASCII "\r"
            else if (peekN(i) == ']')
                ++i;            // TODO: convert to ASCII "]"
            else if (peekN(i) == 'u')
            {
                ++i;
                import std.ascii : isDigit;
                while (peekN(i).isDigit)
                    ++i;
                // TODO: convert to `dchar`
            }
            else if (peekN(i) == '\0')
            {
                errorAtIndex("unterminated escape sequence at end of file");
                return false;
            }
            else
                i += 1;
            return true;
        }
        return false;
    }

    Input getTextLiteralDoubleQuoted() return nothrow @nogc
    {
        drop1();
        size_t i;
        while (!peekN(i).among!('\0', '"'))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        const literal = skipOverN(i);
        if (peek0() == '"')
            drop1();        // pop ending double singlequote
        return literal;
    }

    Input getTextLiteralSingleQuoted() return nothrow @nogc
    {
        drop1();
        size_t i;
        while (!peekN(i).among!('\0', '\''))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        const literal = skipOverN(i);
        if (peek0() == '\'')
            drop1();        // pop ending double singlequote
        return literal;
    }

    Input getHooks() return nothrow @nogc
    {
        size_t i;
        while (!peekN(i).among!('\0', ']')) // may contain whitespace
        {
            if (!skipOverEsc(i))
                ++i;
        }
        if (peekN(i) == ']') // skip ']'
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
                    ds.insertBack('{');
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
                    ds.insertBack('[');
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
                    ds.insertBack('(');
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
                    ds.insertBack('\'');
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
                    ds.insertBack('"');
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
            _token = Token(TOK.hooks, getHooks());
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
                _token = Token(TOK.alwaysIncludePredicate, skipOver2());
            else
                _token = Token(TOK.labelAssignment, skipOver1());
            break;
        case '*':
            _token = Token(TOK.star, skipOver1());
            break;
        case '+':
            if (peek1() == '=')
                _token = Token(TOK.listLabelAssignment, skipOver2());
            else
                _token = Token(TOK.plus, skipOver1());
            break;
        case '|':
            _token = Token(TOK.pipe, skipOver1());
            break;
        case '~':
            _token = Token(TOK.tilde, skipOver1());
            break;
        case '?':
            _token = Token(TOK.optOrSemPred, skipOver1());
            break;
        case '<':
            _token = Token(TOK.lt, skipOver1());
            break;
        case '>':
            _token = Token(TOK.gt, skipOver1());
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
                        case "lexer": _token = Token(TOK.LEXER, symbol); break;
                        case "parser": _token = Token(TOK.PARSER, symbol); break;
                        case "grammar": _token = Token(TOK.GRAMMAR, symbol); break;
                        case "options": _token = Token(TOK.OPTIONS, symbol); break;
                        case "header": _token = Token(TOK.HEADER, symbol); break;
                        case "tokens": _token = Token(TOK.TOKENS, symbol); break;
                        case "channels": _token = Token(TOK.CHANNELS, symbol); break;
                        case "mode": _token = Token(TOK.MODE, symbol); break;
                        case "private": _token = Token(TOK.PRIVATE, symbol); break;
                        case "protected": _token = Token(TOK.PROTECTED, symbol); break;
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
            {
                _token = Token(TOK._error);
                errorAtIndex("unexpected character");
            }
        }
    }

    void infoAtFront(const scope string msg) const @trusted nothrow @nogc scope
    {
        messageAtToken(front, "Info", msg);
    }

    void warningAtFront(const scope string msg) const @trusted nothrow @nogc scope
    {
        messageAtToken(front, "Warning", msg);
    }

    void errorAtFront(const scope string msg) const @trusted nothrow @nogc scope
    {
        messageAtToken(front, "Error", msg);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    private void messageAtToken(in Token token,
                                const scope string tag,
                                const scope string msg) const @trusted nothrow @nogc scope
    {
        import core.stdc.stdio : printf;
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
                      in size_t i = 0) const @trusted nothrow @nogc scope
    {
        messageAtIndex("Error", msg, i);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    void warningAtIndex(const scope string msg,
                        in size_t i = 0) const @trusted nothrow @nogc scope
    {
        messageAtIndex("Warning", msg, i);
    }

    void infoAtIndex(const scope string msg,
                     in size_t i = 0, in const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        messageAtIndex("Info", msg, i, ds);
    }

    void messageAtIndex(const scope string tag,
                        const scope string msg,
                        in size_t i = 0,
                        in const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        import core.stdc.stdio : printf;
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
    bool _includeComments = false;
    bool _includeWhitespace = false;
}

/// Node.
enum NODE
{
    grammar,                    ///< Grammar defintion (name).
    rule                        ///< Grammar rule.
}

/// AST node.
abstract class Node
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        this.head = head;
    }
    Token head;
}

abstract class BranchN(uint n) : Node
{
@safe pure nothrow @nogc:
    this(in Token head, Node[n] sub)
    {
        super(head);
        this.sub = sub;
    }
    Node[n] sub;
}

abstract class BranchM : Node
{
@safe pure nothrow @nogc:
    this(in Token head, Node[] subs = null)
    {
        super(head);
        this.subs = subs;
    }
    Node[] subs;
}

/// Sequence.
final class SeqM : BranchM
{
@safe pure nothrow @nogc:
    this(in Token head, Node[] subs = null)
    {
        super(head);
        this.subs = subs;
    }
    Node[] subs;
}

/// Rule of alternatives.
class RuleAltM : BranchM
{
@safe pure nothrow @nogc:
    this(in Token head, Node[] subs = null)
    {
        super(head);
        this.subs = subs;
    }
    Node[] subs;
}

/// Fragment rule of alternatives.
final class FragmentRuleAltM : RuleAltM
{
@safe pure nothrow @nogc:
    this(in Token head, Node[] subs = null)
    {
        super(head);
        this.subs = subs;
    }
    Node[] subs;
}

class Leaf : Node
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

class Symbol : Node
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

class LineComment : Node
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

class BlockComment : Node
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class Grammar : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

final class LexerGrammar : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

/// See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687210/Quick+Starter+on+Parser+Grammars+-+No+Past+Experience+Required
final class ParserGrammar : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

final class Import : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head, const Input[] modules)
    {
        super(head);
        this.modules = modules;
    }
    const Input[] modules;
}

final class Mode : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head, Input name)
    {
        super(head);
        this.name = name;
    }
    Input name;
}

final class Options : Leaf
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

final class Header : Leaf
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

final class ScopeSymbolAction : Leaf
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

final class ScopeSymbol : Leaf
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

final class ScopeAction : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head,
         in Token code)
    {
        super(head);
        this.code = code;
    }
    Token code;
}

final class AttributeSymbol : Leaf
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

final class Action : Leaf
{
@safe pure nothrow @nogc:
    this(in Token head)
    {
        super(head);
    }
}

final class ActionSymbol : Leaf
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

final class Channels : Leaf
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

final class Tokens : Leaf
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

final class Class : Leaf
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

/** G4 parser.
 *
 * See: `ANTLRv4Parser.g4`
 */
struct G4Parser
{
@safe pure:
    this(in Input input,
         const scope string path = null,
         in bool includeComments = false) @trusted
    {
        _lexer = G4Lexer(input, path, includeComments);
        _front = nextFront();
    }

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _lexer.empty;
    }

    inout(Node) front() inout scope return @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _front;
    }

    void popFront()
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        _front = nextFront();
    }

    private RuleAltM getRule(in Token name,
                             in bool isFragment,
                             ActionSymbol actionSymbol = null,
                             Action action = null) @trusted
    {
        _lexer.popFrontEnforceTOK(TOK.colon, "no colon");
        Appender!(Node[]) alts; // TODO: use static array with length being number of `TOK.pipe` till `TOK.semicolon`
        while (_lexer.front.tok != TOK.semicolon)
        {
            Appender!(Node[]) seq; // TODO: use stack for small arrays
            while (_lexer.front.tok != TOK.pipe &&
                   _lexer.front.tok != TOK.semicolon)
            {
                // TODO: use static array with length being number of tokens till `TOK.pipe`
                seq.put(new Symbol(_lexer.frontPop()));
            }
            if (!seq.data.length)
            {
                // `seq` may be empty
                // _lexer.infoAtFront("empty sequence");
            }
            alts.put(new SeqM(name, seq.data));
            if (_lexer.front.tok == TOK.pipe)
                _lexer.popFront(); // skip terminator
        }

        _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");

        // needed for ANTLRv2.g2:
        if (!empty)
        {
            // if (_lexer.front == Token(TOK.symbol, "exception"))
            //     _lexer.popFront();
            // if (_lexer.front == Token(TOK.symbol, "catch"))
            //     _lexer.popFront();
            if (_lexer.front.tok == TOK.hooks)
                _lexer.popFront();
            if (_lexer.front.tok == TOK.action)
                _lexer.popFront();
        }

        return (isFragment ?
                new FragmentRuleAltM(name, alts.data) :
                new RuleAltM(name, alts.data));
    }

    Input[] getArgs(in TOK separator,
                    in TOK terminator)
    {
        Appender!(Input[]) result;
        while (true)
        {
            result.put(_lexer.frontPopEnforceTOK(TOK.symbol).input);
            if (_lexer.front.tok != separator)
                break;
            _lexer.popFront();
        }
        _lexer.popFrontEnforceTOK(terminator, "no terminating semicolon");
        return result.data;
    }

    AttributeSymbol getAttributeSymbol() nothrow
    {
        return new AttributeSymbol(_lexer.frontPop(),
                                   _lexer.frontPopEnforceTOK(TOK.action,
                                                             "missing action"));
    }

    ActionSymbol getActionSymbol() nothrow
    {
        return new ActionSymbol(_lexer.frontPop(),
                                _lexer.frontPopEnforceTOK(TOK.action,
                                                          "missing action"));
    }

    Leaf getScope(in Token head)
    {
        if (_lexer.front.tok == TOK.symbol)
        {
            const symbol = _lexer.frontPop().input;
            if (_lexer.front.tok == TOK.action)
                return new ScopeSymbolAction(head, symbol,
                                             _lexer.frontPopEnforceTOK(TOK.action,
                                                                       "missing action"));
            else
            {
                auto result = new ScopeSymbol(head, symbol);
                _lexer.frontPopEnforceTOK(TOK.semicolon,
                                          "missing terminating semicolon");
                return result;
            }
        }
        else
        {
            return new ScopeAction(head,
                                   _lexer.frontPopEnforceTOK(TOK.action,
                                                             "missing action"));
        }
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
    Leaf skipOverScope()
    {
        if (_lexer.front == Token(TOK.symbol, "scope"))
            return getScope(_lexer.frontPop());
        return null;
    }

    Options makeOptions(in Token head) nothrow
    {
        return new Options(head,
                           _lexer.frontPopEnforceTOK(TOK.action,
                                                     "missing action"));
    }

    Header makeHeader(in Token head)
    {
        const name = (_lexer.front.tok == TOK.textLiteralDoubleQuoted ?
                      _lexer.frontPop() :
                      Token.init);
        const action = _lexer.frontPopEnforceTOK(TOK.action,
                                                 "missing action");
        return new Header(head, name, action);
    }

    Action makeAction(in Token head)
    {
        return new Action(head);
    }

    /// Skip over options if any.
    Options skipOverOptions()
    {
        if (_lexer.front.tok == TOK.OPTIONS)
            return makeOptions(_lexer.frontPop());
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
        if (_lexer.front.tok == TOK.hooks)
        {
            // _lexer.infoAtFront("TODO: use TOK.hooks");
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
            return getActionSymbol();
        return null;
    }

    Node nextFront() @trusted
    {
        // _lexer.infoAtFront("");
        switch (_lexer.front.tok)
        {
        case TOK.LEXER:
        case TOK.PARSER:
        case TOK.GRAMMAR:
            const head = _lexer.frontPop();
            bool lexerFlag;
            bool parserFlag;
            if (head.tok == TOK.LEXER)
            {
                lexerFlag = true;
                _lexer.popFrontEnforceTOK(TOK.GRAMMAR, "expected `grammar` after `lexer`");
            }
            else if (head.tok == TOK.PARSER)
            {
                parserFlag = true;
                _lexer.popFrontEnforceTOK(TOK.GRAMMAR, "expected `grammar` after `parser`");
            }

            if (lexerFlag)
            {
                auto front = new LexerGrammar(head, _lexer.frontPop().input);
                _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
                return front;
            }
            else if (parserFlag)
            {
                auto front = new ParserGrammar(head, _lexer.frontPop().input);
                _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
                return front;
            }
            else
            {
                if (_lexer.front.tok == TOK.colon)
                    return getRule(head, false);
                else
                {
                    auto front = new Grammar(head, _lexer.frontPop().input);
                    _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
                    return front;
                }
            }
        case TOK.MODE:
            auto front = new Mode(_lexer.frontPop(), _lexer.frontPop().input);
            _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
            return front;
        case TOK.HEADER:
            const head = _lexer.frontPop();
            if (_lexer.front.tok == TOK.colon)
                return getRule(head, false); // normal rule
            else
                return makeHeader(head);
        case TOK.OPTIONS:
            const head = _lexer.frontPop();
            if (_lexer.front.tok == TOK.colon)
                return getRule(head, false); // normal rule
            else
                return makeOptions(head);
        case TOK.CHANNELS:
            return new Channels(_lexer.frontPop(),
                                  _lexer.frontPopEnforceTOK(TOK.action,
                                                            "missing action"));
        case TOK.TOKENS:
            return new Tokens(_lexer.frontPop(),
                                _lexer.frontPopEnforceTOK(TOK.action,
                                                          "missing action"));
        case TOK.PRIVATE:
            const privateFlag = true; // TODO: use
            _lexer.popFront();
            _lexer.frontEnforceTOK(TOK.symbol, "expected symbol after `private`");
            goto case TOK.symbol;
        case TOK.PROTECTED:
            const protectedFlag = true; // TODO: use
            _lexer.popFront();
            _lexer.frontEnforceTOK(TOK.symbol, "expected symbol after `protected`");
            goto case TOK.symbol;
        case TOK.symbol:
            const head = _lexer.frontPop();
            switch (head.input)
            {
            case `class`:
                if (_lexer.front.tok == TOK.colon)
                    return getRule(head, false); // normal rule
                else
                {
                    auto front = new Class(head,
                                           _lexer.frontPopEnforceTOK(TOK.symbol,
                                                                     "missing symbol").input,
                                           _lexer.skipOverToken(Token(TOK.symbol, "extends")).input ?
                                           _lexer.frontPop().input :
                                           null);
                    _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
                    return front;
                }
            case `scope`:
                if (_lexer.front.tok == TOK.colon)
                    return getRule(head, false); // normal rule
                else
                    return getScope(head);
            case `import`:
                return new Import(head, getArgs(TOK.comma, TOK.semicolon));
            case `fragment`: // lexer helper rule, not real token for parser.
                return getRule(_lexer.frontPop(), true);
            default:
                while (_lexer.front.tok != TOK.colon)
                {
                    if (skipOverExclusion()) // TODO: use
                        continue;
                    if (skipOverReturns())  // TODO: use
                        continue;
                    if (skipOverHooks())    // TODO: use
                        continue;
                    if (const _ = skipOverSymbol("locals")) // TODO: use
                        continue;
                    if (const _options = skipOverOptions()) // TODO: use
                        continue;
                    if (const _scope = skipOverScope())     // TODO: use
                        continue;
                    if (const _action = skipOverAction()) // TODO: use
                        continue;
                    if (const _actionSymbol = skipOverActionSymbol()) // TODO: use
                        continue;
                }
                return getRule(head, false);
            }
        case TOK.attributeSymbol:
            return getAttributeSymbol();
        case TOK.actionSymbol:
            return getActionSymbol();
        case TOK.blockComment:
            return new BlockComment(_lexer.frontPop());
        case TOK.lineComment:
            return new LineComment(_lexer.frontPop());
        case TOK.action:
            return new Action(_lexer.frontPop());
        default:
            _lexer.errorAtFront("TODO: handle");
            assert(false);
        }
    }

private:
    G4Lexer _lexer;
    Node _front;
}

/// G4 filer parser.
struct G4FileParser           // TODO: convert to `class`
{
@safe:
    this(in string filePath)
    {
        import std.path : expandTilde;
        const path = filePath.expandTilde;
        const data = cast(Input)rawReadPath(path); // cast to Input because we don't want to keep all file around:
        parser = G4Parser(data, filePath, false);
    }
    ~this() @nogc {}
    G4Parser parser;
    alias parser this;
}

///
@trusted unittest
{
    import nxt.array_algorithm : endsWith;
    import std.file : dirEntries, SpanMode;
    import std.path : expandTilde;

    const root = "~/Work/grammars-v4/".expandTilde;
    const testLexer = true;
    const testParser = true;

    if (testLexer)
        foreach (e; dirEntries(root, SpanMode.breadth))
        {
            const fn = e.name;
            if (fn.endsWith(`.g`) ||
                fn.endsWith(`.g2`) ||
                fn.endsWith(`.g4`))
            {
                debug writeln("Lexing ", fn, " ...");
                const data = cast(Input)rawReadPath(fn);
                auto lexer = G4Lexer(data, fn, false);
                while (!lexer.empty)
                    lexer.popFront();
            }
        }

    if (testParser)
        foreach (e; dirEntries(root, SpanMode.breadth))
        {
            const fn = e.name;
            if (fn.endsWith(`.g`) ||
                fn.endsWith(`.g2`) ||
                fn.endsWith(`.g4`))
            {
                if (fn.endsWith(`Antlr3.g`))
                    continue;
                if (fn.endsWith(`ANTLRv2.g2`))
                    continue;
                // if (!fn.endsWith(`Thrift.g4`))
                //     continue;
                debug writeln("Parsing ", fn, " ...");
                auto parser = G4FileParser(fn);
                while (!parser.empty)
                    parser.popFront();
            }
        }
}
