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

import std.stdio : writeln;

// `d-deps.el` requires these to be at the top:
import nxt.line_column : offsetLineColumn;
import nxt.file_ex : rawReadPath;

enum useKeywords = true;

alias Input = const(char)[];

///< Token kind. TODO: make this a string type like with std.experimental.lexer
enum TOK
{
    unknown,                    ///< Unknown.

    whitespace,

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

    alts,                       ///< Alternatives within '[' ... ']'

    textLiteralSingleQuoted,    ///< Text (string) literal, surrounded by single quotes.
    textLiteralDoubleQuoted,    ///< Text (string) literal, surrounded by double quotes.

    colon,                      ///< Colon `:`.
    semicolon,                  ///< Semicolon `;`.
    hash,                       ///< Hash `#`
    labelAssignment,            ///< Label assignment `=`
    listLabelAssignment,        ///< List label assignment `+=`

    optOrSemPred,               ///< Optional or semantic predicate (`?`)
    zeroOrMore,                 ///< Zero or more (`*`)
    oneOrMore,                  ///< One or more (`+`)
    alternative,                ///< Alternative (`|`)
    negation,                   ///< Match negation (`~`)
    lt,                         ///<  `<`
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
    this(TOK tok, const(char)[] input = null)
    {
        this.tok = tok;
        this.input = input;
        // debug writeln("tok:", tok, " input:", input);
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

@safe pure:

    this(Input input,
         string path = null,
         bool includeComments = false,
         bool includeWhitespace = false) @trusted
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

    Token front() const scope return nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _token;
    }

    void popFront() scope nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        nextFront();
    }

    void popFrontEnforce(in TOK tok, in string msg) nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        const result = frontPop;
        if (result.tok != tok)
            error(msg);
    }

    Token frontPop() scope return nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        auto result = front;
        popFront();
        return result;
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

    Input getAlternatives() return nothrow @nogc
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

    void nextFront() scope nothrow @nogc @trusted
    {
        switch (peekChar)
        {
        case '/':
            if (peekCharNth(1) == '/') // `//`
            {
                _offset += 2;
                skipLineComment();
                if (_includeComments)
                    _token = Token(TOK.lineComment);
                else
                    nextFront();
            }
            else if (peekCharNth(1) == '*') // `/*`
            {
                _offset += 2;
                skipBlockComment();
                if (_includeComments)
                    _token = Token(TOK.blockComment);
                else
                    nextFront();
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
            _token = Token(TOK.alts, getAlternatives());
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
        case ' ':
        case '\t':
        case '\n':
        case '\v':
        case '\r':
        case '\f':
            // TODO: extend to std.uni
            // import std.uni : isWhite;
            // assert(peekChar.isWhite);
            const ws = getWhitespace();
            if (_includeWhitespace)
                _token = Token(TOK.whitespace, ws);
            else
                nextFront();
            break;
        case '\0':
            _token = Token.init;
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
            {
                _token = Token(TOK._error);
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
        // TODO: remove printf
        debug printf("%.*s(%u,%u): %s: %.*s at offset %llu being char `%c` ds:`%.*s`\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     tag.ptr,
                     cast(int)msg.length, msg.ptr,
                     _offset + i,
                     peekCharNth(i),
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
    this(Token token)
    {
        this.token = token;
    }
    Token token;
}

abstract class BranchN(uint n) : Node
{
@safe pure nothrow @nogc:
    this(Token token, Node[n] sub)
    {
        super(token);
        this.sub = sub;
    }
    Node[n] sub;
}

abstract class BranchM : Node
{
@safe pure nothrow @nogc:
    this(Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

/// Sequence.
final class SeqM : BranchM
{
@safe pure nothrow @nogc:
    this(Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

/// Alternative.
final class AltM : BranchM
{
@safe pure nothrow @nogc:
    this(Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

class Leaf : Node
{
@safe pure nothrow @nogc:
    this(Token token)
    {
        super(token);
    }
}

class Symbol : Node
{
@safe pure nothrow @nogc:
    this(Token token)
    {
        super(token);
    }
}

class Grammar : Leaf
{
@safe pure nothrow @nogc:
    this(Token token, Input name)
    {
        // debug writeln("token:", token, " name:", name);
        super(token);
        this.name = name;
    }
    Input name;
}

/** G4 parser.
 *
 * See: `ANTLRv4Parser.g4`
 */
struct G4Parser
{
@safe pure:
    this(Input input,
         string path = null,
         bool includeComments = false) @trusted
    {
        _lexer = G4Lexer(input, path, includeComments);
        nextFront();
    }

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _lexer.empty;
    }

    Node front() scope return @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _front;
    }

    void popFront()
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        nextFront();
    }

    void nextFront() @trusted
    {
        switch (_lexer.front.tok)
        {
        case TOK.GRAMMAR:
            if (_lexer.empty)
                _lexer.error("expected name after grammar");
            _front = new Grammar(_lexer.frontPop,
                                 _lexer.frontPop.input);
            _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
            break;
        case TOK.symbol:
            const name = _lexer.frontPop;
            _lexer.popFrontEnforce(TOK.colon, "no colon");
            scope Node[] alts;
            while (_lexer.front.tok != TOK.semicolon)
            {
                debug writeln("i:", _lexer.front);
                scope Node[] seq;
                while (_lexer.front.tok != TOK.alts)
                {
                    debug writeln("j:", _lexer.front);
                    seq ~= new Symbol(_lexer.frontPop);
                }
                alts ~= new SeqM(name, seq);
            }
            _front = new AltM(name, alts);
            _lexer.popFrontEnforce(TOK.semicolon, "no terminating semicolon");
            break;
        case TOK.blockComment:
        case TOK.lineComment:
            return _lexer.popFront();   // ignore
        default:
            return _lexer.error("handle");
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
    this(const string filePath)
    {
        import std.path : expandTilde;
        const path = filePath.expandTilde;
        const data = cast(Input)rawReadPath(path); // cast to Input because we don't want to keep all file around:
        parser = G4Parser(data, filePath, false);
    }
    ~this() @nogc
    {
    }
    G4Parser parser;
    alias parser this;
}

///
unittest
{
    const testLexer = true;
    const testParser = true;
    import std.file : dirEntries, SpanMode;
    import std.path : expandTilde;
    foreach (dirEntry; dirEntries("~/Work/grammars-v4/".expandTilde, SpanMode.breadth))
    {
        import nxt.array_algorithm : endsWith;
        const fpath = dirEntry.name;

        // skip grammars with inline code. TODO: handle these by skipping over matching braces
        // if (!fpath.endsWith(`links.g2`))
        //     continue;

        if (fpath.endsWith(`.g`) ||
            fpath.endsWith(`.g2`) ||
            fpath.endsWith(`.g4`))
        {
            if (!fpath.endsWith(`pascal.g4`)) // only pascal
                continue;

            // test lexer
            if (testLexer)
            {
                debug writeln("Lexing ", fpath);
                const data = cast(Input)rawReadPath(fpath); // cast to Input because we don't want to keep all file around:
                auto lexer = G4Lexer(data, fpath, false);
                while (!lexer.empty)
                {
                    lexer.popFront();
                }
            }

            if (testParser)
            {
                debug writeln("Parsing ", fpath);
                auto fparser = G4FileParser(fpath);
                while (!fparser.empty)
                {
                    // debug writeln(fparser._lexer.front);
                    fparser.popFront();
                }
            }
        }
    }
}
