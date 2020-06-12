/**
   Whirlpool hashing algorithm implementation. This module conforms to the APIs defined in std.digest.

   Based on the original Whirlpool implementation by Paulo S.L.M. Barreto and Vincent Rijmen.
*/
module nxt.digestx.whirlpool;

public import std.digest;
import std.range;

/**
 * Template API Whirlpool implementation.
 */
struct Whirlpool
{
    /// Initializes the digest calculation.
    void start() @safe pure nothrow @nogc
    {
        _lenBuf = 0;
        _bufferPos = 0;
        _hash[] = 0;
        _tailBlock = false;
        _bitLength[] = 0;
    }

    /// Feed the digest with data.
    void put(scope const(ubyte)[] data...) @trusted pure nothrow @nogc
    {
        bufLength(data.length);

        while (true)
        {
            immutable cap = _buffer.length - _bufferPos;
            if (cap <= data.length)
            {
                _buffer[_bufferPos .. $] = data[0 .. cap];
                processBuffer();
                _bufferPos = 0;

                data = data[cap .. $];
            }
            else
            {
                _buffer[_bufferPos .. _bufferPos + data.length] = data;
                _bufferPos += data.length;

                data = null;
                break;
            }
        }
    }

    void put(R)(R r) if (isInputRange!R && hasLength!R)
    {
        bufLength(r.length);

        foreach (immutable i; r)
        {
            _buffer[_bufferPos] = i;
            _bufferPos++;
            if (_bufferPos == _buffer.length)
            {
                processBuffer();
                _bufferPos = 0;
            }
        }
    }

    /// Returns the Whirlpool hash. This also calls start to reset the internal state.
    ubyte[64] finish() @trusted pure nothrow @nogc
    {
        // append a '1'-bit
        // As the buffer is byte-wise in this implementation, _buffer[_bufferPos] is not used yet.
        _buffer[_bufferPos] = 0x80;
        _bufferPos++;

        if (_bufferPos > 32)
        {
            _buffer[_bufferPos .. $] = 0;
            processBuffer();
            _bufferPos = 0;
        }

        if (_bufferPos < 32)
            _buffer[_bufferPos .. 32] = 0;
        if (_lenBuf != 0)
            addLength(_lenBuf);
        _buffer[32 .. $] = _bitLength;
        processBuffer();

        ubyte[64] digest = void;

        for (int i = 0, j = 0; i < 8; i++, j += 8)
        {
            immutable h = _hash[i];
            digest[j] = cast(ubyte)(h >> 56);
            digest[j + 1] = cast(ubyte)(h >> 48);
            digest[j + 2] = cast(ubyte)(h >> 40);
            digest[j + 3] = cast(ubyte)(h >> 32);
            digest[j + 4] = cast(ubyte)(h >> 24);
            digest[j + 5] = cast(ubyte)(h >> 16);
            digest[j + 6] = cast(ubyte)(h >> 8);
            digest[j + 7] = cast(ubyte) h;
        }

        start();

        return digest;
    }

private:

    // buffers sum of data length and add into _bitLength when necessary,
    // to reduce bignum operation.
    ulong _lenBuf = void;

    void bufLength(ulong bytes) @safe pure nothrow @nogc
    {
        ulong sum = _lenBuf + bytes;
        if (sum < _lenBuf || sum < bytes)
        {
            addLength(_lenBuf);
            _lenBuf = bytes;
        }
        else
        {
            _lenBuf = sum;
        }
    }

    ubyte[32] _bitLength = void;

    void addLength(ulong bytes) @trusted pure nothrow @nogc
    {
        uint carry = _bitLength[31] + ((bytes << 3) & 0xFF);
        _bitLength[31] = cast(ubyte) carry;
        carry >>= 8;
        bytes >>= 5;
        for (int i = 30; i >= 0; i--)
        {
            carry += _bitLength[i] + (bytes & 0xFF);
            _bitLength[i] = cast(ubyte) carry;
            carry >>= 8;
            bytes >>= 8;
        }
    }

    ubyte[64] _buffer = void;
    size_t _bufferPos = void;

    ulong[8] _hash = void;

    bool _tailBlock;

    void processBuffer() @trusted pure nothrow @nogc
    {
        ulong[8] block = void;

        // map the buffer to a block
        for (int i = 0, j = 0; i < 8; i++, j += 8)
        {
            block[i] = (cast(ulong) _buffer[j] << 56) ^ (cast(ulong) _buffer[j + 1] << 48) ^ (
                cast(ulong) _buffer[j + 2] << 40) ^ (cast(ulong) _buffer[j + 3] << 32) ^ (
                    cast(ulong) _buffer[j + 4] << 24) ^ (cast(ulong) _buffer[j + 5] << 16) ^ (
                        cast(ulong) _buffer[j + 6] << 8) ^ (cast(ulong) _buffer[j + 7]);
        }

        // compute and apply K^0 to the cipher state
        ulong[8] state = void;
        state[] = block[] ^ _hash[];

        // iterate over all rounds
        if (_tailBlock) // not the first block
        {
            ulong[8] K = _hash;

            foreach (immutable rcr; rc)
            {
                ulong[8] L = void;

                // compute K^r from K^{r-1}
                mixin(genTransform("L", "K"));
                L[0] ^= rcr;

                K = L;

                // apply the r-th round transformation
                mixin(genTransform("L", "state"));

                state[] = L[] ^ K[];
            }
        }
        else // use precompiled K[] for first block
        {
            foreach (immutable k; pcK)
            {
                ulong[8] L = void;
                mixin(genTransform("L", "state"));
                state[] = L[] ^ k[];
            }

            _tailBlock = true;
        }

        // apply the Miyaguchi-Preneel compression function:
        _hash[] ^= state[] ^ block[];
    }
}

/// Convenience alias for digest function in std.digest using the Whirlpool implementation.
auto whirlpoolOf(T...)(T data)
{
    return digest!(Whirlpool, T)(data);
}

/// OOP API for Whirlpool
alias WhirlpoolDigest = WrapperDigest!Whirlpool;

///
unittest
{
    import nxt.digestx.whirlpool;

    ubyte[1024] data;
    Whirlpool wp;
    wp.start();
    wp.put(data[]);
    wp.start();
    wp.put(data[]);
    ubyte[64] hash = wp.finish();

    // Template API

    ubyte[64] hash2 = whirlpoolOf("abc");
    assert(digest!Whirlpool("abc") == hash2);
    assert(hexDigest!Whirlpool("abc") == toHexString(hash2));

    // OOP API

    Digest wpDigest = new WhirlpoolDigest;
    ubyte[] hash3 = wpDigest.digest("abc");
    assert(toHexString(hash3) == "4E2448A4C6F486BB16B6562C73B4020BF3043E3A731BCE721AE1B303D97E6D4C"
           ~ "7181EEBDB6C57E277D0E34957114CBD6C797FC9D95D8B582D225292076D4EEF5");
}

@safe pure nothrow /*@nogc*/ unittest
{
    static assert(isDigest!Whirlpool);

    assert(digest!Whirlpool("The quick brown fox jumps over the lazy dog")
           == hexString!"B97DE512E91E3828B40D2B0FDCE9CEB3C4A71F9BEA8D88E75C4FA854DF36725F"
           ~ hexString!"D2B52EB6544EDCACD6F8BEDDFEA403CB55AE31F03AD62A5EF54E42EE82C3FB35");

    // ISO test vectors

    assert(digest!Whirlpool("") == hexString!"19FA61D75522A4669B44E39C1D2E1726C530232130D407F89AFEE0964997F7A7"
           ~ hexString!"3E83BE698B288FEBCF88E3E03C4F0757EA8964E59B63D93708B138CC42A66EB3");

    assert(digest!Whirlpool("a") == hexString!"8ACA2602792AEC6F11A67206531FB7D7F0DFF59413145E6973C45001D0087B42"
           ~ hexString!"D11BC645413AEFF63A42391A39145A591A92200D560195E53B478584FDAE231A");

    assert(digest!Whirlpool("abc") == hexString!"4E2448A4C6F486BB16B6562C73B4020BF3043E3A731BCE721AE1B303D97E6D4C"
           ~ hexString!"7181EEBDB6C57E277D0E34957114CBD6C797FC9D95D8B582D225292076D4EEF5");

    assert(digest!Whirlpool("message digest") == hexString!"378C84A4126E2DC6E56DCC7458377AAC838D00032230F53CE1F5700C0FFB4D3B"
           ~ hexString!"8421557659EF55C106B4B52AC5A4AAA692ED920052838F3362E86DBD37A8903E");

    assert(digest!Whirlpool("abcdefghijklmnopqrstuvwxyz")
           == hexString!"F1D754662636FFE92C82EBB9212A484A8D38631EAD4238F5442EE13B8054E41B"
           ~ hexString!"08BF2A9251C30B6A0B8AAE86177AB4A6F68F673E7207865D5D9819A3DBA4EB3B");

    assert(digest!Whirlpool("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == hexString!"DC37E008CF9EE69BF11F00ED9ABA26901DD7C28CDEC066CC6AF42E40F82F3A1E"
           ~ hexString!"08EBA26629129D8FB7CB57211B9281A65517CC879D7B962142C65F5A7AF01467");

    assert(digest!Whirlpool("1234567890123456789012345678901234567890" ~ "1234567890123456789012345678901234567890")
           == hexString!"466EF18BABB0154D25B9D38A6414F5C08784372BCCB204D6549C4AFADB601429"
           ~ hexString!"4D5BD8DF2A6C44E538CD047B2681A51A2C60481E88C5A20B2C2A80CF3A9A083B");

    assert(digest!Whirlpool("abcdbcdecdefdefgefghfghighijhijk")
           == hexString!"2A987EA40F917061F5D6F0A0E4644F488A7A5A52DEEE656207C562F988E95C69"
           ~ hexString!"16BDC8031BC5BE1B7B947639FE050B56939BAAA0ADFF9AE6745B7B181C3BE3FD");
}

pure nothrow /*@nogc*/ unittest
{
    import std.range : repeat;

    assert(digest!Whirlpool(repeat('a',
                                   10 ^^ 6)) == hexString!"0C99005BEB57EFF50A7CF005560DDF5D29057FD86B20BFD62DECA0F1CCEA4AF5"
           ~ hexString!"1FC15490EDDC47AF32BB2B66C34FF9AD8C6008AD677F77126953B226E4ED8B01");
}

@trusted pure nothrow /*@nogc*/ unittest
{
    Whirlpool wp;
    wp.put(cast(ubyte[])("abc"));
    wp.start();
    wp.put(cast(ubyte[])("abc"));
    assert(wp.finish() == hexString!"4E2448A4C6F486BB16B6562C73B4020BF3043E3A731BCE721AE1B303D97E6D4C"
           ~ hexString!"7181EEBDB6C57E277D0E34957114CBD6C797FC9D95D8B582D225292076D4EEF5");
}

private:

string genTransform(string assignTo, string from)
{
    import std.string : format;

    string ret;
    foreach (i; 0 .. 8)
    {
        ret ~= format("%s[%s]=", assignTo, i);
        for (int t = 0, s = 56; t < 8; t++, s -= 8)
        {
            ret ~= format("C[%s][(%s[%s] >> %s) & 0xFF]", t, from, (i - t) & 7, s);
            if (t != 7)
                ret ~= "^";
        }
        ret ~= ";\n";
    }
    return ret;
}

// number of rounds
enum int numRounds = 10;

// circulant table
immutable ulong[256][8] C = [
    [
        0x18186018c07830d8UL, 0x23238c2305af4626UL, 0xc6c63fc67ef991b8UL,
        0xe8e887e8136fcdfbUL, 0x878726874ca113cbUL, 0xb8b8dab8a9626d11UL,
        0x0101040108050209UL, 0x4f4f214f426e9e0dUL, 0x3636d836adee6c9bUL,
        0xa6a6a2a6590451ffUL,
        0xd2d26fd2debdb90cUL, 0xf5f5f3f5fb06f70eUL, 0x7979f979ef80f296UL,
        0x6f6fa16f5fcede30UL, 0x91917e91fcef3f6dUL, 0x52525552aa07a4f8UL,
        0x60609d6027fdc047UL, 0xbcbccabc89766535UL, 0x9b9b569baccd2b37UL,
        0x8e8e028e048c018aUL, 0xa3a3b6a371155bd2UL, 0x0c0c300c603c186cUL,
        0x7b7bf17bff8af684UL,
        0x3535d435b5e16a80UL,
        0x1d1d741de8693af5UL, 0xe0e0a7e05347ddb3UL, 0xd7d77bd7f6acb321UL,
        0xc2c22fc25eed999cUL, 0x2e2eb82e6d965c43UL, 0x4b4b314b627a9629UL,
        0xfefedffea321e15dUL, 0x575741578216aed5UL, 0x15155415a8412abdUL,
        0x7777c1779fb6eee8UL, 0x3737dc37a5eb6e92UL, 0xe5e5b3e57b56d79eUL,
        0x9f9f469f8cd92313UL,
        0xf0f0e7f0d317fd23UL,
        0x4a4a354a6a7f9420UL, 0xdada4fda9e95a944UL, 0x58587d58fa25b0a2UL,
        0xc9c903c906ca8fcfUL, 0x2929a429558d527cUL, 0x0a0a280a5022145aUL,
        0xb1b1feb1e14f7f50UL, 0xa0a0baa0691a5dc9UL, 0x6b6bb16b7fdad614UL,
        0x85852e855cab17d9UL, 0xbdbdcebd8173673cUL, 0x5d5d695dd234ba8fUL,
        0x1010401080502090UL,
        0xf4f4f7f4f303f507UL,
        0xcbcb0bcb16c08bddUL, 0x3e3ef83eedc67cd3UL, 0x0505140528110a2dUL,
        0x676781671fe6ce78UL, 0xe4e4b7e47353d597UL, 0x27279c2725bb4e02UL,
        0x4141194132588273UL, 0x8b8b168b2c9d0ba7UL, 0xa7a7a6a7510153f6UL,
        0x7d7de97dcf94fab2UL, 0x95956e95dcfb3749UL, 0xd8d847d88e9fad56UL,
        0xfbfbcbfb8b30eb70UL,
        0xeeee9fee2371c1cdUL,
        0x7c7ced7cc791f8bbUL, 0x6666856617e3cc71UL, 0xdddd53dda68ea77bUL,
        0x17175c17b84b2eafUL, 0x4747014702468e45UL, 0x9e9e429e84dc211aUL,
        0xcaca0fca1ec589d4UL, 0x2d2db42d75995a58UL, 0xbfbfc6bf9179632eUL,
        0x07071c07381b0e3fUL, 0xadad8ead012347acUL, 0x5a5a755aea2fb4b0UL,
        0x838336836cb51befUL,
        0x3333cc3385ff66b6UL,
        0x636391633ff2c65cUL, 0x02020802100a0412UL, 0xaaaa92aa39384993UL,
        0x7171d971afa8e2deUL, 0xc8c807c80ecf8dc6UL, 0x19196419c87d32d1UL,
        0x494939497270923bUL, 0xd9d943d9869aaf5fUL, 0xf2f2eff2c31df931UL,
        0xe3e3abe34b48dba8UL, 0x5b5b715be22ab6b9UL, 0x88881a8834920dbcUL,
        0x9a9a529aa4c8293eUL,
        0x262698262dbe4c0bUL,
        0x3232c8328dfa64bfUL, 0xb0b0fab0e94a7d59UL, 0xe9e983e91b6acff2UL,
        0x0f0f3c0f78331e77UL, 0xd5d573d5e6a6b733UL, 0x80803a8074ba1df4UL,
        0xbebec2be997c6127UL, 0xcdcd13cd26de87ebUL, 0x3434d034bde46889UL,
        0x48483d487a759032UL, 0xffffdbffab24e354UL, 0x7a7af57af78ff48dUL,
        0x90907a90f4ea3d64UL,
        0x5f5f615fc23ebe9dUL,
        0x202080201da0403dUL, 0x6868bd6867d5d00fUL, 0x1a1a681ad07234caUL,
        0xaeae82ae192c41b7UL, 0xb4b4eab4c95e757dUL, 0x54544d549a19a8ceUL,
        0x93937693ece53b7fUL, 0x222288220daa442fUL, 0x64648d6407e9c863UL,
        0xf1f1e3f1db12ff2aUL, 0x7373d173bfa2e6ccUL, 0x12124812905a2482UL,
        0x40401d403a5d807aUL,
        0x0808200840281048UL,
        0xc3c32bc356e89b95UL, 0xecec97ec337bc5dfUL, 0xdbdb4bdb9690ab4dUL,
        0xa1a1bea1611f5fc0UL, 0x8d8d0e8d1c830791UL, 0x3d3df43df5c97ac8UL,
        0x97976697ccf1335bUL, 0x0000000000000000UL, 0xcfcf1bcf36d483f9UL,
        0x2b2bac2b4587566eUL, 0x7676c57697b3ece1UL, 0x8282328264b019e6UL,
        0xd6d67fd6fea9b128UL,
        0x1b1b6c1bd87736c3UL,
        0xb5b5eeb5c15b7774UL, 0xafaf86af112943beUL, 0x6a6ab56a77dfd41dUL,
        0x50505d50ba0da0eaUL, 0x45450945124c8a57UL, 0xf3f3ebf3cb18fb38UL,
        0x3030c0309df060adUL, 0xefef9bef2b74c3c4UL, 0x3f3ffc3fe5c37edaUL,
        0x55554955921caac7UL, 0xa2a2b2a2791059dbUL, 0xeaea8fea0365c9e9UL,
        0x656589650fecca6aUL,
        0xbabad2bab9686903UL,
        0x2f2fbc2f65935e4aUL, 0xc0c027c04ee79d8eUL, 0xdede5fdebe81a160UL,
        0x1c1c701ce06c38fcUL, 0xfdfdd3fdbb2ee746UL, 0x4d4d294d52649a1fUL,
        0x92927292e4e03976UL, 0x7575c9758fbceafaUL, 0x06061806301e0c36UL,
        0x8a8a128a249809aeUL, 0xb2b2f2b2f940794bUL, 0xe6e6bfe66359d185UL,
        0x0e0e380e70361c7eUL,
        0x1f1f7c1ff8633ee7UL,
        0x6262956237f7c455UL, 0xd4d477d4eea3b53aUL, 0xa8a89aa829324d81UL,
        0x96966296c4f43152UL, 0xf9f9c3f99b3aef62UL, 0xc5c533c566f697a3UL,
        0x2525942535b14a10UL, 0x59597959f220b2abUL, 0x84842a8454ae15d0UL,
        0x7272d572b7a7e4c5UL, 0x3939e439d5dd72ecUL, 0x4c4c2d4c5a619816UL,
        0x5e5e655eca3bbc94UL,
        0x7878fd78e785f09fUL,
        0x3838e038ddd870e5UL, 0x8c8c0a8c14860598UL, 0xd1d163d1c6b2bf17UL,
        0xa5a5aea5410b57e4UL, 0xe2e2afe2434dd9a1UL, 0x616199612ff8c24eUL,
        0xb3b3f6b3f1457b42UL, 0x2121842115a54234UL, 0x9c9c4a9c94d62508UL,
        0x1e1e781ef0663ceeUL, 0x4343114322528661UL, 0xc7c73bc776fc93b1UL,
        0xfcfcd7fcb32be54fUL,
        0x0404100420140824UL,
        0x51515951b208a2e3UL, 0x99995e99bcc72f25UL, 0x6d6da96d4fc4da22UL,
        0x0d0d340d68391a65UL, 0xfafacffa8335e979UL, 0xdfdf5bdfb684a369UL,
        0x7e7ee57ed79bfca9UL, 0x242490243db44819UL, 0x3b3bec3bc5d776feUL,
        0xabab96ab313d4b9aUL, 0xcece1fce3ed181f0UL, 0x1111441188552299UL,
        0x8f8f068f0c890383UL,
        0x4e4e254e4a6b9c04UL,
        0xb7b7e6b7d1517366UL, 0xebeb8beb0b60cbe0UL, 0x3c3cf03cfdcc78c1UL,
        0x81813e817cbf1ffdUL, 0x94946a94d4fe3540UL, 0xf7f7fbf7eb0cf31cUL,
        0xb9b9deb9a1676f18UL, 0x13134c13985f268bUL, 0x2c2cb02c7d9c5851UL,
        0xd3d36bd3d6b8bb05UL, 0xe7e7bbe76b5cd38cUL, 0x6e6ea56e57cbdc39UL,
        0xc4c437c46ef395aaUL,
        0x03030c03180f061bUL,
        0x565645568a13acdcUL, 0x44440d441a49885eUL, 0x7f7fe17fdf9efea0UL,
        0xa9a99ea921374f88UL, 0x2a2aa82a4d825467UL, 0xbbbbd6bbb16d6b0aUL,
        0xc1c123c146e29f87UL, 0x53535153a202a6f1UL, 0xdcdc57dcae8ba572UL,
        0x0b0b2c0b58271653UL, 0x9d9d4e9d9cd32701UL, 0x6c6cad6c47c1d82bUL,
        0x3131c43195f562a4UL,
        0x7474cd7487b9e8f3UL,
        0xf6f6fff6e309f115UL, 0x464605460a438c4cUL, 0xacac8aac092645a5UL,
        0x89891e893c970fb5UL, 0x14145014a04428b4UL, 0xe1e1a3e15b42dfbaUL,
        0x16165816b04e2ca6UL, 0x3a3ae83acdd274f7UL, 0x6969b9696fd0d206UL,
        0x09092409482d1241UL, 0x7070dd70a7ade0d7UL, 0xb6b6e2b6d954716fUL,
        0xd0d067d0ceb7bd1eUL,
        0xeded93ed3b7ec7d6UL,
        0xcccc17cc2edb85e2UL, 0x424215422a578468UL, 0x98985a98b4c22d2cUL,
        0xa4a4aaa4490e55edUL, 0x2828a0285d885075UL, 0x5c5c6d5cda31b886UL,
        0xf8f8c7f8933fed6bUL, 0x8686228644a411c2UL,
        ], [
            0xd818186018c07830UL, 0x2623238c2305af46UL, 0xb8c6c63fc67ef991UL,
            0xfbe8e887e8136fcdUL, 0xcb878726874ca113UL, 0x11b8b8dab8a9626dUL,
            0x0901010401080502UL, 0x0d4f4f214f426e9eUL, 0x9b3636d836adee6cUL,
            0xffa6a6a2a6590451UL,
            0x0cd2d26fd2debdb9UL, 0x0ef5f5f3f5fb06f7UL, 0x967979f979ef80f2UL,
            0x306f6fa16f5fcedeUL, 0x6d91917e91fcef3fUL, 0xf852525552aa07a4UL,
            0x4760609d6027fdc0UL, 0x35bcbccabc897665UL, 0x379b9b569baccd2bUL,
            0x8a8e8e028e048c01UL, 0xd2a3a3b6a371155bUL, 0x6c0c0c300c603c18UL,
            0x847b7bf17bff8af6UL,
            0x803535d435b5e16aUL,
            0xf51d1d741de8693aUL, 0xb3e0e0a7e05347ddUL, 0x21d7d77bd7f6acb3UL,
            0x9cc2c22fc25eed99UL, 0x432e2eb82e6d965cUL, 0x294b4b314b627a96UL,
            0x5dfefedffea321e1UL, 0xd5575741578216aeUL, 0xbd15155415a8412aUL,
            0xe87777c1779fb6eeUL, 0x923737dc37a5eb6eUL, 0x9ee5e5b3e57b56d7UL,
            0x139f9f469f8cd923UL,
            0x23f0f0e7f0d317fdUL,
            0x204a4a354a6a7f94UL, 0x44dada4fda9e95a9UL, 0xa258587d58fa25b0UL,
            0xcfc9c903c906ca8fUL, 0x7c2929a429558d52UL, 0x5a0a0a280a502214UL,
            0x50b1b1feb1e14f7fUL, 0xc9a0a0baa0691a5dUL, 0x146b6bb16b7fdad6UL,
            0xd985852e855cab17UL, 0x3cbdbdcebd817367UL, 0x8f5d5d695dd234baUL,
            0x9010104010805020UL,
            0x07f4f4f7f4f303f5UL,
            0xddcbcb0bcb16c08bUL, 0xd33e3ef83eedc67cUL, 0x2d0505140528110aUL,
            0x78676781671fe6ceUL, 0x97e4e4b7e47353d5UL, 0x0227279c2725bb4eUL,
            0x7341411941325882UL, 0xa78b8b168b2c9d0bUL, 0xf6a7a7a6a7510153UL,
            0xb27d7de97dcf94faUL, 0x4995956e95dcfb37UL, 0x56d8d847d88e9fadUL,
            0x70fbfbcbfb8b30ebUL,
            0xcdeeee9fee2371c1UL,
            0xbb7c7ced7cc791f8UL, 0x716666856617e3ccUL, 0x7bdddd53dda68ea7UL,
            0xaf17175c17b84b2eUL, 0x454747014702468eUL, 0x1a9e9e429e84dc21UL,
            0xd4caca0fca1ec589UL, 0x582d2db42d75995aUL, 0x2ebfbfc6bf917963UL,
            0x3f07071c07381b0eUL, 0xacadad8ead012347UL, 0xb05a5a755aea2fb4UL,
            0xef838336836cb51bUL,
            0xb63333cc3385ff66UL,
            0x5c636391633ff2c6UL, 0x1202020802100a04UL, 0x93aaaa92aa393849UL,
            0xde7171d971afa8e2UL, 0xc6c8c807c80ecf8dUL, 0xd119196419c87d32UL,
            0x3b49493949727092UL, 0x5fd9d943d9869aafUL, 0x31f2f2eff2c31df9UL,
            0xa8e3e3abe34b48dbUL, 0xb95b5b715be22ab6UL, 0xbc88881a8834920dUL,
            0x3e9a9a529aa4c829UL,
            0x0b262698262dbe4cUL,
            0xbf3232c8328dfa64UL, 0x59b0b0fab0e94a7dUL, 0xf2e9e983e91b6acfUL,
            0x770f0f3c0f78331eUL, 0x33d5d573d5e6a6b7UL, 0xf480803a8074ba1dUL,
            0x27bebec2be997c61UL, 0xebcdcd13cd26de87UL, 0x893434d034bde468UL,
            0x3248483d487a7590UL, 0x54ffffdbffab24e3UL, 0x8d7a7af57af78ff4UL,
            0x6490907a90f4ea3dUL,
            0x9d5f5f615fc23ebeUL,
            0x3d202080201da040UL, 0x0f6868bd6867d5d0UL, 0xca1a1a681ad07234UL,
            0xb7aeae82ae192c41UL, 0x7db4b4eab4c95e75UL, 0xce54544d549a19a8UL,
            0x7f93937693ece53bUL, 0x2f222288220daa44UL, 0x6364648d6407e9c8UL,
            0x2af1f1e3f1db12ffUL, 0xcc7373d173bfa2e6UL, 0x8212124812905a24UL,
            0x7a40401d403a5d80UL,
            0x4808082008402810UL,
            0x95c3c32bc356e89bUL, 0xdfecec97ec337bc5UL, 0x4ddbdb4bdb9690abUL,
            0xc0a1a1bea1611f5fUL, 0x918d8d0e8d1c8307UL, 0xc83d3df43df5c97aUL,
            0x5b97976697ccf133UL, 0x0000000000000000UL, 0xf9cfcf1bcf36d483UL,
            0x6e2b2bac2b458756UL, 0xe17676c57697b3ecUL, 0xe68282328264b019UL,
            0x28d6d67fd6fea9b1UL,
            0xc31b1b6c1bd87736UL,
            0x74b5b5eeb5c15b77UL, 0xbeafaf86af112943UL, 0x1d6a6ab56a77dfd4UL,
            0xea50505d50ba0da0UL, 0x5745450945124c8aUL, 0x38f3f3ebf3cb18fbUL,
            0xad3030c0309df060UL, 0xc4efef9bef2b74c3UL, 0xda3f3ffc3fe5c37eUL,
            0xc755554955921caaUL, 0xdba2a2b2a2791059UL, 0xe9eaea8fea0365c9UL,
            0x6a656589650feccaUL,
            0x03babad2bab96869UL,
            0x4a2f2fbc2f65935eUL, 0x8ec0c027c04ee79dUL, 0x60dede5fdebe81a1UL,
            0xfc1c1c701ce06c38UL, 0x46fdfdd3fdbb2ee7UL, 0x1f4d4d294d52649aUL,
            0x7692927292e4e039UL, 0xfa7575c9758fbceaUL, 0x3606061806301e0cUL,
            0xae8a8a128a249809UL, 0x4bb2b2f2b2f94079UL, 0x85e6e6bfe66359d1UL,
            0x7e0e0e380e70361cUL,
            0xe71f1f7c1ff8633eUL,
            0x556262956237f7c4UL, 0x3ad4d477d4eea3b5UL, 0x81a8a89aa829324dUL,
            0x5296966296c4f431UL, 0x62f9f9c3f99b3aefUL, 0xa3c5c533c566f697UL,
            0x102525942535b14aUL, 0xab59597959f220b2UL, 0xd084842a8454ae15UL,
            0xc57272d572b7a7e4UL, 0xec3939e439d5dd72UL, 0x164c4c2d4c5a6198UL,
            0x945e5e655eca3bbcUL,
            0x9f7878fd78e785f0UL,
            0xe53838e038ddd870UL, 0x988c8c0a8c148605UL, 0x17d1d163d1c6b2bfUL,
            0xe4a5a5aea5410b57UL, 0xa1e2e2afe2434dd9UL, 0x4e616199612ff8c2UL,
            0x42b3b3f6b3f1457bUL, 0x342121842115a542UL, 0x089c9c4a9c94d625UL,
            0xee1e1e781ef0663cUL, 0x6143431143225286UL, 0xb1c7c73bc776fc93UL,
            0x4ffcfcd7fcb32be5UL,
            0x2404041004201408UL,
            0xe351515951b208a2UL, 0x2599995e99bcc72fUL, 0x226d6da96d4fc4daUL,
            0x650d0d340d68391aUL, 0x79fafacffa8335e9UL, 0x69dfdf5bdfb684a3UL,
            0xa97e7ee57ed79bfcUL, 0x19242490243db448UL, 0xfe3b3bec3bc5d776UL,
            0x9aabab96ab313d4bUL, 0xf0cece1fce3ed181UL, 0x9911114411885522UL,
            0x838f8f068f0c8903UL,
            0x044e4e254e4a6b9cUL,
            0x66b7b7e6b7d15173UL, 0xe0ebeb8beb0b60cbUL, 0xc13c3cf03cfdcc78UL,
            0xfd81813e817cbf1fUL, 0x4094946a94d4fe35UL, 0x1cf7f7fbf7eb0cf3UL,
            0x18b9b9deb9a1676fUL, 0x8b13134c13985f26UL, 0x512c2cb02c7d9c58UL,
            0x05d3d36bd3d6b8bbUL, 0x8ce7e7bbe76b5cd3UL, 0x396e6ea56e57cbdcUL,
            0xaac4c437c46ef395UL,
            0x1b03030c03180f06UL,
            0xdc565645568a13acUL, 0x5e44440d441a4988UL, 0xa07f7fe17fdf9efeUL,
            0x88a9a99ea921374fUL, 0x672a2aa82a4d8254UL, 0x0abbbbd6bbb16d6bUL,
            0x87c1c123c146e29fUL, 0xf153535153a202a6UL, 0x72dcdc57dcae8ba5UL,
            0x530b0b2c0b582716UL, 0x019d9d4e9d9cd327UL, 0x2b6c6cad6c47c1d8UL,
            0xa43131c43195f562UL,
            0xf37474cd7487b9e8UL,
            0x15f6f6fff6e309f1UL, 0x4c464605460a438cUL, 0xa5acac8aac092645UL,
            0xb589891e893c970fUL, 0xb414145014a04428UL, 0xbae1e1a3e15b42dfUL,
            0xa616165816b04e2cUL, 0xf73a3ae83acdd274UL, 0x066969b9696fd0d2UL,
            0x4109092409482d12UL, 0xd77070dd70a7ade0UL, 0x6fb6b6e2b6d95471UL,
            0x1ed0d067d0ceb7bdUL,
            0xd6eded93ed3b7ec7UL,
            0xe2cccc17cc2edb85UL, 0x68424215422a5784UL, 0x2c98985a98b4c22dUL,
            0xeda4a4aaa4490e55UL, 0x752828a0285d8850UL, 0x865c5c6d5cda31b8UL,
            0x6bf8f8c7f8933fedUL, 0xc28686228644a411UL,
            ], [
                0x30d818186018c078UL, 0x462623238c2305afUL, 0x91b8c6c63fc67ef9UL,
                0xcdfbe8e887e8136fUL, 0x13cb878726874ca1UL, 0x6d11b8b8dab8a962UL,
                0x0209010104010805UL, 0x9e0d4f4f214f426eUL, 0x6c9b3636d836adeeUL,
                0x51ffa6a6a2a65904UL,
                0xb90cd2d26fd2debdUL, 0xf70ef5f5f3f5fb06UL, 0xf2967979f979ef80UL,
                0xde306f6fa16f5fceUL, 0x3f6d91917e91fcefUL, 0xa4f852525552aa07UL,
                0xc04760609d6027fdUL, 0x6535bcbccabc8976UL, 0x2b379b9b569baccdUL,
    0x018a8e8e028e048cUL, 0x5bd2a3a3b6a37115UL, 0x186c0c0c300c603cUL,
    0xf6847b7bf17bff8aUL,
    0x6a803535d435b5e1UL,
    0x3af51d1d741de869UL, 0xddb3e0e0a7e05347UL, 0xb321d7d77bd7f6acUL,
    0x999cc2c22fc25eedUL, 0x5c432e2eb82e6d96UL, 0x96294b4b314b627aUL,
    0xe15dfefedffea321UL, 0xaed5575741578216UL, 0x2abd15155415a841UL,
    0xeee87777c1779fb6UL, 0x6e923737dc37a5ebUL, 0xd79ee5e5b3e57b56UL,
    0x23139f9f469f8cd9UL,
    0xfd23f0f0e7f0d317UL,
    0x94204a4a354a6a7fUL, 0xa944dada4fda9e95UL, 0xb0a258587d58fa25UL,
    0x8fcfc9c903c906caUL, 0x527c2929a429558dUL, 0x145a0a0a280a5022UL,
    0x7f50b1b1feb1e14fUL, 0x5dc9a0a0baa0691aUL, 0xd6146b6bb16b7fdaUL,
    0x17d985852e855cabUL, 0x673cbdbdcebd8173UL, 0xba8f5d5d695dd234UL,
    0x2090101040108050UL,
    0xf507f4f4f7f4f303UL,
    0x8bddcbcb0bcb16c0UL, 0x7cd33e3ef83eedc6UL, 0x0a2d050514052811UL,
    0xce78676781671fe6UL, 0xd597e4e4b7e47353UL, 0x4e0227279c2725bbUL,
    0x8273414119413258UL, 0x0ba78b8b168b2c9dUL, 0x53f6a7a7a6a75101UL,
    0xfab27d7de97dcf94UL, 0x374995956e95dcfbUL, 0xad56d8d847d88e9fUL,
    0xeb70fbfbcbfb8b30UL,
    0xc1cdeeee9fee2371UL,
    0xf8bb7c7ced7cc791UL, 0xcc716666856617e3UL, 0xa77bdddd53dda68eUL,
    0x2eaf17175c17b84bUL, 0x8e45474701470246UL, 0x211a9e9e429e84dcUL,
    0x89d4caca0fca1ec5UL, 0x5a582d2db42d7599UL, 0x632ebfbfc6bf9179UL,
    0x0e3f07071c07381bUL, 0x47acadad8ead0123UL, 0xb4b05a5a755aea2fUL,
    0x1bef838336836cb5UL,
    0x66b63333cc3385ffUL,
    0xc65c636391633ff2UL, 0x041202020802100aUL, 0x4993aaaa92aa3938UL,
    0xe2de7171d971afa8UL, 0x8dc6c8c807c80ecfUL, 0x32d119196419c87dUL,
    0x923b494939497270UL, 0xaf5fd9d943d9869aUL, 0xf931f2f2eff2c31dUL,
    0xdba8e3e3abe34b48UL, 0xb6b95b5b715be22aUL, 0x0dbc88881a883492UL,
    0x293e9a9a529aa4c8UL,
    0x4c0b262698262dbeUL,
    0x64bf3232c8328dfaUL, 0x7d59b0b0fab0e94aUL, 0xcff2e9e983e91b6aUL,
    0x1e770f0f3c0f7833UL, 0xb733d5d573d5e6a6UL, 0x1df480803a8074baUL,
    0x6127bebec2be997cUL, 0x87ebcdcd13cd26deUL, 0x68893434d034bde4UL,
    0x903248483d487a75UL, 0xe354ffffdbffab24UL, 0xf48d7a7af57af78fUL,
    0x3d6490907a90f4eaUL,
    0xbe9d5f5f615fc23eUL,
    0x403d202080201da0UL, 0xd00f6868bd6867d5UL, 0x34ca1a1a681ad072UL,
    0x41b7aeae82ae192cUL, 0x757db4b4eab4c95eUL, 0xa8ce54544d549a19UL,
    0x3b7f93937693ece5UL, 0x442f222288220daaUL, 0xc86364648d6407e9UL,
    0xff2af1f1e3f1db12UL, 0xe6cc7373d173bfa2UL, 0x248212124812905aUL,
    0x807a40401d403a5dUL,
    0x1048080820084028UL,
    0x9b95c3c32bc356e8UL, 0xc5dfecec97ec337bUL, 0xab4ddbdb4bdb9690UL,
    0x5fc0a1a1bea1611fUL, 0x07918d8d0e8d1c83UL, 0x7ac83d3df43df5c9UL,
    0x335b97976697ccf1UL, 0x0000000000000000UL, 0x83f9cfcf1bcf36d4UL,
    0x566e2b2bac2b4587UL, 0xece17676c57697b3UL, 0x19e68282328264b0UL,
    0xb128d6d67fd6fea9UL,
    0x36c31b1b6c1bd877UL,
    0x7774b5b5eeb5c15bUL, 0x43beafaf86af1129UL, 0xd41d6a6ab56a77dfUL,
    0xa0ea50505d50ba0dUL, 0x8a5745450945124cUL, 0xfb38f3f3ebf3cb18UL,
    0x60ad3030c0309df0UL, 0xc3c4efef9bef2b74UL, 0x7eda3f3ffc3fe5c3UL,
    0xaac755554955921cUL, 0x59dba2a2b2a27910UL, 0xc9e9eaea8fea0365UL,
    0xca6a656589650fecUL,
    0x6903babad2bab968UL,
    0x5e4a2f2fbc2f6593UL, 0x9d8ec0c027c04ee7UL, 0xa160dede5fdebe81UL,
    0x38fc1c1c701ce06cUL, 0xe746fdfdd3fdbb2eUL, 0x9a1f4d4d294d5264UL,
    0x397692927292e4e0UL, 0xeafa7575c9758fbcUL, 0x0c3606061806301eUL,
    0x09ae8a8a128a2498UL, 0x794bb2b2f2b2f940UL, 0xd185e6e6bfe66359UL,
    0x1c7e0e0e380e7036UL,
    0x3ee71f1f7c1ff863UL,
    0xc4556262956237f7UL, 0xb53ad4d477d4eea3UL, 0x4d81a8a89aa82932UL,
    0x315296966296c4f4UL, 0xef62f9f9c3f99b3aUL, 0x97a3c5c533c566f6UL,
    0x4a102525942535b1UL, 0xb2ab59597959f220UL, 0x15d084842a8454aeUL,
    0xe4c57272d572b7a7UL, 0x72ec3939e439d5ddUL, 0x98164c4c2d4c5a61UL,
    0xbc945e5e655eca3bUL,
    0xf09f7878fd78e785UL,
    0x70e53838e038ddd8UL, 0x05988c8c0a8c1486UL, 0xbf17d1d163d1c6b2UL,
    0x57e4a5a5aea5410bUL, 0xd9a1e2e2afe2434dUL, 0xc24e616199612ff8UL,
    0x7b42b3b3f6b3f145UL, 0x42342121842115a5UL, 0x25089c9c4a9c94d6UL,
    0x3cee1e1e781ef066UL, 0x8661434311432252UL, 0x93b1c7c73bc776fcUL,
    0xe54ffcfcd7fcb32bUL,
    0x0824040410042014UL,
    0xa2e351515951b208UL, 0x2f2599995e99bcc7UL, 0xda226d6da96d4fc4UL,
    0x1a650d0d340d6839UL, 0xe979fafacffa8335UL, 0xa369dfdf5bdfb684UL,
    0xfca97e7ee57ed79bUL, 0x4819242490243db4UL, 0x76fe3b3bec3bc5d7UL,
    0x4b9aabab96ab313dUL, 0x81f0cece1fce3ed1UL, 0x2299111144118855UL,
    0x03838f8f068f0c89UL,
    0x9c044e4e254e4a6bUL,
    0x7366b7b7e6b7d151UL, 0xcbe0ebeb8beb0b60UL, 0x78c13c3cf03cfdccUL,
    0x1ffd81813e817cbfUL, 0x354094946a94d4feUL, 0xf31cf7f7fbf7eb0cUL,
    0x6f18b9b9deb9a167UL, 0x268b13134c13985fUL, 0x58512c2cb02c7d9cUL,
    0xbb05d3d36bd3d6b8UL, 0xd38ce7e7bbe76b5cUL, 0xdc396e6ea56e57cbUL,
    0x95aac4c437c46ef3UL,
    0x061b03030c03180fUL,
    0xacdc565645568a13UL, 0x885e44440d441a49UL, 0xfea07f7fe17fdf9eUL,
    0x4f88a9a99ea92137UL, 0x54672a2aa82a4d82UL, 0x6b0abbbbd6bbb16dUL,
    0x9f87c1c123c146e2UL, 0xa6f153535153a202UL, 0xa572dcdc57dcae8bUL,
    0x16530b0b2c0b5827UL, 0x27019d9d4e9d9cd3UL, 0xd82b6c6cad6c47c1UL,
    0x62a43131c43195f5UL,
    0xe8f37474cd7487b9UL,
    0xf115f6f6fff6e309UL, 0x8c4c464605460a43UL, 0x45a5acac8aac0926UL,
    0x0fb589891e893c97UL, 0x28b414145014a044UL, 0xdfbae1e1a3e15b42UL,
    0x2ca616165816b04eUL, 0x74f73a3ae83acdd2UL, 0xd2066969b9696fd0UL,
    0x124109092409482dUL, 0xe0d77070dd70a7adUL, 0x716fb6b6e2b6d954UL,
    0xbd1ed0d067d0ceb7UL,
    0xc7d6eded93ed3b7eUL,
    0x85e2cccc17cc2edbUL, 0x8468424215422a57UL, 0x2d2c98985a98b4c2UL,
    0x55eda4a4aaa4490eUL, 0x50752828a0285d88UL, 0xb8865c5c6d5cda31UL,
    0xed6bf8f8c7f8933fUL, 0x11c28686228644a4UL,
], [
    0x7830d818186018c0UL, 0xaf462623238c2305UL, 0xf991b8c6c63fc67eUL,
    0x6fcdfbe8e887e813UL, 0xa113cb878726874cUL, 0x626d11b8b8dab8a9UL,
    0x0502090101040108UL, 0x6e9e0d4f4f214f42UL, 0xee6c9b3636d836adUL,
    0x0451ffa6a6a2a659UL,
    0xbdb90cd2d26fd2deUL, 0x06f70ef5f5f3f5fbUL, 0x80f2967979f979efUL,
    0xcede306f6fa16f5fUL, 0xef3f6d91917e91fcUL, 0x07a4f852525552aaUL,
    0xfdc04760609d6027UL, 0x766535bcbccabc89UL, 0xcd2b379b9b569bacUL,
    0x8c018a8e8e028e04UL, 0x155bd2a3a3b6a371UL, 0x3c186c0c0c300c60UL,
    0x8af6847b7bf17bffUL,
    0xe16a803535d435b5UL,
    0x693af51d1d741de8UL, 0x47ddb3e0e0a7e053UL, 0xacb321d7d77bd7f6UL,
    0xed999cc2c22fc25eUL, 0x965c432e2eb82e6dUL, 0x7a96294b4b314b62UL,
    0x21e15dfefedffea3UL, 0x16aed55757415782UL, 0x412abd15155415a8UL,
    0xb6eee87777c1779fUL, 0xeb6e923737dc37a5UL, 0x56d79ee5e5b3e57bUL,
    0xd923139f9f469f8cUL,
    0x17fd23f0f0e7f0d3UL,
    0x7f94204a4a354a6aUL, 0x95a944dada4fda9eUL, 0x25b0a258587d58faUL,
    0xca8fcfc9c903c906UL, 0x8d527c2929a42955UL, 0x22145a0a0a280a50UL,
    0x4f7f50b1b1feb1e1UL, 0x1a5dc9a0a0baa069UL, 0xdad6146b6bb16b7fUL,
    0xab17d985852e855cUL, 0x73673cbdbdcebd81UL, 0x34ba8f5d5d695dd2UL,
    0x5020901010401080UL,
    0x03f507f4f4f7f4f3UL,
    0xc08bddcbcb0bcb16UL, 0xc67cd33e3ef83eedUL, 0x110a2d0505140528UL,
    0xe6ce78676781671fUL, 0x53d597e4e4b7e473UL, 0xbb4e0227279c2725UL,
    0x5882734141194132UL, 0x9d0ba78b8b168b2cUL, 0x0153f6a7a7a6a751UL,
    0x94fab27d7de97dcfUL, 0xfb374995956e95dcUL, 0x9fad56d8d847d88eUL,
    0x30eb70fbfbcbfb8bUL,
    0x71c1cdeeee9fee23UL,
    0x91f8bb7c7ced7cc7UL, 0xe3cc716666856617UL, 0x8ea77bdddd53dda6UL,
    0x4b2eaf17175c17b8UL, 0x468e454747014702UL, 0xdc211a9e9e429e84UL,
    0xc589d4caca0fca1eUL, 0x995a582d2db42d75UL, 0x79632ebfbfc6bf91UL,
    0x1b0e3f07071c0738UL, 0x2347acadad8ead01UL, 0x2fb4b05a5a755aeaUL,
    0xb51bef838336836cUL,
    0xff66b63333cc3385UL,
    0xf2c65c636391633fUL, 0x0a04120202080210UL, 0x384993aaaa92aa39UL,
    0xa8e2de7171d971afUL, 0xcf8dc6c8c807c80eUL, 0x7d32d119196419c8UL,
    0x70923b4949394972UL, 0x9aaf5fd9d943d986UL, 0x1df931f2f2eff2c3UL,
    0x48dba8e3e3abe34bUL, 0x2ab6b95b5b715be2UL, 0x920dbc88881a8834UL,
    0xc8293e9a9a529aa4UL,
    0xbe4c0b262698262dUL,
    0xfa64bf3232c8328dUL, 0x4a7d59b0b0fab0e9UL, 0x6acff2e9e983e91bUL,
    0x331e770f0f3c0f78UL, 0xa6b733d5d573d5e6UL, 0xba1df480803a8074UL,
    0x7c6127bebec2be99UL, 0xde87ebcdcd13cd26UL, 0xe468893434d034bdUL,
    0x75903248483d487aUL, 0x24e354ffffdbffabUL, 0x8ff48d7a7af57af7UL,
    0xea3d6490907a90f4UL,
    0x3ebe9d5f5f615fc2UL,
    0xa0403d202080201dUL, 0xd5d00f6868bd6867UL, 0x7234ca1a1a681ad0UL,
    0x2c41b7aeae82ae19UL, 0x5e757db4b4eab4c9UL, 0x19a8ce54544d549aUL,
    0xe53b7f93937693ecUL, 0xaa442f222288220dUL, 0xe9c86364648d6407UL,
    0x12ff2af1f1e3f1dbUL, 0xa2e6cc7373d173bfUL, 0x5a24821212481290UL,
    0x5d807a40401d403aUL,
    0x2810480808200840UL,
    0xe89b95c3c32bc356UL, 0x7bc5dfecec97ec33UL, 0x90ab4ddbdb4bdb96UL,
    0x1f5fc0a1a1bea161UL, 0x8307918d8d0e8d1cUL, 0xc97ac83d3df43df5UL,
    0xf1335b97976697ccUL, 0x0000000000000000UL, 0xd483f9cfcf1bcf36UL,
    0x87566e2b2bac2b45UL, 0xb3ece17676c57697UL, 0xb019e68282328264UL,
    0xa9b128d6d67fd6feUL,
    0x7736c31b1b6c1bd8UL,
    0x5b7774b5b5eeb5c1UL, 0x2943beafaf86af11UL, 0xdfd41d6a6ab56a77UL,
    0x0da0ea50505d50baUL, 0x4c8a574545094512UL, 0x18fb38f3f3ebf3cbUL,
    0xf060ad3030c0309dUL, 0x74c3c4efef9bef2bUL, 0xc37eda3f3ffc3fe5UL,
    0x1caac75555495592UL, 0x1059dba2a2b2a279UL, 0x65c9e9eaea8fea03UL,
    0xecca6a656589650fUL,
    0x686903babad2bab9UL,
    0x935e4a2f2fbc2f65UL, 0xe79d8ec0c027c04eUL, 0x81a160dede5fdebeUL,
    0x6c38fc1c1c701ce0UL, 0x2ee746fdfdd3fdbbUL, 0x649a1f4d4d294d52UL,
    0xe0397692927292e4UL, 0xbceafa7575c9758fUL, 0x1e0c360606180630UL,
    0x9809ae8a8a128a24UL, 0x40794bb2b2f2b2f9UL, 0x59d185e6e6bfe663UL,
    0x361c7e0e0e380e70UL,
    0x633ee71f1f7c1ff8UL,
    0xf7c4556262956237UL, 0xa3b53ad4d477d4eeUL, 0x324d81a8a89aa829UL,
    0xf4315296966296c4UL, 0x3aef62f9f9c3f99bUL, 0xf697a3c5c533c566UL,
    0xb14a102525942535UL, 0x20b2ab59597959f2UL, 0xae15d084842a8454UL,
    0xa7e4c57272d572b7UL, 0xdd72ec3939e439d5UL, 0x6198164c4c2d4c5aUL,
    0x3bbc945e5e655ecaUL,
    0x85f09f7878fd78e7UL,
    0xd870e53838e038ddUL, 0x8605988c8c0a8c14UL, 0xb2bf17d1d163d1c6UL,
    0x0b57e4a5a5aea541UL, 0x4dd9a1e2e2afe243UL, 0xf8c24e616199612fUL,
    0x457b42b3b3f6b3f1UL, 0xa542342121842115UL, 0xd625089c9c4a9c94UL,
    0x663cee1e1e781ef0UL, 0x5286614343114322UL, 0xfc93b1c7c73bc776UL,
    0x2be54ffcfcd7fcb3UL,
    0x1408240404100420UL,
    0x08a2e351515951b2UL, 0xc72f2599995e99bcUL, 0xc4da226d6da96d4fUL,
    0x391a650d0d340d68UL, 0x35e979fafacffa83UL, 0x84a369dfdf5bdfb6UL,
    0x9bfca97e7ee57ed7UL, 0xb44819242490243dUL, 0xd776fe3b3bec3bc5UL,
    0x3d4b9aabab96ab31UL, 0xd181f0cece1fce3eUL, 0x5522991111441188UL,
    0x8903838f8f068f0cUL,
    0x6b9c044e4e254e4aUL,
    0x517366b7b7e6b7d1UL, 0x60cbe0ebeb8beb0bUL, 0xcc78c13c3cf03cfdUL,
    0xbf1ffd81813e817cUL, 0xfe354094946a94d4UL, 0x0cf31cf7f7fbf7ebUL,
    0x676f18b9b9deb9a1UL, 0x5f268b13134c1398UL, 0x9c58512c2cb02c7dUL,
    0xb8bb05d3d36bd3d6UL, 0x5cd38ce7e7bbe76bUL, 0xcbdc396e6ea56e57UL,
    0xf395aac4c437c46eUL,
    0x0f061b03030c0318UL,
    0x13acdc565645568aUL, 0x49885e44440d441aUL, 0x9efea07f7fe17fdfUL,
    0x374f88a9a99ea921UL, 0x8254672a2aa82a4dUL, 0x6d6b0abbbbd6bbb1UL,
    0xe29f87c1c123c146UL, 0x02a6f153535153a2UL, 0x8ba572dcdc57dcaeUL,
    0x2716530b0b2c0b58UL, 0xd327019d9d4e9d9cUL, 0xc1d82b6c6cad6c47UL,
    0xf562a43131c43195UL,
    0xb9e8f37474cd7487UL,
    0x09f115f6f6fff6e3UL, 0x438c4c464605460aUL, 0x2645a5acac8aac09UL,
    0x970fb589891e893cUL, 0x4428b414145014a0UL, 0x42dfbae1e1a3e15bUL,
    0x4e2ca616165816b0UL, 0xd274f73a3ae83acdUL, 0xd0d2066969b9696fUL,
    0x2d12410909240948UL, 0xade0d77070dd70a7UL, 0x54716fb6b6e2b6d9UL,
    0xb7bd1ed0d067d0ceUL,
    0x7ec7d6eded93ed3bUL,
    0xdb85e2cccc17cc2eUL, 0x578468424215422aUL, 0xc22d2c98985a98b4UL,
    0x0e55eda4a4aaa449UL, 0x8850752828a0285dUL, 0x31b8865c5c6d5cdaUL,
    0x3fed6bf8f8c7f893UL, 0xa411c28686228644UL,
], [
    0xc07830d818186018UL, 0x05af462623238c23UL, 0x7ef991b8c6c63fc6UL,
    0x136fcdfbe8e887e8UL, 0x4ca113cb87872687UL, 0xa9626d11b8b8dab8UL,
    0x0805020901010401UL, 0x426e9e0d4f4f214fUL, 0xadee6c9b3636d836UL,
    0x590451ffa6a6a2a6UL,
    0xdebdb90cd2d26fd2UL, 0xfb06f70ef5f5f3f5UL, 0xef80f2967979f979UL,
    0x5fcede306f6fa16fUL, 0xfcef3f6d91917e91UL, 0xaa07a4f852525552UL,
    0x27fdc04760609d60UL, 0x89766535bcbccabcUL, 0xaccd2b379b9b569bUL,
    0x048c018a8e8e028eUL, 0x71155bd2a3a3b6a3UL, 0x603c186c0c0c300cUL,
    0xff8af6847b7bf17bUL,
    0xb5e16a803535d435UL,
    0xe8693af51d1d741dUL, 0x5347ddb3e0e0a7e0UL, 0xf6acb321d7d77bd7UL,
    0x5eed999cc2c22fc2UL, 0x6d965c432e2eb82eUL, 0x627a96294b4b314bUL,
    0xa321e15dfefedffeUL, 0x8216aed557574157UL, 0xa8412abd15155415UL,
    0x9fb6eee87777c177UL, 0xa5eb6e923737dc37UL, 0x7b56d79ee5e5b3e5UL,
    0x8cd923139f9f469fUL,
    0xd317fd23f0f0e7f0UL,
    0x6a7f94204a4a354aUL, 0x9e95a944dada4fdaUL, 0xfa25b0a258587d58UL,
    0x06ca8fcfc9c903c9UL, 0x558d527c2929a429UL, 0x5022145a0a0a280aUL,
    0xe14f7f50b1b1feb1UL, 0x691a5dc9a0a0baa0UL, 0x7fdad6146b6bb16bUL,
    0x5cab17d985852e85UL, 0x8173673cbdbdcebdUL, 0xd234ba8f5d5d695dUL,
    0x8050209010104010UL,
    0xf303f507f4f4f7f4UL,
    0x16c08bddcbcb0bcbUL, 0xedc67cd33e3ef83eUL, 0x28110a2d05051405UL,
    0x1fe6ce7867678167UL, 0x7353d597e4e4b7e4UL, 0x25bb4e0227279c27UL,
    0x3258827341411941UL, 0x2c9d0ba78b8b168bUL, 0x510153f6a7a7a6a7UL,
    0xcf94fab27d7de97dUL, 0xdcfb374995956e95UL, 0x8e9fad56d8d847d8UL,
    0x8b30eb70fbfbcbfbUL,
    0x2371c1cdeeee9feeUL,
    0xc791f8bb7c7ced7cUL, 0x17e3cc7166668566UL, 0xa68ea77bdddd53ddUL,
    0xb84b2eaf17175c17UL, 0x02468e4547470147UL, 0x84dc211a9e9e429eUL,
    0x1ec589d4caca0fcaUL, 0x75995a582d2db42dUL, 0x9179632ebfbfc6bfUL,
    0x381b0e3f07071c07UL, 0x012347acadad8eadUL, 0xea2fb4b05a5a755aUL,
    0x6cb51bef83833683UL,
    0x85ff66b63333cc33UL,
    0x3ff2c65c63639163UL, 0x100a041202020802UL, 0x39384993aaaa92aaUL,
    0xafa8e2de7171d971UL, 0x0ecf8dc6c8c807c8UL, 0xc87d32d119196419UL,
    0x7270923b49493949UL, 0x869aaf5fd9d943d9UL, 0xc31df931f2f2eff2UL,
    0x4b48dba8e3e3abe3UL, 0xe22ab6b95b5b715bUL, 0x34920dbc88881a88UL,
    0xa4c8293e9a9a529aUL,
    0x2dbe4c0b26269826UL,
    0x8dfa64bf3232c832UL, 0xe94a7d59b0b0fab0UL, 0x1b6acff2e9e983e9UL,
    0x78331e770f0f3c0fUL, 0xe6a6b733d5d573d5UL, 0x74ba1df480803a80UL,
    0x997c6127bebec2beUL, 0x26de87ebcdcd13cdUL, 0xbde468893434d034UL,
    0x7a75903248483d48UL, 0xab24e354ffffdbffUL, 0xf78ff48d7a7af57aUL,
    0xf4ea3d6490907a90UL,
    0xc23ebe9d5f5f615fUL,
    0x1da0403d20208020UL, 0x67d5d00f6868bd68UL, 0xd07234ca1a1a681aUL,
    0x192c41b7aeae82aeUL, 0xc95e757db4b4eab4UL, 0x9a19a8ce54544d54UL,
    0xece53b7f93937693UL, 0x0daa442f22228822UL, 0x07e9c86364648d64UL,
    0xdb12ff2af1f1e3f1UL, 0xbfa2e6cc7373d173UL, 0x905a248212124812UL,
    0x3a5d807a40401d40UL,
    0x4028104808082008UL,
    0x56e89b95c3c32bc3UL, 0x337bc5dfecec97ecUL, 0x9690ab4ddbdb4bdbUL,
    0x611f5fc0a1a1bea1UL, 0x1c8307918d8d0e8dUL, 0xf5c97ac83d3df43dUL,
    0xccf1335b97976697UL, 0x0000000000000000UL, 0x36d483f9cfcf1bcfUL,
    0x4587566e2b2bac2bUL, 0x97b3ece17676c576UL, 0x64b019e682823282UL,
    0xfea9b128d6d67fd6UL,
    0xd87736c31b1b6c1bUL,
    0xc15b7774b5b5eeb5UL, 0x112943beafaf86afUL, 0x77dfd41d6a6ab56aUL,
    0xba0da0ea50505d50UL, 0x124c8a5745450945UL, 0xcb18fb38f3f3ebf3UL,
    0x9df060ad3030c030UL, 0x2b74c3c4efef9befUL, 0xe5c37eda3f3ffc3fUL,
    0x921caac755554955UL, 0x791059dba2a2b2a2UL, 0x0365c9e9eaea8feaUL,
    0x0fecca6a65658965UL,
    0xb9686903babad2baUL,
    0x65935e4a2f2fbc2fUL, 0x4ee79d8ec0c027c0UL, 0xbe81a160dede5fdeUL,
    0xe06c38fc1c1c701cUL, 0xbb2ee746fdfdd3fdUL, 0x52649a1f4d4d294dUL,
    0xe4e0397692927292UL, 0x8fbceafa7575c975UL, 0x301e0c3606061806UL,
    0x249809ae8a8a128aUL, 0xf940794bb2b2f2b2UL, 0x6359d185e6e6bfe6UL,
    0x70361c7e0e0e380eUL,
    0xf8633ee71f1f7c1fUL,
    0x37f7c45562629562UL, 0xeea3b53ad4d477d4UL, 0x29324d81a8a89aa8UL,
    0xc4f4315296966296UL, 0x9b3aef62f9f9c3f9UL, 0x66f697a3c5c533c5UL,
    0x35b14a1025259425UL, 0xf220b2ab59597959UL, 0x54ae15d084842a84UL,
    0xb7a7e4c57272d572UL, 0xd5dd72ec3939e439UL, 0x5a6198164c4c2d4cUL,
    0xca3bbc945e5e655eUL,
    0xe785f09f7878fd78UL,
    0xddd870e53838e038UL, 0x148605988c8c0a8cUL, 0xc6b2bf17d1d163d1UL,
    0x410b57e4a5a5aea5UL, 0x434dd9a1e2e2afe2UL, 0x2ff8c24e61619961UL,
    0xf1457b42b3b3f6b3UL, 0x15a5423421218421UL, 0x94d625089c9c4a9cUL,
    0xf0663cee1e1e781eUL, 0x2252866143431143UL, 0x76fc93b1c7c73bc7UL,
    0xb32be54ffcfcd7fcUL,
    0x2014082404041004UL,
    0xb208a2e351515951UL, 0xbcc72f2599995e99UL, 0x4fc4da226d6da96dUL,
    0x68391a650d0d340dUL, 0x8335e979fafacffaUL, 0xb684a369dfdf5bdfUL,
    0xd79bfca97e7ee57eUL, 0x3db4481924249024UL, 0xc5d776fe3b3bec3bUL,
    0x313d4b9aabab96abUL, 0x3ed181f0cece1fceUL, 0x8855229911114411UL,
    0x0c8903838f8f068fUL,
    0x4a6b9c044e4e254eUL,
    0xd1517366b7b7e6b7UL, 0x0b60cbe0ebeb8bebUL, 0xfdcc78c13c3cf03cUL,
    0x7cbf1ffd81813e81UL, 0xd4fe354094946a94UL, 0xeb0cf31cf7f7fbf7UL,
    0xa1676f18b9b9deb9UL, 0x985f268b13134c13UL, 0x7d9c58512c2cb02cUL,
    0xd6b8bb05d3d36bd3UL, 0x6b5cd38ce7e7bbe7UL, 0x57cbdc396e6ea56eUL,
    0x6ef395aac4c437c4UL,
    0x180f061b03030c03UL,
    0x8a13acdc56564556UL, 0x1a49885e44440d44UL, 0xdf9efea07f7fe17fUL,
    0x21374f88a9a99ea9UL, 0x4d8254672a2aa82aUL, 0xb16d6b0abbbbd6bbUL,
    0x46e29f87c1c123c1UL, 0xa202a6f153535153UL, 0xae8ba572dcdc57dcUL,
    0x582716530b0b2c0bUL, 0x9cd327019d9d4e9dUL, 0x47c1d82b6c6cad6cUL,
    0x95f562a43131c431UL,
    0x87b9e8f37474cd74UL,
    0xe309f115f6f6fff6UL, 0x0a438c4c46460546UL, 0x092645a5acac8aacUL,
    0x3c970fb589891e89UL, 0xa04428b414145014UL, 0x5b42dfbae1e1a3e1UL,
    0xb04e2ca616165816UL, 0xcdd274f73a3ae83aUL, 0x6fd0d2066969b969UL,
    0x482d124109092409UL, 0xa7ade0d77070dd70UL, 0xd954716fb6b6e2b6UL,
    0xceb7bd1ed0d067d0UL,
    0x3b7ec7d6eded93edUL,
    0x2edb85e2cccc17ccUL, 0x2a57846842421542UL, 0xb4c22d2c98985a98UL,
    0x490e55eda4a4aaa4UL, 0x5d8850752828a028UL, 0xda31b8865c5c6d5cUL,
    0x933fed6bf8f8c7f8UL, 0x44a411c286862286UL,
], [
    0x18c07830d8181860UL, 0x2305af462623238cUL, 0xc67ef991b8c6c63fUL,
    0xe8136fcdfbe8e887UL, 0x874ca113cb878726UL, 0xb8a9626d11b8b8daUL,
    0x0108050209010104UL, 0x4f426e9e0d4f4f21UL, 0x36adee6c9b3636d8UL,
    0xa6590451ffa6a6a2UL,
    0xd2debdb90cd2d26fUL, 0xf5fb06f70ef5f5f3UL, 0x79ef80f2967979f9UL,
    0x6f5fcede306f6fa1UL, 0x91fcef3f6d91917eUL, 0x52aa07a4f8525255UL,
    0x6027fdc04760609dUL, 0xbc89766535bcbccaUL, 0x9baccd2b379b9b56UL,
    0x8e048c018a8e8e02UL, 0xa371155bd2a3a3b6UL, 0x0c603c186c0c0c30UL,
    0x7bff8af6847b7bf1UL,
    0x35b5e16a803535d4UL,
    0x1de8693af51d1d74UL, 0xe05347ddb3e0e0a7UL, 0xd7f6acb321d7d77bUL,
    0xc25eed999cc2c22fUL, 0x2e6d965c432e2eb8UL, 0x4b627a96294b4b31UL,
    0xfea321e15dfefedfUL, 0x578216aed5575741UL, 0x15a8412abd151554UL,
    0x779fb6eee87777c1UL, 0x37a5eb6e923737dcUL, 0xe57b56d79ee5e5b3UL,
    0x9f8cd923139f9f46UL,
    0xf0d317fd23f0f0e7UL,
    0x4a6a7f94204a4a35UL, 0xda9e95a944dada4fUL, 0x58fa25b0a258587dUL,
    0xc906ca8fcfc9c903UL, 0x29558d527c2929a4UL, 0x0a5022145a0a0a28UL,
    0xb1e14f7f50b1b1feUL, 0xa0691a5dc9a0a0baUL, 0x6b7fdad6146b6bb1UL,
    0x855cab17d985852eUL, 0xbd8173673cbdbdceUL, 0x5dd234ba8f5d5d69UL,
    0x1080502090101040UL,
    0xf4f303f507f4f4f7UL,
    0xcb16c08bddcbcb0bUL, 0x3eedc67cd33e3ef8UL, 0x0528110a2d050514UL,
    0x671fe6ce78676781UL, 0xe47353d597e4e4b7UL, 0x2725bb4e0227279cUL,
    0x4132588273414119UL, 0x8b2c9d0ba78b8b16UL, 0xa7510153f6a7a7a6UL,
    0x7dcf94fab27d7de9UL, 0x95dcfb374995956eUL, 0xd88e9fad56d8d847UL,
    0xfb8b30eb70fbfbcbUL,
    0xee2371c1cdeeee9fUL,
    0x7cc791f8bb7c7cedUL, 0x6617e3cc71666685UL, 0xdda68ea77bdddd53UL,
    0x17b84b2eaf17175cUL, 0x4702468e45474701UL, 0x9e84dc211a9e9e42UL,
    0xca1ec589d4caca0fUL, 0x2d75995a582d2db4UL, 0xbf9179632ebfbfc6UL,
    0x07381b0e3f07071cUL, 0xad012347acadad8eUL, 0x5aea2fb4b05a5a75UL,
    0x836cb51bef838336UL,
    0x3385ff66b63333ccUL,
    0x633ff2c65c636391UL, 0x02100a0412020208UL, 0xaa39384993aaaa92UL,
    0x71afa8e2de7171d9UL, 0xc80ecf8dc6c8c807UL, 0x19c87d32d1191964UL,
    0x497270923b494939UL, 0xd9869aaf5fd9d943UL, 0xf2c31df931f2f2efUL,
    0xe34b48dba8e3e3abUL, 0x5be22ab6b95b5b71UL, 0x8834920dbc88881aUL,
    0x9aa4c8293e9a9a52UL,
    0x262dbe4c0b262698UL,
    0x328dfa64bf3232c8UL, 0xb0e94a7d59b0b0faUL, 0xe91b6acff2e9e983UL,
    0x0f78331e770f0f3cUL, 0xd5e6a6b733d5d573UL, 0x8074ba1df480803aUL,
    0xbe997c6127bebec2UL, 0xcd26de87ebcdcd13UL, 0x34bde468893434d0UL,
    0x487a75903248483dUL, 0xffab24e354ffffdbUL, 0x7af78ff48d7a7af5UL,
    0x90f4ea3d6490907aUL,
    0x5fc23ebe9d5f5f61UL,
    0x201da0403d202080UL, 0x6867d5d00f6868bdUL, 0x1ad07234ca1a1a68UL,
    0xae192c41b7aeae82UL, 0xb4c95e757db4b4eaUL, 0x549a19a8ce54544dUL,
    0x93ece53b7f939376UL, 0x220daa442f222288UL, 0x6407e9c86364648dUL,
    0xf1db12ff2af1f1e3UL, 0x73bfa2e6cc7373d1UL, 0x12905a2482121248UL,
    0x403a5d807a40401dUL,
    0x0840281048080820UL,
    0xc356e89b95c3c32bUL, 0xec337bc5dfecec97UL, 0xdb9690ab4ddbdb4bUL,
    0xa1611f5fc0a1a1beUL, 0x8d1c8307918d8d0eUL, 0x3df5c97ac83d3df4UL,
    0x97ccf1335b979766UL, 0x0000000000000000UL, 0xcf36d483f9cfcf1bUL,
    0x2b4587566e2b2bacUL, 0x7697b3ece17676c5UL, 0x8264b019e6828232UL,
    0xd6fea9b128d6d67fUL,
    0x1bd87736c31b1b6cUL,
    0xb5c15b7774b5b5eeUL, 0xaf112943beafaf86UL, 0x6a77dfd41d6a6ab5UL,
    0x50ba0da0ea50505dUL, 0x45124c8a57454509UL, 0xf3cb18fb38f3f3ebUL,
    0x309df060ad3030c0UL, 0xef2b74c3c4efef9bUL, 0x3fe5c37eda3f3ffcUL,
    0x55921caac7555549UL, 0xa2791059dba2a2b2UL, 0xea0365c9e9eaea8fUL,
    0x650fecca6a656589UL,
    0xbab9686903babad2UL,
    0x2f65935e4a2f2fbcUL, 0xc04ee79d8ec0c027UL, 0xdebe81a160dede5fUL,
    0x1ce06c38fc1c1c70UL, 0xfdbb2ee746fdfdd3UL, 0x4d52649a1f4d4d29UL,
    0x92e4e03976929272UL, 0x758fbceafa7575c9UL, 0x06301e0c36060618UL,
    0x8a249809ae8a8a12UL, 0xb2f940794bb2b2f2UL, 0xe66359d185e6e6bfUL,
    0x0e70361c7e0e0e38UL,
    0x1ff8633ee71f1f7cUL,
    0x6237f7c455626295UL, 0xd4eea3b53ad4d477UL, 0xa829324d81a8a89aUL,
    0x96c4f43152969662UL, 0xf99b3aef62f9f9c3UL, 0xc566f697a3c5c533UL,
    0x2535b14a10252594UL, 0x59f220b2ab595979UL, 0x8454ae15d084842aUL,
    0x72b7a7e4c57272d5UL, 0x39d5dd72ec3939e4UL, 0x4c5a6198164c4c2dUL,
    0x5eca3bbc945e5e65UL,
    0x78e785f09f7878fdUL,
    0x38ddd870e53838e0UL, 0x8c148605988c8c0aUL, 0xd1c6b2bf17d1d163UL,
    0xa5410b57e4a5a5aeUL, 0xe2434dd9a1e2e2afUL, 0x612ff8c24e616199UL,
    0xb3f1457b42b3b3f6UL, 0x2115a54234212184UL, 0x9c94d625089c9c4aUL,
    0x1ef0663cee1e1e78UL, 0x4322528661434311UL, 0xc776fc93b1c7c73bUL,
    0xfcb32be54ffcfcd7UL,
    0x0420140824040410UL,
    0x51b208a2e3515159UL, 0x99bcc72f2599995eUL, 0x6d4fc4da226d6da9UL,
    0x0d68391a650d0d34UL, 0xfa8335e979fafacfUL, 0xdfb684a369dfdf5bUL,
    0x7ed79bfca97e7ee5UL, 0x243db44819242490UL, 0x3bc5d776fe3b3becUL,
    0xab313d4b9aabab96UL, 0xce3ed181f0cece1fUL, 0x1188552299111144UL,
    0x8f0c8903838f8f06UL,
    0x4e4a6b9c044e4e25UL,
    0xb7d1517366b7b7e6UL, 0xeb0b60cbe0ebeb8bUL, 0x3cfdcc78c13c3cf0UL,
    0x817cbf1ffd81813eUL, 0x94d4fe354094946aUL, 0xf7eb0cf31cf7f7fbUL,
    0xb9a1676f18b9b9deUL, 0x13985f268b13134cUL, 0x2c7d9c58512c2cb0UL,
    0xd3d6b8bb05d3d36bUL, 0xe76b5cd38ce7e7bbUL, 0x6e57cbdc396e6ea5UL,
    0xc46ef395aac4c437UL,
    0x03180f061b03030cUL,
    0x568a13acdc565645UL, 0x441a49885e44440dUL, 0x7fdf9efea07f7fe1UL,
    0xa921374f88a9a99eUL, 0x2a4d8254672a2aa8UL, 0xbbb16d6b0abbbbd6UL,
    0xc146e29f87c1c123UL, 0x53a202a6f1535351UL, 0xdcae8ba572dcdc57UL,
    0x0b582716530b0b2cUL, 0x9d9cd327019d9d4eUL, 0x6c47c1d82b6c6cadUL,
    0x3195f562a43131c4UL,
    0x7487b9e8f37474cdUL,
    0xf6e309f115f6f6ffUL, 0x460a438c4c464605UL, 0xac092645a5acac8aUL,
    0x893c970fb589891eUL, 0x14a04428b4141450UL, 0xe15b42dfbae1e1a3UL,
    0x16b04e2ca6161658UL, 0x3acdd274f73a3ae8UL, 0x696fd0d2066969b9UL,
    0x09482d1241090924UL, 0x70a7ade0d77070ddUL, 0xb6d954716fb6b6e2UL,
    0xd0ceb7bd1ed0d067UL,
    0xed3b7ec7d6eded93UL,
    0xcc2edb85e2cccc17UL, 0x422a578468424215UL, 0x98b4c22d2c98985aUL,
    0xa4490e55eda4a4aaUL, 0x285d8850752828a0UL, 0x5cda31b8865c5c6dUL,
    0xf8933fed6bf8f8c7UL, 0x8644a411c2868622UL,
], [
    0x6018c07830d81818UL, 0x8c2305af46262323UL, 0x3fc67ef991b8c6c6UL,
    0x87e8136fcdfbe8e8UL, 0x26874ca113cb8787UL, 0xdab8a9626d11b8b8UL,
    0x0401080502090101UL, 0x214f426e9e0d4f4fUL, 0xd836adee6c9b3636UL,
    0xa2a6590451ffa6a6UL,
    0x6fd2debdb90cd2d2UL, 0xf3f5fb06f70ef5f5UL, 0xf979ef80f2967979UL,
    0xa16f5fcede306f6fUL, 0x7e91fcef3f6d9191UL, 0x5552aa07a4f85252UL,
    0x9d6027fdc0476060UL, 0xcabc89766535bcbcUL, 0x569baccd2b379b9bUL,
    0x028e048c018a8e8eUL, 0xb6a371155bd2a3a3UL, 0x300c603c186c0c0cUL,
    0xf17bff8af6847b7bUL,
    0xd435b5e16a803535UL,
    0x741de8693af51d1dUL, 0xa7e05347ddb3e0e0UL, 0x7bd7f6acb321d7d7UL,
    0x2fc25eed999cc2c2UL, 0xb82e6d965c432e2eUL, 0x314b627a96294b4bUL,
    0xdffea321e15dfefeUL, 0x41578216aed55757UL, 0x5415a8412abd1515UL,
    0xc1779fb6eee87777UL, 0xdc37a5eb6e923737UL, 0xb3e57b56d79ee5e5UL,
    0x469f8cd923139f9fUL,
    0xe7f0d317fd23f0f0UL,
    0x354a6a7f94204a4aUL, 0x4fda9e95a944dadaUL, 0x7d58fa25b0a25858UL,
    0x03c906ca8fcfc9c9UL, 0xa429558d527c2929UL, 0x280a5022145a0a0aUL,
    0xfeb1e14f7f50b1b1UL, 0xbaa0691a5dc9a0a0UL, 0xb16b7fdad6146b6bUL,
    0x2e855cab17d98585UL, 0xcebd8173673cbdbdUL, 0x695dd234ba8f5d5dUL,
    0x4010805020901010UL,
    0xf7f4f303f507f4f4UL,
    0x0bcb16c08bddcbcbUL, 0xf83eedc67cd33e3eUL, 0x140528110a2d0505UL,
    0x81671fe6ce786767UL, 0xb7e47353d597e4e4UL, 0x9c2725bb4e022727UL,
    0x1941325882734141UL, 0x168b2c9d0ba78b8bUL, 0xa6a7510153f6a7a7UL,
    0xe97dcf94fab27d7dUL, 0x6e95dcfb37499595UL, 0x47d88e9fad56d8d8UL,
    0xcbfb8b30eb70fbfbUL,
    0x9fee2371c1cdeeeeUL,
    0xed7cc791f8bb7c7cUL, 0x856617e3cc716666UL, 0x53dda68ea77bddddUL,
    0x5c17b84b2eaf1717UL, 0x014702468e454747UL, 0x429e84dc211a9e9eUL,
    0x0fca1ec589d4cacaUL, 0xb42d75995a582d2dUL, 0xc6bf9179632ebfbfUL,
    0x1c07381b0e3f0707UL, 0x8ead012347acadadUL, 0x755aea2fb4b05a5aUL,
    0x36836cb51bef8383UL,
    0xcc3385ff66b63333UL,
    0x91633ff2c65c6363UL, 0x0802100a04120202UL, 0x92aa39384993aaaaUL,
    0xd971afa8e2de7171UL, 0x07c80ecf8dc6c8c8UL, 0x6419c87d32d11919UL,
    0x39497270923b4949UL, 0x43d9869aaf5fd9d9UL, 0xeff2c31df931f2f2UL,
    0xabe34b48dba8e3e3UL, 0x715be22ab6b95b5bUL, 0x1a8834920dbc8888UL,
    0x529aa4c8293e9a9aUL,
    0x98262dbe4c0b2626UL,
    0xc8328dfa64bf3232UL, 0xfab0e94a7d59b0b0UL, 0x83e91b6acff2e9e9UL,
    0x3c0f78331e770f0fUL, 0x73d5e6a6b733d5d5UL, 0x3a8074ba1df48080UL,
    0xc2be997c6127bebeUL, 0x13cd26de87ebcdcdUL, 0xd034bde468893434UL,
    0x3d487a7590324848UL, 0xdbffab24e354ffffUL, 0xf57af78ff48d7a7aUL,
    0x7a90f4ea3d649090UL,
    0x615fc23ebe9d5f5fUL,
    0x80201da0403d2020UL, 0xbd6867d5d00f6868UL, 0x681ad07234ca1a1aUL,
    0x82ae192c41b7aeaeUL, 0xeab4c95e757db4b4UL, 0x4d549a19a8ce5454UL,
    0x7693ece53b7f9393UL, 0x88220daa442f2222UL, 0x8d6407e9c8636464UL,
    0xe3f1db12ff2af1f1UL, 0xd173bfa2e6cc7373UL, 0x4812905a24821212UL,
    0x1d403a5d807a4040UL,
    0x2008402810480808UL,
    0x2bc356e89b95c3c3UL, 0x97ec337bc5dfececUL, 0x4bdb9690ab4ddbdbUL,
    0xbea1611f5fc0a1a1UL, 0x0e8d1c8307918d8dUL, 0xf43df5c97ac83d3dUL,
    0x6697ccf1335b9797UL, 0x0000000000000000UL, 0x1bcf36d483f9cfcfUL,
    0xac2b4587566e2b2bUL, 0xc57697b3ece17676UL, 0x328264b019e68282UL,
    0x7fd6fea9b128d6d6UL,
    0x6c1bd87736c31b1bUL,
    0xeeb5c15b7774b5b5UL, 0x86af112943beafafUL, 0xb56a77dfd41d6a6aUL,
    0x5d50ba0da0ea5050UL, 0x0945124c8a574545UL, 0xebf3cb18fb38f3f3UL,
    0xc0309df060ad3030UL, 0x9bef2b74c3c4efefUL, 0xfc3fe5c37eda3f3fUL,
    0x4955921caac75555UL, 0xb2a2791059dba2a2UL, 0x8fea0365c9e9eaeaUL,
    0x89650fecca6a6565UL,
    0xd2bab9686903babaUL,
    0xbc2f65935e4a2f2fUL, 0x27c04ee79d8ec0c0UL, 0x5fdebe81a160dedeUL,
    0x701ce06c38fc1c1cUL, 0xd3fdbb2ee746fdfdUL, 0x294d52649a1f4d4dUL,
    0x7292e4e039769292UL, 0xc9758fbceafa7575UL, 0x1806301e0c360606UL,
    0x128a249809ae8a8aUL, 0xf2b2f940794bb2b2UL, 0xbfe66359d185e6e6UL,
    0x380e70361c7e0e0eUL,
    0x7c1ff8633ee71f1fUL,
    0x956237f7c4556262UL, 0x77d4eea3b53ad4d4UL, 0x9aa829324d81a8a8UL,
    0x6296c4f431529696UL, 0xc3f99b3aef62f9f9UL, 0x33c566f697a3c5c5UL,
    0x942535b14a102525UL, 0x7959f220b2ab5959UL, 0x2a8454ae15d08484UL,
    0xd572b7a7e4c57272UL, 0xe439d5dd72ec3939UL, 0x2d4c5a6198164c4cUL,
    0x655eca3bbc945e5eUL,
    0xfd78e785f09f7878UL,
    0xe038ddd870e53838UL, 0x0a8c148605988c8cUL, 0x63d1c6b2bf17d1d1UL,
    0xaea5410b57e4a5a5UL, 0xafe2434dd9a1e2e2UL, 0x99612ff8c24e6161UL,
    0xf6b3f1457b42b3b3UL, 0x842115a542342121UL, 0x4a9c94d625089c9cUL,
    0x781ef0663cee1e1eUL, 0x1143225286614343UL, 0x3bc776fc93b1c7c7UL,
    0xd7fcb32be54ffcfcUL,
    0x1004201408240404UL,
    0x5951b208a2e35151UL, 0x5e99bcc72f259999UL, 0xa96d4fc4da226d6dUL,
    0x340d68391a650d0dUL, 0xcffa8335e979fafaUL, 0x5bdfb684a369dfdfUL,
    0xe57ed79bfca97e7eUL, 0x90243db448192424UL, 0xec3bc5d776fe3b3bUL,
    0x96ab313d4b9aababUL, 0x1fce3ed181f0ceceUL, 0x4411885522991111UL,
    0x068f0c8903838f8fUL,
    0x254e4a6b9c044e4eUL,
    0xe6b7d1517366b7b7UL, 0x8beb0b60cbe0ebebUL, 0xf03cfdcc78c13c3cUL,
    0x3e817cbf1ffd8181UL, 0x6a94d4fe35409494UL, 0xfbf7eb0cf31cf7f7UL,
    0xdeb9a1676f18b9b9UL, 0x4c13985f268b1313UL, 0xb02c7d9c58512c2cUL,
    0x6bd3d6b8bb05d3d3UL, 0xbbe76b5cd38ce7e7UL, 0xa56e57cbdc396e6eUL,
    0x37c46ef395aac4c4UL,
    0x0c03180f061b0303UL,
    0x45568a13acdc5656UL, 0x0d441a49885e4444UL, 0xe17fdf9efea07f7fUL,
    0x9ea921374f88a9a9UL, 0xa82a4d8254672a2aUL, 0xd6bbb16d6b0abbbbUL,
    0x23c146e29f87c1c1UL, 0x5153a202a6f15353UL, 0x57dcae8ba572dcdcUL,
    0x2c0b582716530b0bUL, 0x4e9d9cd327019d9dUL, 0xad6c47c1d82b6c6cUL,
    0xc43195f562a43131UL,
    0xcd7487b9e8f37474UL,
    0xfff6e309f115f6f6UL, 0x05460a438c4c4646UL, 0x8aac092645a5acacUL,
    0x1e893c970fb58989UL, 0x5014a04428b41414UL, 0xa3e15b42dfbae1e1UL,
    0x5816b04e2ca61616UL, 0xe83acdd274f73a3aUL, 0xb9696fd0d2066969UL,
    0x2409482d12410909UL, 0xdd70a7ade0d77070UL, 0xe2b6d954716fb6b6UL,
    0x67d0ceb7bd1ed0d0UL,
    0x93ed3b7ec7d6ededUL,
    0x17cc2edb85e2ccccUL, 0x15422a5784684242UL, 0x5a98b4c22d2c9898UL,
    0xaaa4490e55eda4a4UL, 0xa0285d8850752828UL, 0x6d5cda31b8865c5cUL,
    0xc7f8933fed6bf8f8UL, 0x228644a411c28686UL,
], [
    0x186018c07830d818UL, 0x238c2305af462623UL, 0xc63fc67ef991b8c6UL,
    0xe887e8136fcdfbe8UL, 0x8726874ca113cb87UL, 0xb8dab8a9626d11b8UL,
    0x0104010805020901UL, 0x4f214f426e9e0d4fUL, 0x36d836adee6c9b36UL,
    0xa6a2a6590451ffa6UL,
    0xd26fd2debdb90cd2UL, 0xf5f3f5fb06f70ef5UL, 0x79f979ef80f29679UL,
    0x6fa16f5fcede306fUL, 0x917e91fcef3f6d91UL, 0x525552aa07a4f852UL,
    0x609d6027fdc04760UL, 0xbccabc89766535bcUL, 0x9b569baccd2b379bUL,
    0x8e028e048c018a8eUL, 0xa3b6a371155bd2a3UL, 0x0c300c603c186c0cUL,
    0x7bf17bff8af6847bUL,
    0x35d435b5e16a8035UL,
    0x1d741de8693af51dUL, 0xe0a7e05347ddb3e0UL, 0xd77bd7f6acb321d7UL,
    0xc22fc25eed999cc2UL, 0x2eb82e6d965c432eUL, 0x4b314b627a96294bUL,
    0xfedffea321e15dfeUL, 0x5741578216aed557UL, 0x155415a8412abd15UL,
    0x77c1779fb6eee877UL, 0x37dc37a5eb6e9237UL, 0xe5b3e57b56d79ee5UL,
    0x9f469f8cd923139fUL,
    0xf0e7f0d317fd23f0UL,
    0x4a354a6a7f94204aUL, 0xda4fda9e95a944daUL, 0x587d58fa25b0a258UL,
    0xc903c906ca8fcfc9UL, 0x29a429558d527c29UL, 0x0a280a5022145a0aUL,
    0xb1feb1e14f7f50b1UL, 0xa0baa0691a5dc9a0UL, 0x6bb16b7fdad6146bUL,
    0x852e855cab17d985UL, 0xbdcebd8173673cbdUL, 0x5d695dd234ba8f5dUL,
    0x1040108050209010UL,
    0xf4f7f4f303f507f4UL,
    0xcb0bcb16c08bddcbUL, 0x3ef83eedc67cd33eUL, 0x05140528110a2d05UL,
    0x6781671fe6ce7867UL, 0xe4b7e47353d597e4UL, 0x279c2725bb4e0227UL,
    0x4119413258827341UL, 0x8b168b2c9d0ba78bUL, 0xa7a6a7510153f6a7UL,
    0x7de97dcf94fab27dUL, 0x956e95dcfb374995UL, 0xd847d88e9fad56d8UL,
    0xfbcbfb8b30eb70fbUL,
    0xee9fee2371c1cdeeUL,
    0x7ced7cc791f8bb7cUL, 0x66856617e3cc7166UL, 0xdd53dda68ea77bddUL,
    0x175c17b84b2eaf17UL, 0x47014702468e4547UL, 0x9e429e84dc211a9eUL,
    0xca0fca1ec589d4caUL, 0x2db42d75995a582dUL, 0xbfc6bf9179632ebfUL,
    0x071c07381b0e3f07UL, 0xad8ead012347acadUL, 0x5a755aea2fb4b05aUL,
    0x8336836cb51bef83UL,
    0x33cc3385ff66b633UL,
    0x6391633ff2c65c63UL, 0x020802100a041202UL, 0xaa92aa39384993aaUL,
    0x71d971afa8e2de71UL, 0xc807c80ecf8dc6c8UL, 0x196419c87d32d119UL,
    0x4939497270923b49UL, 0xd943d9869aaf5fd9UL, 0xf2eff2c31df931f2UL,
    0xe3abe34b48dba8e3UL, 0x5b715be22ab6b95bUL, 0x881a8834920dbc88UL,
    0x9a529aa4c8293e9aUL,
    0x2698262dbe4c0b26UL,
    0x32c8328dfa64bf32UL, 0xb0fab0e94a7d59b0UL, 0xe983e91b6acff2e9UL,
    0x0f3c0f78331e770fUL, 0xd573d5e6a6b733d5UL, 0x803a8074ba1df480UL,
    0xbec2be997c6127beUL, 0xcd13cd26de87ebcdUL, 0x34d034bde4688934UL,
    0x483d487a75903248UL, 0xffdbffab24e354ffUL, 0x7af57af78ff48d7aUL,
    0x907a90f4ea3d6490UL,
    0x5f615fc23ebe9d5fUL,
    0x2080201da0403d20UL, 0x68bd6867d5d00f68UL, 0x1a681ad07234ca1aUL,
    0xae82ae192c41b7aeUL, 0xb4eab4c95e757db4UL, 0x544d549a19a8ce54UL,
    0x937693ece53b7f93UL, 0x2288220daa442f22UL, 0x648d6407e9c86364UL,
    0xf1e3f1db12ff2af1UL, 0x73d173bfa2e6cc73UL, 0x124812905a248212UL,
    0x401d403a5d807a40UL,
    0x0820084028104808UL,
    0xc32bc356e89b95c3UL, 0xec97ec337bc5dfecUL, 0xdb4bdb9690ab4ddbUL,
    0xa1bea1611f5fc0a1UL, 0x8d0e8d1c8307918dUL, 0x3df43df5c97ac83dUL,
    0x976697ccf1335b97UL, 0x0000000000000000UL, 0xcf1bcf36d483f9cfUL,
    0x2bac2b4587566e2bUL, 0x76c57697b3ece176UL, 0x82328264b019e682UL,
    0xd67fd6fea9b128d6UL,
    0x1b6c1bd87736c31bUL,
    0xb5eeb5c15b7774b5UL, 0xaf86af112943beafUL, 0x6ab56a77dfd41d6aUL,
    0x505d50ba0da0ea50UL, 0x450945124c8a5745UL, 0xf3ebf3cb18fb38f3UL,
    0x30c0309df060ad30UL, 0xef9bef2b74c3c4efUL, 0x3ffc3fe5c37eda3fUL,
    0x554955921caac755UL, 0xa2b2a2791059dba2UL, 0xea8fea0365c9e9eaUL,
    0x6589650fecca6a65UL,
    0xbad2bab9686903baUL,
    0x2fbc2f65935e4a2fUL, 0xc027c04ee79d8ec0UL, 0xde5fdebe81a160deUL,
    0x1c701ce06c38fc1cUL, 0xfdd3fdbb2ee746fdUL, 0x4d294d52649a1f4dUL,
    0x927292e4e0397692UL, 0x75c9758fbceafa75UL, 0x061806301e0c3606UL,
    0x8a128a249809ae8aUL, 0xb2f2b2f940794bb2UL, 0xe6bfe66359d185e6UL,
    0x0e380e70361c7e0eUL,
    0x1f7c1ff8633ee71fUL,
    0x62956237f7c45562UL, 0xd477d4eea3b53ad4UL, 0xa89aa829324d81a8UL,
    0x966296c4f4315296UL, 0xf9c3f99b3aef62f9UL, 0xc533c566f697a3c5UL,
    0x25942535b14a1025UL, 0x597959f220b2ab59UL, 0x842a8454ae15d084UL,
    0x72d572b7a7e4c572UL, 0x39e439d5dd72ec39UL, 0x4c2d4c5a6198164cUL,
    0x5e655eca3bbc945eUL,
    0x78fd78e785f09f78UL,
    0x38e038ddd870e538UL, 0x8c0a8c148605988cUL, 0xd163d1c6b2bf17d1UL,
    0xa5aea5410b57e4a5UL, 0xe2afe2434dd9a1e2UL, 0x6199612ff8c24e61UL,
    0xb3f6b3f1457b42b3UL, 0x21842115a5423421UL, 0x9c4a9c94d625089cUL,
    0x1e781ef0663cee1eUL, 0x4311432252866143UL, 0xc73bc776fc93b1c7UL,
    0xfcd7fcb32be54ffcUL,
    0x0410042014082404UL,
    0x515951b208a2e351UL, 0x995e99bcc72f2599UL, 0x6da96d4fc4da226dUL,
    0x0d340d68391a650dUL, 0xfacffa8335e979faUL, 0xdf5bdfb684a369dfUL,
    0x7ee57ed79bfca97eUL, 0x2490243db4481924UL, 0x3bec3bc5d776fe3bUL,
    0xab96ab313d4b9aabUL, 0xce1fce3ed181f0ceUL, 0x1144118855229911UL,
    0x8f068f0c8903838fUL,
    0x4e254e4a6b9c044eUL,
    0xb7e6b7d1517366b7UL, 0xeb8beb0b60cbe0ebUL, 0x3cf03cfdcc78c13cUL,
    0x813e817cbf1ffd81UL, 0x946a94d4fe354094UL, 0xf7fbf7eb0cf31cf7UL,
    0xb9deb9a1676f18b9UL, 0x134c13985f268b13UL, 0x2cb02c7d9c58512cUL,
    0xd36bd3d6b8bb05d3UL, 0xe7bbe76b5cd38ce7UL, 0x6ea56e57cbdc396eUL,
    0xc437c46ef395aac4UL,
    0x030c03180f061b03UL,
    0x5645568a13acdc56UL, 0x440d441a49885e44UL, 0x7fe17fdf9efea07fUL,
    0xa99ea921374f88a9UL, 0x2aa82a4d8254672aUL, 0xbbd6bbb16d6b0abbUL,
    0xc123c146e29f87c1UL, 0x535153a202a6f153UL, 0xdc57dcae8ba572dcUL,
    0x0b2c0b582716530bUL, 0x9d4e9d9cd327019dUL, 0x6cad6c47c1d82b6cUL,
    0x31c43195f562a431UL,
    0x74cd7487b9e8f374UL,
    0xf6fff6e309f115f6UL, 0x4605460a438c4c46UL, 0xac8aac092645a5acUL,
    0x891e893c970fb589UL, 0x145014a04428b414UL, 0xe1a3e15b42dfbae1UL,
    0x165816b04e2ca616UL, 0x3ae83acdd274f73aUL, 0x69b9696fd0d20669UL,
    0x092409482d124109UL, 0x70dd70a7ade0d770UL, 0xb6e2b6d954716fb6UL,
    0xd067d0ceb7bd1ed0UL,
    0xed93ed3b7ec7d6edUL,
    0xcc17cc2edb85e2ccUL, 0x4215422a57846842UL, 0x985a98b4c22d2c98UL,
    0xa4aaa4490e55eda4UL, 0x28a0285d88507528UL, 0x5c6d5cda31b8865cUL,
    0xf8c7f8933fed6bf8UL, 0x86228644a411c286UL,
]];

immutable ulong[numRounds] rc = [
    0x1823c6e887b8014fUL, 0x36a6d2f5796f9152UL, 0x60bc9b8ea30c7b35UL,
    0x1de0d7c22e4bfe57UL, 0x157737e59ff04adaUL, 0x58c9290ab1a06b85UL,
    0xbd5d10f4cb3e0567UL, 0xe427418ba77d95d8UL, 0xfbee7c66dd17479eUL, 0xca2dbf07ad5a8333UL,
];

// precomputed K[] based on the initial state
immutable ulong[8][numRounds] pcK = [
    [
    0x300BEEC0AF902967, 0x2828282828282828, 0x2828282828282828, 0x2828282828282828,
    0x2828282828282828, 0x2828282828282828, 0x2828282828282828, 0x2828282828282828
], [
    0x3BAB89F8EAD1AE24, 0x4445456645E9CBAF, 0x70FEA4A4C5A4B289, 0xC5FAA9E1E1CCE1A0,
    0x48ACC05CFCFCB8FC, 0x8FF70E26908F8F69, 0x96791407D7857979, 0xF8A8F868B8C878F8
], [
    0xD319BFDB30467058, 0x295B23D1AFCF37DB, 0x12C8AC28B95AC98, 0x81639EB1C0B206A7,
    0x445E607AB0B209DB, 0x735B2CCFBC8CBC71, 0xDC670924EFEDDDD3, 0x7B8D3BF0D73B7D19
], [
    0x38BEAAC1DE116586, 0x687CF3D04A87337F, 0xF337FADB98ADF057, 0xC5E24258EE358DBC,
    0x1109F0E8996E247E, 0x1C5D6ED10B03401, 0xFBC952F17B28ECD3, 0x3256DC0CC7F12740
], [
    0xAF25A520949BCF14, 0xC13626A9E3C4534D, 0xE60F7D867740F9E1, 0x915DE6BBE26A0629,
    0x965A54CC4CFE5E8D, 0xBEE931CB62323AA6, 0xB17B591896846A47, 0xD4F0C9362759AF31
], [
    0xE2F9B5C025370BB0, 0x392BCBA2168494A5, 0x608AF8CEFA348C14, 0x7AA53764418C9219,
    0xB3F346A1FA833F89, 0x97493F487802CF7C, 0xDCADE8BA1E008F23, 0x92774F49EDB0323D
], [
    0x75416382774DFF2F, 0xFFFA38D055034600, 0xBF7D02493E98F361, 0xF4A860C29AE5CE0B,
    0xC8DF5A44EE5D9D27, 0x23F45A55047500A4, 0xB016101202F9E28C, 0xAC30CD296833331D
], [
    0x36BF1826884AD89, 0x9940C662D8467163, 0x4C433E174B19C210, 0xE29CCFD34CFF86C5,
    0x21FF11A042DF2653, 0x1B8E00CB6CE44B13, 0xA6123BF7A347B7CE, 0xD918900E3B2833CA
], [
    0xD01C677A0A9A2CF9, 0x2A942F534A63B6B2, 0x88422246FEACA8B4, 0x474A5CC73D583559,
    0x74A6925DA55C6FA1, 0x7717E68CC4735C39, 0x82A3B0B53EC1AC6, 0x2AF658EB814DE762
], [
    0x489548B601EEBC3A, 0xA50D6BC66BED8E81, 0xE0CE3DCF88265A75, 0xC28C4ADBC0F69CE9,
    0x54B79CD57F718513, 0x43414B8A977D0B7B, 0x631935BBDBF6157A, 0x6A7A4EF637018227
],];

version(unittest)
{
    import std.conv : hexString;
}
