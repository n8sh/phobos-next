/**
   Tiger algorithm implementation. This module conforms to the APIs defined in std.digest.
*/
module nxt.digestx.tiger;

public import std.digest;

/**
   Template API Tiger implementation.

   Parameters:
   digestSize = the size of digest in byte

   passes = the number of passes of the digest algorithm

   tiger2 = tiger2 mode (uses 0x80 as padding value)
*/
struct TigerImpl(uint digestSize, uint passes, bool tiger2 = false)
{
    static assert(digestSize <= 24, "digestSize must be smaller than 24(Tiger-192)");

    /// Initializes the digest calculation.
    void start() @safe pure nothrow @nogc
    {
        _length = 0;
        _res = [0x0123456789ABCDEFUL, 0xFEDCBA9876543210UL, 0xF096A5B4C3B2E187UL];
        _bufPos = 0;
    }

    /// Feeds the digest with data.
    void put(scope const(ubyte)[] data...) @safe pure nothrow @nogc
    {
        assert(data.length <= ulong.max - _length, "length overflow");
        _length += data.length;

        while (true)
        {
            immutable cap = _buf.length - _bufPos;

            if (data.length >= cap)
            {
                _buf[_bufPos .. $] = data[0 .. cap];
                tigerCompress(_bufUL);
                _bufPos = 0;
                data = data[cap .. $];
            }
            else
            {
                _buf[_bufPos .. _bufPos + data.length] = data[0 .. $];
                _bufPos += data.length;
                data = null;
                break;
            }
        }
    }

    /// Returns the Tiger hash. This also calls start to reset the internal state.
    ubyte[digestSize] finish() @safe pure nothrow @nogc
    {
        // TODO: code duplication
        version (BigEndian)
        {
            union U
            {
                ubyte[64] temp;
                ulong[8] tempUL;
            }

            U u = void;

            foreach (immutable i, ref const b; _buf[0 .. _bufPos])
                u.temp[i ^ 7] = b;

            u.temp[_bufPos ^ 7] = tiger2 ? 0x80 : 0x01;
            _bufPos++;

            for (; _bufPos & 7; _bufPos++)
                u.temp[_bufPos ^ 7] = 0;

            if (_bufPos > 56)
            {
                assert(_bufPos == 64);
                //u.temp[_bufPos .. $] = 0;
                tigerCompress(u.tempUL);
                _bufPos = 0;
                u.tempUL[0 .. 7] = 0;
            }
            else
            {
                u.temp[_bufPos .. 56] = 0;
            }

            u.tempUL[7] = _length << 3;
            tigerCompress(u.tempUL);
        }
        else
        {
            _buf[_bufPos] = tiger2 ? 0x80 : 0x01;
            _bufPos++;
            for (; _bufPos & 7; _bufPos++)
                _buf[_bufPos] = 0;

            if (_bufPos > 56)
            {
                assert(_bufPos == 64);
                //_buf[_bufPos .. $] = 0;
                tigerCompress(_bufUL);
                _bufPos = 0;

                _bufUL[0 .. 7] = 0;
            }
            else
            {
                _buf[_bufPos .. 56] = 0;
            }

            _bufUL[7] = _length << 3;
            tigerCompress(_bufUL);
        }

        static auto ref trustedCast(To, From)(auto ref From from) @trusted
        {
            return cast(To) from;
        }

        immutable ubyte[digestSize] result = trustedCast!(ubyte[])(_res)[0 .. digestSize];

        start();

        return result;
    }

private:

    union
    {
        ubyte[64] _buf = void;
        ulong[8] _bufUL = void;
    }

    int _bufPos = void;

    void tigerCompress(ref ulong[8] str) @safe pure nothrow @nogc
    {
        ulong a = _res[0], b = _res[1], c = _res[2];

        ulong x0 = str[0], x1 = str[1], x2 = str[2], x3 = str[3], x4 = str[4],
        x5 = str[5], x6 = str[6], x7 = str[7];

        immutable aa = a, bb = b, cc = c;

        foreach (immutable pass; 0 .. passes)
        {
            if (pass != 0)
            {
                // key_schedule
                x0 -= x7 ^ 0xA5A5A5A5A5A5A5A5UL;
                x1 ^= x0;
                x2 += x1;
                x3 -= x2 ^ ((~x1) << 19);
                x4 ^= x3;
                x5 += x4;
                x6 -= x5 ^ ((~x4) >> 23);
                x7 ^= x6;
                x0 += x7;
                x1 -= x0 ^ ((~x7) << 19);
                x2 ^= x1;
                x3 += x2;
                x4 -= x3 ^ ((~x2) >> 23);
                x5 ^= x4;
                x6 += x5;
                x7 -= x6 ^ 0x0123456789ABCDEFUL;
            }

            // pass
            immutable mul = pass == 0 ? 5 : (pass == 1 ? 7 : 9);
            round!(a, b, c)(x0, mul);
            round!(b, c, a)(x1, mul);
            round!(c, a, b)(x2, mul);
            round!(a, b, c)(x3, mul);
            round!(b, c, a)(x4, mul);
            round!(c, a, b)(x5, mul);
            round!(a, b, c)(x6, mul);
            round!(b, c, a)(x7, mul);

            immutable tmpa = a;
            a = c;
            c = b;
            b = tmpa;
        }

        // feedforward
        a ^= aa;
        b -= bb;
        c += cc;

        _res[0] = a;
        _res[1] = b;
        _res[2] = c;
    }

    ulong _length = void;

    ulong[3] _res = void;
}

/// Alias for Tiger-192(3 pass), result is ubyte[24]
alias Tiger = TigerImpl!(24, 3);
/// Alias for Tiger-160(3 pass), result is ubyte[20] (first 20 bytes of Tiger-192)
alias Tiger160 = TigerImpl!(20, 3);
/// Alias for Tiger-128(3 pass), result is ubyte[16] (first 16 bytes of Tiger-192)
alias Tiger128 = TigerImpl!(16, 3);

/// Alias for Tiger2-192(3 pass), result is ubyte[24]
alias Tiger2 = TigerImpl!(24, 3, true);
/// Alias for Tiger2-160(3 pass), result is ubyte[20] (first 20 bytes of Tiger-192)
alias Tiger2_160 = TigerImpl!(20, 3, true);
/// Alias for Tiger2-128(3 pass), result is ubyte[16] (first 16 bytes of Tiger-192)
alias Tiger2_128 = TigerImpl!(16, 3, true);

/// OOP API for Tiger-192(3 pass)
alias TigerDigest = WrapperDigest!Tiger;
/// OOP API for Tiger-160(3 pass)
alias Tiger160Digest = WrapperDigest!Tiger160;
/// OOP API for Tiger-128(3 pass)
alias Tiger128Digest = WrapperDigest!Tiger128;

/// OOP API for Tiger2-192(3 pass)
alias Tiger2Digest = WrapperDigest!Tiger2;
/// OOP API for Tiger2-160(3 pass)
alias Tiger2_160Digest = WrapperDigest!Tiger2_160;
/// OOP API for Tiger2-128(3 pass)
alias Tiger2_128Digest = WrapperDigest!Tiger2_128;

///
@trusted nothrow unittest
{
    Tiger st;
    st.start();
    st.put(cast(ubyte[])"The quick brown fox jumps over the lazy dog");
    assert(st.finish() == hexString!"6D12A41E72E644F017B6F0E2F7B44C6285F06DD5D2C5B075");

    // Template API
    assert(digest!Tiger("abc") == hexString!"2AAB1484E8C158F2BFB8C5FF41B57A525129131C957B5F93");
    assert(digest!Tiger160("abc") == hexString!"2AAB1484E8C158F2BFB8C5FF41B57A525129131C");
    assert(digest!Tiger128("abc") == hexString!"2AAB1484E8C158F2BFB8C5FF41B57A52");

    assert(digest!Tiger2("abc") == hexString!"F68D7BC5AF4B43A06E048D7829560D4A9415658BB0B1F3BF");

    // OOP API
    Digest t = new TigerDigest;
    t.put(cast(ubyte[])"Tiger");
    assert(t.finish() == hexString!"DD00230799F5009FEC6DEBC838BB6A27DF2B9D6F110C7937");

    t = new Tiger160Digest;
    t.put(cast(ubyte[])"Tiger");
    assert(t.finish() == hexString!"DD00230799F5009FEC6DEBC838BB6A27DF2B9D6F");

    t = new Tiger128Digest;
    t.put(cast(ubyte[])"Tiger");
    assert(t.finish() == hexString!"DD00230799F5009FEC6DEBC838BB6A27");

    t = new Tiger2Digest;
    t.put(cast(ubyte[])"Tiger");
    assert(t.finish() == hexString!"FE40798B8EB937FD977608930548D6A894C20B04CBEF7A42");
}

/// Convenience alias for std.digest using Tiger-192.
auto tigerOf(T...)(T data)
{
    return digest!(Tiger, T)(data);
}

///
@safe pure nothrow @nogc unittest
{
    assert(tigerOf("abc") == hexString!"2AAB1484E8C158F2BFB8C5FF41B57A525129131C957B5F93");
}

/// Convenience alias for std.digest using Tiger2-192.
auto tiger2Of(T...)(T data)
{
    return digest!(Tiger2, T)(data);
}

pure nothrow @nogc unittest
{
    assert(tigerOf("") == hexString!"3293AC630C13F0245F92BBB1766E16167A4E58492DDE73F3");
    assert(tiger2Of("") == hexString!"4441BE75F6018773C206C22745374B924AA8313FEF919F41");

    assert(tigerOf("a") == hexString!"77BEFBEF2E7EF8AB2EC8F93BF587A7FC613E247F5F247809");
    assert(tiger2Of("a") == hexString!"67E6AE8E9E968999F70A23E72AEAA9251CBC7C78A7916636");

    assert(tigerOf("message digest") == hexString!"D981F8CB78201A950DCF3048751E441C517FCA1AA55A29F6");
    assert(tiger2Of("message digest") == hexString!"E29419A1B5FA259DE8005E7DE75078EA81A542EF2552462D");

    assert(tigerOf(
               "abcdefghijklmnopqrstuvwxyz") == hexString!"1714A472EEE57D30040412BFCC55032A0B11602FF37BEEE9");
    assert(tiger2Of(
               "abcdefghijklmnopqrstuvwxyz") == hexString!"F5B6B6A78C405C8547E91CD8624CB8BE83FC804A474488FD");

    assert(tigerOf("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
           == hexString!"0F7BF9A19B9C58F2B7610DF7E84F0AC3A71C631E7B53F78E");
    assert(tiger2Of("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
           == hexString!"A6737F3997E8FBB63D20D2DF88F86376B5FE2D5CE36646A9");

    assert(tigerOf("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == hexString!"8DCEA680A17583EE502BA38A3C368651890FFBCCDC49A8CC");
    assert(tiger2Of("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
           == hexString!"EA9AB6228CEE7B51B77544FCA6066C8CBB5BBAE6319505CD");

    assert(tigerOf("12345678901234567890123456789012345678901234567890123456789012345678901234567890")
           == hexString!"1C14795529FD9F207A958F84C52F11E887FA0CABDFD91BFD");
    assert(tiger2Of("12345678901234567890123456789012345678901234567890123456789012345678901234567890")
           == hexString!"D85278115329EBAA0EEC85ECDC5396FDA8AA3A5820942FFF");
}

private:

// Tiger round function
void round(alias a, alias b, alias c)(in ulong x, in int mul)
{
    c ^= x;

    a -= table1[cast(ubyte) c] ^ table2[cast(ubyte)(c >> (2 * 8))] ^ table3[cast(ubyte)(c >> (4 * 8))] ^ table4[cast(
            ubyte)(c >> (6 * 8))];

    b += table4[cast(ubyte)(c >> (1 * 8))] ^ table3[cast(ubyte)(c >> (3 * 8))] ^ table2[cast(
            ubyte)(c >> (5 * 8))] ^ table1[cast(ubyte)(c >> (7 * 8))];

    b *= mul;
}

// Tiger S Boxes
immutable ulong[4 * 256] table1 = [
    0x02AAB17CF7E90C5EUL, 0xAC424B03E243A8ECUL, 0x72CD5BE30DD5FCD3UL,
    0x6D019B93F6F97F3AUL, 0xCD9978FFD21F9193UL, 0x7573A1C9708029E2UL,
    0xB164326B922A83C3UL, 0x46883EEE04915870UL, 0xEAACE3057103ECE6UL,
    0xC54169B808A3535CUL,
    0x4CE754918DDEC47CUL, 0x0AA2F4DFDC0DF40CUL, 0x10B76F18A74DBEFAUL,
    0xC6CCB6235AD1AB6AUL, 0x13726121572FE2FFUL, 0x1A488C6F199D921EUL,
    0x4BC9F9F4DA0007CAUL, 0x26F5E6F6E85241C7UL, 0x859079DBEA5947B6UL,
    0x4F1885C5C99E8C92UL, 0xD78E761EA96F864BUL, 0x8E36428C52B5C17DUL,
    0x69CF6827373063C1UL,
    0xB607C93D9BB4C56EUL,
    0x7D820E760E76B5EAUL, 0x645C9CC6F07FDC42UL, 0xBF38A078243342E0UL,
    0x5F6B343C9D2E7D04UL, 0xF2C28AEB600B0EC6UL, 0x6C0ED85F7254BCACUL,
    0x71592281A4DB4FE5UL, 0x1967FA69CE0FED9FUL, 0xFD5293F8B96545DBUL,
    0xC879E9D7F2A7600BUL, 0x860248920193194EUL, 0xA4F9533B2D9CC0B3UL,
    0x9053836C15957613UL,
    0xDB6DCF8AFC357BF1UL,
    0x18BEEA7A7A370F57UL, 0x037117CA50B99066UL, 0x6AB30A9774424A35UL,
    0xF4E92F02E325249BUL, 0x7739DB07061CCAE1UL, 0xD8F3B49CECA42A05UL,
    0xBD56BE3F51382F73UL, 0x45FAED5843B0BB28UL, 0x1C813D5C11BF1F83UL,
    0x8AF0E4B6D75FA169UL, 0x33EE18A487AD9999UL, 0x3C26E8EAB1C94410UL,
    0xB510102BC0A822F9UL,
    0x141EEF310CE6123BUL,
    0xFC65B90059DDB154UL, 0xE0158640C5E0E607UL, 0x884E079826C3A3CFUL,
    0x930D0D9523C535FDUL, 0x35638D754E9A2B00UL, 0x4085FCCF40469DD5UL,
    0xC4B17AD28BE23A4CUL, 0xCAB2F0FC6A3E6A2EUL, 0x2860971A6B943FCDUL,
    0x3DDE6EE212E30446UL, 0x6222F32AE01765AEUL, 0x5D550BB5478308FEUL,
    0xA9EFA98DA0EDA22AUL,
    0xC351A71686C40DA7UL,
    0x1105586D9C867C84UL, 0xDCFFEE85FDA22853UL, 0xCCFBD0262C5EEF76UL,
    0xBAF294CB8990D201UL, 0xE69464F52AFAD975UL, 0x94B013AFDF133E14UL,
    0x06A7D1A32823C958UL, 0x6F95FE5130F61119UL, 0xD92AB34E462C06C0UL,
    0xED7BDE33887C71D2UL, 0x79746D6E6518393EUL, 0x5BA419385D713329UL,
    0x7C1BA6B948A97564UL,
    0x31987C197BFDAC67UL,
    0xDE6C23C44B053D02UL, 0x581C49FED002D64DUL, 0xDD474D6338261571UL,
    0xAA4546C3E473D062UL, 0x928FCE349455F860UL, 0x48161BBACAAB94D9UL,
    0x63912430770E6F68UL, 0x6EC8A5E602C6641CUL, 0x87282515337DDD2BUL,
    0x2CDA6B42034B701BUL, 0xB03D37C181CB096DUL, 0xE108438266C71C6FUL,
    0x2B3180C7EB51B255UL,
    0xDF92B82F96C08BBCUL,
    0x5C68C8C0A632F3BAUL, 0x5504CC861C3D0556UL, 0xABBFA4E55FB26B8FUL,
    0x41848B0AB3BACEB4UL, 0xB334A273AA445D32UL, 0xBCA696F0A85AD881UL,
    0x24F6EC65B528D56CUL, 0x0CE1512E90F4524AUL, 0x4E9DD79D5506D35AUL,
    0x258905FAC6CE9779UL, 0x2019295B3E109B33UL, 0xF8A9478B73A054CCUL,
    0x2924F2F934417EB0UL,
    0x3993357D536D1BC4UL,
    0x38A81AC21DB6FF8BUL, 0x47C4FBF17D6016BFUL, 0x1E0FAADD7667E3F5UL,
    0x7ABCFF62938BEB96UL, 0xA78DAD948FC179C9UL, 0x8F1F98B72911E50DUL,
    0x61E48EAE27121A91UL, 0x4D62F7AD31859808UL, 0xECEBA345EF5CEAEBUL,
    0xF5CEB25EBC9684CEUL, 0xF633E20CB7F76221UL, 0xA32CDF06AB8293E4UL,
    0x985A202CA5EE2CA4UL,
    0xCF0B8447CC8A8FB1UL,
    0x9F765244979859A3UL, 0xA8D516B1A1240017UL, 0x0BD7BA3EBB5DC726UL,
    0xE54BCA55B86ADB39UL, 0x1D7A3AFD6C478063UL, 0x519EC608E7669EDDUL,
    0x0E5715A2D149AA23UL, 0x177D4571848FF194UL, 0xEEB55F3241014C22UL,
    0x0F5E5CA13A6E2EC2UL, 0x8029927B75F5C361UL, 0xAD139FABC3D6E436UL,
    0x0D5DF1A94CCF402FUL,
    0x3E8BD948BEA5DFC8UL,
    0xA5A0D357BD3FF77EUL, 0xA2D12E251F74F645UL, 0x66FD9E525E81A082UL,
    0x2E0C90CE7F687A49UL, 0xC2E8BCBEBA973BC5UL, 0x000001BCE509745FUL,
    0x423777BBE6DAB3D6UL, 0xD1661C7EAEF06EB5UL, 0xA1781F354DAACFD8UL,
    0x2D11284A2B16AFFCUL, 0xF1FC4F67FA891D1FUL, 0x73ECC25DCB920ADAUL,
    0xAE610C22C2A12651UL,
    0x96E0A810D356B78AUL,
    0x5A9A381F2FE7870FUL, 0xD5AD62EDE94E5530UL, 0xD225E5E8368D1427UL,
    0x65977B70C7AF4631UL, 0x99F889B2DE39D74FUL, 0x233F30BF54E1D143UL,
    0x9A9675D3D9A63C97UL, 0x5470554FF334F9A8UL, 0x166ACB744A4F5688UL,
    0x70C74CAAB2E4AEADUL, 0xF0D091646F294D12UL, 0x57B82A89684031D1UL,
    0xEFD95A5A61BE0B6BUL,
    0x2FBD12E969F2F29AUL,
    0x9BD37013FEFF9FE8UL, 0x3F9B0404D6085A06UL, 0x4940C1F3166CFE15UL,
    0x09542C4DCDF3DEFBUL, 0xB4C5218385CD5CE3UL, 0xC935B7DC4462A641UL,
    0x3417F8A68ED3B63FUL, 0xB80959295B215B40UL, 0xF99CDAEF3B8C8572UL,
    0x018C0614F8FCB95DUL, 0x1B14ACCD1A3ACDF3UL, 0x84D471F200BB732DUL,
    0xC1A3110E95E8DA16UL,
    0x430A7220BF1A82B8UL,
    0xB77E090D39DF210EUL, 0x5EF4BD9F3CD05E9DUL, 0x9D4FF6DA7E57A444UL,
    0xDA1D60E183D4A5F8UL, 0xB287C38417998E47UL, 0xFE3EDC121BB31886UL,
    0xC7FE3CCC980CCBEFUL, 0xE46FB590189BFD03UL, 0x3732FD469A4C57DCUL,
    0x7EF700A07CF1AD65UL, 0x59C64468A31D8859UL, 0x762FB0B4D45B61F6UL,
    0x155BAED099047718UL,
    0x68755E4C3D50BAA6UL,
    0xE9214E7F22D8B4DFUL, 0x2ADDBF532EAC95F4UL, 0x32AE3909B4BD0109UL,
    0x834DF537B08E3450UL, 0xFA209DA84220728DUL, 0x9E691D9B9EFE23F7UL,
    0x0446D288C4AE8D7FUL, 0x7B4CC524E169785BUL, 0x21D87F0135CA1385UL,
    0xCEBB400F137B8AA5UL, 0x272E2B66580796BEUL, 0x3612264125C2B0DEUL,
    0x057702BDAD1EFBB2UL,
    0xD4BABB8EACF84BE9UL,
    0x91583139641BC67BUL, 0x8BDC2DE08036E024UL, 0x603C8156F49F68EDUL,
    0xF7D236F7DBEF5111UL, 0x9727C4598AD21E80UL, 0xA08A0896670A5FD7UL,
    0xCB4A8F4309EBA9CBUL, 0x81AF564B0F7036A1UL, 0xC0B99AA778199ABDUL,
    0x959F1EC83FC8E952UL, 0x8C505077794A81B9UL, 0x3ACAAF8F056338F0UL,
    0x07B43F50627A6778UL,
    0x4A44AB49F5ECCC77UL,
    0x3BC3D6E4B679EE98UL, 0x9CC0D4D1CF14108CUL, 0x4406C00B206BC8A0UL,
    0x82A18854C8D72D89UL, 0x67E366B35C3C432CUL, 0xB923DD61102B37F2UL,
    0x56AB2779D884271DUL, 0xBE83E1B0FF1525AFUL, 0xFB7C65D4217E49A9UL,
    0x6BDBE0E76D48E7D4UL, 0x08DF828745D9179EUL, 0x22EA6A9ADD53BD34UL,
    0xE36E141C5622200AUL,
    0x7F805D1B8CB750EEUL,
    0xAFE5C7A59F58E837UL, 0xE27F996A4FB1C23CUL, 0xD3867DFB0775F0D0UL,
    0xD0E673DE6E88891AUL, 0x123AEB9EAFB86C25UL, 0x30F1D5D5C145B895UL,
    0xBB434A2DEE7269E7UL, 0x78CB67ECF931FA38UL, 0xF33B0372323BBF9CUL,
    0x52D66336FB279C74UL, 0x505F33AC0AFB4EAAUL, 0xE8A5CD99A2CCE187UL,
    0x534974801E2D30BBUL,
    0x8D2D5711D5876D90UL,
    0x1F1A412891BC038EUL, 0xD6E2E71D82E56648UL, 0x74036C3A497732B7UL,
    0x89B67ED96361F5ABUL, 0xFFED95D8F1EA02A2UL, 0xE72B3BD61464D43DUL,
    0xA6300F170BDC4820UL, 0xEBC18760ED78A77AUL
    ];

immutable ulong[256] table2 = [
    0xE6A6BE5A05A12138UL, 0xB5A122A5B4F87C98UL, 0x563C6089140B6990UL,
    0x4C46CB2E391F5DD5UL, 0xD932ADDBC9B79434UL, 0x08EA70E42015AFF5UL,
    0xD765A6673E478CF1UL, 0xC4FB757EAB278D99UL, 0xDF11C6862D6E0692UL,
    0xDDEB84F10D7F3B16UL,
    0x6F2EF604A665EA04UL, 0x4A8E0F0FF0E0DFB3UL, 0xA5EDEEF83DBCBA51UL,
    0xFC4F0A2A0EA4371EUL, 0xE83E1DA85CB38429UL, 0xDC8FF882BA1B1CE2UL,
    0xCD45505E8353E80DUL, 0x18D19A00D4DB0717UL, 0x34A0CFEDA5F38101UL,
    0x0BE77E518887CAF2UL, 0x1E341438B3C45136UL, 0xE05797F49089CCF9UL,
    0xFFD23F9DF2591D14UL,
    0x543DDA228595C5CDUL,
    0x661F81FD99052A33UL, 0x8736E641DB0F7B76UL, 0x15227725418E5307UL,
    0xE25F7F46162EB2FAUL, 0x48A8B2126C13D9FEUL, 0xAFDC541792E76EEAUL,
    0x03D912BFC6D1898FUL, 0x31B1AAFA1B83F51BUL, 0xF1AC2796E42AB7D9UL,
    0x40A3A7D7FCD2EBACUL, 0x1056136D0AFBBCC5UL, 0x7889E1DD9A6D0C85UL,
    0xD33525782A7974AAUL,
    0xA7E25D09078AC09BUL,
    0xBD4138B3EAC6EDD0UL, 0x920ABFBE71EB9E70UL, 0xA2A5D0F54FC2625CUL,
    0xC054E36B0B1290A3UL, 0xF6DD59FF62FE932BUL, 0x3537354511A8AC7DUL,
    0xCA845E9172FADCD4UL, 0x84F82B60329D20DCUL, 0x79C62CE1CD672F18UL,
    0x8B09A2ADD124642CUL, 0xD0C1E96A19D9E726UL, 0x5A786A9B4BA9500CUL,
    0x0E020336634C43F3UL,
    0xC17B474AEB66D822UL,
    0x6A731AE3EC9BAAC2UL, 0x8226667AE0840258UL, 0x67D4567691CAECA5UL,
    0x1D94155C4875ADB5UL, 0x6D00FD985B813FDFUL, 0x51286EFCB774CD06UL,
    0x5E8834471FA744AFUL, 0xF72CA0AEE761AE2EUL, 0xBE40E4CDAEE8E09AUL,
    0xE9970BBB5118F665UL, 0x726E4BEB33DF1964UL, 0x703B000729199762UL,
    0x4631D816F5EF30A7UL,
    0xB880B5B51504A6BEUL,
    0x641793C37ED84B6CUL, 0x7B21ED77F6E97D96UL, 0x776306312EF96B73UL,
    0xAE528948E86FF3F4UL, 0x53DBD7F286A3F8F8UL, 0x16CADCE74CFC1063UL,
    0x005C19BDFA52C6DDUL, 0x68868F5D64D46AD3UL, 0x3A9D512CCF1E186AUL,
    0x367E62C2385660AEUL, 0xE359E7EA77DCB1D7UL, 0x526C0773749ABE6EUL,
    0x735AE5F9D09F734BUL,
    0x493FC7CC8A558BA8UL,
    0xB0B9C1533041AB45UL, 0x321958BA470A59BDUL, 0x852DB00B5F46C393UL,
    0x91209B2BD336B0E5UL, 0x6E604F7D659EF19FUL, 0xB99A8AE2782CCB24UL,
    0xCCF52AB6C814C4C7UL, 0x4727D9AFBE11727BUL, 0x7E950D0C0121B34DUL,
    0x756F435670AD471FUL, 0xF5ADD442615A6849UL, 0x4E87E09980B9957AUL,
    0x2ACFA1DF50AEE355UL,
    0xD898263AFD2FD556UL,
    0xC8F4924DD80C8FD6UL, 0xCF99CA3D754A173AUL, 0xFE477BACAF91BF3CUL,
    0xED5371F6D690C12DUL, 0x831A5C285E687094UL, 0xC5D3C90A3708A0A4UL,
    0x0F7F903717D06580UL, 0x19F9BB13B8FDF27FUL, 0xB1BD6F1B4D502843UL,
    0x1C761BA38FFF4012UL, 0x0D1530C4E2E21F3BUL, 0x8943CE69A7372C8AUL,
    0xE5184E11FEB5CE66UL,
    0x618BDB80BD736621UL,
    0x7D29BAD68B574D0BUL, 0x81BB613E25E6FE5BUL, 0x071C9C10BC07913FUL,
    0xC7BEEB7909AC2D97UL, 0xC3E58D353BC5D757UL, 0xEB017892F38F61E8UL,
    0xD4EFFB9C9B1CC21AUL, 0x99727D26F494F7ABUL, 0xA3E063A2956B3E03UL,
    0x9D4A8B9A4AA09C30UL, 0x3F6AB7D500090FB4UL, 0x9CC0F2A057268AC0UL,
    0x3DEE9D2DEDBF42D1UL,
    0x330F49C87960A972UL,
    0xC6B2720287421B41UL, 0x0AC59EC07C00369CUL, 0xEF4EAC49CB353425UL,
    0xF450244EEF0129D8UL, 0x8ACC46E5CAF4DEB6UL, 0x2FFEAB63989263F7UL,
    0x8F7CB9FE5D7A4578UL, 0x5BD8F7644E634635UL, 0x427A7315BF2DC900UL,
    0x17D0C4AA2125261CUL, 0x3992486C93518E50UL, 0xB4CBFEE0A2D7D4C3UL,
    0x7C75D6202C5DDD8DUL,
    0xDBC295D8E35B6C61UL,
    0x60B369D302032B19UL, 0xCE42685FDCE44132UL, 0x06F3DDB9DDF65610UL,
    0x8EA4D21DB5E148F0UL, 0x20B0FCE62FCD496FUL, 0x2C1B912358B0EE31UL,
    0xB28317B818F5A308UL, 0xA89C1E189CA6D2CFUL, 0x0C6B18576AAADBC8UL,
    0xB65DEAA91299FAE3UL, 0xFB2B794B7F1027E7UL, 0x04E4317F443B5BEBUL,
    0x4B852D325939D0A6UL,
    0xD5AE6BEEFB207FFCUL,
    0x309682B281C7D374UL, 0xBAE309A194C3B475UL, 0x8CC3F97B13B49F05UL,
    0x98A9422FF8293967UL, 0x244B16B01076FF7CUL, 0xF8BF571C663D67EEUL,
    0x1F0D6758EEE30DA1UL, 0xC9B611D97ADEB9B7UL, 0xB7AFD5887B6C57A2UL,
    0x6290AE846B984FE1UL, 0x94DF4CDEACC1A5FDUL, 0x058A5BD1C5483AFFUL,
    0x63166CC142BA3C37UL,
    0x8DB8526EB2F76F40UL,
    0xE10880036F0D6D4EUL, 0x9E0523C9971D311DUL, 0x45EC2824CC7CD691UL,
    0x575B8359E62382C9UL, 0xFA9E400DC4889995UL, 0xD1823ECB45721568UL,
    0xDAFD983B8206082FUL, 0xAA7D29082386A8CBUL, 0x269FCD4403B87588UL,
    0x1B91F5F728BDD1E0UL, 0xE4669F39040201F6UL, 0x7A1D7C218CF04ADEUL,
    0x65623C29D79CE5CEUL,
    0x2368449096C00BB1UL,
    0xAB9BF1879DA503BAUL, 0xBC23ECB1A458058EUL, 0x9A58DF01BB401ECCUL,
    0xA070E868A85F143DUL, 0x4FF188307DF2239EUL, 0x14D565B41A641183UL,
    0xEE13337452701602UL, 0x950E3DCF3F285E09UL, 0x59930254B9C80953UL,
    0x3BF299408930DA6DUL, 0xA955943F53691387UL, 0xA15EDECAA9CB8784UL,
    0x29142127352BE9A0UL,
    0x76F0371FFF4E7AFBUL,
    0x0239F450274F2228UL, 0xBB073AF01D5E868BUL, 0xBFC80571C10E96C1UL,
    0xD267088568222E23UL, 0x9671A3D48E80B5B0UL, 0x55B5D38AE193BB81UL,
    0x693AE2D0A18B04B8UL, 0x5C48B4ECADD5335FUL, 0xFD743B194916A1CAUL,
    0x2577018134BE98C4UL, 0xE77987E83C54A4ADUL, 0x28E11014DA33E1B9UL,
    0x270CC59E226AA213UL,
    0x71495F756D1A5F60UL,
    0x9BE853FB60AFEF77UL, 0xADC786A7F7443DBFUL, 0x0904456173B29A82UL,
    0x58BC7A66C232BD5EUL, 0xF306558C673AC8B2UL, 0x41F639C6B6C9772AUL,
    0x216DEFE99FDA35DAUL, 0x11640CC71C7BE615UL, 0x93C43694565C5527UL,
    0xEA038E6246777839UL, 0xF9ABF3CE5A3E2469UL, 0x741E768D0FD312D2UL,
    0x0144B883CED652C6UL,
    0xC20B5A5BA33F8552UL,
    0x1AE69633C3435A9DUL, 0x97A28CA4088CFDECUL, 0x8824A43C1E96F420UL,
    0x37612FA66EEEA746UL, 0x6B4CB165F9CF0E5AUL, 0x43AA1C06A0ABFB4AUL,
    0x7F4DC26FF162796BUL, 0x6CBACC8E54ED9B0FUL, 0xA6B7FFEFD2BB253EUL,
    0x2E25BC95B0A29D4FUL, 0x86D6A58BDEF1388CUL, 0xDED74AC576B6F054UL,
    0x8030BDBC2B45805DUL,
    0x3C81AF70E94D9289UL,
    0x3EFF6DDA9E3100DBUL, 0xB38DC39FDFCC8847UL, 0x123885528D17B87EUL,
    0xF2DA0ED240B1B642UL, 0x44CEFADCD54BF9A9UL, 0x1312200E433C7EE6UL,
    0x9FFCC84F3A78C748UL, 0xF0CD1F72248576BBUL, 0xEC6974053638CFE4UL,
    0x2BA7B67C0CEC4E4CUL, 0xAC2F4DF3E5CE32EDUL, 0xCB33D14326EA4C11UL,
    0xA4E9044CC77E58BCUL,
    0x5F513293D934FCEFUL,
    0x5DC9645506E55444UL, 0x50DE418F317DE40AUL, 0x388CB31A69DDE259UL,
    0x2DB4A83455820A86UL, 0x9010A91E84711AE9UL, 0x4DF7F0B7B1498371UL,
    0xD62A2EABC0977179UL, 0x22FAC097AA8D5C0EUL
    ];

immutable ulong[256] table3 = [
    0xF49FCC2FF1DAF39BUL, 0x487FD5C66FF29281UL, 0xE8A30667FCDCA83FUL,
    0x2C9B4BE3D2FCCE63UL, 0xDA3FF74B93FBBBC2UL, 0x2FA165D2FE70BA66UL,
    0xA103E279970E93D4UL, 0xBECDEC77B0E45E71UL, 0xCFB41E723985E497UL,
    0xB70AAA025EF75017UL,
    0xD42309F03840B8E0UL, 0x8EFC1AD035898579UL, 0x96C6920BE2B2ABC5UL,
    0x66AF4163375A9172UL, 0x2174ABDCCA7127FBUL, 0xB33CCEA64A72FF41UL,
    0xF04A4933083066A5UL, 0x8D970ACDD7289AF5UL, 0x8F96E8E031C8C25EUL,
    0xF3FEC02276875D47UL, 0xEC7BF310056190DDUL, 0xF5ADB0AEBB0F1491UL,
    0x9B50F8850FD58892UL,
    0x4975488358B74DE8UL,
    0xA3354FF691531C61UL, 0x0702BBE481D2C6EEUL, 0x89FB24057DEDED98UL,
    0xAC3075138596E902UL, 0x1D2D3580172772EDUL, 0xEB738FC28E6BC30DUL,
    0x5854EF8F63044326UL, 0x9E5C52325ADD3BBEUL, 0x90AA53CF325C4623UL,
    0xC1D24D51349DD067UL, 0x2051CFEEA69EA624UL, 0x13220F0A862E7E4FUL,
    0xCE39399404E04864UL,
    0xD9C42CA47086FCB7UL,
    0x685AD2238A03E7CCUL, 0x066484B2AB2FF1DBUL, 0xFE9D5D70EFBF79ECUL,
    0x5B13B9DD9C481854UL, 0x15F0D475ED1509ADUL, 0x0BEBCD060EC79851UL,
    0xD58C6791183AB7F8UL, 0xD1187C5052F3EEE4UL, 0xC95D1192E54E82FFUL,
    0x86EEA14CB9AC6CA2UL, 0x3485BEB153677D5DUL, 0xDD191D781F8C492AUL,
    0xF60866BAA784EBF9UL,
    0x518F643BA2D08C74UL,
    0x8852E956E1087C22UL, 0xA768CB8DC410AE8DUL, 0x38047726BFEC8E1AUL,
    0xA67738B4CD3B45AAUL, 0xAD16691CEC0DDE19UL, 0xC6D4319380462E07UL,
    0xC5A5876D0BA61938UL, 0x16B9FA1FA58FD840UL, 0x188AB1173CA74F18UL,
    0xABDA2F98C99C021FUL, 0x3E0580AB134AE816UL, 0x5F3B05B773645ABBUL,
    0x2501A2BE5575F2F6UL,
    0x1B2F74004E7E8BA9UL,
    0x1CD7580371E8D953UL, 0x7F6ED89562764E30UL, 0xB15926FF596F003DUL,
    0x9F65293DA8C5D6B9UL, 0x6ECEF04DD690F84CUL, 0x4782275FFF33AF88UL,
    0xE41433083F820801UL, 0xFD0DFE409A1AF9B5UL, 0x4325A3342CDB396BUL,
    0x8AE77E62B301B252UL, 0xC36F9E9F6655615AUL, 0x85455A2D92D32C09UL,
    0xF2C7DEA949477485UL,
    0x63CFB4C133A39EBAUL,
    0x83B040CC6EBC5462UL, 0x3B9454C8FDB326B0UL, 0x56F56A9E87FFD78CUL,
    0x2DC2940D99F42BC6UL, 0x98F7DF096B096E2DUL, 0x19A6E01E3AD852BFUL,
    0x42A99CCBDBD4B40BUL, 0xA59998AF45E9C559UL, 0x366295E807D93186UL,
    0x6B48181BFAA1F773UL, 0x1FEC57E2157A0A1DUL, 0x4667446AF6201AD5UL,
    0xE615EBCACFB0F075UL,
    0xB8F31F4F68290778UL,
    0x22713ED6CE22D11EUL, 0x3057C1A72EC3C93BUL, 0xCB46ACC37C3F1F2FUL,
    0xDBB893FD02AAF50EUL, 0x331FD92E600B9FCFUL, 0xA498F96148EA3AD6UL,
    0xA8D8426E8B6A83EAUL, 0xA089B274B7735CDCUL, 0x87F6B3731E524A11UL,
    0x118808E5CBC96749UL, 0x9906E4C7B19BD394UL, 0xAFED7F7E9B24A20CUL,
    0x6509EADEEB3644A7UL,
    0x6C1EF1D3E8EF0EDEUL,
    0xB9C97D43E9798FB4UL, 0xA2F2D784740C28A3UL, 0x7B8496476197566FUL,
    0x7A5BE3E6B65F069DUL, 0xF96330ED78BE6F10UL, 0xEEE60DE77A076A15UL,
    0x2B4BEE4AA08B9BD0UL, 0x6A56A63EC7B8894EUL, 0x02121359BA34FEF4UL,
    0x4CBF99F8283703FCUL, 0x398071350CAF30C8UL, 0xD0A77A89F017687AUL,
    0xF1C1A9EB9E423569UL,
    0x8C7976282DEE8199UL,
    0x5D1737A5DD1F7ABDUL, 0x4F53433C09A9FA80UL, 0xFA8B0C53DF7CA1D9UL,
    0x3FD9DCBC886CCB77UL, 0xC040917CA91B4720UL, 0x7DD00142F9D1DCDFUL,
    0x8476FC1D4F387B58UL, 0x23F8E7C5F3316503UL, 0x032A2244E7E37339UL,
    0x5C87A5D750F5A74BUL, 0x082B4CC43698992EUL, 0xDF917BECB858F63CUL,
    0x3270B8FC5BF86DDAUL,
    0x10AE72BB29B5DD76UL,
    0x576AC94E7700362BUL, 0x1AD112DAC61EFB8FUL, 0x691BC30EC5FAA427UL,
    0xFF246311CC327143UL, 0x3142368E30E53206UL, 0x71380E31E02CA396UL,
    0x958D5C960AAD76F1UL, 0xF8D6F430C16DA536UL, 0xC8FFD13F1BE7E1D2UL,
    0x7578AE66004DDBE1UL, 0x05833F01067BE646UL, 0xBB34B5AD3BFE586DUL,
    0x095F34C9A12B97F0UL,
    0x247AB64525D60CA8UL,
    0xDCDBC6F3017477D1UL, 0x4A2E14D4DECAD24DUL, 0xBDB5E6D9BE0A1EEBUL,
    0x2A7E70F7794301ABUL, 0xDEF42D8A270540FDUL, 0x01078EC0A34C22C1UL,
    0xE5DE511AF4C16387UL, 0x7EBB3A52BD9A330AUL, 0x77697857AA7D6435UL,
    0x004E831603AE4C32UL, 0xE7A21020AD78E312UL, 0x9D41A70C6AB420F2UL,
    0x28E06C18EA1141E6UL,
    0xD2B28CBD984F6B28UL,
    0x26B75F6C446E9D83UL, 0xBA47568C4D418D7FUL, 0xD80BADBFE6183D8EUL,
    0x0E206D7F5F166044UL, 0xE258A43911CBCA3EUL, 0x723A1746B21DC0BCUL,
    0xC7CAA854F5D7CDD3UL, 0x7CAC32883D261D9CUL, 0x7690C26423BA942CUL,
    0x17E55524478042B8UL, 0xE0BE477656A2389FUL, 0x4D289B5E67AB2DA0UL,
    0x44862B9C8FBBFD31UL,
    0xB47CC8049D141365UL,
    0x822C1B362B91C793UL, 0x4EB14655FB13DFD8UL, 0x1ECBBA0714E2A97BUL,
    0x6143459D5CDE5F14UL, 0x53A8FBF1D5F0AC89UL, 0x97EA04D81C5E5B00UL,
    0x622181A8D4FDB3F3UL, 0xE9BCD341572A1208UL, 0x1411258643CCE58AUL,
    0x9144C5FEA4C6E0A4UL, 0x0D33D06565CF620FUL, 0x54A48D489F219CA1UL,
    0xC43E5EAC6D63C821UL,
    0xA9728B3A72770DAFUL,
    0xD7934E7B20DF87EFUL, 0xE35503B61A3E86E5UL, 0xCAE321FBC819D504UL,
    0x129A50B3AC60BFA6UL, 0xCD5E68EA7E9FB6C3UL, 0xB01C90199483B1C7UL,
    0x3DE93CD5C295376CUL, 0xAED52EDF2AB9AD13UL, 0x2E60F512C0A07884UL,
    0xBC3D86A3E36210C9UL, 0x35269D9B163951CEUL, 0x0C7D6E2AD0CDB5FAUL,
    0x59E86297D87F5733UL,
    0x298EF221898DB0E7UL,
    0x55000029D1A5AA7EUL, 0x8BC08AE1B5061B45UL, 0xC2C31C2B6C92703AUL,
    0x94CC596BAF25EF42UL, 0x0A1D73DB22540456UL, 0x04B6A0F9D9C4179AUL,
    0xEFFDAFA2AE3D3C60UL, 0xF7C8075BB49496C4UL, 0x9CC5C7141D1CD4E3UL,
    0x78BD1638218E5534UL, 0xB2F11568F850246AUL, 0xEDFABCFA9502BC29UL,
    0x796CE5F2DA23051BUL,
    0xAAE128B0DC93537CUL,
    0x3A493DA0EE4B29AEUL, 0xB5DF6B2C416895D7UL, 0xFCABBD25122D7F37UL,
    0x70810B58105DC4B1UL, 0xE10FDD37F7882A90UL, 0x524DCAB5518A3F5CUL,
    0x3C9E85878451255BUL, 0x4029828119BD34E2UL, 0x74A05B6F5D3CECCBUL,
    0xB610021542E13ECAUL, 0x0FF979D12F59E2ACUL, 0x6037DA27E4F9CC50UL,
    0x5E92975A0DF1847DUL,
    0xD66DE190D3E623FEUL,
    0x5032D6B87B568048UL, 0x9A36B7CE8235216EUL, 0x80272A7A24F64B4AUL,
    0x93EFED8B8C6916F7UL, 0x37DDBFF44CCE1555UL, 0x4B95DB5D4B99BD25UL,
    0x92D3FDA169812FC0UL, 0xFB1A4A9A90660BB6UL, 0x730C196946A4B9B2UL,
    0x81E289AA7F49DA68UL, 0x64669A0F83B1A05FUL, 0x27B3FF7D9644F48BUL,
    0xCC6B615C8DB675B3UL,
    0x674F20B9BCEBBE95UL,
    0x6F31238275655982UL, 0x5AE488713E45CF05UL, 0xBF619F9954C21157UL,
    0xEABAC46040A8EAE9UL, 0x454C6FE9F2C0C1CDUL, 0x419CF6496412691CUL,
    0xD3DC3BEF265B0F70UL, 0x6D0E60F5C3578A9EUL
    ];

immutable ulong[256] table4 = [
    0x5B0E608526323C55UL, 0x1A46C1A9FA1B59F5UL, 0xA9E245A17C4C8FFAUL,
    0x65CA5159DB2955D7UL, 0x05DB0A76CE35AFC2UL, 0x81EAC77EA9113D45UL,
    0x528EF88AB6AC0A0DUL, 0xA09EA253597BE3FFUL, 0x430DDFB3AC48CD56UL,
    0xC4B3A67AF45CE46FUL,
    0x4ECECFD8FBE2D05EUL, 0x3EF56F10B39935F0UL, 0x0B22D6829CD619C6UL,
    0x17FD460A74DF2069UL, 0x6CF8CC8E8510ED40UL, 0xD6C824BF3A6ECAA7UL,
    0x61243D581A817049UL, 0x048BACB6BBC163A2UL, 0xD9A38AC27D44CC32UL,
    0x7FDDFF5BAAF410ABUL, 0xAD6D495AA804824BUL, 0xE1A6A74F2D8C9F94UL,
    0xD4F7851235DEE8E3UL,
    0xFD4B7F886540D893UL,
    0x247C20042AA4BFDAUL, 0x096EA1C517D1327CUL, 0xD56966B4361A6685UL,
    0x277DA5C31221057DUL, 0x94D59893A43ACFF7UL, 0x64F0C51CCDC02281UL,
    0x3D33BCC4FF6189DBUL, 0xE005CB184CE66AF1UL, 0xFF5CCD1D1DB99BEAUL,
    0xB0B854A7FE42980FUL, 0x7BD46A6A718D4B9FUL, 0xD10FA8CC22A5FD8CUL,
    0xD31484952BE4BD31UL,
    0xC7FA975FCB243847UL,
    0x4886ED1E5846C407UL, 0x28CDDB791EB70B04UL, 0xC2B00BE2F573417FUL,
    0x5C9590452180F877UL, 0x7A6BDDFFF370EB00UL, 0xCE509E38D6D9D6A4UL,
    0xEBEB0F00647FA702UL, 0x1DCC06CF76606F06UL, 0xE4D9F28BA286FF0AUL,
    0xD85A305DC918C262UL, 0x475B1D8732225F54UL, 0x2D4FB51668CCB5FEUL,
    0xA679B9D9D72BBA20UL,
    0x53841C0D912D43A5UL,
    0x3B7EAA48BF12A4E8UL, 0x781E0E47F22F1DDFUL, 0xEFF20CE60AB50973UL,
    0x20D261D19DFFB742UL, 0x16A12B03062A2E39UL, 0x1960EB2239650495UL,
    0x251C16FED50EB8B8UL, 0x9AC0C330F826016EUL, 0xED152665953E7671UL,
    0x02D63194A6369570UL, 0x5074F08394B1C987UL, 0x70BA598C90B25CE1UL,
    0x794A15810B9742F6UL,
    0x0D5925E9FCAF8C6CUL,
    0x3067716CD868744EUL, 0x910AB077E8D7731BUL, 0x6A61BBDB5AC42F61UL,
    0x93513EFBF0851567UL, 0xF494724B9E83E9D5UL, 0xE887E1985C09648DUL,
    0x34B1D3C675370CFDUL, 0xDC35E433BC0D255DUL, 0xD0AAB84234131BE0UL,
    0x08042A50B48B7EAFUL, 0x9997C4EE44A3AB35UL, 0x829A7B49201799D0UL,
    0x263B8307B7C54441UL,
    0x752F95F4FD6A6CA6UL,
    0x927217402C08C6E5UL, 0x2A8AB754A795D9EEUL, 0xA442F7552F72943DUL,
    0x2C31334E19781208UL, 0x4FA98D7CEAEE6291UL, 0x55C3862F665DB309UL,
    0xBD0610175D53B1F3UL, 0x46FE6CB840413F27UL, 0x3FE03792DF0CFA59UL,
    0xCFE700372EB85E8FUL, 0xA7BE29E7ADBCE118UL, 0xE544EE5CDE8431DDUL,
    0x8A781B1B41F1873EUL,
    0xA5C94C78A0D2F0E7UL,
    0x39412E2877B60728UL, 0xA1265EF3AFC9A62CUL, 0xBCC2770C6A2506C5UL,
    0x3AB66DD5DCE1CE12UL, 0xE65499D04A675B37UL, 0x7D8F523481BFD216UL,
    0x0F6F64FCEC15F389UL, 0x74EFBE618B5B13C8UL, 0xACDC82B714273E1DUL,
    0xDD40BFE003199D17UL, 0x37E99257E7E061F8UL, 0xFA52626904775AAAUL,
    0x8BBBF63A463D56F9UL,
    0xF0013F1543A26E64UL,
    0xA8307E9F879EC898UL, 0xCC4C27A4150177CCUL, 0x1B432F2CCA1D3348UL,
    0xDE1D1F8F9F6FA013UL, 0x606602A047A7DDD6UL, 0xD237AB64CC1CB2C7UL,
    0x9B938E7225FCD1D3UL, 0xEC4E03708E0FF476UL, 0xFEB2FBDA3D03C12DUL,
    0xAE0BCED2EE43889AUL, 0x22CB8923EBFB4F43UL, 0x69360D013CF7396DUL,
    0x855E3602D2D4E022UL,
    0x073805BAD01F784CUL,
    0x33E17A133852F546UL, 0xDF4874058AC7B638UL, 0xBA92B29C678AA14AUL,
    0x0CE89FC76CFAADCDUL, 0x5F9D4E0908339E34UL, 0xF1AFE9291F5923B9UL,
    0x6E3480F60F4A265FUL, 0xEEBF3A2AB29B841CUL, 0xE21938A88F91B4ADUL,
    0x57DFEFF845C6D3C3UL, 0x2F006B0BF62CAAF2UL, 0x62F479EF6F75EE78UL,
    0x11A55AD41C8916A9UL,
    0xF229D29084FED453UL,
    0x42F1C27B16B000E6UL, 0x2B1F76749823C074UL, 0x4B76ECA3C2745360UL,
    0x8C98F463B91691BDUL, 0x14BCC93CF1ADE66AUL, 0x8885213E6D458397UL,
    0x8E177DF0274D4711UL, 0xB49B73B5503F2951UL, 0x10168168C3F96B6BUL,
    0x0E3D963B63CAB0AEUL, 0x8DFC4B5655A1DB14UL, 0xF789F1356E14DE5CUL,
    0x683E68AF4E51DAC1UL,
    0xC9A84F9D8D4B0FD9UL,
    0x3691E03F52A0F9D1UL, 0x5ED86E46E1878E80UL, 0x3C711A0E99D07150UL,
    0x5A0865B20C4E9310UL, 0x56FBFC1FE4F0682EUL, 0xEA8D5DE3105EDF9BUL,
    0x71ABFDB12379187AUL, 0x2EB99DE1BEE77B9CUL, 0x21ECC0EA33CF4523UL,
    0x59A4D7521805C7A1UL, 0x3896F5EB56AE7C72UL, 0xAA638F3DB18F75DCUL,
    0x9F39358DABE9808EUL,
    0xB7DEFA91C00B72ACUL,
    0x6B5541FD62492D92UL, 0x6DC6DEE8F92E4D5BUL, 0x353F57ABC4BEEA7EUL,
    0x735769D6DA5690CEUL, 0x0A234AA642391484UL, 0xF6F9508028F80D9DUL,
    0xB8E319A27AB3F215UL, 0x31AD9C1151341A4DUL, 0x773C22A57BEF5805UL,
    0x45C7561A07968633UL, 0xF913DA9E249DBE36UL, 0xDA652D9B78A64C68UL,
    0x4C27A97F3BC334EFUL,
    0x76621220E66B17F4UL,
    0x967743899ACD7D0BUL, 0xF3EE5BCAE0ED6782UL, 0x409F753600C879FCUL,
    0x06D09A39B5926DB6UL, 0x6F83AEB0317AC588UL, 0x01E6CA4A86381F21UL,
    0x66FF3462D19F3025UL, 0x72207C24DDFD3BFBUL, 0x4AF6B6D3E2ECE2EBUL,
    0x9C994DBEC7EA08DEUL, 0x49ACE597B09A8BC4UL, 0xB38C4766CF0797BAUL,
    0x131B9373C57C2A75UL,
    0xB1822CCE61931E58UL,
    0x9D7555B909BA1C0CUL, 0x127FAFDD937D11D2UL, 0x29DA3BADC66D92E4UL,
    0xA2C1D57154C2ECBCUL, 0x58C5134D82F6FE24UL, 0x1C3AE3515B62274FUL,
    0xE907C82E01CB8126UL, 0xF8ED091913E37FCBUL, 0x3249D8F9C80046C9UL,
    0x80CF9BEDE388FB63UL, 0x1881539A116CF19EUL, 0x5103F3F76BD52457UL,
    0x15B7E6F5AE47F7A8UL,
    0xDBD7C6DED47E9CCFUL,
    0x44E55C410228BB1AUL, 0xB647D4255EDB4E99UL, 0x5D11882BB8AAFC30UL,
    0xF5098BBB29D3212AUL, 0x8FB5EA14E90296B3UL, 0x677B942157DD025AUL,
    0xFB58E7C0A390ACB5UL, 0x89D3674C83BD4A01UL, 0x9E2DA4DF4BF3B93BUL,
    0xFCC41E328CAB4829UL, 0x03F38C96BA582C52UL, 0xCAD1BDBD7FD85DB2UL,
    0xBBB442C16082AE83UL,
    0xB95FE86BA5DA9AB0UL,
    0xB22E04673771A93FUL, 0x845358C9493152D8UL, 0xBE2A488697B4541EUL,
    0x95A2DC2DD38E6966UL, 0xC02C11AC923C852BUL, 0x2388B1990DF2A87BUL,
    0x7C8008FA1B4F37BEUL, 0x1F70D0C84D54E503UL, 0x5490ADEC7ECE57D4UL,
    0x002B3C27D9063A3AUL, 0x7EAEA3848030A2BFUL, 0xC602326DED2003C0UL,
    0x83A7287D69A94086UL,
    0xC57A5FCB30F57A8AUL,
    0xB56844E479EBE779UL, 0xA373B40F05DCBCE9UL, 0xD71A786E88570EE2UL,
    0x879CBACDBDE8F6A0UL, 0x976AD1BCC164A32FUL, 0xAB21E25E9666D78BUL,
    0x901063AAE5E5C33CUL, 0x9818B34448698D90UL, 0xE36487AE3E1E8ABBUL,
    0xAFBDF931893BDCB4UL, 0x6345A0DC5FBBD519UL, 0x8628FE269B9465CAUL,
    0x1E5D01603F9C51ECUL,
    0x4DE44006A15049B7UL,
    0xBF6C70E5F776CBB1UL, 0x411218F2EF552BEDUL, 0xCB0C0708705A36A3UL,
    0xE74D14754F986044UL, 0xCD56D9430EA8280EUL, 0xC12591D7535F5065UL,
    0xC83223F1720AEF96UL, 0xC3A0396F7363A51FUL
    ];

version(unittest)
{
    import std.conv : hexString;
}