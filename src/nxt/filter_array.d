import std.algorithm;
import std.range;

enum int[] ints = [1, 2, 3, 4];
alias fn = (int a) { return (a % 2) == 0; };
enum div2 = ints.filter!(fn).array;
