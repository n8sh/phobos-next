module sso_string;

struct SSOArray(T, uint smallSize)
{
    union
    {
        Large large;
        Small small;
    }
}
