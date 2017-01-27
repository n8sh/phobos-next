module attr;

struct sillyWalk { int i; }

enum isSillyWalk(alias T) = is(typeof(T) == sillyWalk);

import std.typetuple;
alias hasSillyWalk(alias what) = anySatisfy!(isSillyWalk, __traits(getAttributes, what));
enum hasSillyWalk(what) = false;

alias helper(alias T) = T;
alias helper(T) = T;

/** Find all functions with certain attribute.
    See also: https://stackoverflow.com/questions/25555329/d-finding-all-functions-with-certain-attribute
*/
void allWithSillyWalk(alias a, alias onEach)()
{
    pragma(msg, "Processing: " ~ a.stringof);
    foreach(memberName; __traits(allMembers, a))
    {
        // guards against errors from trying to access private stuff etc.
        static if(__traits(compiles, __traits(getMember, a, memberName)))
        {
            alias member = helper!(__traits(getMember, a, memberName));

            // pragma(msg, "looking at " ~ memberName);
            import std.string;
            static if(!is(typeof(member)) && member.stringof.startsWith("module "))
            {
                enum mn = member.stringof["module ".length .. $];
                mixin("import " ~ mn ~ ";");
                allWithSillyWalk!(mixin(mn), onEach);
            }

            static if(hasSillyWalk!(member))
            {
                onEach!member;
            }
        }
    }
}
