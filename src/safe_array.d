module safe_array;

import array_ex : UncopyableArray;
import borrown : Owned;

alias SafeUncopyableArray(E, bool useGCAllocation = false) = Owned!(UncopyableArray!(E, useGCAllocation));
alias SafeCopyableArray(E, bool useGCAllocation = false) = Owned!(CopyableArray!(E, useGCAllocation));
