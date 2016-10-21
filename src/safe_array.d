module safe_array;

import array_ex : UncopyableArray;
import borrown : Owned;

alias SafeArray(E, bool useGCAllocation = false) = Owned!(UncopyableArray!(E, useGCAllocation));
