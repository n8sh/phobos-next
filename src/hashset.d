module hashset;

import hashmap_or_hashset;
public import hashmap_or_hashset : InsertionStatus;

/** Hash map storing keys of type `K`.
 */
alias HashSet(K,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBinMinCapacity = 1) = HashMapOrSet!(K, void,
                                                            Allocator,
                                                            hasher,
                                                            smallBinMinCapacity);
