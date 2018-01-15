module hashmap;

import hashmap_or_hashset;
public import hashmap_or_hashset : InsertionStatus, removeAllMatching, filtered;

/** Hash map storing keys of type `K` and values of type `V`.
 */
alias HashMap(K, V,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBinMinCapacity = 1) = HashMapOrSet!(K, V,
                                                            Allocator,
                                                            hasher,
                                                            smallBinMinCapacity);
