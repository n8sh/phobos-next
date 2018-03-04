module soo_hashmap;

import sso_hashmap_or_hashset;
public import sso_hashmap_or_hashset : removeAllMatching, filtered, byElement, intersectedWith;

/** Hash map storing keys of type `K` and values of type `V`.
 */
alias HashMap(K, V,
              alias Allocator = null,
              alias hasher = hashOf,
              uint smallBinMinCapacity = 1) = HashMapOrSet!(K, V,
                                                            Allocator,
                                                            hasher,
                                                            smallBinMinCapacity);
