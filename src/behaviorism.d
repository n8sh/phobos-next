/** Introspection of interface and in turn behaviour.

    Enables

    - Automatic Behaviour Visualization
    - Automatic Test Stub Construction
 */
module behaviorism;

enum Flag
{
    mutating,
    reordering,
    sorting,
    shuffling,
    subset,
}

alias Dimensionality = uint;
