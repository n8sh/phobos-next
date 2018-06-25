module enum_ex;

/** relation Direction.
 */
@safe:

/** Enumeration wrapper that uses optimized conversion to string (via `toString`
 * member).
 *
 * See_Also: https://forum.dlang.org/thread/ppndhxvzayedgpbjculm@forum.dlang.org?page=1
 */
struct Enum(E)
if (is(E == enum))
{
    @property string toString() @safe pure nothrow @nogc
    {
        import std.meta : AliasSeq;
        alias members = AliasSeq!(__traits(allMembers, E));
        final switch (_enum)
        {
            static foreach (index, member; members)
            {
                static if (index == 0 ||
                           (__traits(getMember, E, members[index - 1]) !=
                            __traits(getMember, E, member)))
                {
                case __traits(getMember, E, member):
                    return member;
                }
            }
        }
    }
    E _enum;                    // the wrapped enum
    alias _enum this;
}

///
@safe pure unittest
{
    enum X { a,
             b,
             _b = b             // enumerator alias
    }
    alias EnumX = Enum!X;
    assert(EnumX(X.a).toString == "a");
    assert(EnumX(X.b).toString == "b");
    assert(EnumX(X._b).toString == "b"); // alias encodes to original
}

enum Rel
{
    undefined,     // needed for good default-value for `Nullable(Role, Role.init)` in `OpenHashMapOrSet`
    nullValue = undefined,

    isRelatedTo, /**< he most general relation. There is some positive relationship
                  * between A and B, but ConceptNet can't determine what that * relationship
                  is based on the data. This was called * "ConceptuallyRelatedTo" in
                  ConceptNet 2 through 4.  */

    isAKindOf, /**< A is a subtype or a specific instance of B; every A is a B. (We do
                * not make the type-token distinction, because people don't usually
                * make that distinction.) This is the hyponym relation in WordNet. */

    isASubkindOf,               ///< SUMO:`subclass`. BFO:`is_a`. WordNet: isAHyponymOf
    // TODO remove these two aliases because only for nouns, while hyponym is for all part of speech (POS)?

    isASubrelationOf,           ///< SUMO:`subrelation`

    subAttribute,          ///< SUMO:`subAttribute`

    isPartitionedInto,          ///< SUMO:`partition`

    isASiblingClassOf,

    isAnInstanceOf, /// NOTE Normally same as instanceHypernymOf in everyday language
    // isAnInstanceHyponymOf = isAnInstanceOf, ///< WordNet

    // TODO _isAnInstanceOf_AtTime_,       ///< BFO, TODO infers continuant, universal, time

    isAPartOf, /**< A is a part of B. This is the part meronym relation in
                  WordNet. /r/PartOf /c/en/gearshift /c/en/car.
                  SUMO:`part`
               */

    isAProperPartOf, /**< A is a part of B. This is the part meronym relation in
                        WordNet. /r/PartOf /c/en/gearshift /c/en/car
                        SUMO:`properPart`
                     */

    isADemonymInstanceOf, // SUMO-WordNet: See_Also: https://github.com/ontologyportal/sumo/issues/85
    isAnExpressionInstanceOf,   // my own naming

    isAMemberOf, /**< A is a member of B; B is a group that includes A. This is
                    the member meronym relation in WordNet.  SUMO:`member`
                 */

    isTheFamilyNameOf,          // SUMO:`familyName`
    isAGivenNameOf,             // SUMO:`givenName`

    isInvolvedInEvent, /// SUMO:`involvedInEvent`. reversion of BFO:`has_participant`.

    // TODO replace with hasProperty, hasAttribute or hasPropertyAttribute
    demonym,               ///< See_Also: https://en.wikipedia.org/wiki/Demonym. TODO merge can this be merged with `DemonymFn`?

    hasA, /**< B belongs to A, either as an inherent part or due to a social
             construct of possession. HasA is often the reverse of PartOf. /r/HasA
             /c/en/bird /c/en/wing ; /r/HasA /c/en/pen /c/en/ink */

    hasProperty, /**< A has B as a property; A can be described as
                    B. /r/HasProperty /c/en/ice /c/en/solid
                    See_Also: https://english.stackexchange.com/questions/150529/what-is-the-difference-between-property-and-attribute
                    SUMO:`property`
                 */

    /** if you can say: `X` is `A`, then `Y` is an attribute of `X`, note that
        in Swedish you can say: "jag är snickare" (about the role as a
        "snickare"), but in English you have to use an article: "I'm a
        carpenter". Example:
        - I'm glad
        - the flower is red
    */
    attribute,               ///< SUMO:`attribute`
    // is_ = attribute,

    color,                   ///< SUMO:`color`

    hasPropertyAttribute,       // the dog (subject:A) has a weight (property:B) of 1 kilogram (attribute:C)
    // hasAOf = hasPropertyAttribute,
    // TODO _hasAProperty_ofAttribute_

    isCapableOf, /**< Something that A can typically do is B. /r/CapableOf
                    /c/en/knife /c/en/cut */
    // hasAbilityTo = isCapableOf,
    // can = isCapableOf,

    hasRole,                    ///< BFO. SUMO:`subrelation` of `attribute`?

    // or isLocatedNear?
    atLocation, /**<
                   ConceptNet: atLocation
                   SUMO:`located`
                   A is a typical location for B, or A is the inherent location
                   of B. Some instances of this would be considered meronyms in
                   WordNet. /r/AtLocation /c/en/butter /c/en/refrigerator; /r/AtLocation
                   /c/en/boston /c/en/massachusetts */

    isATranslationOf, /**< A and B are _concepts (or assertions) in different
                         languages, and overlap in meaning in such a way that they
                         can be considered translations of each other. (This
                         cannot, of course be taken as an exact equivalence.) */

    isDefinedAs, /**< A and B overlap considerably in meaning, and B is a more
                    explanatory version of A. (This is similar to TranslationOf,
                    but within one language.) */
    hasUsageExampleSentence,
    hasExplanation,             // SUMO:`documentation`

    madeOf,                     ///< WordNet:`substanceHolonym`
    // consistOf = madeOf,

    isKnownFor,
    madeAtLocation,           // location

    isDerivedFrom, /**< A is a word or phrase that appears within B and contributes
                      to B's meaning. /r/DerivedFrom /c/en/pocketbook /c/en/book
                   */

    nounAttributeAdjective, ///< WordNet. A noun for which adjectives express values. The noun weight is an attribute, for which the adjectives light and heavy express values.
    hasDerivationallyRelatedForm, ///< WordNet

    isAMemberOfTopicDomain,       ///< WordNet
    isAMemberOfRegionDomain,      ///< WordNet. Similar to atLocation
    isAMemberOfUsageDomain,       ///< WordNet

    alsoSee,                   ///< WordNet. TODO replace with isRelatedTo
    pertainym,                 ///< WordNet

    disjoint, /// http://www.w3.org/TR/owl-ref/#disjointWith-def. SUMO:`disjoint`. BFO:`disjoint_from`

    participatesIn,             // BFO
    actsIn,
    growsAtLocation,
    attends,
    worksFor,

    worksInAcademicField, // TODO ternary relation worksIn
    writesForPublication, // TODO ternary relation writesFor

    leads,
    coaches,
    ceoOf,
    represents, // SUMO:`represents`
    concerns, // TODO relate

    isAMultipleOf,

    writtenAboutInPublication,

    plays,
    playsInstrument,
    playsIn,
    playsFor,

    shapes,
    cutsInto,
    breaksInto,

    wins,
    loses,

    contributesTo,
    isATopMemberOf,

    livesIn,

    languageSchoolInCity,

    isGrownAtLocation,          // TODO subrelation of atLocation
    isProducedAtLocation,       // TODO subrelation of atLocation

    wasFoundedBy,               // TODO singleton event
    wasCreatedBy,               // TODO singleton event

    editedBy,
    madeBy,
    producedBy,
    manufacturedBy,
    directedBy,
    distributedBy,
    ownedBy,
    managedBy,

    inRoom,

    diedBecauseOf,

    birthplace,          // TODO subrelation of atLocation
    deathplace,             // TODO subrelation of atLocation. SUMO:`deathplace` in reverse
    buriedAtLocation,           // TODO subrelation of atLocation
    isRestingAtLocation,        // TODO subrelation of atLocation
    wasRaisedAtLocation, // TODO subrelation of atLocation. TODO X wasRaisedAtLocation Y <=EQUIVALENT=> raise X => atLocation Y

    hasOfficeIn,                // TODO officeOf(X) atLocation Y
    headquarteredIn,            // TODO headquarterOf(X) atLocation Y

    hasContext,                 /// TODO Remove and use Lemma.context and knet.traversal.hasContext
    isASlangFor,
    isAnIdiomFor,

    isLocatedNear,        // TODO subrelation of atLocation
    isPhysicallyConnectedWith,
    isBorderedBy,         // x isBorderedBy Y === border(X) atLocation border(Y)

    controls,

    causes, /**< A and B are events, and it is typical for A to cause B. */
    // leadsTo = causes,

    entails,                    ///< SUMO:`=>`
    isEquivalentWith,           ///< SUMO:`<=>`

    causesSideEffect,

    decreasesRiskOf,
    treats,

    isASubeventOf, /**< A and B are events, and A happens as a subevent of B. */
    isTheFirstSubeventOf,/**< A and B are events, and A happens as the first subevent of B. */
    isTheLastSubeventOf, /**< A and B are events, and A happens as the last subevent of B. */

    hasPrerequisite, /**< In order for A to happen, B needs to happen; B is a
                        dependency of A. /r/HasPrerequisite/ /c/en/drive/ /c/en/get_in_car/ */

    result,                  ///< SUMO:`result`
    duration,                   ///< SUMO:`duration`
    finishes,                   ///< SUMO:`finishes`. TODO aliases `ends`

    agent,                   ///< SUMO:`agent`
    agentUses,

    hasPurpose,                /**< ConceptNet:`isUsedFor`: A is used for B; the
                                  purpose of A is B. /r/UsedFor /c/en/bridge
                                  /c/en/cross_water */     ///< SUMO:`hasPurpose`
    isUsedFor,

    beginAtTime,
    endAtTime,

    startsBeforeBeginningOf,
    endsBeforeBeginningOf,      ///< SUMO:`earlier`

    hasEmotion, // TODO remove this and use "love" isA "emotion"?

    isMotivatedByGoal, /**< Someone does A because they want result B; A is a step
                          toward accomplishing the goal B. */
    isObstructedBy, /**< A is a goal that can be prevented by B; B is an obstacle in
                       the way of A. */

    causesDesire,

    desires, /**< A is a conscious entity that typically wants B. Many assertions
                of this type use the appropriate language's word for "person" as
                A. /r/Desires /c/en/person /c/en/love */
    eats,
    owns,

    starts,
    // begins = starts,

    kills,
    injures,
    selfinjures,

    acquires,
    affiliatesWith,

    hires,
    sponsors, // TODO related

    creates, /**< B is a process that creates A. /r/CreatedBy /c/en/cake
                /c/en/bake */
    develops,
    produces,
    writes,

    receivesAction,

    isTheSameAs, // TODO should this be same as synonym or translation?
    isAnAliasFor,

    isASynonymFor, /**< A and B have very similar meanings. This is the synonym relation
                      in WordNet as well. */

    isATroponymFor, // A isATroponymFor B: A is a verb that indicates more precisely the manner of doing something by replacing a verb (B) of a more generalized meaning

    isAnObsolescentFor,

    isAnAntonymFor, isAnOppositeOf = isAnAntonymFor, /**< A and B are opposites in some relevant way, such as being
                                                        opposite ends of a scale, or fundamentally similar things with a
                                                        key difference between them. Counterintuitively, two _concepts
                                                        must be quite similar before people consider them antonyms. This
                                                        is the antonym relation in WordNet as well. /r/Antonym
                                                        /c/en/black /c/en/white; /r/Antonym /c/en/hot /c/en/cold */

    isHomonymWith, // Word with same spelling and pronounciation, but different meaning

    isContronymWith,  // Word with same spelling but with different pronounciation and meaning
    isAContranymFor = isContronymWith,
    isAAutoAntonymFor = isAContranymFor,
    isAHomographFor = isAAutoAntonymFor,

    isAHomophoneFor, // Word with same pronounication, but different spelling and meaning

    isTheReversionOf,

    // pattern building blocks
    isASequenceOf,              // TODO SUMO:`listFn`?
    isAnAlternativeOf,          // TODO SUMO mapping
    isAnOptionOf,               // TODO SUMO mapping

    isARepetitionOf,               // TODO SUMO mapping
    isARepetitionZeroOrMoreOf,     // TODO SUMO mapping
    isARepetitionOneOrMoreOf,      // TODO SUMO mapping
    isARepetitionTwoOrMoreOf,      // TODO SUMO mapping

    isARetronymFor, differentation = isARetronymFor, // $(EM acoustic) guitar. https://en.wikipedia.org/wiki/Retronym

    isATogetherWritingFor,

    isASymbolFor,
    isAnAbbreviationFor, shorthandFor = isAnAbbreviationFor,
    isAContractionFor,  // sammandragning

    isAnAcronymFor,
    isAnEmoticonFor,

    arisesFrom,
    emptiesInto,

    isCompoundlyDerivedFrom,

    isEtymologicallyDerivedFrom,
    isEtymologicallyBlendedFrom,

    inheritsFrom,

    // comparison. TODO generalize using adjectives and three way bilink
    isTallerThan,
    isLargerThan,
    isHeavierThan,

    isOlderThan,
    areMoreThan,

    isASymbolOf,
    isANameOf,

    isSimilarTo,
    isSimilarInSizeTo, // TODO generalize to hasSimilarPropertyAs(`size`)
    isSimilarInAppearanceTo, // TODO generalize to hasSimilarPropertyAs(`appearance`)
    looksLike = isSimilarInAppearanceTo, // TODO generalize to hasSimilarPropertyAs(`look`)
    soundsLike,         // TODO generalize to hasSimilarPropertyAs(`appearance`)

    adjectivePertainsTo,
    adverbPertainsTo,
    isParticipleOf,

    belongsToLetterClass,

    belongsToWordClass,         // lexical instance

    hasLexiconCategoryAndSymbol, // SUMO:`lexicon`

    hasPartOfSpeech,
    hasGrammaticalGender, /// See_Also: https://github.com/ontologyportal/sumo/issues/124

    isMorphologicalWordFormOf,
    hasPluralForm,              // TODO infer nouns
    isParticipleOfVerb, // TODO replace with isMorphologicalWordFormOf

    hasNameDay,                 // `namnsdag` in Swedish

    hasWikipediaURL, // final part of the Wikipedia URL, for instance `Upper_ontology` for the URL https://en.wikipedia.org/wiki/Upper_ontology

    isLearnedAtAgeYear,         // TODO SUMO: https://github.com/ontologyportal/sumo/issues/104
    hasAgeYearOfAqcuisition = isLearnedAtAgeYear, // See_Also: https://opendata.stackexchange.com/questions/11057/word-list-with-learning-age/11862#11862

    duringTime,                 // actually a time-period
    isProxyFor,
    isMutualProxyFor,

    competesWith,
    isInvolvedWith,
    collaboratesWith,

    graduatedFrom,
    agentCreated,

    createdAtDate,              // actually a time-period
    destroyedAtDate,            // actually a time-period

    beginActiveYearsAtDate,     // actually a time-period
    endActiveYearsAtDate,       // actually a time-period

    bornInTime,                 // actually a time-period
    diedInTime,                 // actually a time-period
    completedInTime,            // actually a time-period

    bornWithName,

    beginServiceInTime, startedServiceInTime = beginServiceInTime,
    endedServiceInTime,

    beginActiveYearsInTime,
    endedActiveYearsInTime,

    deathplaceAge,

    foundedAtLocation,
    foundedIn,
    foundedInTime, // TODO replace by higher-order predicate: Skänninge city was founded at 1200
    releasedInTime,

    marriedIn,
    marriedInTime,

    chargedWithCrime,

    movedTo, // TODO infers atLocation

    influences,

    isCookedWith,
    isServedWith,
    isWornWith,

    won,

    exists,                     // SUMO:`exists`

    // family relations
    ancestor,                   // SUMO:`ancestor`
    relative,                   // SUMO:`relative`
    familyRelation,             // SUMO:`familyRelation`
    parent,                     // has parent. SUMO:`parent`. NOTE reversion of `child`
    hasFather,                  // has father. SUMO:`father`
    hasMother,                  // has mother. SUMO:`mother`
    hasSpouse,                  // has spouse. SUMO:`spouse`
    hasWife,                    // has wife. SUMO:`wife` in reverse
    hasHusband,                 // has husband. SUMO:`husband` in reverse
    hasSibling,                 // has sibling. SUMO:`sibling`
    hasBrother,                 // has brother. SUMO:`brother`
    hasSister,                  // has sister. SUMO:`sister`
    hasSon,                     // has son. SUMO:`son` in reverse
    hasDaughter,                // has daughter. SUMO:`daughter` in reverse

    // comparisons:
    isEqualTo,
    lessThan,
    greaterThan,

    // SUMO specific:
    hasEdgeDomain,              // SUMO:`domain`
    hasEdgeRange,               // SUMO:`range`

    // TODO double-check these SUMO terms:
    earthAltitude,                  // SUMO:`earthAltitude`
    successorAttribute,             // SUMO:`successorAttribute`
    dateDissolved,                  // SUMO:`dateDissolved`
    manufacturer,                   // SUMO:`manufacturer`
    overlapsSpatially,              // SUMO:`overlapsSpatially`
    lessThanOrEqual,                // SUMO:`lessThanOrEqual`
    relatedInternalConcept,         // SUMO:`relatedInternalConcept`
    containsInformation,            // SUMO:`containsInformation`
    meatOfAnimal,                   // SUMO:`meatOfAnimal`
    independenceDate,               // SUMO:`independenceDate`
    biochemicalAgentDelivery,       // SUMO:`biochemicalAgentDelivery`
    names,                          // SUMO:`names`
    effectiveDose,                  // SUMO:`effectiveDose`
    industryProductType,            // SUMO:`industryProductType`
    possesses,                      // SUMO:`possesses`
    landAreaOnly,                   // SUMO:`landAreaOnly`
    causesSubclass,                 // SUMO:`causesSubclass`
    engineeringSubcomponent,        // SUMO:`engineeringSubcomponent`
    udaCanSignify,                  // SUMO:`udaCanSignify`
    models,                         // SUMO:`models`
    length,                         // SUMO:`length`
    leaderPosition,                 // SUMO:`leaderPosition`
    caliber,                        // SUMO:`caliber`
    disjointRelation,               // SUMO:`disjointRelation`
    contraryAttribute,              // SUMO:`contraryAttribute`
    hasHeadq,                       // SUMO:`hasHeadq`
    exhaustiveAttribute,            // SUMO:`exhaustiveAttribute`
    successorOrganization,          // SUMO:`successorOrganization`
    conventionalShortName,          // SUMO:`conventionalShortName`
    diskTypeForDrive,               // SUMO:`diskTypeForDrive`
    diseaseSymptom,                 // SUMO:`diseaseSymptom`
    lethalDose,                     // SUMO:`lethalDose`
    exhaustiveDecomposition,        // SUMO:`exhaustiveDecomposition`
    groundSubsurfaceType,           // SUMO:`groundSubsurfaceType`
    elevation,                      // SUMO:`elevation`
    biologicalAgentCarrier,         // SUMO:`biologicalAgentCarrier`
    defautlMaximumMass,             // SUMO:`defautlMaximumMass`
    defaultMinimumMeasure,          // SUMO:`defaultMinimumMeasure`
    biochemicalAgentSyndrome,       // SUMO:`biochemicalAgentSyndrome`
    comparativeArea,                // SUMO:`comparativeArea`
    typicalPart,                    // SUMO:`typicalPart`
    localShortName,                 // SUMO:`localShortName`
    governmentType,                 // SUMO:`governmentType`
    disjointDecomposition,          // SUMO:`disjointDecomposition`
    chiefOfStateType,               // SUMO:`chiefOfStateType`
    dateEstablished,                // SUMO:`dateEstablished`
    subsumesContentClass,           // SUMO:`subsumesContentClass`
    identityElement,                // SUMO:`identityElement`
    abstractPart,                   // SUMO:`abstractPart`
    flowsInto,                      // SUMO:`flowsInto`
    naturalHazardTypeInArea,        // SUMO:`naturalHazardTypeInArea`
    defaultMaximumLength,           // SUMO:`defaultMaximumLength`
    trichotomizingOn,               // SUMO:`trichotomizingOn`
    properPartTypes,                // SUMO:`properPartTypes`
    termFormant,                    // SUMO:`termFormant`
    sectorCompositionOfGDPInPeriod, // SUMO:`sectorCompositionOfGDPInPeriod`
    defaultMinimumLength,           // SUMO:`defaultMinimumLength`
    holdsDuring,                    // SUMO:`holdsDuring`
    partlyLocated,                  // SUMO:`partlyLocated`
    subsumesContentInstance,        // SUMO:`subsumesContentInstance`
    economyType,                    // SUMO:`economyType`
    successorAttributeClosure,      // SUMO:`successorAttributeClosure`
    infantMortality,                // SUMO:`infantMortality`
    cardinality,                    // SUMO:`cardinality`
    primaryGeopoliticalSubdivision, // SUMO:`primaryGeopoliticalSubdivision`
    organizationalObjective,        // SUMO:`organizationalObjective`
    initialPart,                    // SUMO:`initialPart`
    capitalCity,                    // SUMO:`capitalCity`
    greaterThanOrEqual,             // SUMO:`greaterThanOrEqual`
    defaultMinimumSphereRadius,     // SUMO:`defaultMinimumSphereRadius`
    defaultMinimumHeight,           // SUMO:`defaultMinimumHeight`
    diseaseMortality,               // SUMO:`diseaseMortality`
    agreementEffectiveDate,         // SUMO:`agreementEffectiveDate`
    currencyValue,                  // SUMO:`currencyValue`
    increasesLikelihood,            // SUMO:`increasesLikelihood`
    boilingPoint,                   // SUMO:`boilingPoint`
    defaultMinimumWidth,            // SUMO:`defaultMinimumWidth`
    molecularRatio,                 // SUMO:`molecularRatio`
    established,                    // SUMO:`established`
    geographicalSubregion,          // SUMO:`geographicalSubregion`
    claimedTerritory,               // SUMO:`claimedTerritory`
    dependentGeopoliticalArea,      // SUMO:`dependentGeopoliticalArea`
    depth,                          // SUMO:`depth`
    commandRankOfEchelon,           // SUMO:`commandRankOfEchelon`
    carCode,                        // SUMO:`carCode`
    Documentation,                  // SUMO:`Documentation`
    currencyType,                   // SUMO:`currencyType`
    initiallyContainsPart,          // SUMO:`initiallyContainsPart`
    localLongName,                  // SUMO:`localLongName`
    meltingPoint,                   // SUMO:`meltingPoint`
    utterance,                      // SUMO:`utterance`
    hasPartTypes,                   // SUMO:`hasPartTypes`
    havePartTypes,                  // SUMO:`havePartTypes`
    populationGrowth,               // SUMO:`populationGrowth`
    habitatOfOrganism,              // SUMO:`habitatOfOrganism`
    subCollection,                  // SUMO:`subCollection`
    meronym,                        // SUMO:`meronym`
    orbits,                         // SUMO:`orbits`
    atomicNumber,                   // SUMO:`atomicNumber`
    defaultMaximumSphereRadius,     // SUMO:`defaultMaximumSphereRadius`
    defaultMeasure,                 // SUMO:`defaultMeasure`
    genlAttribute,                  // SUMO:`genlAttribute`
    subOrganization,                // SUMO:`subOrganization`
    agentOperatesInArea,            // SUMO:`agentOperatesInArea`
    partTypes,                      // SUMO:`partTypes`
    defaultMaximumHeight,           // SUMO:`defaultMaximumHeight`
    subProcess,                     // SUMO:`subProcess`
    longitude,                      // SUMO:`longitude`
    subEchelon,                     // SUMO:`subEchelon`
    defaultMaximumMeasure,          // SUMO:`defaultMaximumMeasure`
    subField,                       // SUMO:`subField`
    nationalHoliday,                // SUMO:`nationalHoliday`
    secretesToxin,                  // SUMO:`secretesToxin`
    citizen,                        // SUMO:`citizen`
    meetsSpatially,                 // SUMO:`meetsSpatially`
    oppositeDirection,              // SUMO:`oppositeDirection`
    inverse,                        // SUMO:`inverse`
    abbrev,                         // SUMO:`abbrev`
    acronym,                        // SUMO:`acronym`
    militaryOfArea,                 // SUMO:`militaryOfArea`
    inflationRateInCountry,         // SUMO:`inflationRateInCountry`
    government,                     // SUMO:`government`
    geopoliticalSubdivision,        // SUMO:`geopoliticalSubdivision`
    decreasesLikelihood,            // SUMO:`decreasesLikelihood`
    groundSurfaceType,              // SUMO:`groundSurfaceType`
    externalImage,                  // SUMO:`externalImage`
    forall,                         // SUMO:`forall`
    formerName,                     // SUMO:`formerName`
    productOfAnimal,                // SUMO:`productOfAnimal`
    defaultMaximumWidht,            // SUMO:`defaultMaximumWidht`
    inScopeOfInterest,              // SUMO:`inScopeOfInterest`
    actionTendency,                 // SUMO:`actionTendency`
    pastTense,                      // SUMO:`pastTense`
    subLanguage,                    // SUMO:`subLanguage`
    traverses,                      // SUMO:`traverses`
    agreementAdoptionDate,          // SUMO:`agreementAdoptionDate`
    modalAttribute,                 // SUMO:`modalAttribute`
    latitude,                       // SUMO:`latitude`
    physicalDomain,                 // SUMO:`physicalDomain`
    taxDeferredIncome,              // SUMO:`taxDeferredIncome`
    qoSSlack,                       // SUMO:`qoSSlack`
    trusts,                         // SUMO:`trusts`
    distrusts,                      // SUMO:`distrusts`
    counselInCase,                  // SUMO:`counselInCase`
    ingredientAmount,               // SUMO:`ingredientAmount`
    customerValue,                  // SUMO:`customerValue`
    releaseForSale,                 // SUMO:`releaseForSale`
    attitudeForObject,              // SUMO:`attitudeForObject`
    attitudeForFormula,             // SUMO:`attitudeForFormula`
    askPrice,                       // SUMO:`askPrice`
    connected,                      // SUMO:`connected`
    totalArea,                      // SUMO:`totalArea`
    geographicSubregion,            // SUMO:`geographicSubregion`
    defaultMaximumWidth,            // SUMO:`defaultMaximumWidth`
    conventionalLongName,           // SUMO:`conventionalLongName`
    headquartersOfOrganization,     // SUMO:`headquartersOfOrganization`
    rangeSubclass,                  // SUMO:`rangeSubclass`
    betweenOnPath,                  // SUMO:`betweenOnPath`
    typicallyContainsPart,          // SUMO:`typicallyContainsPart`

    engineers,                  // SUMO:`engineers`
    employs,                    // SUMO:`employs`
    accountHolder,              // SUMO:`accountHolder`

    signedBy,                   // SUMO:`signedBy`

    listedOn,                   // SUMO:`listedOn`
    liquidity,                  // SUMO:`liquidity`
    riskLevel,                  // SUMO:`riskLevel`
    yieldLevel,                 // SUMO:`yieldLevel`
    limitPrice,                 // SUMO:`limitPrice`
    monthlyIncome,              // SUMO:`monthlyIncome`
    accountNumber,              // SUMO:`accountNumber`
    stockSymbol,                // SUMO:`stockSymbol`
    checkNumber,                // SUMO:`checkNumber`
    netAmount,                  // SUMO:`netAmount`
    riskTolerance,              // SUMO:`riskTolerance`
    confirmationNumber,         // SUMO:`confirmationNumber`
    bankAccount,                // SUMO:`bankAccount`

    accountAt, // SUMO:`accountAt`
    cardAccount, // SUMO:`cardAccount`
    checkAccount, // SUMO:`checkAccount`
    primeInterestRate, // SUMO:`primeInterestRate`
    fixedInterestRate, // SUMO:`fixedInterestRate`
    maturityDate, // SUMO:`maturityDate`
    originalBalance, // SUMO:`originalBalance`
    principalAmount, // SUMO:`principalAmount`
    creditLimit, // SUMO:`creditLimit`
    floorLoan, // SUMO:`floorLoan`
    downPayment, // SUMO:`downPayment`
    amountCharged, // SUMO:`amountCharged`
    lender, // SUMO:`lender`
    borrower, // SUMO:`borrower`
    loanForPurchase, // SUMO:`loanForPurchase`
    securedBy, // SUMO:`securedBy`
    totalBalance, // SUMO:`totalBalance`
    appraisedValue, // SUMO:`appraisedValue`
    benchmark, // SUMO:`benchmark`
    inflationRate, // SUMO:`inflationRate`
    shareOf, // SUMO:`shareOf`
    shareHolder, // SUMO:`shareHolder`
    stockHolder, // SUMO:`stockHolder`
    yield, // SUMO:`yield`
    couponInterest, // SUMO:`couponInterest`
    accruedInterest, // SUMO:`accruedInterest`
    faceValue, // SUMO:`faceValue`
    callDate, // SUMO:`callDate`
    creditRanking, // SUMO:`creditRanking`
    bondRating, // SUMO:`bondRating`
    optionHolder, // SUMO: `SUMO:`optionHolder`
    optionSeller, // SUMO: `optionSeller`
    strikePrice, // SUMO: `strikePrice`
    premium, // SUMO: `premium`
    underlier, // SUMO: `underlier`
    inTheMoney, // SUMO: `inTheMoney`
    atTheMoney, // SUMO: `atTheMoney`
    outOfTheMoney, // SUMO: `outOfTheMoney`
    finalPrice, // SUMO: `finalPrice`
    cardCode, // SUMO: `cardCode`
    pin, // SUMO: `pin`
    phoneNumber, // SUMO: `phoneNumber`
    dayPhone, // SUMO: `dayPhone`
    eveningPhone, // SUMO: `eveningPhone`
    financialAccount, // SUMO: `financialAccount`
    lastStatement, // SUMO: `lastStatement`
    loanInterest, // SUMO: `loanInterest`
    financialResponseTo, // SUMO: `financialResponseTo`
    accountStatus, // SUMO: `accountStatus`
    administrator, // SUMO: `administrator`
    administratorStatus, // SUMO: `administratorStatus`
    dateOfStatement, // SUMO: `dateOfStatement`
    lastStatementBalance, // SUMO: `lastStatementBalance`
    statementAccount, // SUMO: `statementAccount`
    statementPeriod, // SUMO: `statementPeriod`
    statementInterest, // SUMO: `statementInterest`
    loanFeeAmount, // SUMO: `loanFeeAmount`

    // binary predicates:
    issuedBy,
    insured,
    customer,
    emailAddress,
    absorbedDose,
    areaOfOperation,
    coordinates,
    structure,
    observesHoliday,
    states,
    composer,
    keyName,
    fullName,
    fullNameIndexOrder,
    nameIndexOrder,
    agentName,
    humanName,
    organizationName,
    paternalUncle,
    maternalUncle,
    paternalAunt,
    maternalAunt,
    fathersBrothersWife,
    mothersBrothersWife,
    fathersSistersHusband,
    mothersSistersHusband,
    fathersBrothersSon,
    fathersBrothersDaughter,
    mothersBrothersSon,
    mothersBrothersDaughter,
    fathersSistersSon,
    fathersSistersDaughter,
    mothersSistersSon,
    mothersSistersDaughter,
    surfaceWindSpeed,
    surfaceWindDirection,
    lowAltitudeWindSpeed,
    mediumAltitudeWindSpeed,
    highAltitudeWindSpeed,
    windRelativePosition,
    cloudCoverFraction,
    airTemperature,
    seaSurfaceTemperature,
    relativeHumidity,
    precipitationState,
    precipitationRate,
    precipitationAmount,
    jailer,
    attorney,
    recordForAgreement,
    militaryAge,
    fitForMilitaryService,
    militaryExpendituresInUSDollars,
    militaryExpendituresFractionOfGDP,
    older,
    wavelength,
    sententialObject,
    sententialSubject,
    speaksLanguage,
    soundFrequency,
    yearOfFounding,
    physicalEnd,
    effectiveRange,
    powerComponent,
    deviceState,
    discovers,
    measurementReading,
    potentialOfHydrogen,
    fleetSize,
    carries,
    tangent,
    sliceOfFigure,
    sideOfFigure,
    patientMedical,
    parasite,
    conjugate,
    neighbor,
    capacity,
    humanCapacity,
    targetInAttack,
    landlord,
    tenant,
    expects,
    fears,
    hopes,
    doubts,
    disapproves,
    lacks,
    groupMember,
    medicalPatient,
    hostileForces,
    intelligenceQuotient,
    secretesSubstance,
    half,
    third,
    quarter,
    most,
    creator,
    partyToAgreement,
    agreementPeriod,
    agreementExpirationDate,
    agreementActive,
    record,
    stored,
    titles,
    familyName,
    middleName,
    givenName,
    commentator,
    describes,
    registeredItem,
    student,
    almaMater,
    teacher,
    onboard,
    axis,
    waterDepth,
    hasAward,
    occupation,
    directed,
    constructionPeriod,
    contractor,
    hasExpertise,
    hasOccupation,
    protonNumber,
    electronNumber,
    memberCount,
    memberType,
    inventory,
    operator,
    transported,
    cargo,
    serviceProvider,
    serviceRecipient,
    benefits,
    friend,
    coworker,
    cohabitant,
    grandparent,
    grandmother,
    grandfather,
    aunt,
    cousin,
    nephew,
    niece,
    uncle,
    stepfather,
    stepmother,
    alias_,
    workAddress,
    homeAddress,
    deceptiveIdentifier,
    stranger,
    mutualStranger,
    domesticPartner,
    legalGuardian,
    financialAsset,
    affiliatedOrganization,
    ideologicalAffiliationOfOrganization,
    religiousAffiliationOfOrganization,
    allegiance,
    ancestorOrganization,
    anniversary,
    birthday,
    birthdate,
    deathdate,
    equipmentType,
    grammaticalRelation,
    pathInSystem,
    distanceOnPath,
    routeInSystem,
    inString,
    subString,
    stringLength,
    siteForContact,
    postContactSite,
    unitNumber,
    floorCode,
    postNeighborhood,
    postCountry,
    postDistrict,
    postCity,
    postPostcodeArea,
    postStreet,
    postStreetNumber,
    postPostOfficeBox,
    postAddressText,
    telecomContactDevice,
    deviceTelecomNumber,
    telecomCoreNumber,
    telecomCountryCode,
    telecomAreaCode,
    telecomCode2,
    telecomExtension,
    telephoneNumber,
    personalPhoneNumber,
    homePhoneNumber,
    workPhoneNumber,
    mobilePhoneNumber,
    faxNumber,
    voltageMeasure,
    hasUniform,
    conforms,
    approves,
    policyOwner,
    abbreviation,
    covers,
    approximateValue,
    sideEffect,
    internationalDispute,
    illicitDrugConsumer,
    illicitDrugProducer,
    hostedOn,
    categoryOf,
    advertisedOn,
    prohibitedItem,
    reservePrice,
    webStoreAdvertisement,
    unpaidItem,
    confidenceInterval,
    pValue,
    accountAtSite,
    userFeedbackScore,
    viewedItemList,
    registeredUser,
    confirmedRegisteredUser,
    webSeller,
    underageUser,
    webVisitor,
    viewedListing,
    homePage,
    userDatabase,
    siteCatalog,
    searchResult,
    searchQueryRewrite,
    businessUnit,
    crossFunctionalTeamFocus,
    experimentUpdate,
    finalExperimentReport,
    conversionEvent,
    experimentalControlProcess,
    controlGroup,
    experimentalVariableProcess,
    treatmentGroup,
    treatedUser,
    experimentalTreatmentCollection,
    qualifiedExperiment,
    qualifiedPageView,
    qualifiedTreatment,
    treatedPage,
    treatedPageDefinition,
    exclusiveEvent,
    successEvent,
    total,
    meceCollection,
    runningOn,
    computerRunning,
    programRunning,
    directoryOf,
    runsOn,
    startupOf,
    shutdownOf,
    environmentAttributes,
    standardInputDevice,
    standardOutputDevice,
    standardErrorDevice,
    hostOf,
    stateOfProcess,
    rMProgramOf,
    criticalityLevel,
    startupTimeDelay,
    settlingTime,
    maximumReplications,
    ipAddressOf,
    portNumber,
    processID,
    status,
    unitMeasuringPerformance,
    systemMeasured,
    dataID,
    heartBeatRate,
    monitorConnectivityData,
    monitorApplicationData,
    defaultNetwork,
    numberOfCPUs,
    thresholdOf,
    bandwidthOf,
    priority,
    softwarePath,
    simpleDeadline,
    batchLatency,
    batchInterArrival,
    slidingWindowSize,
    dataStreamSlack,
    dependencyType,
    hasDependency,
    dependencyDelay,
    commandLineArguments,
    systemBehavior,
    designPattern,
    taskRelation,
    formOfAdaptation,
    granularity,
    complexity,
    strictness,
    abstractionLevel,
    memorySize,
    computerResponseTo,
    responseTime,
    responseRate,
    requestRate,
    load,
    dataProcessed,
    processAborted,
    task,
    resourceUsed,
    imageResolution,
    hostStatus,
    hostJitter,
    collectRate,
    sendRate,
    coding,
    filename,
    mimeType,
    ingredient,
    shape,
    displayedUpon,
    accessableFromMenu,
    screenOfGUIE,
    accessableFromMenuItem,
    hasGUEState,
    guiElementCovered,
    guiElementCoveredBy,
    guiElementPartiallyCovered,
    guiElementPartiallyCoveredBy,
    guiElementUncovered,
    personTransportCapability,
    maximumPayloadCapacity,
    absoluteHeight,
    mapOfArea,
    waterAreaOnly,
    totalLandBoundary,
    totalCoastline,
    maritimeClaimType,
    climateTypeInArea,
    rainySeasonInArea,
    warmSeasonInArea,
    drySeasonInArea,
    coolSeasonInArea,
    hotSeasonInArea,
    coldSeasonInArea,
    slopeGradient,
    terrainInArea,
    naturalResourceTypeInArea,
    arableLandArea,
    permanentCropLandArea,
    otherLandUseArea,
    irrigatedLandArea,
    environmentalProblemTypeInArea,
    bioindicatorForHabitat,
    totalBiomass,
    dateOpenedForSignature,
    unratifiedSignatoryToAgreement,
    headingWRTTrueNorth,
    headingWRTMagneticNorth,
    headingWRTCompassNorth,
    meanSeaLevel,
    flows,
    tributary,
    connectedDownstream,
    flowCurrent,
    streamOutfall,
    vegetationType,
    regionalIssue,
    approximateDiameter,
    baptismdate,
    baptismplace,
    burialplace,
    totalLengthOfRailwaySystem,
    lengthOfElectrifiedRailway,
    lengthOfMultipleTrackRailway,
    lengthOfBroadGaugeRailway,
    lengthOfDualGaugeRailway,
    lengthOfNarrowGaugeRailway,
    lengthOfStandardGaugeRailway,
    lengthOfUnclassifiedGaugeRailway,
    trackWidth,
    totalLengthOfHighwaySystem,
    lengthOfPavedHighway,
    lengthOfExpresswaySystem,
    lengthOfUnpavedHighway,
    totalLengthOfWaterways,
    totalPipelineInArea,
    lengthOfCrudeOilPipeline,
    lengthOfNaturalGasPipeline,
    lengthOfPetroleumProductPipeline,
    fleetGrossRegisteredTonnage,
    fleetDeadWeightTonnage,
    cargoType,
    marineInventory,
    flagState,
    topSpeed,
    vesselDisplacement,
    vesselDeadWeightTonnage,
    vesselGrossRegisteredTonnage,
    trafficableForTrafficType,
    navigableForShippingTonnage,
    navigableForDraft,
    ladenDraft,
    routeStart,
    routeEnd,
    passengerCapacityMaxNumber,
    providesDestination,
    offers,
    propositionOwner,
    validFor,
    validityPeriod,
    validPaymentType,
    pricePolicy,
    producedOn,
    recordingLength,
    recordingCompany,
    lyricist,
    musicInterpretation,
    songArtist,
    musicVideo,
    discography,
    albumRelease,
    albumArtist,
    albumType,
    albumCoverImage,
    albumLength,
    albumTrack,
    musicGenre,
    anthem,
    contestOrganizer,
    musicChartBy,
    musicChartPeriod,
    originalExpressedInLanguage,
    dampingRatio,
    resonantFrequency,
    typicalTemporalPart,
    typicallyContainsTemporalPart,
    cylinderBore,
    minCylinderVolume,
    maxCylinderVolume,
    compressionRatio,
    pistonStroke,
    engineIdleSpeed,
    governorSpeed,
    coilCount,
    engineCylinders,
    engineDisplacement,
    geneticSubstrateOfVirus,
    hasMolecularStructuralAttribute,
    lifeStageAchieved,
    associatedFunctionality,
    internetCountryCode,
    instance,
    subclass,
    subrelation,
    equal,
    range,
    valence,
    piece,
    component,
    material,
    leader,
    property,
    lessThanOrEqualTo,
    greaterThanOrEqualTo,
    involvedInEvent,
    resourceExhausted,
    independentProbability,
    needs,
    wants,
    considers,
    believes,
    knows,
    inList,
    subList,
    initialList,
    identicalListItems,
    closedOn,
    reflexiveOn,
    irreflexiveOn,
    partialOrderingOn,
    totalOrderingOn,
    equivalenceRelationOn,
    distributes,
    relatedEvent,
    causesProposition,
    copy,
    time,
    exactlyLocated,
    precondition,
    hindersSubclass,
    preventsSubclass,
    prevents,
    hinders,
    refers,
    expressedInLanguage,
    subProposition,
    uses,
    multiplicativeFactor,
    average,
    subset,
    element,
    graphPart,
    subGraph,
    pathLength,
    arcWeight,
    abstractCounterpart,
    subSystem,
    systemPart,
    graphMeasure,
    weight,
    measure,
    linearExtent,
    height,
    radius,
    diameter,
    larger,
    smaller,
    monetaryValue,
    barometricPressure,
    frequency,
    temporalPart,
    beforeOrEqual,
    overlapsTemporally,
    meetsTemporally,
    earlier,
    cooccur,
    date,
    overlapsPartially,
    bottom,
    top,
    side,
    hole,
    grasps,
    transactionAmount,
    developmentalForm,
    inhabits,
    home,
    stays,
    authors,
    editor,
    publishes,
    version_,
    wears,
    daughter,
    son,
    sibling,
    legalRelation,
    acquaintance,
    mutualAcquaintance,
    premise,
    conclusion,
    consistent,
    faces,
    truth,
    holdsRight,
    holdsObligation,
    geometricPart,
    pointOfFigure,
    angleOfFigure,
    parallel,
    angularMeasure,
    lineMeasure,
    totalGDP,
    realGrowthRateOfGDP,
    perCapitaGDP,
    populationFractionBelowPovertyLine,
    lowestDecileShareOfHouseholdIncome,
    highestDecileShareOfHouseholdIncome,
    incomeDistributionByGiniIndex,
    inflationRateOfConsumerPrices,
    laborForceTotal,
    unemploymentRateOfArea,
    annualRevenuesOfArea,
    annualExpendituresOfArea,
    capitalExpendituresOfArea,
    industryOfArea,
    organizationProductType,
    industryServiceType,
    organizationServiceType,
    resultType,
    industrialProductionGrowthRate,
    annualElectricityProduction,
    annualElectricityConsumption,
    annualElectricityExport,
    annualElectricityImport,
    agriculturalProductType,
    annualExportTotal,
    exportCommodityType,
    exportPartner,
    annualImportTotal,
    importCommodityType,
    importPartner,
    externalDebt,
    economicAidDonated,
    economicAidReceivedNet,
    currencyCode,
    currencyExchangePerUSDollar,
    currencyExchangeRate,
    fiscalYearPeriod,
    administrativeCenter,
    nationalCelebration,
    commemoratesDate,
    holidayTimeInArea,
    legalSystemType,
    suffrageAgeMinimum,
    suffrageAgeMaximum,
    executiveBranch,
    electionForPosition,
    electionForOrganization,
    legislativeBranch,
    chamberOfLegislature,
    seatsInOrganizationCount,
    judicialBranch,
    politicalPartyOfCountry,
    associateInOrganization,
    aimOfOrganization,
    diplomaticRelations,
    flagDescription,
    flagImage,
    facility,
    businessHours,
    standardRetailHours,
    dressCode,
    deliveryRegion,
    reservingEntity,
    reservationChannel,
    fulfillingEntity,
    reservationStart,
    reservationEnd,
    numberOfCustomers,
    potentialCustomer,
    expectedDegree,
    colleague,
    expectedYearOfGraduation,
    classmate,
    parentcompany,
    foundingdate,
    agriculturalArtifactType,
    canCarry,
    widthLimit,
    lengthLimit,
    massLimit,
    sphereRadius,
    organizationRepresentative,
    designs,
    filmCinematographer,
    childOf,
    stepChildOf,
    ActivePolicy,
    policyEffectiveDate,
    policyPeriod,
    serviceInstrument,
    facebookFriend,
    tags,
    externalVideoCaption,
    externalImageCaption,
    numberFunctionRoom,
    numberBar,
    numberRestaurant,
    hotelAmenity,
    hotelPolicy,
    annualEnergyConsumption,
    idleStatePowerConsumption,
    freshFoodCompartmentVolume,
    froozenFoodCompartmentVolume,
    energyConsumptionPerWashingCycle,
    waterConsumptionPerWashingCycle,
    clothesWasherWaterFactor,
    washPerformanceClass,
    spinDryingPerformanceClass,
    maximumSpinSpeed,
    totalCottonCapicity,
    laundryApplianceMaximumClothesVolume,
    clothesWasherModifiedEnergyFactor,
    platePlacesCount,
    servingPiecesCount,
    waterUsePerDishwasherCycle,
    videoPlaybackPowerConsumption,
    videoRecordingPowerConsumption,
    audioPlaybackPowerConsumption,
    audioRecordingPowerConsumption,
    amplifierEfficiency,
    averageChargerEnergyRatio,
    guest,
    allRoomsPhysicalAmenity,
    someRoomsPhysicalAmenity,
    someRoomsServiceAmenity,
    allRoomsServiceAmenity,
    allRoomsPolicy,
    someRoomsPolicy,
    someRoomsAttribute,
    roomAmenity,
    freeRoomAmenity,
    paidRoomAmenity,
    roomPolicy,
    roomAttribute,
    maxRoomCapacity,
    propertyAmenity,
    freePropertyAmenity,
    paidPropertyAmenity,
    activityCapability,
    propertyPolicy,
    orgStaff,
    checkInTime,
    checkOutTime,
    numberOfFloors,
    yearBuilt,
    lastRenovation,
    colocatedAgent,
    functionRoomAmenity,
    freeFunctionRoomAmenity,
    paidFunctionRoomAmenity,
    nearOrientation,
    onOrientation,
    adjacentOrientation,
    viewType,
    productBrand,
    brandIcon,
    subBrand,
    includedMeal,
    policyLocationCoverage,
    accommodationProvider,
    physicalInclusion,
    processInclusion,
    policyInclusion,
    mealPlanInclusion,
    numberOccupant,
    numberAdultOccupant,
    numberChildOccupant,
    numberSeniorOccupant,
    reservedRoom,
    reservedPackage,
    rateDetail,
    ratingsAgent,

    // ternary predicates:
    lexicon,
    interestEarned, // SUMO:`interestEarned`
    simpleInterest, // SUMO:`simpleInterest`
    compoundInterest, // SUMO:`compoundInterest`
    interestRatePerPeriod, // SUMO:`interestRatePerPeriod`
    currentInterestRate, // SUMO:`currentInterestRate`
    amountDue, // SUMO:`amountDue`
    currentAccountBalance, // SUMO:`currentAccountBalance`
    minimumBalance, // SUMO:`minimumBalance`
    periodicPayment, // SUMO:`periodicPayment`
    minimumPayment, // SUMO:`minimumPayment`
    overdraft, // SUMO:`overdraft`
    accountsBalance,
    netWorth,                   // SUMO:`netWorth`
    serviceFee, // SUMO:`serviceFee`
    potentialLoss, // SUMO:`potentialLoss`
    price,
    bidPrice, // SUMO:`bidPrice`
    orderFor, // SUMO:`orderFor`
    splitFor, // SUMO:`splitFor`
    income, // SUMO: `income`
    incomeEarned, // SUMO: `incomeEarned`
    afterTaxIncome, // SUMO: `afterTaxIncome`
    beforeTaxIncome, // SUMO: `beforeTaxIncome`
    employeeContribution, // SUMO: `employeeContribution`
    compensationPackage, // SUMO: `compensationPackage`
    closingPrice, // SUMO: `closingPrice`
    customerRepresentative, // SUMO: `customerRepresentative`
    availableBalance, // SUMO: `availableBalance`
    availableCash, // SUMO: `availableCash`
    paymentsPerPeriod, // SUMO: `paymentsPerPeriod`
    purchasesPerPeriod, // SUMO: `purchasesPerPeriod`
    creditsPerPeriod, // SUMO: `creditsPerPeriod`
    dailyLimit, // SUMO: `dailyLimit`
    buyingPowerAmount, // SUMO: `buyingPowerAmount`
    marginBalanceAmount, // SUMO: `marginBalanceAmount`
    shortBalanceAmount, // SUMO: `shortBalanceAmount`
    marketValueAmount, // SUMO: `marketValueAmount`
    capacityByArrangement,
    roomStay,
    detainedAtTimeInPlace,
    exactCardinality,
    minCardinality,
    maxCardinality,
    codeMapping,                    // SUMO:`codeMapping`
    titleInLanguage,
    alternativeTitle,
    sortingTitle,
    displayTitle,
    abbreviatedDisplayTitle,
    referenceTitle,
    originalTitle,
    groupingTitle,
    misspelledTitle,
    subtitle,
    translatedTitle,
    nameAfterKeyName,
    nameBeforeKeyName,
    monitorComponentData,
    benchmarkPerformance,
    beliefGroupPercentInRegion,     // SUMO:`beliefGroupPercentInRegion`
    ethnicityPercentInRegion,       // SUMO:`ethnicityPercentInRegion`
    languagePercentInRegion,        // SUMO:`languagePercentInRegion`. generalizes `usesLanguage`
    ageOfMajorityForProcess,
    militaryExpendituresInUSDollarsInPeriod,
    militaryExpendituresFractionOfGDPInPeriod,
    surfaceWindVelocity,
    lowAltitudeWindVelocity,
    mediumAltitudeWindVelocity,
    highAltitudeWindVelocity,
    daylightHoursInterval,
    daylightHoursTotal,
    overcastDaysInPeriod,
    averageTemperatureForPeriod,
    highestTemperatureForPeriod,
    lowestTemperatureForPeriod,
    averagePrecipitationForPeriod,
    totalPrecipitationForPeriod,
    visibilityInMeteorology,
    rainfallIntensity,
    snowfallIntensity,
    biochemicalAgentAntidote,       // SUMO:`biochemicalAgentAntidote`
    diseaseTreatment,               // SUMO:`diseaseTreatment`
    diseaseIncubation,              // SUMO:`diseaseIncubation`
    dateUsed,
    nounGender,
    memberTypeCount,
    subordinateInOrganization,
    subordinatePosition,
    playsRoleInEvent,
    locatedAtTime,
    equipmentCount,
    stringConcatenation,
    precedesInString,
    canonicalPlaceName,
    uniqueIdWithRespectTo,
    buys,
    statisticalPopulation,
    tTest,
    webcart,
    browserID,
    webPurchases,
    webSales,
    visitorParameter,
    watchingListings,
    firstTimeBuyers,
    firstTimeSellers,
    newRegisteredUsers,
    disputedPossession,
    drugShipmentDestination,
    illicitDrugShipmentDestination,
    illicitDrugTransshipmentPoint,
    contraryAttributeWRT,
    classIntersection,              // SUMO:`classIntersection`
    weaponCarryingCapability,
    maximumPayloadHeightWidth,
    objectGeographicCoordinates,    // SUMO:`objectGeographicCoordinates`
    sharedBorderLength,             // SUMO:`sharedBorderLength`
    averageRainfallForPeriod,
    slopeGradientTowardsOrientation,
    courseWRTTrueNorth,
    magneticVariation,
    courseWRTMagneticNorth,
    courseWRTCompassNorth,
    relativeBearing,
    highTide,
    lowTide,
    vegetationTypePattern,
    fOCShipsByOrigin,
    totalFacilityTypeInArea,
    routeBetween,
    transitwayCapacityCount,
    transitwayCapacityRate,
    offersAtTime,
    negotiatedPrice,
    productPrice,
    contractedRentalPrice,
    musician,
    contestEntry,
    contestParticipantRepresentation,
    releaseForConsumption,
    communicationSatelliteForArea,
    domain,
    domainSubclass,                 // SUMO:`domainSubclass`
    documentation,
    format,                         // SUMO:`format`
    termFormat,                     // SUMO:`termFormat`
    relatedExternalConcept,         // SUMO:`relatedExternalConcept`
    synonymousExternalConcept,      // SUMO:`synonymousExternalConcept`
    subsumingExternalConcept,       // SUMO:`subsumingExternalConcept`
    subsumedExternalConcept,        // SUMO:`subsumedExternalConcept`
    greaterThanByQuality,
    conditionalProbability,
    prefers,
    capability,                     // SUMO:`capability`
    hasPurposeForAgent,
    confersNorm,
    deprivesNorm,
    between,                        // SUMO:`between`
    representsForAgent,             // SUMO:`representsForAgent`
    representsInLanguage,
    links,
    distance,
    temporallyBetween,
    temporallyBetweenOrEqual,
    connects,                       // SUMO:`connects`
    orientation,                    // SUMO:`orientation`
    occupiesPosition,
    confersRight,
    confersObligation,
    pointOfIntersection,
    geometricDistance,
    comment,                        // SUMO:`comment`
    dependentAreaOfType,
    agreementRevisionDate,
    chiefOfState,
    headOfGovernment,
    roleAppointsRole,
    roleApprovesRole,
    roleNominatesRole,
    termLength,
    electionDatePlannedForPosition,
    candidateForPosition,
    electionWinner,
    seatsWonInElection,
    seatsHeldInOrganization,
    associateWithStatus,
    diplomaticRepresentationType,
    representativeAgentToAgent,
    diplomaticOrganizationType,
    chanceryAddressInArea,
    chanceryMailingAddressInArea,
    chanceryTelephoneNumberInArea,
    chanceryFAXNumberInArea,
    corkageFee,
    subjectiveAttribute,
    contestObject,
    totalGDPInPeriod,               // SUMO:`totalGDPInPeriod`
    realGrowthRateOfGDPInPeriod,
    perCapitaGDPInPeriod,
    sectorCompositionOfGDP,
    sectorValueOfGDP,
    populationFractionBelowPovertyLineInPeriod,
    lowestDecileShareOfHouseholdIncomeInPeriod,
    highestDecileShareOfHouseholdIncomeInPeriod,
    incomeDistributionByGiniIndexInPeriod,
    inflationRateOfConsumerPricesInPeriod,
    laborForceTotalInPeriod,
    laborForceFractionByOccupation,
    unemploymentRateOfAreaInPeriod,
    annualRevenuesOfAreaInPeriod,
    annualExpendituresOfAreaInPeriod,
    capitalExpendituresOfAreaInPeriod,
    industryRankByOutput,
    industrialProductionGrowthRateInPeriod,
    electricityProductionInPeriod,
    electricityFractionFromSource,
    electricityConsumptionInPeriod,
    electricityExportInPeriod,
    electricityImportInPeriod,
    agriculturalProductTypeByRank,
    exportTotalInPeriod,
    exportCommodityTypeByRank,
    exportPartnerInPeriod,
    exportPartnerByRank,
    exportPartnerByFraction,
    importTotalInPeriod,
    importCommodityTypeByRank,
    importPartnerInPeriod,
    importPartnerByRank,
    importPartnerByFraction,
    externalDebtInPeriod,
    economicAidDonatedInPeriod,
    economicAidReceivedNetInPeriod,
    currencyExchangeRateInPeriod,
    memberAtTime,
    agriculturalArtifactTypeByRank,
    basedIn,
    weddingdate,
    areaOfResponsibility,

    // functions:

    complementFunction,         // SUMO:`complementFn`

    // arithmetic functions:
    arithmeticNegationFn,       // negation
    arithmeticInversionFn,      // inversion
    arithmeticAdditionFn,       // addition (plus)
    arithmeticSubtractionFn,    // subtration (minus)
    arithmeticMultiplicationFn, // multiplication (times). SUMO: `MultiplicationFn`
    arithmeticDivisionFn,       // division (div)

    // logical Functions:
    // TODO move these to another enum `FUN` when `Fun` has been added
    logicalAndFunction,
    logicalOrFunction,
    logicalXorFunction,
    logicalNotFunction,

    immediateFutureFn,          // SUMO:`ImmediateFutureFn`,
    currencyFn,                 // SUMO: `CurrencyFn`
    accountFn,                  // SUMO: `AccountFn`
    receivingAnObjectFn,        // SUMO: `ReceivingAnObjectFn`
    populationFn,               // SUMO: `PopulationFn`
    maleToFemaleRatioFn,        // SUMO: `MaleToFemaleRatioFn`
    beliefGroupMemberFn,        // SUMO: `BeliefGroupMemberFn`
    addressFn,                  // SUMO: `AddressFn`
    startupFn,                  // SUMO: `StartupFn`
    shutdownFn,                 // SUMO: `ShutdownFn`
    initialProfileFn,           // SUMO: `InitialProfileFn`
    cpuUtilizationFn,           // SUMO: `CpuUtilizationFn`
    availableForMilitaryServiceMaleFn, // SUMO: `AvailableForMilitaryServiceMaleFn`
    fitForMilitaryServiceMaleFn,       // SUMO: `FitForMilitaryServiceMaleFn`
    deadFn,                            // SUMO: `DeadFn`
    startFn,                           // SUMO: `StartFn`
    stopFn,                            // SUMO: `StopFn`
    diameterFn,                        // SUMO: `DiameterFn`
    radiusFn,                          // SUMO: `RadiusFn`
    lastFn,                            // SUMO: `LastFn`
    firstFn,                           // SUMO: `FirstFn`
    descendantsFn,                     // SUMO: `DescendantsFn`
    residentFn,                        // SUMO: `ResidentFn`
    citizenryFn,                       // SUMO: `CitizenryFn`
    operatingFn,                       // SUMO: `OperatingFn`
    occupationFn,                      // SUMO: `OccupationFn`
    stringLengthFn,                    // SUMO: `StringLengthFn`
    coveringFn,                        // SUMO: `CoveringFn`
    demonymFn,                         // SUMO: `DemonymFn`. TODO merge with `demonym`?
    agentOfOrganismFn,                 // SUMO: `AgentOfOrganismFn`
    conversionRateFn,                  // SUMO: `ConversionRateFn`
    boughtItemsFn,                     // SUMO: `BoughtItemsFn`
    bidCountFn,                        // SUMO: `BidCountFn`
    gmbFn,                             // SUMO: `GmbFn`
    gmvFn,                             // SUMO: `GmvFn`
    qualifyingPurchasesFn,             // SUMO: `QualifyingPurchasesFn`
    ppsFn,                             // SUMO: `PpsFn`
    auctionGMBFn,                      // SUMO: `AuctionGMBFn`
    abpFn,                             // SUMO: `AbpFn`
    aspFn,                             // SUMO: `AspFn`
    srpEngagementFn,                   // SUMO: `SrpEngagementFn`
    siteSpeedFn,                       // SUMO: `SiteSpeedFn`
    qpViewsFn,                         // SUMO: `QpViewsFn`
    qualifyingEventsFn,                // SUMO: `QualifyingEventsFn`
    qualifiedTreatmentsFn,             // SUMO: `QualifiedTreatmentsFn`
    treatedUsersFn,                    // SUMO: `TreatedUsersFn`
    pressingKeyFn,                     // SUMO: `PressingKeyFn`
    juiceOfFn,                         // SUMO: `JuiceOfFn`
    farmOfProductFn,                   // SUMO: `FarmOfProductFn`
    geographicCenterFn,                // SUMO: `GeographicCenterFn`
    perimeterAreaFn,                   // SUMO: `PerimeterAreaFn`
    maritimeClaimsTerritorialSeaFn,    // SUMO: `MaritimeClaimsTerritorialSeaFn`
    innerBoundaryFn,                   // SUMO: `InnerBoundaryFn`
    outerBoundaryFn,                   // SUMO: `OuterBoundaryFn`
    maritimeContiguousZoneFn,          // SUMO: `MaritimeContiguousZoneFn`
    maritimeShelfAreaFn,               // SUMO: `MaritimeShelfAreaFn`
    maritimeExclusiveEconomicZoneFn, // SUMO: `MaritimeExclusiveEconomicZoneFn`
    exclusiveFishingZoneFn,          // SUMO: `ExclusiveFishingZoneFn`
    extendedFishingZoneFn,           // SUMO: `ExtendedFishingZoneFn`
    territorialSeaFn,                // SUMO: `TerritorialSeaFn`
    elevationLowPointFn,             // SUMO: `ElevationLowPointFn`
    elevationHighPointFn,            // SUMO: `ElevationHighPointFn`
    shortageFn,                      // SUMO: `ShortageFn`
    documentFn,                      // SUMO: `DocumentFn`
    agreementOrganizationFn,         // SUMO: `AgreementOrganizationFn`
    squareUnitFn,                    // SUMO: `SquareUnitFn`
    datumFn,                         // SUMO: `DatumFn`
    flowFn,                          // SUMO: `FlowFn`
    flowRegionFn,                    // SUMO: `FlowRegionFn`
    portFacilityFn,                  // SUMO: `PortFacilityFn`
    merchantMarineFn,                // SUMO: `MerchantMarineFn`
    shipRegisterFn,                  // SUMO: `ShipRegisterFn`
    transportationFn,                // SUMO: `TransportationFn`
    tripFn,                          // SUMO: `TripFn`
    transitFn,                       // SUMO: `TransitFn`
    shipBerthingFn,                  // SUMO: `ShipBerthingFn`
    shipCrewFn,                      // SUMO: `ShipCrewFn`
    playingInstrumentFn,             // SUMO: `PlayingInstrumentFn`
    musicalComponentFn,              // SUMO: `MusicalComponentFn`
    lyricalComponentFn,              // SUMO: `LyricalComponentFn`
    nationalAnthemFn,                // SUMO: `NationalAnthemFn`
    overseasAreaFn,                  // SUMO: `OverseasAreaFn`
    constitutionFn,                  // SUMO: `ConstitutionFn`
    regionalLawFn,                   // SUMO: `RegionalLawFn`
    executiveBranchFn,               // SUMO: `ExecutiveBranchFn`
    cabinetFn,                       // SUMO: `CabinetFn`
    electionFn,                      // SUMO: `ElectionFn`
    votingFn,                        // SUMO: `VotingFn`
    memberFn,                        // SUMO: `MemberFn`
    legislatureFn,                   // SUMO: `LegislatureFn`
    judiciaryFn,                     // SUMO: `JudiciaryFn`
    supremeCourtFn,                  // SUMO: `SupremeCourtFn`
    fiscalYearFn,                    // SUMO: `FiscalYearFn`
    powerSetFn,                      // SUMO: `PowerSetFn`
    frontFn,                         // SUMO: `FrontFn`
    backFn,                          // SUMO: `BackFn`
    extensionFn,                     // SUMO: `ExtensionFn`
    probabilityFn,                   // SUMO: `ProbabilityFn`
    listLengthFn,                    // SUMO: `ListLengthFn`
    propertyFn,                      // SUMO: `PropertyFn`
    absoluteValueFn,                 // SUMO: `AbsoluteValueFn`
    ceilingFn,                       // SUMO: `CeilingFn`
    cosineFn,                        // SUMO: `CosineFn`
    denominatorFn,                   // SUMO: `DenominatorFn`
    floorFn,                         // SUMO: `FloorFn`
    imaginaryPartFn,                 // SUMO: `ImaginaryPartFn`
    integerSquareRootFn,             // SUMO: `IntegerSquareRootFn`
    numeratorFn,                     // SUMO: `NumeratorFn`
    rationalNumberFn,                // SUMO: `RationalNumberFn`
    realNumberFn,                    // SUMO: `RealNumberFn`
    reciprocalFn,                    // SUMO: `ReciprocalFn`
    roundFn,                         // SUMO: `RoundFn`
    signumFn,                        // SUMO: `SignumFn`
    sineFn,                          // SUMO: `SineFn`
    squareRootFn,                    // SUMO: `SquareRootFn`
    tangentFn,                       // SUMO: `TangentFn`
    successorFn,                     // SUMO: `SuccessorFn`
    predecessorFn,                   // SUMO: `PredecessorFn`
    complementFn,                    // SUMO: `ComplementFn`
    generalizedUnionFn,              // SUMO: `GeneralizedUnionFn`
    generalizedIntersectionFn,       // SUMO: `GeneralizedIntersectionFn`
    cardinalityFn,                   // SUMO: `CardinalityFn`
    initialNodeFn,                   // SUMO: `InitialNodeFn`
    terminalNodeFn,                  // SUMO: `TerminalNodeFn`
    beginNodeFn,                     // SUMO: `BeginNodeFn`
    endNodeFn,                       // SUMO: `EndNodeFn`
    pathWeightFn,                    // SUMO: `PathWeightFn`
    cutSetFn,                        // SUMO: `CutSetFn`
    minimalCutSetFn,                 // SUMO: `MinimalCutSetFn`
    magnitudeFn,                     // SUMO: `MagnitudeFn`
    unitFn,                          // SUMO: `UnitFn`
    centerOfCircleFn,                // SUMO: `CenterOfCircleFn`
    wealthFn,                        // SUMO: `WealthFn`
    beginFn,                         // SUMO: `BeginFn`
    endFn,                           // SUMO: `EndFn`
    whenFn,                          // SUMO: `WhenFn`
    pastFn,                          // SUMO: `PastFn`
    immediatePastFn,                 // SUMO: `ImmediatePastFn`
    futureFn,                        // SUMO: `FutureFn`
    yearFn,                          // SUMO: `YearFn`
    morningFn,                       // SUMO: `MorningFn`
    afternoonFn,                     // SUMO: `AfternoonFn`
    eveningFn,                       // SUMO: `EveningFn`
    holeHostFn,                      // SUMO: `HoleHostFn`
    holeSkinFn,                      // SUMO: `HoleSkinFn`
    foodForFn,                       // SUMO: `FoodForFn`
    immediateFamilyFn,               // SUMO: `ImmediateFamilyFn`
    governmentFn,                    // SUMO: `GovernmentFn`
    premisesFn,                      // SUMO: `PremisesFn`
    makingFn,                        // SUMO: `MakingFn`
    perDiemFn,                       // SUMO: `PerDiemFn`
    dayBeforeFn,                     // SUMO: `DayBeforeFn`
    classOnSportFn,                  // SUMO: `ClassOnSportFn`
    classOnAcademicFieldFn,          // SUMO: `ClassOnAcademicFieldFn`
    classOnLanguageFn,               // SUMO: `ClassOnLanguageFn`
    lessonOnInstrumentFn,            // SUMO: `LessonOnInstrumentFn`
    lessonOnCookingFoodFn,           // SUMO: `LessonOnCookingFoodFn`
    tourOnAreaFn,                    // SUMO: `TourOnAreaFn`
    agricultureFn,                   // SUMO: `AgricultureFn`
    hasDegreeFn,                     // SUMO: `HasDegreeFn`
    yearsActiveFn,                   // SUMO: `YearsActiveFn`
    religiousDoctrineFn,             // SUMO: `ReligiousDoctrineFn`
    webPageOfFn,                     // SUMO: `WebPageOfFn`
    accountOfServiceFn,              // SUMO: `AccountOfServiceFn`
    measureFn,                       // SUMO: `MeasureFn`
}

@safe pure unittest
{
    alias EnumRel = Enum!Rel;
    EnumRel enumRel;
}
