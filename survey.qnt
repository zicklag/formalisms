/*
# ATProto Private Data Ecosystem Survey

This documents and ecosystem survey that we are doing to evaluate existing
ATProto private data proposals and their affordances.
*/

module affordances {
  type Affordance =
    //
    // User-owned public data
    //
    | UserStorePublicPersonalData
    | NetworkSubscribeToUserPublicData 

    //
    // User-owned private data
    //
    | UserStorePrivatePersonalData
    // Shared meaning others can read
    | UserStorePrivateSharedData

    //
    // Community-owned data
    //
    | CommunityStorePublicData
    | CommunityMembersWriteData
    | CommunityStorePrivateSharedData
    | CommunityAdminsMayManageMembers
    // Flexible as in there are ways to do things like members being able to
    // create posts and edit their own, but not delete others', etc.
    | CommunityFlexibleAccessControl
    // There is a way to make a custom hook that is required to pass before writes
    // to the community repo are allowed.
    | CommunityAuthWriteHook
}

module pds {
  import affordances.*

  pure val affordances = Set(
    UserStorePublicPersonalData,
    NetworkSubscribeToUserPublicData
  )
}

module opensocial {
  import affordances.*

  pure val affordances = Set(
    CommunityStorePublicData,
    CommunityMembersWriteData,
    CommunityAdminsMayManageMembers
  )  
}

module roomy {
  import affordances.*
  import pds
  import opensocial

  pure val requirements = Set(
    UserStorePublicPersonalData,
    UserStorePrivatePersonalData,

    // I think we may not need this. If the community has private shared
    // data, then we can just use that instead of having personal shared
    // data.
    //
    // UserStorePrivateSharedData,

    CommunityMembersWriteData,
    CommunityStorePrivateSharedData,
    CommunityAdminsMayManageMembers,
    CommunityFlexibleAccessControl

    // Do we need this for sure? We might for things like timestamps, etc? Or maybe that can be done
    // at the AppView layer and not at the repository layer. 🤔
    //
    // As long as we have `CommunityFlexibleAccessControl` we should be fine without a write hook I
    // think.
    //
    // CommunityAuthWriteHook 
  )

  // The affordances that the PDS is missing that Roomy needs
  pure val pdsMissing =
    requirements.exclude(pds::affordances);

  // The affordances that opensocial + PDS is missing that Roomy needs
  pure val opensocialMissing = 
    requirements.exclude(union(pds::affordances, opensocial::affordances))
}