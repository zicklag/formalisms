//! # ATProto Private Data Ecosystem Survey
//! 
//! This documents and ecosystem survey that we are doing to evaluate existing
//! ATProto private data proposals and their affordances.

module affordances {
  /// Specific affordances that may be offered by different solutions.
  ///
  /// An implied addition to most of these names is "can" as in `UserCanStorePublicPersonalData`.
  ///
  /// Each affordance is like a specific feature, so these are also considered additive to
  /// each-other.
  type Affordance =
    //
    // User-owned public data
    //

    // Users store public data that is personal and only they can write to it.
    | UserStorePublicPersonalData
    // The user's public data can be subscribed to.
    | UserPublicDataSubscription

    //
    // User-owned private data
    //

    // Users can store private data that is personal and only meant for themselves.
    | UserStorePrivatePersonalData
    // Users can store private data and selectively share it with others.
    | UserStorePrivateSharedData
    // The private data is referenced in public stub records that, by extension,
    // allow the private data to be discovered over the firehose, but not actually
    // fetched without access.
    | UserPrivateDataWithPublicStubs

    //
    // Community-owned data
    //

    // A community can store public data in its own repo.
    | CommunityStorePublicData
    // Community members can write data to the community's shared repo. 
    | CommunityMembersWriteCommunityData
    // A community can store private data that it selectively shares with others ( probably members ).
    | CommunityStorePrivateSharedData
    // Community admins can manage ( add, remove, promote, demote, etc ) members in the community.
    | CommunityAdminsManageMembers
    // There is access control around record reading / writing *to the community repo* such that
    // there are ways to do things like members being able to create posts and edit their own, but
    // not delete others', etc.
    | CommunityDataFlexibleAccessControl
    // There is a way to make a custom auth hook that is required to pass before writes to the
    // community repo are allowed.
    | CommunityAuthWriteHook
}

/// # ATProto PDS
///
/// https://atproto.com/specs/atp
module pds {
  import affordances.*

  pure val affordances = Set(
    UserStorePublicPersonalData,
    UserPublicDataSubscription
  )
}

/// # Permissioned Spaces
///
/// The work-in-progress ATProto permissions data proposal.
///
/// https://dholms.leaflet.pub/3mhj6bcqats2o
module permissioned_spaces {
  import affordances.*

  pure val affordances = Set(
    UserStorePrivatePersonalData,
    UserStorePrivateSharedData,
    CommunityAdminsManageMembers
  )
}

/// # Stratos
/// 
/// https://stratos.zone/
module stratos {
  import affordances.*  

  pure val affordances = Set(
    UserStorePrivateSharedData,
    UserPrivateDataWithPublicStubs,
    CommunityAdminsManageMembers
  )
}

/// # Opensocial Community
///
/// https://opensocial.community/
module opensocial {
  import affordances.*

  pure val affordances = Set(
    CommunityStorePublicData,
    CommunityMembersWriteCommunityData,
    CommunityAdminsManageMembers
  )  
}

/// # Roomy
///
/// https://roomy.space
module roomy {
  import affordances.*
  import pds
  import opensocial
  import stratos
  import permissioned_spaces

  pure val requirements = Set(
    UserStorePublicPersonalData,
    UserStorePrivatePersonalData,

    // I think we may not need this. If the community has private shared
    // data, then we can just use that instead of having personal shared
    // data.
    //
    // UserStorePrivateSharedData,

    CommunityMembersWriteCommunityData,
    CommunityStorePrivateSharedData,
    CommunityAdminsManageMembers,
    CommunityDataFlexibleAccessControl

    // Do we need this for sure? We might for things like timestamps, etc? Or maybe that can be done
    // at the AppView layer and not at the repository layer. 🤔
    //
    // As long as we have `CommunityFlexibleAccessControl` we should be fine without a write hook I
    // think.
    //
    // CommunityAuthWriteHook 
  )

  /// The affordances that stratos + PDS is missing that Roomy needs
  pure val permissionedSpacesMissing = 
    requirements.exclude(union(pds::affordances, permissioned_spaces::affordances))

  /// The affordances that stratos + PDS is missing that Roomy needs
  pure val stratosMissing = 
    requirements.exclude(union(pds::affordances, stratos::affordances))

  /// The affordances that opensocial + PDS is missing that Roomy needs
  pure val opensocialMissing = 
    requirements.exclude(union(pds::affordances, opensocial::affordances))

  /// The affordances missing from all existing solutions
  pure val allMissing = 
    permissionedSpacesMissing
    .intersect(stratosMissing)
    .intersect(opensocialMissing)
}