module roomy {
  import types.*

  import id
  import pds
  import arbiter
  import steward

  pure val users: Set[UserDid] = Set("Alice", "Bob", "Charlie")

  action init = all {
    id::init,
    pds::init,
    arbiter::init,
    steward::init
  }

  action step = any {
    createUserAny,
    createArbiter,
  }

  action createUserAny = all {
    nondet user = users.oneOf()
    pds::createUser(user),

    id::stutter,
    arbiter::stutter,
    steward::stutter
  }

  action createArbiter = all {
    def id = id::n
    arbiter::create(id),

    id::new(1),
    pds::stutter,
    steward::stutter
  }
}

module pds {
  import types.*
  
  var userPdses: UserDid -> UserPds

  action init =
    userPdses' = Map()
  action stutter =
    userPdses' = userPdses

  action createUser(userDid: UserDid): bool = all {
    // User does not already exist
    not(userDid.in(userPdses.keys())),
    // Add user data
    userPdses' = userPdses.put(userDid, {did: userDid, permRepos: Map()})
  }
}

module arbiter {
  import types.*

  var arbiters: Did -> Arbiter
  
  action init =
    arbiters' = Map()
  action stutter =
    arbiters' = arbiters

  action create(did: Did): bool = all {
    // Arbiter doesn't already exist
    not(did.in(arbiters.keys())),
    // Add arbiter
    arbiters' = arbiters.put(did, {
      did: did,
      admins: Set(),
      spaces: Map()
    })
  } 
}

module steward {
  import types.*
  
  action init
  action stutter
}

/// Contains a global counter that can be used to create new IDS.
module id {
  var n: int
  action init =
    n' = 0
  action new(count: int): bool =
    n' = n + count
  action stutter =
    n' = n
}

module types {
  //
  // PDS
  //

  type Did = int
  type UserDid = str
  type SpaceDid = Did
  type Record = str

  /// The PDS data associated to an individual user
  type UserPds = {
    // The DID of the user that this PDS data is for
    did: UserDid,
    // The permissioned repos that the user has on the PDS
    permRepos: SpaceDid -> PermRepo
  }

  /// A permissioned repo
  type PermRepo = {
    // The space that the permissioned repo is a part of
    spaceDid: SpaceDid,
    // The records in the repo
    records: Set[Record]
  }

  //
  // Arbiter
  //

  /// The access a member may have to a permissioned space
  type Access = Read | Write

  /// A permissioned space
  type PermSpace = {
    // The access level for each member in the space
    members: Did -> Access
  }

  /// Data for an arbiter
  type Arbiter = {
    did: Did,
    admins: Set[UserDid],
    spaces: str -> PermSpace,
  }

  //
  // Steward
  //

}