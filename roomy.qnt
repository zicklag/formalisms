module roomy {
  import types.*

  import id
  import pds
  import arbiter
  import steward

  pure val userDids: Set[UserDid] = Set("Alice", "Bob", "Charlie", "Daffodil", "Eve", "Fred", "Greg")

  action init = all {
    id::init,
    pds::init,
    arbiter::init,
    steward::init
  }

  action step = any {
    createUserAny,
    createArbiterAny,
    addArbiterAdminAny,
    removeArbiterAdminAny,
    createSpaceAny,
  }

  val Inv = all {
    arbiter::Inv,
  }

  action createUserAny = all {
    nondet user = userDids.oneOf()
    pds::createUser(user),

    id::stutter,
    arbiter::stutter,
    steward::stutter
  }

  action createArbiter(userDid) = all {
    def arbiterDid = id::n
    arbiter::create(userDid, arbiterDid),
    id::new(1),
    pds::stutter,
    steward::stutter
  }
  action createArbiterAny = all {
    nondet userDid = userDids.oneOf()
    createArbiter(userDid)
  }

  action addArbiterAdmin(user, arbiter, newAdmin) = all {
    arbiter::addAdmin(user, arbiter, newAdmin),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action addArbiterAdminAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    nondet newAdminDid = userDids.exclude(Set(userDid)).oneOf()
    addArbiterAdmin(userDid, arbiterDid, newAdminDid)
  }

  action removeArbiterAdmin(user, arbiter, removedAdmin) = all {
    arbiter::removeAdmin(user, arbiter, removedAdmin),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action removeArbiterAdminAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    def arbiter = arbiter::arbiters.get(arbiterDid)
    all {
      arbiter.admins.size() >= 2,
      nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
      nondet removedAdmin = arbiter.admins.exclude(Set(userDid)).oneOf()
      removeArbiterAdmin(userDid, arbiterDid, removedAdmin)
    }
  }

  action createSpace(userDid, arbiterDid) = all {
    def spaceKey = id::n
    arbiter::createSpace(userDid, arbiterDid, spaceKey),
    id::new(1),
    pds::stutter,
    steward::stutter
  }
  action createSpaceAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    createSpace(userDid, arbiterDid)
  }

  action setSpaceMemberAccess(userDid, arbiterDid, spaceKey, member, access) = all {
    arbiter::setSpaceMemberAccess(userDid, arbiterDid, spaceKey, member, access),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action setSpaceMemberAccessAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    def arbiter = arbiter::arbiters.get(arbiterDid)
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    nondet spaceKey = arbiter.spaces.keys().oneOf()
    nondet member = userDids.exclude(arbiter.admins).oneOf()
    nondet access = 0.to(1).oneOf()
    setSpaceMemberAccess(userDid, arbiterDid, spaceKey, member, if (access == 0) Read else Write)
  }

  action removeSpaceMember(userDid, arbiterDid, spaceKey, member) = all {
    arbiter::removeSpaceMember(userDid, arbiterDid, spaceKey, member),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action removeSpaceMemberAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    def arbiter = arbiter::arbiters.get(arbiterDid)
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    nondet spaceKey = arbiter.spaces.keys().oneOf()
    def space = arbiter.spaces.get(spaceKey)
    nondet member = space.members.keys().exclude(arbiter.admins).oneOf()
    removeSpaceMember(userDid, arbiterDid, spaceKey, member)
  }
}

module pds {
  import types.*
  
  var users: UserDid -> UserPds

  action init =
    users' = Map()
  action stutter =
    users' = users

  action createUser(userDid: UserDid): bool = all {
    // User does not already exist
    not(userDid.in(users.keys())),
    // Add user data
    users' = users.put(userDid, {did: userDid, permRepos: Map()})
  }
}

module arbiter {
  import types.*

  var arbiters: ArbiterDid -> Arbiter
  
  action init =
    arbiters' = Map()
  action stutter =
    arbiters' = arbiters

  val Inv = all {
    InvAdminWriteSpaces,  
  }
  /// Arbiter admins always have write access to spaces owned by the arbiter
  val InvAdminWriteSpaces: bool = 
    arbiters.keys().forall(arbiterDid =>
      def arbiter = arbiters.get(arbiterDid)
      arbiter.admins.forall(adminDid =>
        arbiter.spaces.keys().forall(spaceKey =>
          def space = arbiter.spaces.get(spaceKey)
          adminDid.in(space.members.keys()) and space.members.get(adminDid) == Write
        ) 
      )
    )

  action create(userDid: UserDid, arbiterDid: ArbiterDid): bool = all {
    // Arbiter doesn't already exist
    not(arbiterDid.in(arbiters.keys())),
    // Add arbiter
    arbiters' = arbiters.put(arbiterDid, {
      did: arbiterDid,
      // Make the creator an admin
      admins: Set(userDid),
      spaces: Map()
    })
  } 

  action addAdmin(userDid: UserDid, arbiterDid: ArbiterDid, newAdmin: UserDid): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // user is admin
      userDid.in(arbiter.admins),
      // Add new admin
      def updAdmins = arbiter.admins.union(Set(newAdmin))
      // Add write access to all arbiter's spaces
      def spaces = arbiter.spaces
      def updSpaces = spaces.keys().mapBy(spaceKey => 
        def space = spaces.get(spaceKey)
        space.with("members", space.members.put(newAdmin, Write))
      )
      def updArbiter = arbiter.with("admins", updAdmins).with("spaces", updSpaces)
      arbiters' = arbiters.set(
        arbiterDid,
        updArbiter
      )
    }
  }

  action removeAdmin(userDid: UserDid, arbiterDid: ArbiterDid, removedAdminDid: UserDid): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // There is more than one admin ( we can't remove the last admin )
      arbiter.admins.size() > 1,
      // user is admin
      userDid.in(arbiter.admins),
      // The user to remove is an admin
      removedAdminDid.in(arbiter.admins),
      // Remove the admin
      def admins = arbiter.admins.exclude(Set(removedAdminDid))
      def updated = arbiter.with("admins", admins)
      arbiters' = arbiters.set(
        arbiterDid,
        updated
      )
    }
  }

  action createSpace(userDid: UserDid, arbiterDid: ArbiterDid, spaceKey: SpaceKey): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // Space key does not exist
      not(spaceKey.in(arbiter.spaces.keys())),
      // Add space key
      def spaces = arbiter.spaces.put(spaceKey, { members: arbiter.admins.mapBy(x => Write) })
      def updatedArbiter = arbiter.with("spaces", spaces)
      arbiters' = arbiters.set(
        arbiterDid,
        updatedArbiter
      )
    }
  }

  /// Set the access that a member has to a space
  action setSpaceMemberAccess(
    userDid: UserDid,
    arbiterDid: ArbiterDid,
    spaceKey: SpaceKey,
    memberDid: UserDid,
    access: Access
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // The member DID is not an admin
      not(memberDid.in(arbiter.admins)),
      // Space exists
      spaceKey.in(arbiter.spaces.keys()),
      def space = arbiter.spaces.get(spaceKey)
      def updMembers = space.members.put(memberDid, access)
      def updSpace = space.with("members", updMembers)
      def updSpaces = arbiter.spaces.set(spaceKey, updSpace)
      def updArbiter = arbiter.with("spaces", updSpaces)
      arbiters' = arbiters.set(arbiterDid, updArbiter)
    }
  }

  action removeSpaceMember(
    userDid: UserDid,
    arbiterDid: ArbiterDid,
    spaceKey: SpaceKey,
    memberDid: UserDid,
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // The member DID is not an admin
      not(memberDid.in(arbiter.admins)),
      // Space exists
      spaceKey.in(arbiter.spaces.keys()),
      def space = arbiter.spaces.get(spaceKey)
      def updMembers = space.members.mapRemove(memberDid)
      def updSpace = space.with("members", updMembers)
      def updSpaces = arbiter.spaces.set(spaceKey, updSpace)
      def updArbiter = arbiter.with("spaces", updSpaces)
      arbiters' = arbiters.set(arbiterDid, updArbiter)
    }
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
  import basicSpells from "./basicSpells"
  export basicSpells.*

  //
  // PDS
  //

  type ArbiterDid = int
  type UserDid = str
  type SpaceKey = int
  type Record = str

  /// The PDS data associated to an individual user
  type UserPds = {
    // The DID of the user that this PDS data is for
    did: UserDid,
    // The permissioned repos that the user has on the PDS
    permRepos: SpaceKey -> PermRepo
  }

  /// A permissioned repo
  type PermRepo = {
    arbiterDid: ArbiterDid,
    // The space that the permissioned repo is a part of
    spaceKey: SpaceKey,
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
    members: UserDid -> Access
  }

  /// Data for an arbiter
  type Arbiter = {
    did: ArbiterDid,
    admins: Set[UserDid],
    spaces: SpaceKey -> PermSpace,
  }

  //
  // Steward
  //

}