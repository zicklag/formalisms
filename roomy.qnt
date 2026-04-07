module roomy {
  import types.*

  import id
  import pds
  import arbiter
  import steward

  pure val userDids: Set[UserDid] = Set("Alice", "Bob", "Charlie", "Daffodil", "Eve", "Fred", "Greg")
  pure val roleIds: Set[RoleId] = Set("members", "team", "collaborators")

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
    setSpaceMemberAccessAny,
    createRoleAny,
    addRoleMemberAny,
    removeRoleMemberAny,
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
    nondet member = userDids.map(x => User(x)).union(arbiter.roles.keys().map(x => Role(x))).oneOf()
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
    nondet member = space.members.keys().oneOf()
    removeSpaceMember(userDid, arbiterDid, spaceKey, member)
  }

  action createRole(userDid, arbiterDid, roleId) = all {
    arbiter::createRole(userDid, arbiterDid, roleId),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action createRoleAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    def arbiter = arbiter::arbiters.get(arbiterDid)
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    nondet roleId = roleIds.oneOf()
    createRole(userDid, arbiterDid, roleId)
  }

  action addRoleMember(userDid, arbiterDid, roleId, member) = all {
    arbiter::addRoleMember(userDid, arbiterDid, roleId, member),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action addRoleMemberAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    def arbiter = arbiter::arbiters.get(arbiterDid)
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    nondet roleId = arbiter.roles.keys().oneOf()
    nondet memberOrRole = 0.to(1).oneOf()
    nondet member = userDids.oneOf()
    addRoleMember(userDid, arbiterDid, roleId, member)
  }

  action removeRoleMember(userDid, arbiterDid, roleId, member) = all {
    arbiter::removeRoleMember(userDid, arbiterDid, roleId, member),
    id::stutter,
    pds::stutter,
    steward::stutter
  }
  action removeRoleMemberAny = all {
    nondet arbiterDid = arbiter::arbiters.keys().oneOf()
    def arbiter = arbiter::arbiters.get(arbiterDid)
    nondet userDid = arbiter::arbiters.get(arbiterDid).admins.oneOf()
    nondet roleId = arbiter.roles.keys().oneOf()
    def role = arbiter.roles.get(roleId)
    nondet member = role.oneOf()
    removeRoleMember(userDid, arbiterDid, roleId, member)
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

  /// Arbiter invariants
  val Inv = all {
    InvAtLeastOneAdmin,
    InvRoleDoesNotSquashIndividualWrite,
    InvAllAdminsCanWrite,
  }

  /// There is always at least one admin for any given arbiter
  val InvAtLeastOneAdmin =
    arbiters.keys().forall(arbiterDid =>
      def arbiter = arbiters.get(arbiterDid)
      arbiter.admins.size() >= 1
    )

  /// Invariant: all admins can write to every space managed by the arbiter they are an admin of.
  val InvAllAdminsCanWrite =
    arbiters.keys().forall(arbiterDid =>
      def arbiter = arbiters.get(arbiterDid)
      arbiter.spaces.keys().forall(spaceKey =>
        def space = arbiter.spaces.get(spaceKey) 
        match getSpaceMemberList(arbiterDid, spaceKey) {
          | Some(flatUserAccessList) =>
            arbiter.admins.forall(adminDid =>
              flatUserAccessList.get(adminDid) == Write 
            )
          | None => false
        }
      )
    )
  
  /// Invariant: If you have direct write access, then that means the flat access list should have reported
  /// your access as Write, not allowing the read access inherited from a role to squash your write
  /// access.
  val InvRoleDoesNotSquashIndividualWrite =
    arbiters.keys().forall(arbiterDid =>
      def arbiter = arbiters.get(arbiterDid)
      arbiter.spaces.keys().forall(spaceKey =>
        def space = arbiter.spaces.get(spaceKey) 
        match getSpaceMemberList(arbiterDid, spaceKey) {
          | Some(flatUserAccessList) =>
            space.members.keys()
            .exclude(arbiter.admins.map(x => User(x))).forall(member =>
              match member {
                | User(userDid) =>
                  if (space.members.keys().contains(User(userDid)))
                    def flatAccess = flatUserAccessList.get(userDid)
                    def directAccess = space.members.get(User(userDid))
                    (directAccess == Write).implies(flatAccess == Write)
                  else true
                | Role(roleId) => true
              } 
            )
          | None => false
        }
      )
    )

  /// Get the flattened member list for a space, evaluating roles
  def getSpaceMemberList(arbiterDid: ArbiterDid, spaceKey: SpaceKey): Option[UserDid -> Access] =
    // Get the arbiter
    if (not(arbiterDid.in(arbiters.keys()))) None
    else def arbiter = arbiters.get(arbiterDid)

    // Get the space
    if (not(spaceKey.in(arbiter.spaces.keys()))) None
    else def space = arbiter.spaces.get(spaceKey)

    // Calculate the member access list, resolving roles as necessary
    def admins = arbiter.admins.fold(Map(), (members, adminDid) => members.put(adminDid, Write))
    def members = space.members.keys().fold(
      admins,
      (members, member) =>
        match member {
          | User(userDid) => 
            if (members.keys().contains(userDid) and members.get(userDid) == Write)
              members // If the member already has write access, just leave it
            else 
              members.put(userDid, space.members.get(member))
          | Role(roleId) =>
            def userDids = arbiter.roles.get(roleId)
            userDids.fold(members, (m, userDid) =>
              if (m.keys().contains(userDid) and m.get(userDid) == Write)
                m // If the member already has write access, just leave it
              else
                m.put(userDid, space.members.get(member))
            )
        }
    )
    Some(members)

  /// Create a new arbiter
  action create(userDid: UserDid, arbiterDid: ArbiterDid): bool = all {
    // Arbiter doesn't already exist
    not(arbiterDid.in(arbiters.keys())),
    // Add arbiter
    arbiters' = arbiters.put(arbiterDid, {
      did: arbiterDid,
      // Make the creator an admin
      admins: Set(userDid),
      roles: Map(),
      spaces: Map()
    })
  } 

  /// Add an admin to the arbiter
  action addAdmin(userDid: UserDid, arbiterDid: ArbiterDid, newAdmin: UserDid): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // user is admin
      userDid.in(arbiter.admins),
      // Add new admin
      def updAdmins = arbiter.admins.union(Set(newAdmin))
      def updArbiter = arbiter.with("admins", updAdmins)
      arbiters' = arbiters.set(
        arbiterDid,
        updArbiter
      )
    }
  }

  /// Remove an admin from the arbiter
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

  /// Create a new space, managed by an arbiter
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
      def spaces = arbiter.spaces.put(spaceKey, { members: Map() })
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
    member: Member,
    access: Access
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // Space exists
      spaceKey.in(arbiter.spaces.keys()),
      def space = arbiter.spaces.get(spaceKey)
      arbiters' = arbiters.set(arbiterDid,
        arbiter.with("spaces", arbiter.spaces.set(spaceKey,
          space.with("members", space.members.put(member, access)) 
        ))
      )
    }
  }

  /// Remove a member's access to a space
  action removeSpaceMember(
    userDid: UserDid,
    arbiterDid: ArbiterDid,
    spaceKey: SpaceKey,
    member: Member,
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // Space exists
      spaceKey.in(arbiter.spaces.keys()),
      def space = arbiter.spaces.get(spaceKey)
      arbiters' = arbiters.set(
        arbiterDid,
        arbiter
          .with("spaces", arbiter.spaces.set(spaceKey,
            space.with("members", space.members.mapRemove(member))  
          )
        )
      )
    }
  }

  /// Add a new role to the arbiter
  action createRole(
    userDid: UserDid,
    arbiterDid: ArbiterDid,
    roleId: RoleId,
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // Role does not already exist
      not(roleId.in(arbiter.roles.keys())),
      // Update roles list
      arbiters' = arbiters.set(
        arbiterDid,
        arbiter.with("roles", arbiter.roles.put(roleId, Set()))
      )
    }
  }

  /// Add a new member to a role
  action addRoleMember(
    userDid: UserDid,
    arbiterDid: ArbiterDid,
    roleId: RoleId,
    member: UserDid
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // Role already exist
      roleId.in(arbiter.roles.keys()),
      // Member not already in role
      not(member.in(arbiter.roles.get(roleId))),
      // Update roles list
      arbiters' = arbiters.set(
        arbiterDid,
        arbiter.with("roles", arbiter.roles.setBy(roleId, role => role.union(Set(member))))
      )
    }
  }

  /// Remove a member from a role
  action removeRoleMember(
    userDid: UserDid,
    arbiterDid: ArbiterDid,
    roleId: RoleId,
    member: UserDid
  ): bool = all {
    // Arbiter exists
    arbiterDid.in(arbiters.keys()),
    def arbiter = arbiters.get(arbiterDid)
    all {
      // The user is an admin
      userDid.in(arbiter.admins),
      // Role already exist
      roleId.in(arbiter.roles.keys()),
      // Member is in role
      member.in(arbiter.roles.get(roleId)),
      // Update roles list
      arbiters' = arbiters.set(
        arbiterDid,
        arbiter.with("roles", arbiter.roles.setBy(roleId, role => role.exclude(Set(member))))
      )
    }
  }
}

module steward {
  import types.*

  var placeholder: int
  
  action init = placeholder' = 0
  action stutter = placeholder' = placeholder
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
  import basicSpells.* from "./basicSpells"
  export basicSpells.*

  //
  // PDS
  //

  type ArbiterDid = int
  type UserDid = str
  type SpaceKey = int
  type Record = str
  type RoleId = str

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

  type Member = Role(RoleId) | User(UserDid)

  /// A permissioned space
  type PermSpace = {
    // The access level for each member in the space
    members: Member -> Access
  }

  /// Data for an arbiter
  type Arbiter = {
    did: ArbiterDid,
    admins: Set[UserDid],
    roles: RoleId -> Set[UserDid],
    spaces: SpaceKey -> PermSpace,
  }
}