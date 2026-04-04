module permissioned_spaces {
  import types.*

  import time
  import pds

  import app(CLIENT_ID = "pig") as pig
  import app(CLIENT_ID = "moose") as moose

  pure val clients = Set("pig", "moose")

  action init = all {
    time::init,
    pds::init,
    pig::init,
    moose::init
  }

  action step = any {
    // Time passing
    passTime,
    // User actions
    any {
      loginAny,
      createSpaceAny,
      // addMemberToSpaceAny,
    }
  }

  action passTime(): bool = all {
    time::step,
    pds::stutter,
    pig::stutter,
    moose::stutter,
  }

  action login(user: UserDid): bool = all {
    time::stutter,
    pds::login(user),
    pig::stutter,
    moose::stutter,
  }
  action loginAny =
    nondet user = oneOf(pds::users)
    login(user)

  action createSpace(user: UserDid, mode: ClientMode): bool = all {
    time::stutter,
    pds::createSpace(user, mode),
    pig::stutter,
    moose::stutter,
  }
  action createSpaceAny =
    nondet user = oneOf(pds::users)
    nondet mode = 0.to(1).oneOf()
    createSpace(user, if (mode == 0) DefaultAllow else DefaultDeny)

  action addMemberToSpace(user: UserDid, space: SpaceDid, newMember: UserDid, access: Access): bool = all {
    time::stutter,
    pds::addMemberToSpace(user, space, newMember, access),
    pig::stutter,
    moose::stutter,
  }
  // action addMemberToSpaceAny =
  //   nondet space = pds::spaces.oneOf()
  //   nondet access = 0.to(1).oneOf()
}

module pds {
  import types.*
  import time.time

  pure val users = Set("Alice", "Bob", "Charlie")

  /// User OAuth sessions
  var userSessions: UserDid -> Option[OAuthSession]

  /// The list of space DIDs controlled by a given user
  var userControlledDids: UserDid -> Set[SpaceDid]

  /// Counter used to create new space DIDs
  var nextDid: SpaceDid

  /// The permissioned spaces that have been created
  var spaces: Set[Space]

  /// The permissioned repos stored on the PDS
  var permRepos: Set[PermRepo]

  action init = all {
    userSessions' = users.mapBy(x => None),
    userControlledDids' = users.mapBy(x => Set()),
    nextDid' = 0,
    spaces' = Set(),
    permRepos' = Set(),
  }

  action stutter = all {
    // Unchanged
    userSessions' = userSessions,
    userControlledDids' = userControlledDids,
    nextDid' = nextDid,
    spaces' = spaces,
    permRepos' = permRepos,
  }

  /// User logs into the PDS, creating a new oauth session
  action login(user: UserDid): bool = all {
    // Does user exist
    user.in(users),
    // Does the user not already have a session?
    userSessions.get(user) == None,

    // Create new user session
    userSessions' = userSessions.put(user, Some({ userDid: user })),

    // Unchanged
    userControlledDids' = userControlledDids,
    nextDid' = nextDid,
    spaces' = spaces,
    permRepos' = permRepos
  }

  action createSpace(owner: UserDid, mode: ClientMode): bool = all {
    // User exists
    owner.in(users),

    // Create a new space
    spaces' = spaces.union(Set({
      // The new space's owning DID
      did: nextDid,       
      // The space key, this allows one DID to own multiple spaces, but in this case we just create
      // a new owning DID for every space and just set the key to 1 every time.
      key: 1,
      clientMode: mode,
      clientList: Set(),
      // Give the creator write access to the space
      members: Set((owner, Write))
    })),

    // We give the key for the new space DID to the owner, so they control the space
    userControlledDids' = userControlledDids.setBy(owner, x => x.union(Set(nextDid))),

    // Increment the next space DID
    nextDid' = nextDid + 1,

    // Unchanged
    userSessions' = userSessions,
    permRepos' = permRepos,
  }

  action addMemberToSpace(user: UserDid, spaceDid: SpaceDid, newMember: UserDid, access: Access): bool =
    all {
      // User exists
      user.in(users),

      // Space exists
      spaces.filter(x => x.did == spaceDid).size() == 1,

      // User controls the DID for the space they are modifying
      spaceDid.in(userControlledDids.get(user)),

      // Update spaces
      spaces' = spaces.map(x =>
        // The space that was selected
        if (x.did == spaceDid) {
          // Has a new member
          x.with("members", x.members.union(Set((newMember, access))))
        }
        else x)
    }

  /// Get a member grant token that can be used to sync a private space.
  ///
  /// This will fail if the session is for a user that is not added to the space.
  def getMemberGrantToken(
    clientId: ClientId,
    session: OAuthSession,
    spaceDid: SpaceDid
  ): Option[MemberGrantToken] =
    pure def userDid = session.userDid
    pure def space = spaces.filter(x => x.did == spaceDid).getOnlyElement()
    pure def inClientList = space.clientList.exists(x => x == clientId)
    pure def clientAllowed = match space.clientMode {
      | DefaultAllow => not(inClientList)
      | DefaultDeny => inClientList
    }
    pure def userAllowed = space.members.exists(x => x._1 == userDid)

    if (userAllowed and clientAllowed)
      Some({
        clientId: clientId,
        userDid: userDid,
        spaceDid: spaceDid,
        expiresAt: time + 5
      })
    else None

  /// Redeem a member grant token for a space credential.
  ///
  /// The client that is requesting the token must match the one in the grant.
  def getSpaceCredential(clientId: ClientId, grant: MemberGrantToken): Option[SpaceCred] = 
    if (clientId == grant.clientId and spaces.exists(x => x.did == grant.spaceDid))
      Some({
        spaceDid: grant.spaceDid, // valid for the given space
        expiresAt: time + 120     // expires in two hours ( 120 minutes )
      })
    else None

  /// Read permissioned data from a specific permissioned repo for a space
  def readPermData(cred: SpaceCred, spaceDid: SpaceDid, permRepo: UserDid): Option[Data] =
    def repoExists = permRepos.exists(x => x.spaceDid == spaceDid and x.userDid == permRepo)
    if (
      cred.spaceDid == spaceDid and
      time <= cred.expiresAt
    )
      if (repoExists) Some("data") else Some("empty repo")
    else None
}


module app {
  import types.*
  import pds
  import time.time

  const CLIENT_ID: ClientId

  var spaceCredentials: Set[SpaceCred]
  var latestData: Option[Data]

  action init = all {
    spaceCredentials' = Set(),
    latestData' = None
  }

  action stutter = all {
    spaceCredentials' = spaceCredentials,
    latestData' = latestData
  }

  action readFromSpace(session: OAuthSession, spaceDid: SpaceDid, permRepo: UserDid): bool = 
    // Filter out expired credentials
    def notExpiredCreds = spaceCredentials.filter(x => time >= x.expiresAt)
    // Check whether we already have a credential for the space we want to read
    def hasCredForSpace = spaceCredentials.exists(x => x.spaceDid == spaceDid)
    // Get a new credential if we don't have one for the space we need to read
    def newCreds = if (not(hasCredForSpace))
      // First get a grant token that we can use to request a credential
      def grant = pds::getMemberGrantToken(CLIENT_ID, session, spaceDid)
      // Then get the credential if the grant was successful
      def newCred = match grant {
        | Some(grant) => pds::getSpaceCredential(CLIENT_ID, grant)
        | None => None
      }
      // Then return the new set of credentials
      match newCred {
        | Some(cred) => notExpiredCreds.union(Set(cred))
        | None => notExpiredCreds
      }
    else notExpiredCreds

    // Get the matching credential
    def matchingCred = newCreds.filter(x => x.spaceDid == spaceDid)

    all {
      // Continue if we can load it
      matchingCred.size() == 1,
      // Update the credentials list
      spaceCredentials' = newCreds,
      // Just capture the latest data for introspection
      latestData' = pds::readPermData(matchingCred.getOnlyElement(), spaceDid, permRepo)
    }
}

module time {
  import types.*

  var time: Timestamp

  action init =
    time' = 0  

  action stutter = 
    time' = time

  action step = any {
    time' = time + 1,
  }
}

module types {
  // Utils
  type Option[a] = Some(a) | None

  // App
  type ClientId = str

  // Time, we'll say it's in minutes
  type Timestamp = int

  //
  // User
  //

  type UserDid = str
  type OAuthSession = { userDid: UserDid }
  type UserState = {
    session: Option[OAuthSession]
  }

  //
  // PDS
  //

  type MemberGrantToken = {
    userDid: UserDid,
    clientId: ClientId,
    spaceDid: SpaceDid,
    expiresAt: Timestamp
  }

  //
  // Permissioned Repo
  // 

  type PermRepo = {
    userDid: UserDid,
    spaceDid: SpaceDid,
    // Data goes in here, but we leave it out for now
  }

  //
  // Space
  //

  type SpaceDid = int
  type SpaceKey = int
  type Access = Read | Write
  type SpaceCred = { spaceDid: SpaceDid, expiresAt: int }
  type Data = str

  /// Whether the space allows clients by default and uses a blocklist, or the space denys clients
  //by default and uses an allow list.
  type ClientMode = DefaultAllow | DefaultDeny

  type Space = {
    // The Did that owns the space
    did: SpaceDid,
    // The space key
    key: SpaceKey,
    // The mode for accepting client connections
    clientMode: ClientMode,
    // Depending on the cient mode this is either a block or an allow list
    clientList: Set[ClientId],
    // The list of members in the space
    members: Set[(UserDid, Access)]
  }
}

