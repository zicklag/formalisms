module permissioned_spaces {

}

module user {
  import types.*

  pure val users = Set("Alice", "Bob", "Charlie")

  var userState: UserDid -> UserState

  action init = all {
    userState' = users.mapBy(x => { session: None })
  }
}

module pds {
  import time.time
  import types.*
  import user.users
  import space.spaces

  /// Create an oauth session with the PDS after authenticating a user.
  def authenticate(user: UserDid): Option[OAuthSession] = {
    if (user.in(users)) 
      Some({ userDid: user })
    else
      None
  }

  /// Get a member grant token that can be used to sync a private space.
  ///
  /// This will fail if the session is for a user that is not added to the space.
  def getMemberGrant(
    clientId: ClientId,
    session: OAuthSession,
    spaceDid: SpaceDid
  ): Option[MemberGrantToken] =
    pure def userDid = session.userDid
    pure def space = spaces.filter(x => x.did == spaceDid).chooseSome()
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
}

module space {
  import types.*
  import time.time

  var nextDid: SpaceDid
  var spaces: Set[Space]

  /// Redeem a member grant token for a space credential.
  ///
  /// The client that is requesting the token must match the one in the grant.
  def getSpaceCredential(clientId: ClientId, grant: MemberGrantToken): Option[SpaceCred] = 
    if (clientId == grant.clientId)
      Some({
        spaceDid: grant.spaceDid, // valid for the given space
        expiresAt: time + 120     // expires in two hours
      })
    else None

  action init = all {
    nextDid' = 0,
    spaces' = Set()
  }
}

module app {
  import types.*

  const CLIENT_ID: ClientId

  var spaceCredentials: Set[SpaceCred]

  action login(session: OAuthSession): bool = all {
    true
  }
}

module time {
  import types.*

  var time: Timestamp

  action init =
    time' = 0  

  action step = any {
    time' = time + 1,
    time' = time
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
  // Space
  //

  type SpaceDid = int
  type SpaceKey = int
  type Access = Read | Write
  type SpaceCred = { spaceDid: SpaceDid, expiresAt: int }

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

