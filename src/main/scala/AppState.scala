sealed trait AppState

case class Locked(failedAttempts: Int = 0) extends AppState

case class Unlocked(
  vault: Vault,
  masterKey: Array[Byte],
  lastActivity: Long) extends AppState

case class Blocked(reason: String) extends AppState
