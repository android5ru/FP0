import java.time.LocalDateTime

case class PasswordRecord (
  id: Int,
  service: String,
  username: String,
  password: String,
  email: Option[String],
  url: Option[String],
  note: Option[String],
  createdAt: LocalDateTime,
  updatedAt: LocalDateTime) extends Serializable

case class Vault(
  records: List[PasswordRecord]) extends Serializable

case class AppConfig(
  storageFile: String,
  logFile: String,
  automaticLockMinutes: Int,
  maximumUnlockAttempts: Int,
  defaultSort: String,
  confirmBeforeDelete: Boolean)

case class CommandResult(
  newState: AppState,
  output: String,
  logMessage: Option[String] = None)

type Command = (AppState, AppConfig, List[String]) => CommandResult