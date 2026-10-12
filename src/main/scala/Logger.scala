import java.io.FileWriter
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

object Logger {
  private val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
  private def timestamp: String = LocalDateTime.now().format(dateFormat)

  private def write(path: String, line: String): Unit = {
    Try{
      val writer = new FileWriter(path, true)
      try {
        writer.write(line + "\n")
      }finally writer.close()
    }match {
      case Failure(ex) =>
        System.err.println(s"[Logger] не удалось записать в лог '$path': ${ex.getMessage}")
      case Success(_) =>
    }
  }

  def log(path: String, level: String, message: String): Unit = {
    write(path, s"$timestamp $level $message")
  }
  def info(path: String, message: String): Unit = log(path, "INFO ", message)
  def warning(path: String, message: String): Unit = log(path, "WARN ", message)
  def error(path: String, message: String): Unit = log(path, "ERROR ", message)
}
