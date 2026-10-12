import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object AuxiliaryFunction {
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  def formatDate(dt: LocalDateTime): String = dt.format(dateFormatter)

  def findRecordById(vault: Vault, idStr: String): Either[String, PasswordRecord] = {
    val idOpt = scala.util.Try(idStr.toInt).toOption
    idOpt match {
      case None =>
        Left(s"Ошибка: идентификатор '$idStr' имеет неправильный формат. Ожидается целое число.")
      case Some(id) =>
        vault.records.find(_.id == id) match {
          case Some(record) => Right(record)
          case None         => Left(s"Ошибка: запись с id=$id не существует.")
        }
    }
  }
  def formatRecordsTable(records: List[PasswordRecord]): String = {
    if (records.isEmpty) return "Записи не найдены."
    val headers = List("ID", "Сервис", "Имя пользователя", "Почта", "Адрес", "Изменена")
    val rows = records.map { r =>
      List(
        r.id.toString,
        r.service,
        r.username,
        r.email.getOrElse("-"),
        r.url.getOrElse("-"),
        formatDate(r.updatedAt)
      )
    }
    val allRows = headers :: rows
    val widths = headers.indices.map { i =>
      allRows.map(row => row(i).length).max
    }
    def pad(s: String, width: Int): String = s + " " * (width - s.length)
    val headerLine = headers.zip(widths).map { case (h, w) => pad(h, w) }.mkString("  ")
    val separator = widths.map("-" * _).mkString("  ")
    val dataLines = rows.map(row => row.zip(widths).map { case (c, w) => pad(c, w) }.mkString("  "))

    (headerLine :: separator :: dataLines).mkString("\n")
  }

  def formatRecordDetail(record: PasswordRecord): String = {
    s"""Сервис: ${record.service}
       |Адрес: ${record.url.getOrElse("-")}
       |Имя пользователя: ${record.username}
       |Почта: ${record.email.getOrElse("-")}
       |Пароль: ********
       |Заметка: ${record.note.getOrElse("-")}
       |Создана: ${formatDate(record.createdAt)}
       |Изменена: ${formatDate(record.updatedAt)}""".stripMargin
  }
}
