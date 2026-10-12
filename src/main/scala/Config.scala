import scala.io.Source
import scala.util.{Failure, Success, Try}

object Config {
  def readFile(path: String): Either[String, String] = {
    Try{
      val source = Source.fromFile(path, "UTF-8")
      try source.mkString finally source.close()
    }match {
      case Success(content) => Right(content)
      case Failure(ex) => Left(s"Не удалось прочитать '$path': ${ex.getMessage}")
    }
  }

  def parseJson(text: String): Either[String, Map[String, String]] = {
    val keyValue = """"(\w+)"\s*:\s*(.+)""".r
    val parseErrors = scala.collection.mutable.ListBuffer[String]()
    val pairs: List[(String, String)] = text.linesIterator.toList.flatMap{
      line => val trimmed = line.trim.stripSuffix(",").trim
        trimmed match {
          case keyValue(key, value) => Some(key -> value.trim)
          case "{" | "}" | "" => None
          case other =>
            if (other.nonEmpty){
              parseErrors += s"Некорректная строка в JSON: '$other'"
            }
            None
        }
    }
    if (parseErrors.nonEmpty) {
      Left(parseErrors.mkString("\n"))
    } else if (pairs.isEmpty) {
      Left("Конфигурационный файл пуст или не содержит параметров")
    } else {
      Right(pairs.toMap)
    }
  }

  def getString(map: Map[String, String], key: String): Either[String, String] = {
    map.get(key) match {
      case Some(v) => Right(v.stripPrefix("\"").stripSuffix("\""))
      case None => Left(s"Параметр '$key' отсутствует в конфигурации")
    }
  }
  def getInt(map: Map[String, String], key: String): Either[String, Int] = {
    getString(map, key).flatMap {v=>
      Try(v.toInt) match {
        case Success(n) => Right(n)
        case Failure(_) => Left(s"Параметр '$key': '$v' не является целым числом")
      }
    }
  }
  def getBoolean(map: Map[String, String], key: String): Either[String, Boolean] = {
    getString(map, key).flatMap {v=>
      v.toLowerCase match {
        case "true" => Right(true)
        case "false" => Right(false)
        case _ => Left(s"Параметр '$key': '$v' не является логическим значением")
      }
    }
  }
  def load(path: String): Either[List[String], AppConfig] = {
    val contentEither: Either[String, String] = readFile(path)
    contentEither match {
      case Left(err) => Left(List(err))
      case Right(content) => parseJson(content) match {
        case Left(err) => Left(List(err))
        case Right(map) =>
          val errors = scala.collection.mutable.ListBuffer[String]()
          val storageFile = getString(map, "storageFile")
          val logFile = getString(map, "logFile")
          val lockMin = getInt(map, "automaticLockMinutes")
          val maxAttempts = getInt(map, "maximumUnlockAttempts")
          val sort = getString(map, "defaultSort")
          val confirm = getBoolean(map, "confirmBeforeDelete")

          storageFile match { case Left(e) => errors += e; case _ => }
          logFile match { case Left(e) => errors += e; case _ => }
          lockMin match { case Left(e) => errors += e; case _ => }
          maxAttempts match { case Left(e) => errors += e; case _ => }
          sort match { case Left(e) => errors += e; case _ => }
          confirm match { case Left(e) => errors += e; case _ => }

          storageFile.foreach(v => if (v.isEmpty) errors += "Параметр 'storageFile': не может быть пустым")
          logFile.foreach(v => if(v.isEmpty) errors += "Параметр 'logFile': не может быть пустым")

          (storageFile, logFile) match {
            case (Right(s), Right(l)) if s == l =>
              errors += s"Параметры 'storageFile' и 'logFile' указывают на один и тот же файл"
            case _ =>
          }

          lockMin.foreach{v=>
            if(v < 1) errors += s"Параметр 'automaticLockMinutes': '$v'. Должно быть целое число не меньше 1"
          }
          maxAttempts.foreach{v =>
            if (v < 1) errors += s"Параметр 'maximumUnlockAttempts': '$v'. Должно быть целое число не меньше 1"
          }
          val allowedSorts = Set("service", "username", "id", "created", "updated")
          sort.foreach {v =>
            if (!allowedSorts.contains(v)) errors += s"Параметр 'defaultSort': '$v'. Допустимые: ${allowedSorts.mkString(", ")}"
          }
          confirm.left.foreach(err => errors += err)

          if (errors.nonEmpty) Left(errors.toList)
          else Right(AppConfig(
            storageFile = storageFile.toOption.get,
            logFile = logFile.toOption.get,
            automaticLockMinutes = lockMin.toOption.get,
            maximumUnlockAttempts = maxAttempts.toOption.get,
            defaultSort = sort.toOption.get,
            confirmBeforeDelete = confirm.toOption.get
          ))
      }
    }
  }
  def formatErrors(errors: List[String]): String = {
    val head = "Ошибка конфигурации:\n"
    val body = errors.zipWithIndex.map{case (err, i) => s"  ${i+1}. $err"}.mkString("\n")
    head + body
  }
}

