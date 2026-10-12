import java.io.File

object CommandProcessor {
  private val commands: Map [String, Command] = Map (
    "help"   -> Commands.help,
    "create" -> Commands.create,
    "open"   -> Commands.open,
    "list"   -> Commands.list,
    "show"   -> Commands.show,
    "pass"   -> Commands.pass,
    "add"    -> Commands.add,
    "edit"   -> Commands.edit,
    "remove" -> Commands.remove,
    "search" -> Commands.search,
    "lock"   -> Commands.lock,
    "reload" -> Commands.reload,
    "exit"   -> Commands.exit
  )
  def checkArgs(args: List[String], requiredCount: Int, cmdName: String): Option[String] = {
    if (args.length < requiredCount) Some(s"Ошибка: в команде '$cmdName' не хватает параметров. Ожидается: '$requiredCount'")
    else None
  }

  def checkAutoLock(state: AppState, config: AppConfig): AppState = state match {
    case u: Unlocked =>
      val elapsedMinutes = (System.currentTimeMillis() - u.lastActivity) / 1000 / 60
      if (elapsedMinutes >= config.automaticLockMinutes) {
        Logger.warning(config.logFile, s"Автоматическая блокировка: неактивность ${elapsedMinutes.toInt} мин")
        Locked(0)
      } else {
        u
      }
    case other => other
  }
  def process (state: AppState, config: AppConfig, input: String): CommandResult = {
    val trimmed = input.trim;
    if (trimmed.isEmpty){
      return CommandResult(state, "")
    }
    val freshState = checkAutoLock(state, config)

    val parts = trimmed.split("\\s+").toList
    val cmdName = parts.head.toLowerCase
    val args = parts.tail

    commands.get(cmdName) match {
      case Some(function) => function(freshState, config, args)
      case None =>
        Logger.warning(config.logFile, s"Неизвестная команда '$cmdName'")
        CommandResult(freshState, s"Неизвестная команда '$cmdName'. Введите 'help' для списка команд")
    }
  }
}

object Commands {
  def readInput(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine().trim
  }
  def checkUnlocked(state: AppState): Either[String, Unlocked] = state match {
    case u: Unlocked => Right(u)
    case _: Locked => Left("Ошибка: Хранилище заблокировано")
    case  Blocked(r) => Left(s"Ошибка: Доступ заблокирован. $r")
  }

  def help (state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val text =
      """Доступные команды:
        |  create      - Создать новое хранилище
        |  open        - Открыть существующее хранилище
        |  list        - Показать список записей
        |  show <id>   - Показать выбранную запись
        |  pass <id>   - Показать пароль выбранной записи
        |  add         - Добавить новую запись
        |  edit <id>   - Изменить существующую запись
        |  remove <id> - Удалить запись
        |  search <q>  - Найти записи по подстроке
        |  lock        - Заблокировать хранилище
        |  reload      - Перезагрузить конфигурацию
        |  exit        - Завершить работу""".stripMargin
    CommandResult(state, text)
  }
  def create (state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    state match {
      case _: Unlocked => return CommandResult(state, "Ошибка: Хранилище уже открыто. Сначала заблокируйте его")
      case _ =>
    }
    val file = new java.io.File(config.storageFile)
    if (file.exists()){
      val answer = readInput(s"Файл '${config.storageFile}' уже существует. Перезаписать: да/нет?\n")
      if (answer.toLowerCase() != "да"){
        return CommandResult(state, "Создание отменено")
      }
    }
    val pass1 = readInput("Введите мастер-пароль: ")
    if (pass1.isEmpty) return CommandResult(state, "Ошибка: Пароль не может быть пустым")
    val pass2 = readInput("Повторите мастер-пароль: ")
    if (pass1 != pass2){
      Logger.warning(config.logFile, "Попытка создания хранилища: пароли не совпали")
      return CommandResult(state, "Ошибка: мастер-пароли не совпадают")
    }
    val key = Crypto.passwordToKey(pass1)
    val emptyVault = Vault(List.empty)
    Storage.saveToFile(emptyVault, key, config.storageFile) match {
      case scala.util.Success(_) =>
        Logger.info(config.logFile, "Новое хранилище успешно создано")
        CommandResult(state, "Хранилище успешно создано и сохранено.")

      case scala.util.Failure(ex) =>
        Logger.error(config.logFile, s"Ошибка создания хранилища: ${ex.getMessage}")
        CommandResult(state, s"Ошибка: Не удалось сохранить хранилище. ${ex.getMessage}")
    }
  }

  def open(state: AppState, config: AppConfig, args: List[String]): CommandResult =
    state match {
      case _: Unlocked => return CommandResult(state, "Ошибка: Хранилище уже открыто.")
      case Blocked(r)  => return CommandResult(state, s"Ошибка: $r")
      case l: Locked   =>
        if (l.failedAttempts >= config.maximumUnlockAttempts) {
          return CommandResult(Blocked("Превышено максимальное количество попыток открытия."), "Доступ заблокирован.")
        }
    }
    val file = new java.io.File(config.storageFile)
    if (!file.exists()) {
      return CommandResult(state, s"Ошибка: Файл хранилища '${config.storageFile}' не найден. Создайте новое хранилище (create).")
    }
    val pass = readInput("Введите мастер-пароль: ")
    val key = Crypto.passwordToKey(pass)

    Storage.loadFromFile(config.storageFile, key) match {
      case scala.util.Success(vault) =>
        Logger.info(config.logFile, "Хранилище успешно открыто")
        val newState = Unlocked(vault, key, System.currentTimeMillis())
        CommandResult(newState, "Хранилище успешно открыто.")

      case scala.util.Failure(_) =>
        val currentAttempts = state.asInstanceOf[Locked].failedAttempts
        val newAttempts = currentAttempts + 1
        Logger.warning(config.logFile, s"Неудачная попытка открытия хранилища (попытка $newAttempts)")
        if (newAttempts >= config.maximumUnlockAttempts) {
          Logger.error(config.logFile, "Превышено максимальное количество попыток открытия. Доступ заблокирован.")
          CommandResult(Blocked("Превышено максимальное количество попыток открытия."), "Ошибка: Превышено количество попыток. Доступ заблокирован до перезапуска.")
        } else {
          val remaining = config.maximumUnlockAttempts - newAttempts
          CommandResult(Locked(newAttempts), s"Ошибка: Неверный мастер-пароль или файл поврежден. Осталось попыток: $remaining")
        }
    }

  def list(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    val sorted = config.defaultSort.toLowerCase match {
      case "service" => unlocked.vault.records.sortBy(_.service)
      case "username" => unlocked.vault.records.sortBy(_.username)
      case "id" => unlocked.vault.records.sortBy(_.id)
      case "created" => unlocked.vault.records.sortBy(_.createdAt)
      case "updated" => unlocked.vault.records.sortBy(_.updatedAt)
      case _ => unlocked.vault.records.sortBy(_.service)
    }

    val output = if (sorted.isEmpty) {
      "Хранилище пусто. Добавьте первую запись командой 'add'."
    } else {
      AuxiliaryFunction.formatRecordsTable(sorted)
    }

    Logger.info(config.logFile, s"Выполнен просмотр списка: записей=${sorted.size}")
    CommandResult(unlocked.copy(lastActivity = System.currentTimeMillis()), output)
  }

  def show(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    if (args.isEmpty) {
      return CommandResult(unlocked, "Ошибка: не указан идентификатор записи. Использование: show <id>")
    }

    AuxiliaryFunction.findRecordById(unlocked.vault, args.head) match {
      case Left(err) => CommandResult(unlocked, err)
      case Right(record) =>
        Logger.info(config.logFile, s"Просмотрена запись: id=${record.id}")
        CommandResult(unlocked.copy(lastActivity = System.currentTimeMillis()), AuxiliaryFunction.formatRecordDetail(record))
    }
  }

  def pass(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    if (args.isEmpty) {
      return CommandResult(unlocked, "Ошибка: не указан идентификатор записи. Использование: pass <id>")
    }

    AuxiliaryFunction.findRecordById(unlocked.vault, args.head) match {
      case Left(err) => CommandResult(unlocked, err)
      case Right(record) =>
        Logger.info(config.logFile, s"Отображён пароль записи: id=${record.id}")
        CommandResult(unlocked.copy(lastActivity = System.currentTimeMillis()), s"Пароль: ${record.password}")
    }
  }

  def add(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    println("Добавление новой записи")
    println("(обязательные поля отмечены *)")

    val service = readInput("Сервис *: ")
    if (service.isEmpty) {
      return CommandResult(unlocked, "Ошибка: поле 'Сервис' обязательно для заполнения.")
    }

    val username = readInput("Имя пользователя *: ")
    if (username.isEmpty) {
      return CommandResult(unlocked, "Ошибка: поле 'Имя пользователя' обязательно для заполнения.")
    }

    val password = readInput("Пароль *: ")
    if (password.isEmpty) {
      return CommandResult(unlocked, "Ошибка: поле 'Пароль' обязательно для заполнения.")
    }

    val email = readInput("Почта (Enter, если нет): ")
    val url = readInput("Адрес сайта (Enter, если нет): ")
    val note = readInput("Заметка (Enter, если нет): ")

    val newId = if (unlocked.vault.records.isEmpty) 1 else unlocked.vault.records.map(_.id).max + 1
    val now = java.time.LocalDateTime.now()

    val newRecord = PasswordRecord(
      id = newId,
      service = service,
      username = username,
      password = password,
      email = if (email.isEmpty) None else Some(email),
      url = if (url.isEmpty) None else Some(url),
      note = if (note.isEmpty) None else Some(note),
      createdAt = now,
      updatedAt = now
    )

    val newVault = unlocked.vault.copy(records = unlocked.vault.records :+ newRecord)

    Storage.saveToFile(newVault, unlocked.masterKey, config.storageFile) match {
      case scala.util.Success(_) =>
        Logger.info(config.logFile, s"Добавлена запись: id=$newId")
        CommandResult(unlocked.copy(vault = newVault, lastActivity = System.currentTimeMillis()),
          s"Запись успешно добавлена (id=$newId).")
      case scala.util.Failure(ex) =>
        Logger.error(config.logFile, s"Ошибка сохранения хранилища: ${ex.getMessage}")
        CommandResult(unlocked, s"Ошибка: не удалось сохранить хранилище. ${ex.getMessage}")
    }
  }

  def edit(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    if (args.isEmpty) {
      return CommandResult(unlocked, "Ошибка: не указан идентификатор записи. Использование: edit <id>")
    }

    AuxiliaryFunction.findRecordById(unlocked.vault, args.head) match {
      case Left(err) => CommandResult(unlocked, err)
      case Right(record) =>
        println(s"Редактирование записи id=${record.id}")
        println("(оставьте поле пустым, чтобы сохранить прежнее значение)")

        val newService = readInput(s"Сервис [${record.service}]: ")
        val newUsername = readInput(s"Имя пользователя [${record.username}]: ")
        val newEmail = readInput(s"Почта [${record.email.getOrElse("-")}]: ")
        val newUrl = readInput(s"Адрес сайта [${record.url.getOrElse("-")}]: ")
        val newNote = readInput(s"Заметка [${record.note.getOrElse("-")}]: ")

        val changePassword = readInput("Изменить пароль? [да/нет]: ")
        val newPassword = if (changePassword.toLowerCase == "да") {
          readInput("Новый пароль: ")
        } else {
          ""
        }

        val updatedRecord = record.copy(
          service = if (newService.isEmpty) record.service else newService,
          username = if (newUsername.isEmpty) record.username else newUsername,
          password = if (newPassword.isEmpty) record.password else newPassword,
          email = if (newEmail.isEmpty) record.email else Some(newEmail),
          url = if (newUrl.isEmpty) record.url else Some(newUrl),
          note = if (newNote.isEmpty) record.note else Some(newNote),
          updatedAt = java.time.LocalDateTime.now()
        )

        val newVault = unlocked.vault.copy(
          records = unlocked.vault.records.map(r => if (r.id == record.id) updatedRecord else r)
        )

        Storage.saveToFile(newVault, unlocked.masterKey, config.storageFile) match {
          case scala.util.Success(_) =>
            Logger.info(config.logFile, s"Изменена запись: id=${record.id}")
            CommandResult(unlocked.copy(vault = newVault, lastActivity = System.currentTimeMillis()),
              s"Запись успешно изменена (id=${record.id}).")
          case scala.util.Failure(ex) =>
            Logger.error(config.logFile, s"Ошибка сохранения хранилища: ${ex.getMessage}")
            CommandResult(unlocked, s"Ошибка: не удалось сохранить хранилище. ${ex.getMessage}")
        }
    }
  }

  def remove(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    if (args.isEmpty) {
      return CommandResult(unlocked, "Ошибка: не указан идентификатор записи. Использование: remove <id>")
    }

    AuxiliaryFunction.findRecordById(unlocked.vault, args.head) match {
      case Left(err) => CommandResult(unlocked, err)
      case Right(record) =>
        if (config.confirmBeforeDelete) {
          println("Будет удалена запись:")
          println(s"  Сервис: ${record.service}")
          println(s"  Имя пользователя: ${record.username}")

          val answer = readInput("Удалить запись? [да/нет]: ")
          if (answer.toLowerCase != "да") {
            return CommandResult(unlocked, "Удаление отменено пользователем.")
          }
        }

        val newVault = unlocked.vault.copy(records = unlocked.vault.records.filterNot(_.id == record.id))

        Storage.saveToFile(newVault, unlocked.masterKey, config.storageFile) match {
          case scala.util.Success(_) =>
            Logger.info(config.logFile, s"Удалена запись: id=${record.id}")
            CommandResult(unlocked.copy(vault = newVault, lastActivity = System.currentTimeMillis()),
              s"Запись удалена (id=${record.id}).")
          case scala.util.Failure(ex) =>
            Logger.error(config.logFile, s"Ошибка сохранения хранилища: ${ex.getMessage}")
            CommandResult(unlocked, s"Ошибка: не удалось сохранить хранилище. ${ex.getMessage}")
        }
    }
  }

  def search(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    val unlocked = checkUnlocked(state) match {
      case Right(unlocked) => unlocked
      case Left(err) => return CommandResult(state, err)
    }
    if (args.isEmpty) {
      return CommandResult(unlocked, "Ошибка: не указан поисковый запрос. Использование: search <запрос>")
    }

    val query = args.mkString(" ").toLowerCase

    val found = unlocked.vault.records.filter { r =>
      r.service.toLowerCase.contains(query) ||
        r.username.toLowerCase.contains(query) ||
        r.url.getOrElse("").toLowerCase.contains(query)
    }

    if (found.isEmpty) {
      Logger.info(config.logFile, s"Выполнен поиск: найдено записей=0")
      CommandResult(unlocked.copy(lastActivity = System.currentTimeMillis()), "Ничего не найдено.")
    } else {
      Logger.info(config.logFile, s"Выполнен поиск: найдено записей=${found.size}")
      val output = s"Найдено записей: ${found.size}\n" + AuxiliaryFunction.formatRecordsTable(found)
      CommandResult(unlocked.copy(lastActivity = System.currentTimeMillis()), output)
    }
  }

  def lock(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    state match {
      case _: Unlocked =>
        Logger.info(config.logFile, "Хранилище заблокировано пользователем")
        CommandResult(Locked(0), "Хранилище успешно заблокировано")
      case _: Locked => CommandResult(state, "Хранилище уже заблокировано")
      case Blocked(r) => CommandResult(state, s"Доступ заблокирован: $r")
    }
  }

  def reload(state: AppState, config: AppConfig, args: List[String]): CommandResult = {
    state match {
      case _: Unlocked =>
        return CommandResult(state, "Ошибка: невозможно перезагрузить конфигурацию при открытом хранилище. Сначала заблокируйте хранилище (lock).")
      case _ =>
    }

    Config.load("config.json") match {
      case Right(newConfig) =>
        Logger.info(config.logFile, "Конфигурация успешно перезагружена")
        CommandResult(state, "Конфигурация успешно перезагружена.")
      case Left(errors) =>
        val errorMessage = Config.formatErrors(errors)
        Logger.error(config.logFile, s"Ошибка перезагрузки конфигурации: ${errors.mkString("; ")}")
        CommandResult(state, s"Ошибка перезагрузки конфигурации:\n$errorMessage")
    }
  }

  def exit(state: AppState, config: AppConfig, args: List[String]): CommandResult =
    CommandResult(state, "Завершение работы")
}