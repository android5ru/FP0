import scala.annotation.tailrec

object Main {
  @tailrec
  def loop(state: AppState, config: AppConfig): Unit = {
    val prompt = state match{
      case _: Locked => "Введите команду: "
      case _: Unlocked => "vault> "
      case _: Blocked => "Blocked> "
    }

    print(prompt)
    val input = scala.io.StdIn.readLine()

    val result = CommandProcessor.process(state, config, input)

    if (result.output.nonEmpty){
      println(result.output)
    }
    result.logMessage.foreach(msg => Logger.info(config.logFile, msg))
    if (input.trim.toLowerCase == "exit"){
      Logger.info(config.logFile, "Приложение завершено")
      return
    }

    loop(result.newState, config)
  }
  def main (args: Array[String]): Unit = {
    println("Локальное хранилище паролей\n")
    val configPath = "config.json"
    val tempLogPath = "vault.log"
    Logger.info(tempLogPath, "Приложение запущено")
    Config.load(configPath)match {
      case Right(config) =>
        Logger.info(config.logFile, "Конфигурация успешно загружена")
        println("Конфигурация успешно загружена")
        loop(Locked(0), config)
      case Left(errors) =>
        val errorMessage = Config.formatErrors(errors)
        println(errorMessage)
        Logger.error(tempLogPath, s"Ошибка конфигурации: ${errors.mkString("; ")}")
    }
  }
}