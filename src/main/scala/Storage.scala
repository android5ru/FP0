import java.io._
import java.nio.file.{Files, Paths}
import scala.util.Try

object Storage {
  def serializeVault(vault: Vault): Try[Array[Byte]] = Try {
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    try {
      oos.writeObject(vault)
      oos.flush()
      bos.toByteArray
    } finally oos.close()
  }
  def deserializeVault(bytes: Array[Byte]): Try[Vault] = Try {
    val bis = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(bis)
    try{
      ois.readObject().asInstanceOf[Vault]
    } finally ois.close()
  }
  def saveToFile(vault: Vault, key: Array[Byte], path: String): Try[Unit] = {
    for {
      rawBytes <- serializeVault(vault)
      encryptedBytes <- Crypto.encrypt(rawBytes, key)
      _ <- writeBytesToFile(encryptedBytes, path)
    } yield ()
  }

  def loadFromFile(path: String, key: Array[Byte]): Try[Vault] = {
    for {
      fileBytes <- readBytesFromFile(path)
      decryptedBytes <- Crypto.decrypt(fileBytes, key)
      vault <- deserializeVault(decryptedBytes)
    } yield vault
  }
  def writeBytesToFile(bytes: Array[Byte], path: String): Try[Unit] = Try{
    Files.write(Paths.get(path), bytes)
  }
  def readBytesFromFile(path: String): Try[Array[Byte]] = Try {
    Files.readAllBytes(Paths.get(path))
  }
}
