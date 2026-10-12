import java.security.MessageDigest
import java.security.SecureRandom
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import scala.util.Try

object Crypto {
  def passwordToKey(password: String): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(password.getBytes("UTF-8"))
  }

  def encrypt(data: Array[Byte], keyBytes: Array[Byte]): Try[Array[Byte]] = Try{
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val key = new SecretKeySpec(keyBytes, "AES")
    val iv = new Array[Byte](16)
    new SecureRandom().nextBytes(iv)
    cipher.init(Cipher.ENCRYPT_MODE, key, new IvParameterSpec(iv))
    val encryptedData = cipher.doFinal(data)
    iv ++ encryptedData
  }

  def decrypt(dataWithIv: Array[Byte], keyBytes: Array[Byte]): Try[Array[Byte]] = Try{
    if (dataWithIv.length < 16){
      throw new IllegalArgumentException("Файл хранилища слишком мал или поврежден")
    }
    val (iv, encryptedData) = dataWithIv.splitAt(16)
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val key =  new SecretKeySpec(keyBytes, "AES")
    cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
    cipher.doFinal(encryptedData)

  }
}
