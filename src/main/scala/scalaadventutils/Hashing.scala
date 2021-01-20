package scalaadventutils

import scala.collection.mutable.ArrayBuffer
import java.security.MessageDigest

object Hashing {

    val lookup = "0123456789abcdef".toCharArray()

    def findHashWithLeadingChars
        ( x: String
        , start: Int
        , numChars: Int
        , toCheck: Int): Int =
        Stream.from(start + 1).dropWhile(i => {
            val hash = md5AsBytes(x + i.toString)

            if (numChars % 2 == 0)
                (0 until numChars / 2).exists(hash(_) != toCheck)
            else
                (0 until (numChars - 1) / 2).exists(hash(_) != toCheck) ||
                ((hash((numChars - 1) / 2) & 0xF0) != toCheck)
        })(0)

    def md5AsString(x: String) =
        md5(x.getBytes).map("%02x".format(_)).mkString

    def md5AsBytes(x: String) = md5(x.getBytes)

    // A fast, bit-twiddling byte to byte implementation
    // Inspired by https://stackoverflow.com/a/9855338
    def md5FromBytes(x: Array[Byte]): Array[Byte] = {
        val hash: ArrayBuffer[Byte] = ArrayBuffer.fill(x.size * 2)(0)

        val is = (0 until x.size)
        val js = (0 until x.size * 2 by 2)

        (is zip js).map(n => {
            val (i, j) = n
            hash(j)     = lookup((x(i) >> 4) & 0xf).toByte
            hash(j + 1) = lookup(x(i) & 0xf).toByte
        })

        md5(hash.toArray)
    }

    def md5Multi(x: String, times: Int): String = {
        var hash = md5AsBytes(x)

        (2 to times).foreach(_ => hash = md5FromBytes(hash) )
        hash.map("%02x".format(_)).mkString
    }

    private def md5(x: Array[Byte])
        = MessageDigest.getInstance("MD5").digest(x)
}
