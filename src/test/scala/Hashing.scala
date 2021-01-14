package scalaadventutils

import org.scalatest.funsuite.AnyFunSuite

class HashingSpec extends AnyFunSuite {

    val string = "abc0"

    test("Hashing: md5") {
        assertResult("577571be4de9dcce85a041ba0410f29f") {
            Hashing.md5AsString(string)
        }

        assertResult("eec80a0c92dc8a0777c619d9bb51e910") {
            Hashing.md5AsString(Hashing.md5AsString(string))
        }

        assert(Hashing.md5AsString(string) ==
               Hashing.md5AsBytes(string).map("%02x".format(_)).mkString)
    }

    test("Hashing: md5Multi") {
        assertResult("577571be4de9dcce85a041ba0410f29f") {
            Hashing.md5Multi(string, 1)
        }

        assertResult("16062ce768787384c81fe17a7a60c7e3") {
            Hashing.md5Multi(string, 3)
        }

        assertResult("a107ff634856bb300138cac6568c0f24") {
            Hashing.md5Multi(string, 2017)
        }

        assert(Hashing.md5AsString(string) == Hashing.md5Multi(string, 1))
    }

    test("Hashing: findHashWithLeadingChars") {
        assertResult(3231929) {
            Hashing.findHashWithLeadingChars("abc", 3231926, 5, 0)
        }

        assertResult(3938038) {
            Hashing.findHashWithLeadingChars("ckczppom", 3930000, 6, 0)
        }

        assertResult(3938038) {
            Hashing.findHashWithLeadingChars("ckczppom", 3938035, 6, 0)
        }

        assertResult(117946) {
            Hashing.findHashWithLeadingChars("ckczppom", 1, 5, 0)
        }
    }
}
