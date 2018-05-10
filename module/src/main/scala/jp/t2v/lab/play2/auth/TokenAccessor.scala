package jp.t2v.lab.play2.auth

import play.api.mvc.{RequestHeader, Result}

trait TokenAccessor {

  def extract(request: RequestHeader): Option[AuthenticityToken]

  def put(token: AuthenticityToken)(result: Result)(implicit request: RequestHeader): Result

  def delete(result: Result)(implicit request: RequestHeader): Result

  //TODO Remove the printlns
  protected def verifyHmac(token: SignedToken): Option[AuthenticityToken] = {
    val (hmac, value) = token.splitAt(40)
   // println(s"from Gemma Global\n $token")
   // println(s"\nhmac\t$hmac\nvalue\t$value")
   // println(s"Resultof safeEquals\t ${safeEquals(HMACGenerator.createHMAC(value),hmac)}")
    val safe = safeEquals(HMACGenerator.createHMAC(value), hmac)
    println(s"verifyHmac Called.. on $value and safe is $safe")
    if (safe) Some(value) else None
  }

  protected def sign(token: AuthenticityToken): SignedToken = {
    //  println(s"Unsigned\n $token")
   val signed =  HMACGenerator.createHMAC(token) + token
  // println(s"signedToken:\n$signed")
    val (hmac, value) = signed.splitAt(40)
    //println(s"from GDOX\n $token")
   // println(s"\nhmac\t$hmac\nvalue\t$value")
   // println(s"Resultof safeEquals\t ${safeEquals(HMACGenerator.createHMAC(value),hmac)}")
    println(s"Sign Called.. called on $token \tsigned\t$signed")

    signed
  }

  // Do not change this unless you understand the security issues behind timing attacks.
  // This method intentionally runs in constant time if the two strings have the same length.
  // If it didn't, it would be vulnerable to a timing attack.
  protected def safeEquals(a: String, b: String) = {
   // println(s"From Safe Equals: $a\t$b")
    if (a.length != b.length) {
      false
    } else {
      var equal = 0
      for (i <- Array.range(0, a.length)) {
        equal |= a(i) ^ b(i)
      }
      equal == 0
    }
  }

}
