package fr.acinq.syscoin

import java.security.cert._
import java.security.{KeyStore, PrivateKey, PublicKey, Signature}
import javax.naming.ldap.LdapName

import com.google.protobuf.ByteString
import org.syscoin.protocols.payments.Protos.{PaymentDetails, PaymentRequest, X509Certificates}

import scala.collection.JavaConversions._

/**
 * see https://github.com/syscoin/bips/blob/master/bip-0070.mediawiki
 */
object PaymentProtocol {

  def displayName(certificate: X509Certificate): String = {
    val dn = certificate.getSubjectX500Principal().getName()
    val ldapDN = new LdapName(dn)
    ldapDN.getRdns.find(_.getType == "CN").map(_.getValue.asInstanceOf[String]).getOrElse("")
  }

  def verifySignature(request: PaymentRequest, keystore: KeyStore): (String, PublicKey, TrustAnchor) = {
    val details = PaymentDetails.parseFrom(request.getSerializedPaymentDetails)

    val factory = CertificateFactory.getInstance("X.509")
    val certificates = X509Certificates.parseFrom(request.getPkiData).getCertificateList.map(bytes => factory.generateCertificate(bytes.newInput()).asInstanceOf[X509Certificate])
    val certpath = factory.generateCertPath(certificates)

    // Retrieves the most-trusted CAs from keystore.
    val params = new PKIXParameters(keystore)
    // Revocation not supported in the current version.
    params.setRevocationEnabled(false)
    params.setExplicitPolicyRequired(false)

    // Now verify the certificate chain is correct and trusted. This let's us get an identity linked pubkey.
    val validator = CertPathValidator.getInstance("PKIX")
    val result = validator.validate(certpath, params).asInstanceOf[PKIXCertPathValidatorResult]
    val publicKey = result.getPublicKey

    val algorithm = request.getPkiType match {
      case "x509+sha256" => "SHA256withRSA"
      case "x509+sha1" => "SHA1withRSA"
      case invalid => throw new RuntimeException(s"illegal pki type: $invalid")
    }
    val signature = Signature.getInstance(algorithm)
    signature.initVerify(publicKey)
    signature.update(request.toBuilder.setSignature(ByteString.EMPTY).build().toByteArray)
    if (!signature.verify(request.getSignature.toByteArray)) throw new RuntimeException("invalid signature")
    (displayName(certificates(0)), publicKey, result.getTrustAnchor)
  }

  def sign(request: PaymentRequest, certificates: Seq[X509Certificate], privateKey: PrivateKey) : PaymentRequest = {
    def toByteString(cert: X509Certificate) = ByteString.copyFrom(cert.getEncoded)

    // sign a request with all fields set except for the signature which remains empty
    val request1 = request.toBuilder
      .setPkiType("x509+sha256")
      .setPkiData(X509Certificates.newBuilder().addAllCertificate(certificates.map(toByteString)).build().toByteString)
      .setSignature(ByteString.EMPTY)
      .build()

    val algorithm = privateKey.getAlgorithm.toUpperCase match {
      case "RSA" => "SHA256withRSA"
      case invalid => throw new RuntimeException(s"illegal algorithm: $invalid")
    }
    val signature = Signature.getInstance(algorithm)
    signature.initSign(privateKey)
    signature.update(request1.toByteArray)
    val sig = signature.sign()

    // add the signature field to the request
    val request2 = request1.toBuilder.setSignature(ByteString.copyFrom(sig))
    request2.build()
  }
}
