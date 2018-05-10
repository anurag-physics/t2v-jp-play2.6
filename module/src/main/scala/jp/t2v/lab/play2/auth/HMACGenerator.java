package jp.t2v.lab.play2.auth;

import java.io.StringWriter;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import javax.crypto.Mac;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.codec.binary.Hex;

public class HMACGenerator {

    /**
     * Generates a hash-based message authentication code
     * @param message
     *            The message to generate the hmac for
     * @return A hash-based message authentication code
     * @throws NoSuchAlgorithmException
     *             If the algorithm is not supported by the provider
     * @throws InvalidKeyException
     *             If the key is invalid
     */
    public static String createHMAC( String message)
            throws NoSuchAlgorithmException, InvalidKeyException {

        String key = "1jpg1Sh9vJpd2K]V1a2/iBlTCro<KrL2loG[Gs_Nv2ckb8SQIMFGVtsqTOkXYXNM";
        // Create a key instance using the bytes of our secret key argument and
        // the proper algorithm
        SecretKeySpec signingKey = new SecretKeySpec(key.getBytes(), "HmacSHA1");
        // Create a Mac instance using Bouncy Castle as the provider
        // and the specified algorithm
        Mac mac = Mac.getInstance("HmacSHA1");

        // Initialize using the key and update with the data to
        // generate the mac from
        mac.init(signingKey);
        byte[] digest = mac.doFinal(message.getBytes());

        return Hex.encodeHexString(digest);

    }

}