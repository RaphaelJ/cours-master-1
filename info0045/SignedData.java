package info0045;

import java.io.*;
import java.util.*;
import java.security.*;
import javax.crypto.*;

/**
 * This class encapsulates a plain text with an authentication token.
 * SignedData implements Serializable and can thus be used with readObject() and
 * writeObject().
 * When constructed, it then generates a SHA-256 HMAC of the data with the
 * given key to ensure integrity and authentication.
 */
public class SignedData implements Serializable {

    public static final String HMAC_ALGORITHM   = "HmacSHA256";

    public final byte[] plaintext;
    public final byte[] signature;

    /**
     * Signs the given plaintext with the given key.
     */
    public SignedData(SecretKey key, byte[] plaintext)
        throws InvalidKeyException, NoSuchAlgorithmException
    {
        this.plaintext = plaintext;
        this.signature = genSignature(plaintext, key);
    }

    /**
     * Returns true if this current SignedData signature checks the given HMAC
     * key.
     */
    public boolean isValid(SecretKey key)
        throws InvalidKeyException, NoSuchAlgorithmException
    {
        return Arrays.equals(
            this.signature, genSignature(this.plaintext, key)
        );
    }

    /**
     * Uses the given key to generate an SHA-256 HMAC of the given data.
     */
    public static byte[] genSignature(byte[] data, SecretKey key)
        throws InvalidKeyException, NoSuchAlgorithmException
    {
        Mac mac = Mac.getInstance(HMAC_ALGORITHM);
        mac.init(key);
        return mac.doFinal(data);
    }
}