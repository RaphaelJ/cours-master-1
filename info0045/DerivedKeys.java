package info0045;

import java.io.*;
import java.security.*;
import java.security.spec.*;
import javax.crypto.*;
import javax.crypto.spec.*;

import java.util.*;

/**
 * Generates and store a pair of keys Keys are derived using a Password Based
 * Key Derivation Function (PBKDF).
 */
public class DerivedKeys implements Serializable {

    public static final int    KEYS_LEN    = 256;

    // The PBKDF algorithm needs a salt to generate keys.
    // Running the algorithm with a given password and a given salt is
    // deterministic.
    public static final String CIPHER_SALT = "CIPHER";
    public static final String HMAC_SALT   = "HMAC";

    public final SecretKey cipher;
    public final SecretKey hmac;

    public DerivedKeys(SecretKey cipher, SecretKey hmac)
    {
        this.cipher = cipher;
        this.hmac   = hmac;
    }

    /**
     * Uses the given password to generate a cipher and an HMAC key.
     */
    public DerivedKeys(String pwd)
        throws InvalidKeySpecException, NoSuchAlgorithmException,
               UnsupportedEncodingException
    {
        this(pwd, "");
    }

    /**
     * Uses the given password and the additional salt to generate a cipher and
     * an HMAC key.
     */
    public DerivedKeys(String pwd, String salt)
        throws InvalidKeySpecException, NoSuchAlgorithmException,
               UnsupportedEncodingException
    {
        this.cipher = derviveFromPassword(pwd, getBytes(CIPHER_SALT + salt));
        this.hmac   = derviveFromPassword(pwd, getBytes(HMAC_SALT   + salt));
    }

    /**
     * Uses the Password Based Key Derivation Function (PBKDF) algorithm to
     * generate keys from passwords.
     */
    public static SecretKey derviveFromPassword(String pwd, byte[] salt)
        throws InvalidKeySpecException, NoSuchAlgorithmException,
               UnsupportedEncodingException
    {
        // We tried the algorithm with SHA-256 but it doesn't seem to be
        // supported on about every platform.
        final String KEY_GENERATOR = "PBKDF2WithHmacSHA1";
        char[] chars = pwd.toCharArray();

        SecretKeyFactory key_fact = SecretKeyFactory.getInstance(KEY_GENERATOR);
        SecretKey pbe_key = key_fact.generateSecret(
            new PBEKeySpec(chars, salt, 1, KEYS_LEN)
        );

        // Converts the password based key into an AES one.
        return new SecretKeySpec(pbe_key.getEncoded(), "AES");
    }

    /**
     * Returns the ASCII string encoded in a byte array.
     */
    private static byte[] getBytes(String str)
        throws UnsupportedEncodingException
    {
        return str.getBytes("US-ASCII");
    }
}