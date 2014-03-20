package info0045;

import java.io.*;
import java.security.*;
import javax.crypto.*;

/**
 * Used to store a pair of keys, potentially derived from another one.
 */
public class DerivedKeys {

    public final SecretKey cipher;
    public final SecretKey hmac;

    public DerivedKeys(SecretKey cipher, SecretKey hmac)
    {
        this.cipher = cipher;
        this.hmac   = hmac;
    }

    /**
     * Uses the given key to generate a cipher and an HMAC key.
     */
    public DerivedKeys(SecretKey key)
        throws NoSuchAlgorithmException
    {
        this(key.getEncoded());
    }

    /**
     * Uses the given string to generate a cipher and an HMAC key.
     */
    public DerivedKeys(String seed)
        throws NoSuchAlgorithmException, UnsupportedEncodingException
    {
        this(seed.getBytes("US-ASCII"));
    }

    /**
     * Uses the given byte string to generate a key and an HMAC key.
     */
    public DerivedKeys(byte[] seed)
        throws NoSuchAlgorithmException
    {
        // Uses the key as a seed for the random key generator.
        KeyGenerator gen = KeyGenerator.getInstance("AES");
        gen.init(new SecureRandom(seed));

        this.cipher = gen.generateKey();
        this.hmac   = gen.generateKey();
    }
}