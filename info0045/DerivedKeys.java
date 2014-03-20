package info0045;

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
    {
        this(key.getEncoded);
    }

    /**
     * Uses the given string to generate a cipher and an HMAC key.
     */
    public DerivedKeys(String seed)
    {
        this(seed.getBytes("US-ASCII"));
    }

    /**
     * Uses the given byte string to generate a key and an HMAC key.
     */
    public DerivedKeys(byte[] seed)
    {
        // Uses the key as a seed for the random key generator.
        KeyGenerator gen = KeyGenerator.getInstance("AES");
        gen.init(new SecureRandom(key.getEncoded());

        this(gen.generateKey(), gen.generateKey());
    }
}