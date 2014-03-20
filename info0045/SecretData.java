package info0045;

import java.io.*;
import java.util.*;
import java.security.*;
import javax.crypto.*;

/**
 * This class encapsulates a cipher text with an authentication token.
 * SecretData implements Serializable and can thus be used with readObject() and
 * writeObject().
 * When constructed, it uses the given cipher key to encrypt the plain text
 * using AES. It then generates a SHA-256 HMAC of the encrypted data with the
 * corresponding key to ensure integrity and authentication.
 */
public class SecretData implements Serializable {

    // Bouncy Castle doesn't seem to support any padding method when using CTR.
    public static final String CIPHER_ALGORITHM = "AES/CTR/NoPadding";
    public static final String HMAC_ALGORITHM   = "HmacSHA256";

    public final byte[] ciphertext;
    public final byte[] signature;

    /**
     * Encrypts and signs then given plaintext with the given key pair.
     */
    public SecretData(DerivedKeys keys, InputStream plaintext)
        throws IOException, InvalidKeyException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        // Encrypts the plain text.
        Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, keys.cipher);
        this.ciphertext = toByteArray(
            new CipherInputStream(plaintext, cipher)
        );

        this.signature = genSignature(this.ciphertext, keys.hmac);
    }

    /**
     * Encrypts and signs then given plaintext with the given key pair.
     */
    public SecretData(DerivedKeys keys, byte[] plaintext)
        throws InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        this(keys, new ByteArrayInputStream(plaintext));
    }

    /**
     * Returns true if this current SecretData signature checks the given HMAC
     * key.
     */
    public boolean isValid(SecretKey hmacKey)
        throws InvalidKeyException, NoSuchAlgorithmException
    {
        return Arrays.equals(
            this.signature, genSignature(this.ciphertext, hmacKey)
        );
    }

    /**
     * Returns the deciphered cipher text using the given cipher key.
     * Doesn't check integrity nor authentication.
     */
    public byte[] getPlaintext(SecretKey cipherKey)
        throws InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, cipherKey);
        return toByteArray(
            new CipherInputStream(
                new ByteArrayInputStream(this.ciphertext), cipher
            )
        );
    }

    /**
     * Reads the complete input stream and write its content into a byte array.
     */
    private static byte[] toByteArray(InputStream is) throws IOException
    {
        ByteArrayOutputStream os = new ByteArrayOutputStream();

        int nRead;
        byte[] data = new byte[1024];

        while ((nRead = is.read(data, 0, data.length)) != -1)
            os.write(data, 0, nRead);
        os.flush();

        return os.toByteArray();
    }

    /**
     * Uses the given key to generate an SHA-256 HMAC of the given data.
     */
    private static byte[] genSignature(byte[] data, SecretKey key)
        throws InvalidKeyException, NoSuchAlgorithmException
    {
        Mac mac = Mac.getInstance(HMAC_ALGORITHM);
        mac.init(key);
        return mac.doFinal(data);
    }
}