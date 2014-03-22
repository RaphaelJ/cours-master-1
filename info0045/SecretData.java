package info0045;

import java.io.*;
import java.util.*;
import java.security.*;
import javax.crypto.*;
import javax.crypto.spec.*;

/**
 * This class encapsulates a ciphered text with an authentication token.
 * SecretData implements Serializable and can thus be used with readObject() and
 * writeObject().
 * When constructed, it uses the given cipher key to encrypt the plain text (of
 * type T) using AES. It then generates a SHA-256 HMAC of the encrypted data
 * with the corresponding key to ensure integrity and authentication.
 */
public class SecretData<T extends Serializable> implements Serializable {

    // Bouncy Castle doesn't seem to support any padding method when using CTR.
    public static final String CIPHER_ALGORITHM = "AES/CTR/NoPadding";

    public static final String HMAC_ALGORITHM   = "HmacSHA256";

    public final byte[] iv; // Initialization Vector
    public final byte[] ciphertext;
    public final byte[] signature;

    /**
     * Encrypts and signs the given plaintext with the given key pair.
     */
    public SecretData(DerivedKeys keys, T plaintext)
        throws IOException, InvalidKeyException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        // Encrypts the plain text.
        Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, keys.cipher);
        this.iv = cipher.getIV();
        this.ciphertext = toByteArray(
            new CipherInputStream(toInputStream(plaintext), cipher)
        );

        this.signature = genSignature(this.ciphertext, keys.hmac);
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
     * Check integrity and authentication before deciphering the content.
     */
    public T getPlaintext(DerivedKeys keys)
        throws ClassNotFoundException, InvalidAlgorithmParameterException,
               InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException, SignatureException
    {
        if (!this.isValid(keys.hmac))
            throw new SignatureException("Invalid/Unauthenticated content.");

        Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);
        cipher.init(
            Cipher.DECRYPT_MODE, keys.cipher, new IvParameterSpec(this.iv)
        );

        // Deserializes the object from the deciphered stream.
        CipherInputStream plaintextStream = new CipherInputStream(
            new ByteArrayInputStream(this.ciphertext), cipher
        );

        return (T) (new ObjectInputStream(plaintextStream)).readObject();
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

    /**
     * Reads the complete input stream and write its content into a byte array.
     */
    public static byte[] toByteArray(InputStream is) throws IOException
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
     * Serializes an object and returns its representation into an InputStream.
     */
    public static InputStream toInputStream(Serializable object)
        throws IOException
    {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(baos);

        oos.writeObject(object);

        oos.flush();
        oos.close();

        return new ByteArrayInputStream(baos.toByteArray());
    }
}