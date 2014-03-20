package info0045;

/**
 * This class encapsulates a plain text in an encrypted and authenticated way.
 * It uses the given cipher key to encrypt the plain text using AES. It then
 * generates an HMAC of the encrypted data with the corresponding key to ensure
 * integrity and authentication.
 */
public class SecretData {

    private SecretData(DerivedKeys keys, InputStream plaintext)
    {
    }

    private SecretData(InputStream cipherText)
    {
    }
    // Encrypt
    public static SecretData fromPlaintext(
        DerivedKeys keys, InputStream plaintext
    )
    {
        Cipher cipher = Cipher.getInstance("AES/CTR/PKCS1Padding");
        cipher.init(Cipher.ENCRYPT_MODE, keys.cipher);
        byte[] ciphertext = toByteArray(new CipherInputStream(file, cipher));

        Mac mac = Mac.getInstance("HmacSHA256");
        mac.init(keys.hmac);
        mac.;
    }
    // Decrypt
    public static SecretData fromCiphertext(
        DerivedKeys keys, InputStream plaintext
    ) throws InvalidHmacException
    {
    }

    public InputStream getCiphertext()
    {
        
    }

    private static byte[] toByteArray(InputStream is)
    {
        ByteArrayOutputStream os = new ByteArrayOutputStream();

        int nRead;
        byte[] data = new byte[1024];

        while ((nRead = is.read(data, 0, data.length)) != -1)
            os.write(data, 0, nRead);
        os.flush();

        return os.toByteArray();
    }
}