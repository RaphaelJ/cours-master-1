/*
 * INFO0045: Assignment 1
 *
 * Provider.java 
 */

package info0045;

import java.io.*;
import java.util.*;

import java.security.*;
import java.security.spec.*;
import java.security.interfaces.*;

import javax.crypto.*;
import javax.crypto.spec.*;
import javax.crypto.interfaces.*;

public class Provider {

    /** 
     * Used to store a pair of keys derived from another one.
     */
    private class DerivedKeys {
        public final SecretKey cipher;
        public final SecretKey hmac;

        /**
         * Uses the given key to generate a cipher key and a HMAC key.
         */
        public DerivedKeys(SecretKey key)
        {
            this(key.getEncoded());
        }

        /**
         * Uses the given string to generate a cipher key and a HMAC key.
         */
        public DerivedKeys(String seed)
        {
            this(seed.getBytes("US-ASCII"));
        }

        /**
         * Uses the given byte string to generate a cipher key and a HMAC key.
         */
        public DerivedKeys(byte[] seed)
        {
            // Uses the key as a seed for the random key generator.
            KeyGenerator gen = KeyGenerator.getInstance("AES");
            gen.init(new SecureRandom(seed));

            this.cipher = gen.generateKey();
            this.hmac   = gen.generateKey();
        }
    }

    /**
     *  Password shared with the server.
     */
    private String master_pwd;

    /**
     *  Name of the file containing the data to encrypt
     */
    private String data_file;

    /**
     *  Name of the file containing the pairs <users, pwd>
     */
    private String users_file;

    /**
     *  Name of the file where to write the encrypted data.
     */
    private String encrypted_data_file;

    /**
     *  Name of the file where to write the encrypted pairs.
     */
    private String encrypted_users_file;

    /**
     *  Your variable declarations (if any) can go here:
     */

    /**
     *  Constructor.
     *
     *  @param master_pwd The password shared with the server.
     *  @param data_file File containing the data to encrypt
     *  @param users_file File containing the keys <user,pwd>
     *  @param encrypted_data_file File where the encrypted data must be written
     *  @param encrypted_users_file File where the encrypted keys <user,pwd>
     */
    public Provider(String master_pwd, String data_file, String users_file,
                    String encrypted_data_file, String encrypted_users_file)
    {
        this.master_pwd = master_pwd;
        this.data_file = data_file;
        this.users_file = users_file;
        this.encrypted_data_file = encrypted_data_file;
        this.encrypted_users_file = encrypted_users_file;

            //
            // Your initialization code (if any) can go here:
            //    
    }

    /**
     * Run the provider.
     * This function must be modified.
     */
    public void run()
    {
        this.encryptDataFile(this.data_file, this.encrypted_data_file);
    }

    public void encryptDataFile(String sourcePath, String destPath)
    {
        try {
            // Derives the keys from the master password.
            DerivedKeys ders_pwd = new DerivedKeys(this.master_pwd);

            // Generates the key used to encrypt the file and its derivatives.
            SecretKey k_rand = KeyGenerator.getInstance("AES").generateKey();
            DerivedKeys ders_rand = new DerivedKeys(k_rand);

            // Encrypt the file content.
            Cipher text_cipher = Cipher.getInstance("AES/None/NoPadding");
            text_cipher.init(Cipher.ENCRYPT_MODE, k_rand);
            FileReader input_file = new FileReader(this.data_file);
            BufferedReader cipherText = new BufferedReader(input_file);
                //new CipherInputStream(data_file, text_cipher);

            // Copy the content of the ciphertext stream to the output file.
            FileWriter output_file = new FileWriter(this.encrypted_data_file);
            BufferedWriter output = new BufferedWriter(output_file);

            String line;
            while ((line = cipherText.readLine()) != null) {
                output.write(line);
                output.newLine();
            }

            input_file.close();
            output_file.close();
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }
    }

    /**
     *  Print the usage of this program on the standard output.
     */
    public static void printUsage()
    {
        System.out.print("Usage: ");
        System.out.println("Provider -p <master_pwd> -d <data> -u <users> " +
                           "-v <encrypt_data> -w <encrypt_users>");
        System.out.println();
        System.out.println("  -p\tPassword shared with the server");
        System.out.println("  -d\tName of the file containing the data to encrypt");
        System.out.println("  -u\tName of the file containing the pairs <user,pwd>");
        System.out.println("  -v\tName of the file where the encrypted data must be written");
        System.out.println("  -w\tName of the file where the encrypted pairs " +
                           "must be written");
    }

    public static void main(String[] args)
    {
        // Check the number of arguments.
        if (args.length != 10) {
            printUsage();
            System.exit(-1);
        }

        // Files containing the data to encrypt and the pairs <user,pwd>
        String data_file = null, users_file = null;
        // Files containing the encrypted data and pairs <user,pwd>
        String encrypted_data_file = null, encrypted_users_file = null;

        // Password shared with the authority server.
        String master_pwd = null;

        // Get the arguments.
        for (int i = 0; i < args.length; i+=2) {
            if ((args[i].equals("-p")) && (master_pwd == null))
                master_pwd = args[i + 1];
            else if ((args[i].equals("-d")) && (data_file == null))
                data_file = args[i + 1];
            else if ((args[i].equals("-u")) && (users_file == null))
                users_file = args[i + 1];
            else if ((args[i].equals("-v")) && (encrypted_data_file == null))
                encrypted_data_file = args[i + 1];
            else if ((args[i].equals("-w")) && (encrypted_users_file == null))
                encrypted_users_file = args[i + 1];
            else {
                printUsage();
                System.exit(-1);
            }
        }

        // Create and run the provider.
        Provider provider = new Provider(
            master_pwd, data_file, users_file, encrypted_data_file,
            encrypted_users_file
        );
        provider.run();
    }
}