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
        throws InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        // Derives the keys from the master password.
        DerivedKeys master_keys = new DerivedKeys(this.master_pwd);

        this.encryptDataFile(master_keys);
        this.encryptPassFile(master_keys);
    }

    /**
     * Creates a new random key, uses it to encrypt the source file.
     * Writes the random key (encrypted using the master password) and the
     * cipher text to the destination file.
     */
    public void encryptDataFile(DerivedKeys master_keys)
        throws InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        // Generates the key used to encrypt the file. Creates a derived key
        // pair.
        SecretKey k_rand = KeyGenerator.getInstance("AES").generateKey();
        DerivedKeys ders_rand = new DerivedKeys(k_rand);

        FileOutputStream output_file = new FileOutputStream(
            this.encrypted_data_file
        );
        ObjectOutputStream output = new ObjectOutputStream(output_file);

        // Writes the encrypted random key to the output file.
        output.writeObject(new SecretData(master_keys, k_rand.getEncoded()));

        // Write the encrypted content to the output file.
        FileInputStream input_file = new FileInputStream(this.data_file);
        output.writeObject(new SecretData(ders_rand, input_file));

        input_file.close();

        output.flush();
        output_file.close();
    }

    /**
     * Encrypt the password file using the keys derived from the master
     * password.
     */
    public void encryptPassFile(DerivedKeys master_keys)
        throws InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException
    {
        // Write the encrypted content to the output file.
        FileInputStream input_file = new FileInputStream(this.users_file);
        FileOutputStream output_file = new FileOutputStream(
            this.encrypted_users_file
        );

        ObjectOutputStream output = new ObjectOutputStream(output_file);
        output.writeObject(new SecretData(master_keys, input_file));

        input_file.close();
        output.flush();
        output_file.close(); 
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
        throws InvalidKeyException, IOException, NoSuchAlgorithmException,
               NoSuchPaddingException
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
