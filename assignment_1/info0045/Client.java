/*
 * INFO0045: Assignment 1
 *
 * Client.java
 */

package info0045;

import java.io.*;

import java.net.*;

import java.util.*;

import java.security.*;
import java.security.spec.*;
import java.security.interfaces.*;

import javax.crypto.*;
import javax.crypto.spec.*;
import javax.crypto.interfaces.*;

public class Client
{
    /**
     *  Sockets for network connection
     */
    private Socket client_socket = null;

    /**
     *  Client's password and username.
     */
    private String client_pwd, client_username;

    /**
     *  Path to the file containg the encrypted data.
     */
    private String encrypted_data_file;

    public Client(
        String client_pwd, String client_username, int port_nb,
        String server_host, String encrypted_data_file
    ) throws IOException, UnknownHostException
    {
        this.client_pwd = client_pwd;
        this.client_username = client_username;
        this.encrypted_data_file = encrypted_data_file;

        this.client_socket = new Socket(server_host, port_nb);
    }

    /**
     * Handle submit button event.
     */
    public void run()
        throws ClassNotFoundException, InvalidAlgorithmParameterException,
               InvalidKeyException, InvalidKeySpecException, IOException,
               NoSuchAlgorithmException, NoSuchPaddingException,
               SignatureException
    {
        //
        // Reads the encrypted file content, along with its encrypted password.
        //

        FileInputStream encrypted_file   = new FileInputStream(
            this.encrypted_data_file
        );
        ObjectInputStream encrypted_data = new ObjectInputStream(
            encrypted_file
        );

        SecretData<String> encrypted_pass    =
            (SecretData<String>) encrypted_data.readObject();
        SecretData<String> encrypted_content =
            (SecretData<String>) encrypted_data.readObject();
        encrypted_file.close();

        //
        // Starts a transaction with the server to get the deciphered encryption
        // key.
        //

        ObjectOutputStream to_server   = new ObjectOutputStream(
            this.client_socket.getOutputStream()
        );
        ObjectInputStream  from_server = new ObjectInputStream(
            this.client_socket.getInputStream()
        );

        // Then we send the encrypted content password ...
        to_server.writeObject(encrypted_pass);
        // .. and our user name.
        to_server.writeObject(this.client_username);

        to_server.flush();

        // We receive the content password, encrypted with our password.
        encrypted_pass = (SecretData<String>) from_server.readObject();

        this.client_socket.close();

        // Displays the deciphered content.
        System.out.println("The content is :");
        System.out.println(
            this.decipherContent(encrypted_content, encrypted_pass)
        );
    }

    /**
     * Uses the given, password encoded, content password to decipher the given
     * encrypted content.
     * Checks the signature of both input.
     */
    private <T extends Serializable> T decipherContent(
        SecretData<T> encrypted_content, SecretData<String> encrypted_pass
    ) throws ClassNotFoundException, InvalidAlgorithmParameterException,
             InvalidKeyException, InvalidKeySpecException, IOException,
             NoSuchAlgorithmException, NoSuchPaddingException,
             SignatureException
    {
        // Derives the keys from the client password with the username as salt.
        DerivedKeys client_keys = new DerivedKeys(
            this.client_pwd, this.client_username
        );

        // Checks the integrity of the key and deciphers it using the client
        // keys.
        DerivedKeys keys = new DerivedKeys(
            encrypted_pass.getPlaintext(client_keys)
        );

        // Uses the deciphered key to check and deciphers the content.
        return encrypted_content.getPlaintext(keys);
    }

    /**
     *  Print the usage of this program on the standard output.
     */
    public static void printUsage()
    {
        System.out.print("Usage: ");
        System.out.println("Client -u <username> -c <client_pwd> -p <port> -h " +
                           "<server_host> -f <data>");
        System.out.println();
        System.out.println("  -u\tUsername");
        System.out.println("  -c\tClient's password");
        System.out.println("  -p\tPort number the server is listening on");
        System.out.println("  -h\tHost the server is running on");
        System.out.println("  -f\tName of the encrypted file containing the data");
    }

    public static void main(String[] args)
        throws ClassNotFoundException, InvalidAlgorithmParameterException,
               InvalidKeyException, InvalidKeySpecException, IOException,
               NoSuchAlgorithmException, NoSuchPaddingException,
               SignatureException, UnknownHostException
    {
        // Check the number of arguments.
        if (args.length != 10)
        {
            printUsage();
            System.exit(-1);
        }

        // File containing the encrypted data.
        String encrypted_data_file = null;

        // Host the server is running on.
        String server_host = null;

        // Port number the server is listening on.
        String port_nb = null;

        // Username.
        String username = null;

        // Password.
        String pwd = null;

        // Get the arguments.
        for (int i = 0; i < args.length; i+=2) {
            if ((args[i].equals("-c")) && (pwd == null))
                pwd = args[i + 1];
            else if ((args[i].equals("-u")) && (username == null))
                username = args[i + 1];
            else if ((args[i].equals("-p")) && (port_nb == null))
                port_nb = args[i + 1];
            else if ((args[i].equals("-h")) && (server_host == null))
                server_host = args[i + 1];
            else if ((args[i].equals("-f")) && (encrypted_data_file == null))
                encrypted_data_file = args[i + 1];
            else {
                printUsage();
                System.exit(-1);
            }
        }

        // Create the client, and run it.
        new Client(
            pwd, username, Integer.parseInt(port_nb), server_host,
            encrypted_data_file
        ).run();
    }
}
