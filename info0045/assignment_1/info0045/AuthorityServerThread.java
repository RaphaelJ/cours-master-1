/*
 * INFO0045: Assignment 1
 *
 * AuthorityServerThread.java 
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


public class AuthorityServerThread extends Thread
{
    /**
     *  Socket to the client.
     */
    private final Socket client_socket;

    /**
     * The master keys used to decipher the user received keys.
     */
    private final DerivedKeys master_keys;

    /**
     * Map containing the keys for every user.
     */
    private final Map<String, DerivedKeys> user_keys;

    /**
     *  Constructor.
     *
     *  @param client_socket client socket.
     *  @param master_keys Keys needed to check and deciphers encryption keys.
     *  @param user_keys A map containing the keys for every user.
     */
    public AuthorityServerThread(
        Socket client_socket, DerivedKeys master_keys,
        Map<String, DerivedKeys> user_keys
    )
    {
        this.client_socket = client_socket;
        this.master_keys   = master_keys;
        this.user_keys     = user_keys;
    }

    /**
     *  Run the server.
     */
    public void run()
    {
        try {
            //
            // Starts a transaction with the client to provide the deciphered
            // encryption key.
            //

            ObjectOutputStream to_client   = new ObjectOutputStream(
                this.client_socket.getOutputStream()
            );
            ObjectInputStream  from_client = new ObjectInputStream(
                this.client_socket.getInputStream()
            );

            // Reads the encrypted content password.
            SecretData<String> encrypted_pass
                = (SecretData<String>) from_client.readObject();

            // Reads the plain text user name.
            String username = (String) from_client.readObject();

            // Checks and deciphers the received key using the master password
            // key, then ciphers it back using the user keys and send the
            // encrypted content to the client.
            DerivedKeys client_keys = this.user_keys.get(username);
            if (client_keys == null) {
                throw new AccessControlException(
                    "User " + username + " is not allowed on this server."
                );
            }

            to_client.writeObject(
                new SecretData<String>(
                    client_keys, encrypted_pass.getPlaintext(this.master_keys)
                )
            );
            to_client.flush();
        } catch (Exception e) {
            System.err.println(
                "Transaction with " +
                this.client_socket.getInetAddress().toString() +
                " failed."
            );
            System.err.println(e.getMessage());
            e.printStackTrace();
        } finally {
            try {
                this.client_socket.close();
            } catch (IOException e) {
                // Nested exception handler block : Because ♥♥♥ Java ♥♥♥
            }
        }
    }
}
