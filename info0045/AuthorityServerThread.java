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
     *  Master password
     */
    private String master_pwd = null;

    /**
     *  Encrypted password file
     */
    String enc_passwd_file = null;

    /**
     *  Sockets for network connection
     */
    private Socket client_socket = null;
    private PrintWriter to_client = null;
    private BufferedReader from_client = null;

    /**
     *  Your variable declarations (if any) can go here:
     */

    /**
     *  Constructor.
     *
     *  @param c_sock The client socket.
     *  @param pwd The master password.
     *  @param enc_f The name of the encrypted password file.
     */
    public AuthorityServerThread(Socket c_sock, String pwd, String enc_f)
    {
        client_socket = c_sock;
        master_pwd = pwd;
        enc_passwd_file = enc_f;
    }

    /**
     *  Run the server.
     */
    public void run()
    {
        // Set up communication streams
        try {
            to_client = new printwriter(client_socket.getoutputstream(), true);
            from_client = new bufferedreader(new inputstreamreader(
                client_socket.getinputstream())
            );
        } catch(IOException iox) {
            System.out.println(iox.getMessage());	    
            iox.printStackTrace();
        }

        //
        // Most of your code can go here, right now we just receive a message
        // from the client and echo if back. Your code should authenticate the 
        // client and provide him with the key for the protected content.        
        // 

        // Receive a message from the client.
        String message = null;
        try {
            message = from_client.readLine();
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }

        // Send a message to the client.
        to_client.println("Client says: " + message);

        // Close the connection 
        try {
            to_client.close();
            from_client.close();
            client_socket.close();
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }
    }
}
