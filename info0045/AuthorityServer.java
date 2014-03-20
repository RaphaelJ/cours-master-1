/*
 * INFO0045: Assignment 1
 *
 * AuthorityServer.java 
 */

package info0045;

import java.io.*;
import java.util.*;
import java.net.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.String;


import java.security.*;
import java.security.spec.*;
import java.security.interfaces.*;

import javax.crypto.*;
import javax.crypto.spec.*;
import javax.crypto.interfaces.*;

public class AuthorityServer
{

    /**
     *  Socket used by the server.
     */
    private ServerSocket server_socket;

    /**
     *  Password shared with the provider.
     */
    private String master_pwd;

    /**
     *  Path to the encrypted password file.
     */
    private String enc_pwd_file;

    /**
     *  Your variable declarations (if any) can go here:
     */

    /**
     *  Constructor.
     *
     *  @param port_nb The number of the port to listen on.
     *  @param master_pwd The password shared with the provider.
     *  @param enc_pwd_file The name of the encrypted password file.
     */
    public AuthorityServer(int port_nb, String master_pwd, String enc_pwd_file)
    {
        this.master_pwd = master_pwd;
        this.enc_pwd_file = enc_pwd_file;

        // Open a socket for incoming connections
        try {
            server_socket = new ServerSocket(port_nb);
        } catch (NumberFormatException nfx) {
            System.out.println(nfx.getMessage());
            nfx.printStackTrace();
        } catch (IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }

        //
        // Your initialization code (if any) can go here:
        //
    }

    public void listen()
    {
        // Wait for clients
        Socket client_socket = null;
        while (true) {
            try
            {
                client_socket = server_socket.accept();
            }
            catch (IOException iox)
            {
                System.out.println(iox.getMessage());
                iox.printStackTrace();
            }
            System.out.println("Client connected: ");
            System.out.println(client_socket.getInetAddress().toString());
            System.out.println();

            // Spawn new thread to handle the client request.
            // Depending on your design you might want to pass more
            // parameters to the AuthorityServerThread.
            new AuthorityServerThread(client_socket, master_pwd, enc_pwd_file).start();
        }
    }

    /**
     * Searches for the corresponding password of the given user in the
     * encrypted password database. Returns null if the user doesn't exist.
     */
    private String lookupPassword(String username)
    {
    	// variables definition
    	String plaintext = "";
    	int index = 0;
    	String ch = ":";
    	int beginIndex = 0;
    	int endIndex = 0;
        try {
            FileReader fr = new FileReader(enc_pwd_file);
            BufferedReader in = new BufferedReader(fr);

            StringBuilder builder = new StringBuilder();
            String temp = in.readLine();
            String fileUserName = "";

            while (temp != null) 
            {
            	endIndex = temp.indexOf (ch);	// find the index of the first occrurence of the character ':'
            	fileUserName = temp.substring (beginIndex, endIndex - 1); // gives the user name before the character ':'
            	if (fileUserName.equals(username)) 
            	{
            		String pwd = temp.substring(endIndex+1,temp.length());
            		return pwd; // return the password corresponding to the user 
            	}
                temp = in.readLine();
            }

            plaintext = builder.toString ();

            in.close();
            fr.close();
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }
        return null;
    }

    protected void finalize()
    {
        // Close the socket
        try {
            server_socket.close();
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
        System.out.println("AuthorityServer -m <master_pwd> -p <port> -f <pwds>");
        System.out.println();
        System.out.println("  -m\tPassword shared with the provider");
        System.out.println("  -p\tPort number to listen on");
        System.out.println("  -f\tName of the encrypted file containing the pairs <user,pwd>");
    }

    public static void main(String[] args)
    {
        // Check the number of arguments.
        if (args.length != 6) {
            printUsage();
            System.exit(-1);
        }

        // File containing the encrypted pairs <user,pwd>
        String pwds_file = null;

        // Password shared with the authority server.
        String master_pwd = null;

        // Port number to listen on.
        String port_nb = null;

        // Get the arguments.
        for (int i = 0; i < args.length; i+=2) {
            if ((args[i].equals("-m")) && (master_pwd == null))
                master_pwd = args[i + 1];
            else if ((args[i].equals("-p")) && (port_nb == null))
                port_nb = args[i + 1];
            else if ((args[i].equals("-f")) && (pwds_file == null))
                pwds_file = args[i + 1];
            else {
                printUsage();
                System.exit(-1);
            }
        }

        // Create and run the server.
        AuthorityServer server = null;
        try {
            server = new AuthorityServer(
                Integer.parseInt(port_nb), master_pwd, pwds_file
            );
        } catch(NumberFormatException nfx) {
            System.out.println(nfx.getMessage());
            nfx.printStackTrace();
        }

        server.listen();
    }
}
