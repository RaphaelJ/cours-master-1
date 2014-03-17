/*
 * INFO0045: Assignment 1
 *
 * Client.java
 */

package info0045;

import java.io.*;
import java.util.*;
import java.net.*;
import java.awt.*;
import java.awt.event.*;

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
    private PrintWriter to_server = null;
    private BufferedReader from_server = null;

    /**
     *  Client's password and username.
     */
    String pwd, username;

    /**
     *  Path to the file containg the encrypted data.
     */
    String enc_data_file;

    //
    // Your variable declarations (if any) can go here:
    //

    public Client(String pwd, String username, int port_nb,
                  String server_host, String enc_data_file)
    {
        this.pwd = pwd;
        this.username = username;
        this.enc_data_file = enc_data_file;

        // Create a socket.
        try {
            client_socket = new Socket(server_host, port_nb);
        } catch(UnknownHostException uhx) {
            System.out.println(uhx.getMessage());
            uhx.printStackTrace();
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }

        //
        // Your initialization code (if any) can go here:
        //

    }

    /**
     * Handle submit button event.
     */  
    public void run()
    {
        // Connect to the server
        try {
            to_server = new PrintWriter(client_socket.getOutputStream(), true);
            from_server = new BufferedReader(
                new InputStreamReader(client_socket.getInputStream())
            );
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }

        //
        // Most of your code can go here, right now we send the username
        // to the server in the clear and display the reply. Your code should
        // authenticate to the server, obtain the key, and finally decrypt 
        // and display the protected content.
        // 

        String status = null;
        String content = "Don't know yet!\n";

        // Send a message to the server
        to_server.println(username);

        // Receive a message from the server and display it
        String reply = null;
        try {
            reply = from_server.readLine();
            System.out.println("Server says: " + reply + "\n");
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }

        // Close the connection 
        try {
            to_server.close();
            from_server.close();
            client_socket.close();
        } catch(IOException iox) {
            System.out.println(iox.getMessage());
            iox.printStackTrace();
        }

        // Display the result        
        System.out.println("The content is: \n" + content);
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
    {
        // Check the number of arguments.
        if (args.length != 10)
        {
            printUsage();
            System.exit(-1);
        }

        // File containing the encrypted data.
        String enc_data_file = null;

        // Host the server is running on.
        String server_host = null;

        // Port number the server is listening on.
        String port_nb = null;

        // Username.
        String username = null;

        // Password.
        String client_pwd = null;

        // Get the arguments.
        for (int i = 0; i < args.length; i+=2) {
            if ((args[i].equals("-c")) && (client_pwd == null))
                client_pwd = args[i + 1];
            else if ((args[i].equals("-u")) && (username == null))
                username = args[i + 1];
            else if ((args[i].equals("-p")) && (port_nb == null))
                port_nb = args[i + 1];
            else if ((args[i].equals("-h")) && (server_host == null))
                server_host = args[i + 1];
            else if ((args[i].equals("-f")) && (enc_data_file == null))
                enc_data_file = args[i + 1];
            else {
                printUsage();
                System.exit(-1);
            }
        }

        // Create the client, and run it.
        Client client = null;
        try {
            client = new Client(client_pwd, username,
                                Integer.parseInt(port_nb),
                                server_host,enc_data_file);
        } catch(NumberFormatException nfx) {
            System.out.println(nfx.getMessage());
            nfx.printStackTrace();
        }

        client.run();
    }
}
