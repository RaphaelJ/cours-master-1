package main;

import java.lang.Math;
import java.util.*;

/**
 * Parameters class essentially consists in gathering all the command line
 * parameters of the SNMP monitor into one single object which will be
 * accessible by classes that need to know such parameters.
 * This avoids writing constructors/methods with too many arguments. The class
 * also provides a method to obtain an IP in the given range by giving a kind of
 * index (e.g. if we give 139.165.222.0/24, there are 256 addresses and address
 * at index 2 is 139.165.222.2).
 */
public class Parameters
{
   public static final int DEFAULT_THREADS = 64;

   public enum AuthLevel { NOAUTH_NOPRIV, AUTH_NOPRIV, AUTH_PRIV };
   public enum AuthProtocol { MD5, SHA1 };
   public enum PrivacyProtocol { DES, AES };

   // Fields are just parameters specified by the assignment.
   private String outputDirectory;
   private String communityName;
   private String userName;
   private AuthLevel securityLevel;
   private AuthProtocol authProtocol;
   private String authPassword;
   private PrivacyProtocol privProtocol;
   private String privPassword;
   private int trapPort;
   private int maxNbThreads;

   // Contains the address range mask in CIDR notation.
   private int splittedMask[];

   // Contains the number of addresses of the mask.
   private int nbAddresses;

   /** Constructs the object from the command line args[] array. */
   public Parameters(String args[]) throws Exception
   {
      LinkedList<String> errors = new LinkedList<String>();

      // Tries to parse the port number.
      int portNumber = Integer.parseInt(args[9]);

      // Tries to parse the maximum number of threads.
      int nbThreads = DEFAULT_THREADS;
      if (args.length == 11)
         nbThreads = Math.max(0, Integer.parseInt(args[10]));

      // Checks if the content of args[] is correct.
      // Builds an exception message if not.
      if (!args[4].equals("noAuthNoPriv") && !args[4].equals("authNoPriv")
          && !args[4].equals("authPriv"))
      {
         errors.add(
            "Security level must be : noAuthNoPriv, authNoPriv or authPriv."
         );
      }
      if (!args[5].equals("MD5") && !args[5].equals("SHA1"))
      {
         errors.add(
            "Authentication protocol must be MD5 or SHA1."
         );
      }
      if (!args[7].equals("DES") && !args[7].equals("AES"))
      {
         errors.add(
            "SNMPv3 privacy protocol must be DES or AES."
         );
      }
      if (portNumber <= 0)
      {
         errors.add(
            "Port for trap must be a positive integer."
         );
      }
      if (nbThreads == 0)
      {
         errors.add(
            "Number of threads must be a positive integer."
         );
      }

      // Tries to parse the address range.
      splittedMask = new int[4];
      int lengthMask = 0;
      try
      {
         String firstSplit[] = args[1].split("/");
         lengthMask = 32 - Integer.parseInt(firstSplit[1]);
         String secondSplit[] = firstSplit[0].split("\\.");
         splittedMask[0] = Integer.parseInt(secondSplit[0]);
         splittedMask[1] = Integer.parseInt(secondSplit[1]);
         splittedMask[2] = Integer.parseInt(secondSplit[2]);
         splittedMask[3] = Integer.parseInt(secondSplit[3]);
      }
      catch (Exception e)
      {
         errors.add(
            "The program failed to parse the address range. Please check it."
         );
      }

      // Throws an exception if the command arguments parsing failed.
      if (errors.size() > 0)
      {
         StringBuilder msg = new StringBuilder("Encountered ");
         if (errors.size() == 1)
            msg.append("one error ");
         else
            msg.append(errors.size() + " errors ");

         msg.append("while parsing the parameters :\n");

         for (String error : errors)
            msg.append("- " + error + '\n');

         throw new IllegalArgumentException(msg.toString());
      }

      // Finally sets the fields.
      outputDirectory = args[0];
      communityName = args[2];
      userName = args[3];

      if (args[4].equals("authNoPriv"))
         securityLevel = AuthLevel.AUTH_NOPRIV;
      else if (args[4].equals("authPriv"))
         securityLevel = AuthLevel.AUTH_PRIV;
      else
         securityLevel = AuthLevel.NOAUTH_NOPRIV;

      if (args[5].equals("SHA1"))
         authProtocol = AuthProtocol.SHA1;
      else
         authProtocol = AuthProtocol.MD5;

      authPassword = args[6];

      if (args[7].equals("AES"))
         privProtocol = PrivacyProtocol.AES;
      else
         privProtocol = PrivacyProtocol.DES;

      privPassword = args[8];
      trapPort = portNumber;
      maxNbThreads = nbThreads;
      nbAddresses = (int) Math.round(Math.pow(2.0, (double) lengthMask));
   }

   // Accessers.

   public String getOutputDirectory() { return outputDirectory; }
   public String getCommunityName() { return communityName; }
   public String getUserName() { return userName; }
   public AuthLevel getSecurityLevel() { return securityLevel; }
   public AuthProtocol getAuthProtocol() { return authProtocol; }
   public String getAuthPassword() { return authPassword; }
   public PrivacyProtocol getPrivProtocol() { return privProtocol; }
   public String getPrivPassword() { return privPassword; }
   public int getTrapPort() { return trapPort; }
   public int getNbAddresses() { return nbAddresses; }
   public int getMaxNbThreads() { return maxNbThreads; }

   // String accessers for the fields denoted with constants.

   public String stringSecurityLevel()
   {
      switch (securityLevel) {
      case AUTH_NOPRIV:   return "authNoPriv";
      case AUTH_PRIV:     return "authPriv";
      case NOAUTH_NOPRIV: return "noAuthNoPriv";
      default:            return null;
      }
   }

   public String stringAuthProtocol()
   {
      switch (authProtocol) {
      case SHA1: return "SHA1";
      case MD5:  return "MD5";
      default:   return null;
      }
   }

   public String stringPrivProtocol()
   {
      switch (privProtocol) {
      case AES: return "AES";
      case DES: return "DES";
      default:  return null;
      }
   }

   /** Gives the index-th IP from the address range in String format.
    * Note that the first (index = 0) address will be the network address while
    * the last one will be the broadcast address. */
   public String getIP(int index) throws IllegalArgumentException
   {
      // Negative or greater that nbAddresses - 1 indexes are out of range.
      if (index < 0 || index >= nbAddresses) {
         throw new IllegalArgumentException(
            "The given IP index is out of range."
         );
      }

      // Copies the mask into a local array.
      int ip[] = new int[] {
         splittedMask[0], splittedMask[1], splittedMask[2], splittedMask[3]
      };

      // Computes and adds the correct index-shift for each byte of the IP.
      int divisor = Math.min(256, nbAddresses);
      int quotient = index, remainder = 0;
      for (int i = 3; i >= 0; i--)
      {
         if (quotient == 0)
            break;

         remainder = quotient % divisor;
         ip[i] += remainder;
         quotient /= divisor;
      }

      return Integer.toString(ip[0]) + "." + Integer.toString(ip[1]) + "." +
             Integer.toString(ip[2]) + "." + Integer.toString(ip[3]);
   }
}
