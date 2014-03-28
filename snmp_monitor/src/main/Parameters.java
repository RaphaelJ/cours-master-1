/*
* Parameters class essentially consists in gathering all the main parameters of the SNMP monitor
* into one single object which will be accessible by classes that need to know such parameters.
* This avoids writing constructors/methods with too many arguments. The class also provides a
* method to obtain an IP in the given range by giving a kind of index (e.g. if we give
* 139.165.222.0/24, there are 256 addresses and address 2 is 139.165.222.2).
*/

package main;

import java.lang.Math;

public class Parameters
{
   // Constants for fields for which there are only 2 or 3 possible values.
   public static final int NOAUTH_NOPRIV = 0;
   public static final int AUTH_NOPRIV = 1;
   public static final int AUTH_PRIV = 2;
   
   public static final int MD5 = 0;
   public static final int SHA1 = 1;
   
   public static final int DES = 0;
   public static final int AES = 1;

   // Fields are just parameters specified by the assignement.
   private String outputDirectory;
   private String communityName;
   private String userName;
   private int securityLevel;
   private int authProtocol;
   private String authPassword;
   private int privProtocol;
   private String privPassword;
   private int trapPort;
   private int maxNbThreads;
   
   // We will also get the address range in CIDR notation; for this one, we use a few more fields.
   private int splittedIP[];
   private int nbAddresses;

   // Constructor; takes directly the args[] array.
   public Parameters(String args[]) throws Exception
   {
      int nbErrors = 0;
      String errors = "";
      
      // Tries to parse the port number.
      int portNumber = 0;
      try
      {
         portNumber = Integer.parseInt(args[9]);
      }
      catch (Exception e) {}
      
      // Tries to parse the maximum number of threads (default value : 64).
      int nbThreads = 64;
      if (args.length == 11)
      {
         try
         {
            int temp = 0;
            temp = Integer.parseInt(args[10]);
            if (temp >= 1)
               nbThreads = temp;
            else
               nbThreads = 0;
         }
         catch (Exception e) {}
      }
   
      // Checks if the content of args[] is correct. Prepare an exception message if not.
      if (!args[4].equals("noAuthNoPriv") && !args[4].equals("authNoPriv") && !args[4].equals("authPriv"))
      {
         nbErrors++;
         errors += "-Security level must be : noAuthNoPriv, authNoPriv or authPriv.\n";
      }
      if (!args[5].equals("MD5") && !args[5].equals("SHA1"))
      {
         nbErrors++;
         errors += "-Authentication protocol must be MD5 or SHA1.\n";
      }
      if (!args[7].equals("DES") && !args[7].equals("AES"))
      {
         nbErrors++;
         errors += "-SNMPv3 privacy protocol must be DES or AES.\n";
      }
      if (portNumber <= 0)
      {
         nbErrors++;
         errors += "-Port for trap must be a positive integer.\n";
      }
      if (nbThreads == 0)
      {
         nbErrors++;
         errors += "-Number of threads must be a positive integer.\n";
      }
      
      // Tries to parse the address range.
      int IP[] = new int[4];
      int lengthMask = 0;
      try
      {
         String firstSplit[] = args[1].split("/");
         lengthMask = 32 - Integer.parseInt(firstSplit[1]);
         String secondSplit[] = firstSplit[0].split("\\.");
         IP[0] = Integer.parseInt(secondSplit[0]);
         IP[1] = Integer.parseInt(secondSplit[1]);
         IP[2] = Integer.parseInt(secondSplit[2]);
         IP[3] = Integer.parseInt(secondSplit[3]);
      }
      catch (Exception e)
      {
         nbErrors++;
         errors += "-The program failed to parse the address range. Please check it.\n";
      }
      
      // Throws an exception if necessary.
      if (nbErrors > 0)
      {
         String msg = "Encountered ";
         if(nbErrors == 1)
            msg += "one error ";
         else
            msg += Integer.toString(nbErrors) + " errors ";
         msg += "while parsing the parameters :\n" + errors;
         throw new Exception(msg);
      }
      
      // Finally sets the fields.
      outputDirectory = args[0];
      communityName = args[2];
      userName = args[3];
      securityLevel = NOAUTH_NOPRIV;
      if (args[4].equals("authNoPriv"))
         securityLevel = AUTH_NOPRIV;
      else if (args[4].equals("authPriv"))
         securityLevel = AUTH_PRIV;
      authProtocol = MD5;
      if (args[5].equals("SHA1"))
         authProtocol = SHA1;
      authPassword = args[6];
      privProtocol = DES;
      if (args[7].equals("AES"))
         privProtocol = AES;
      privPassword = args[8];
      trapPort = portNumber;
      maxNbThreads = nbThreads;
      
      nbAddresses = (int) Math.pow(2.0, (double) lengthMask);
      splittedIP = IP;
   }
   
   // Accessers.
   public String getOutputDirectory() { return outputDirectory; }
   public String getCommunityName() { return communityName; }
   public String getUserName() { return userName; }
   public int getSecurityLevel() { return securityLevel; }
   public int getAuthProtocol() { return authProtocol; }
   public String getAuthPassword() { return authPassword; }
   public int getPrivProtocol() { return privProtocol; }
   public String getPrivPassword() { return privPassword; }
   public int getTrapPort() { return trapPort; }
   public int getNbAddresses() { return nbAddresses; }
   public int getMaxNbThreads() { return maxNbThreads; }
   
   // String accessers for the fields denoted with constants.
   public String stringSecurityLevel()
   {
      if (securityLevel == AUTH_NOPRIV)
         return "authNoPriv";
      else if (securityLevel == AUTH_PRIV)
         return "authPriv";
      return "noAuthNoPriv";
   }
   
   public String stringAuthProtocol()
   {
      if (authProtocol == SHA1)
         return "SHA1";
      return "MD5";
   }
   
   public String stringPrivProtocol()
   {
      if (privProtocol == AES)
         return "AES";
      return "DES";
   }
   
   // Method giving an IP in String format given an "index" (see header of this file).
   public String getIP(int index)
   {
      // Index = 0 or nbAddresses - 1 if out of range (should never happen, but just in case).
      if (index < 0)
         index = 0;
      else if (index >= nbAddresses)
         index = nbAddresses - 1;
      
      int copy[] = new int[4];
      copy[0] = splittedIP[0];
      copy[1] = splittedIP[1];
      copy[2] = splittedIP[2];
      copy[3] = splittedIP[3];
      
      int divider = nbAddresses;
      if (divider > 256)
         divider = 256;
      
      int quotient = index, remainder = 0;
      for (int i = 3; i >= 0; i--)
      {
         if (quotient == 0)
            break;
      
         remainder = quotient % divider;
         copy[i] += remainder;
         quotient /= divider;
      }
      
      String res = Integer.toString(copy[0]) + "." + Integer.toString(copy[1]) + ".";
      res += Integer.toString(copy[2]) + "." + Integer.toString(copy[3]);
      
      return res;
   }
}

