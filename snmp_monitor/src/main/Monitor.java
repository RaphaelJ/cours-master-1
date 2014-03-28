/*
* Main class of the program. For now, it just parses the input parameters and starts a thread
* operating the SNMP discovery. It also provides constants to recognize SNMP version, which are
* used by other classes (especially in the discovery package).
*/

package main;

import discovery.DiscoveryThread;
import database.AgentsPool;

public class Monitor
{
   // Constants to distinct SNMP versions.
   public static final int SNMPv3 = 0;
   public static final int SNMPv2c = 1;
   public static final int SNMPv1 = 2;
   
   // Method giving SNMP version in String format, given the int representation.
   public static String snmpVersion(int version)
   {
      if (version == SNMPv2c)
         return "SNMPv2c";
      else if (version == SNMPv1)
         return "SNMPv1";
      return "SNMPv3";
   }

   public static void main(String args[])
   {
      if (args.length != 10 && args.length != 11)
      {
         System.out.println("Usage : [0] ... [9] [10]");
         System.out.println("[0] : output directory for log files");
         System.out.println("[1] : IPv4 address range (in CIDR) to monitor");
         System.out.println("[2] : community name (SNMPv1/SNMPv2)");
         System.out.println("[3] : user name (SNMPv3)");
         System.out.println("[4] : security level (SNMPv3)");
         System.out.println("[5] : authentication protocol (SNMPv3)");
         System.out.println("[6] : authentication password (SNMPv3)");
         System.out.println("[7] : privacy protocol (SNMPv3)");
         System.out.println("[8] : privacy password (SNMPv3)");
         System.out.println("[9] : port listening for traps");
         System.out.println("[10] : (optional) maximum number of threads for discovery");
         return;
      }
   
      Parameters p = null;
      try
      {
         p = new Parameters(args);
      }
      catch(Exception e)
      {
         System.err.print(e.getMessage());
         return;
      }
      
      System.out.println("-- SNMP monitor --");
      System.out.println("Output directory : " + p.getOutputDirectory());
      System.out.print("IP addresses : from " + p.getIP(0) + " to ");
      System.out.println(p.getIP(p.getNbAddresses() - 1));
      System.out.println("SNMPv1/SNMPv2 community name : " + p.getCommunityName());
      System.out.println("SNMPv3 user name : " + p.getUserName());
      System.out.println("SNMPv3 security level : " + p.stringSecurityLevel());
      System.out.println("SNMPv3 authentication protocol : " + p.stringAuthProtocol());
      System.out.println("SNMPv3 authentication password : " + p.getAuthPassword());
      System.out.println("SNMPv3 privacy protocol : " + p.stringPrivProtocol());
      System.out.println("SNMPv3 privacy password : " + p.getPrivPassword());
      System.out.println("Port listening for traps : " + Integer.toString(p.getTrapPort()));
      String maxNb = Integer.toString(p.getMaxNbThreads());
      System.out.println("Maximum number of threads for discovery : " + maxNb);
      System.out.println();
      
      // Setting the agents pool and launching the discovery thread.
      AgentsPool ap = new AgentsPool(p.getNbAddresses());
      DiscoveryThread dt = null;
      try
      {
         dt = new DiscoveryThread(p, ap, p.getMaxNbThreads());
      }
      catch (Exception e)
      {
         System.err.println(e.getMessage());
         return;
      }
      dt.start();
   }
}

