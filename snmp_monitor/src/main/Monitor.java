package main;

import discovery.DiscoveryThread;
import database.AgentsPool;

/**
 * Main class of the program. For now, it just parses the input parameters and
 * starts a thread operating the SNMP discovery. It also provides constants to
 * recognize SNMP version, which are used by other classes (especially in the
 * discovery package).
 */
public class Monitor
{
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
      System.out.println("Port listening for traps : " + p.getTrapPort());
      System.out.println("Maximum number of threads for discovery : " + p.getMaxNbThreads());
      System.out.println();

      // Setting the agents pool and launching the discovery thread.
      AgentsPool ap = new AgentsPool(p.getNbAddresses());

      new DiscoveryThread(p, ap, p.getMaxNbThreads()).start();
   }
}
