/**
* RemoteAgent is a class representing a single remote SNMP agent detected during the discovery
* step. For now, only the IP of the host computer and the SNMP version are maintained in this
* class, but it should contain some structure with the records for this agent in the final
* version.
*/

package database;

import main.Monitor;

public class RemoteAgent implements Comparable<RemoteAgent>
{
   private String host;
   private int snmpVersion;
   
   public RemoteAgent(String host, int version)
   {
      this.host = host;
      if (version >= 0 && version < 3)
         snmpVersion = version;
      else
         snmpVersion = Monitor.SNMPv3;
   }

   // Accessers/setters
   public String getHost() { return host; }
   public int getSnmpVersion() { return snmpVersion; }
   public void setSnmpVersion(int version) { snmpVersion = version; }
   
   // Comparison method. This is used to sort agents by the IP of their host computer.
   public int compareTo(RemoteAgent ra)
   {
      String host2 = ra.getHost();
      if (host.length() == host2.length())
         return host.compareTo(ra.getHost());
      else if (host.length() < host2.length())
         return -1;      
      return 1;
   }
}

