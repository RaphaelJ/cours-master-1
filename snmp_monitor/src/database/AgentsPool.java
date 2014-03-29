package database;

import java.util.*;

/**
 * AgentsPool consists in a hashmap storing every remote SNMP agent and their
 * respective data, denoted by the IP of the host where they are running.
 * Only one instance of the class will be used during the execution of the SNMP
 * monitor (this property is used to provide concurrent access to the
 * structure).
 */
public class AgentsPool
{
   private HashMap<String, RemoteAgent> agents;

   public AgentsPool(int maxNbAgents)
   {
      agents = new HashMap<String, RemoteAgent>(maxNbAgents);
   }

   public synchronized boolean agentExists(String IP)
   {
      return agents.containsKey(IP);
   }

   // Version of SNMP is the only additionnal data besides IP for now.
   public synchronized void addAgent(String IP, int snmpVersion)
   {
      RemoteAgent newAgent = new RemoteAgent(IP, snmpVersion);
      agents.put(IP, newAgent);
   }
   
   public synchronized RemoteAgent getAgent(String IP)
   {
      return agents.get(IP);
   }
   
   public synchronized void removeAgent(String IP)
   {
      agents.remove(IP);
   }
   
   public synchronized List<RemoteAgent> listAgents()
   {
      List<RemoteAgent> res = new ArrayList<RemoteAgent>(agents.values());
      return res;
   }
}

