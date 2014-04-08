package discovery;

import monitor.*;
import retrieval.*;
import snmp.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

/**
 * Thread checking from time to time the address range given to the program to
 * detect remote SNMP agents and their variables.
 * After checking the address range, the thread sleeps for DISCOVERY_DELAY and
 * then restart the discovery process. There is a reserved number of threads
 * dedicated for the detection to avoid too many concurrent process.
 * If the agent has already been discovered, this thread updates its variable
 * set.
 */
public class DiscoveryThread extends Thread
{
   /** Number of milliseconds between two checks of the address range. */
   public final long DISCOVERY_DELAY = 5000;

   private enum LogEvent { NEW_AGENT, REMOVE_AGENT };

   private final MonitorParameters p;

   private final int             nThreads;
   private final Semaphore       threadSem;
   private final ExecutorService threadPool;

   public DiscoveryThread(MonitorParameters p)
   {
      this.p = p;

      this.nThreads   = p.getMaxNbThreads();
      this.threadSem  = new Semaphore(this.nThreads);
      this.threadPool = Executors.newFixedThreadPool(this.nThreads);
   }

   public void run()
   {
      try {
         this.discoveryLoop();
      } catch (InterruptedException e) {
         System.err.println("The discovery loop has crashes");
         e.printStackTrace();
      }
   }

   public void discoveryLoop() throws InterruptedException
   {
      final TreeMap<String, RemoteAgent> agents = new TreeMap<>();

      // Loops every DISCOVERY_DELAY the entire IP range using a pool of
      // threads.
      for (;;)
      {
         // Checks every host within the range. Skips the network and the
         // broadcast addresses.
         for (int i = 1; i < this.p.getNbAddresses() - 1; i++)
         {
            final String ip = this.p.getIP(i);
            Runnable action; // Action to execute asynchronously. Depends if the
                             // client is already known.

            RemoteAgent existingAgent;
            synchronized (agents) {
               existingAgent = agents.get(ip);
            }

            if (existingAgent != null) {
               // This IP is already known, just refresh its variables list
               // asynchronously.
               final RemoteAgent agent = existingAgent;
               action = new Runnable() {
                  public void run()
                  {
                     try {
                        agent.updateVars();
                     } catch (IOException e) {
                        try {
                           agent.dispose();
                           log(agent, LogEvent.REMOVE_AGENT);
                        } catch (IOException e2) { }
                     }
                  }
               };
            } else {
               // Unknown IP, tries to reach it for the first time.
               // Tries to retrieve the variable set using the three versions of
               // SNMP.
               action = new Runnable() {
                  public void run()
                  {
                     // Tries the three versions of SNMP.
                     RemoteAgent agent = tryAgent(
                        SNMPLink.SNMPVersion.SNMPv3, ip, p
                     );
                     if (agent == null)
                        agent = tryAgent(SNMPLink.SNMPVersion.SNMPv2c, ip, p);
                     if (agent == null)
                        agent = tryAgent(SNMPLink.SNMPVersion.SNMPv1, ip, p);

                     if (agent != null) { // Succeeds to contact the agent.
                        synchronized (agents) {
                           log(agent, LogEvent.NEW_AGENT);
                           agents.put(ip, agent);
                        }
                     }
                  }
               };
            }

            // Acquires the right to start a thread.
            this.threadSem.acquire();

            // Runs the action and releases the thread.
            final Runnable finalAction = action;
            this.threadPool.execute(
               new Runnable() {
                  public void run()
                  {
                     finalAction.run();
                     threadSem.release();
                  }
               }
            );
         }

         // Waits for every thread to finish.
         this.threadSem.acquire(this.nThreads);
         this.threadSem.release(this.nThreads);

         // Waits before next check.
         Thread.sleep(DISCOVERY_DELAY);
      }
   }

   /** Tries to connect to an agent and retrieves its list of variables.
    * Returns the RemoteAgent or null if the connection failed. */
   private static RemoteAgent tryAgent(
      SNMPLink.SNMPVersion version, String host, MonitorParameters p
   )
   {
      RemoteAgent agent = null;
      try {
         agent = new RemoteAgent(version, host, p);
         if (!agent.exists())
            return null;

         agent.updateVars();
         return agent;
      } catch (IOException e) {
         try {
            agent.dispose();
         } catch (IOException e2) { }
         return null;
      }
   }

   private synchronized void log(RemoteAgent agent, LogEvent event)
   {
      switch (event) {
      case NEW_AGENT:
         System.out.println("New agent : " + agent);
         break;
      case REMOVE_AGENT:
         System.out.println("Agent removed : " + agent);
         break;
      }
   }
}
