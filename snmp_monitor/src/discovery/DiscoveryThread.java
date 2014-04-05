

package discovery;

import main.*;
import database.*;

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

   private final Parameters p;

   private int             nThreads;
   private Semaphore       threadSem;
   private ExecutorService threadPool;

   public DiscoveryThread(Parameters p, AgentsPool ap)
   {
      this.p = p;
      this.ap = ap;

      this.nThreads   = p.getMaxNbThreads();
      this.threadSem  = new Semaphore(this.Threads);
      this.threadPool = Executors.newFixedThreadPool(this.Threads);
   }

   public void run()
   {
      Map<String, RemoteAgent> agents;

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
                        agent.dispose();
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
                     RemoteAgent agent = tryAgent(SNMPLink.SNMPv3, ip, p);
                     if (agent == null)
                        agent = tryAgent(SNMPLink.SNMPv2c, ip, p);
                     if (agent == null)
                        agent = tryAgent(SNMPLink.SNMPv1, ip, p);

                     if (agent != null) { // Succeeds to contact the agent.
                        agent.start();
                        synchronized (agents) {
                           agents.put(ip, agent);
                        }
                     }
                  }
               };
            }

            // Acquires the right to start a thread.
            this.threadSem.aquire();

            // Runs the action and releases the thread.
            this.threadPool.execute(
               new Runnable() {
                  action.run();
                  this.threadSem.release();
               }
            );
         }

         // Waits for every thread to finish.
         this.threadSem.aquire(this.nThreads);
         this.threadSem.release(this.nThreads);

         // Waits before next check.
         Thread.sleep(DISCOVERY_DELAY);
      }

   /** Tries to connect to an agent by retrieving its list of variables.
    * Returns the RemoteAgent or null if the connection failed. */
   private RemoteAgent tryAgent(
      SNMPLink.SNMPVersion version, String host, Parameters p
   )
   {
      try {
         RemoteAgent agent = new RemoteAgent(version, host, p);
         agent.updateVars();
         return agent;
      } catch (IOException e) {
         return null;
      }
   }
}
