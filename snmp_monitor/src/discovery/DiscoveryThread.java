/**
* Thread checking from time to time the address range given to the program to detect remote SNMP
* agents running on these hosts. After checking all the addresses in the range, the thread performs
* the same task anew 5s later.
*
* For now, the thread prints out the currently running agents on the standard output every 5s.
*/

package discovery;

import main.*;
import database.*;

import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

public class DiscoveryThread extends Thread
{
   /** Number of milliseconds between two checks of the address range. */
   public final long DISCOVERY_DELAY = 5000;

   private Parameters p;
   private AgentsPool ap;
   private DiscoveryGuardian dg;

   private Map<String, RemoteAgent> agents;

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
      // Loops every DISCOVERY_DELAY the entire IP range using a pool of thread.
      for (;;)
      {
         // Checks every host within the range. Skips the network and the
         // broadcast addresses.
         for (int i = 1; i < this.p.getNbAddresses() - 1; i++)
         {
            String ip = this.p.getIP(i);

            RemoteAgent agent;
            boolean newAgent;
            if (agents.contains(ip)) {
               // This IP is already known, just refresh its variables list.
               agent = agents.get(ip);
               newAgent = false;
            } else {
               // Unknown IP, tries to reach it for the first time.
               agent = new RemoteAgent(ip);
               newAgent = true;
            }

            // Acquires the right to start a thread.
            this.threadSem.aquire();

            // Updates asynchronously the variables of the remote agent.
            final RemoteAgent finalAgent = agent;
            this.threadPool.execute(
               new Runnable() {
                  try {
                     finalAgent.updateVars();
                  } finally {
                     this.threadSem.release();
                  }
               }
            );
         }

         // Waits for every thread to finish.
         this.threadSem.aquire(this.nThreads);
         this.threadSem.release(this.nThreads);

         // Waits before next check.
         Thread.sleep(DISCOVERY_DELAY);
      }
         
         // Prints on the standard output the current agents.
         List<RemoteAgent> list = ap.listAgents();
         Collections.sort(list);
         if (list.size() > 0)
         {
            System.out.println("\nCurrently running SNMP agents :");
            for (int i = 0; i < list.size(); i++)
            {
               RemoteAgent cur = list.get(i);
               String stringSnmp = Monitor.snmpVersion(cur.getSnmpVersion());
               System.out.println(cur.getHost() + " (" + stringSnmp + ")");
            }
            System.out.println();
         }
      
         // Checks every host within the range
         for (int i = 0; i < p.getNbAddresses(); i++)
         {
            String curIP = p.getIP(i);
            
            // Discards .0 and .255 addresses; moves to next address
            if (curIP.endsWith(".0") || curIP.endsWith(".255"))
            {
               continue;
            }
            
            try
            {
               DiscoveryUnit du = new DiscoveryUnit(p, ap, dg, curIP);
               du.start();
               dg.signalNewThreadStarted();
            }
            catch (Exception e)
            {
               // Goes on, this host may be discovered later anyway
            }
            
            // Waits 100ms if too many threads.
            while (dg.tooManyThreads())
            {
               try
               {
                  Thread.sleep(100);
               }
               catch (InterruptedException e) {}
            }
         }
      
         // Waits 5s before next check.
         try
         {
            Thread.sleep(DISCOVERY_DELAY);
         }
         catch (InterruptedException e) {}
      }
   }
}

