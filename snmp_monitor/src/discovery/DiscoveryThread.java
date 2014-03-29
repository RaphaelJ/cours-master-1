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

public class DiscoveryThread extends Thread
{
   private Parameters p;
   private AgentsPool ap;
   private DiscoveryGuardian dg;

   private Map<String, RemoteAgent> agents;

   public DiscoveryThread(Parameters p, AgentsPool ap, int maxNbThreads)
   {
      this.p = p;
      this.ap = ap;
      dg = new DiscoveryGuardian(maxNbThreads);
   }

   public void run()
   {
      for (;;)
      {
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
            Thread.sleep(5000);
         }
         catch (InterruptedException e) {}
      }
   }
}

