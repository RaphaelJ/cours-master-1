/**
* DiscoveryGuardian is a special object used by DiscoveryThread/DiscoveryUnit objects to regulate
* the number of currently running units. When launching a new DiscoveryUnit object, the
* DiscoveryThread object sends a message to this class to signal the new unit and check if there
* are not too many threads/units running. If yes, it will wait a bit before re-asking the guardian
* if there is room for new threads/discovery units.
*
* The units, on their side, send a message to the guardian when done to signal the guardian can
* let the discovery thread initiate new threads/units.
*
* Of course, the access to the DiscoveryGuardian is concurrent, thus the keyword synchronized is
* used on every method.
*/

package discovery;

public class DiscoveryGuardian
{
   private int maxNbThreads;
   private int currentlyRunning;

   public DiscoveryGuardian(int maxNbThreads)
   {
      this.maxNbThreads = maxNbThreads;
      currentlyRunning = 0;
   }
   
   public synchronized void signalNewThreadStarted()
   {
      currentlyRunning++;
   }
   
   public synchronized void signalThreadIsDone()
   {
      currentlyRunning--;
   }
   
   public synchronized boolean tooManyThreads()
   {
      if (currentlyRunning >= maxNbThreads)
         return true;
      return false;
   }
}

