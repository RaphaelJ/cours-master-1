package discovery;

import java.util.concurrent.Semaphore;

/**
 * DiscoveryGuardian is a special object used by DiscoveryThread/DiscoveryUnit
 * objects to regulate the number of currently running units.
 * When launching a new DiscoveryUnit object, the DiscoveryThread object sends a
 * message to this class to signal the new unit and check if there are not too
 * many threads/units running. If yes, it will wait a bit before re-asking the
 * guardian if there is room for new threads/discovery units.
 *
 * The units, on their side, send a message to the guardian when done to signal
 * the guardian can let the discovery thread initiate new threads/units.
 *
 * Of course, the access to the DiscoveryGuardian is concurrent, thus the
 * keyword synchronized is used on every method.
 */
public class DiscoveryGuardian
{
   private final Semaphore semaphore;

   public DiscoveryGuardian(int maxNbThreads)
   {
      this.semaphore = new Semaphore(maxNbThreads);
   }

   /** Runs the given action but acquires the right to spawn a new thread
    * before.
    * Blocks until a new thread slot is available 
   public synchronized void spawnRunnable(Runnable action)
      throws InterruptedException
   {
      semaphore.aquire();

      new Thread {
         public void run()
         {
            action.run();
            semaphore.release();
         }
      }.start();
   }
}
