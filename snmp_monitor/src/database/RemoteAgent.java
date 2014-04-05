package database;

import main.Monitor;

import org.snmp4j.smi.OID;
import org.snmp4j.smi.VariableBinding;

/**
 * RemoteAgent is a class representing a single remote SNMP agent detected
 * during the discovery step.
 * The class provides a method to update its variable set (method called by
 * DiscoveryThread).
 * Every detected variable will be periodically checked for a new value using an
 * adaptive algorithm : first the variable is updated with a delay of
 * DEFAULT_VAR_UPDATE_DELAY. This delay is divided by two for the next
 * update if the variable has been modified and is multiplied by two if it
 * hasn't been modified.
 */
public class RemoteAgent implements Comparable<RemoteAgent>
{
   /** Minimum number of milliseconds between two updates of the same
    * variable. */
   public static final long MIN_VAR_UPDATE_DELAY     = 1000;

   /** Maximum number of milliseconds between two updates of the same
    * variable. */
   public static final long MAX_VAR_UPDATE_DELAY     = 3600 * 1000;

   /** Number of milliseconds between the two first updates of a variable. */
   public static final long DEFAULT_VAR_UPDATE_DELAY = 5000;

   /** After which number of failed updates a variable will be removed. */
   public static final int VAR_UPDATE_RETRIES        = 1;

   private class Variable {
      public final OID oid;
      public volatile String value;

      public volatile long delay = DEFAULT_VAR_UPDATE_DELAY;
      public volatile int retries = 0;

      public Variable(OID oid, String value)
      {
         this.oid   = oid;
         this.value = value;
      }
   }

   private String host;
   private SNMPLink link;

   /** Keeps all tracked variables.
    * This set of variables will be 
    */
   private TreeMap<OID, Variable> variables = new TreeMap<OID, Variable>();

   /** This timer will manage a thread which will be used to periodically probe
    * variable for new values. */
   private Timer varUpdateTimer = new Timer();

   /** Initialises a RemoteAgent instance with an empty set of variables. */
   public RemoteAgent(SNMPLink.SNMPVersion version, String host, Parameters p)
   {
      this.host = host;

      this.link = SNMPLink.getInstance(version, host, p);
   }

   // Accessers/setters
   public String getHost() { return host; }

   /** Stops the update timer and the listening socket. */
   public void dispose()
   {
      this.varUpdateTimer.cancel();
      this.link.dispose();
   }

   /** Goes through the entire MIB tree to get the new variables.
    * Does this by iterating the entire tree using GETNEXT packets starting with
    * the first lexicographic OID (".").
    * Schedules new variables to be updated with the default delay. */
   public void updateVars() throws IOException
   {
      OID cursor = new OID(".");

      // Tries to read the entire OID tree by calling recursively GETNEXT.
      for (;;) {
         PDU response = this.link.getNext(cursor);

         // Stops if the response is empty.
         if (response == null || response.getErrorStatus() != 0)
            break;

         VariableBinding bind = response.get(0);
         OID    oid   = bind.getOid();
         String value = bind.getVariable().toString();

         // Stops if if the PDU is looping (lexicographic order has been
         // reversed).
         if (oid == null || oid.compareTo(cursor) <= 0)
            break;

         synchronized (this.variables) {
            if (this.variables.contains(oid)) {
               Variable var = this.variables.get(oid);
               var.value    = value;
               var.retries  = 0;
            } else { // New variable, schedules it.
               Variable var = new Variable(oid, value);
               this.variables.add(oid, var);
               this.scheduleVar(var, var.delay);
            }
         }

         cursor = oid;
      }
   }

   /** Updates the value of the given variable by probing the remote agent. */
   private updateVar(Variable var)
   {
      synchronized (this.variables) {
         if (!this.variables.contains(var))
            // The variable has been removed since it was scheduled.
            return;
      }

      // Probes the new value of the variable.
      // If the variable value changed, reschedules it with half the delay.
      // If the variable value didn't change, reschedules it by doubling the
      // delay.
      // If the update failed, reschedules the update using the same delay or
      // removes it if the number of retries exceeded VAR_UPDATE_RETRIES.
      String newValue = probeVar(var);
      if (newValue == null) { // Failed to receive the value.
         var.retries++;

         // Removes the variable if too many retries.
         if (var.retries > VAR_UPDATE_RETIES)
            this.removeVar(var);
         else
            this.scheduleVar(var, var.delay)
      } else {                // Received a new value.
         var.retries = 0;

         // Reschedules the variable according to its change.
         if (var.value != newValue)
            this.scheduleVar(var, var.delay / 2);
         else {
            var.value = newValue;
            this.scheduleVar(var, var.delay * 2);
         }
      }
   }

   /** Probes the client for the new value of the given variable.
    * Returns null if the query failed. */
   private String probeVar(Variable var)
   {
      try {
         PDU response = this.link.get();
         return response.get(0).getVariable().toString();
      } catch (IOException e) {
         return null;
      }
   }

   private void removeVar(Variable var)
   {
      synchronized (this.variables) {
         this.variables.remove(var);
      }
   }

   /** Schedules a variable update with the given number of milliseconds as
    * delay. */
   private void scheduleVar(final Variable var, long delay)
   {
      if (delay > MAX_VAR_UPDATE_DELAY)
         delay = MAX_VAR_UPDATE_DELAY;
      else if (delay < MIN_VAR_UPDATE_DELAY)
         delay = MIN_VAR_UPDATE_DELAY;
      var.delay = delay;

      this.varUpdateTimer.schedule(
         new TimerTask() {
            updateVar(var);
         }, delay
      );
   }

   /** Compares two remote agents.
    * This is used to sort agents by the IP of their host computer. */
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
