package database;

import main.Monitor;

/**
 * RemoteAgent is a class representing a single remote SNMP agent detected
 * during the discovery step.
 * For now, only the IP of the host computer and the SNMP version are maintained
 * in this class, but it should contain some structure with the records for this
 * agent in the final version.
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
   public static final int VAR_UPDATE_RETRIES       = 1;

   private class Variable implements Comparable<Variable> {
      public final OID oid;
      public String value;

      public long delay = DEFAULT_VAR_UPDATE_DELAY;
      public int reties = 0;

      public Variable(OID oid, String value)
      {
         this.oid   = oid;
         this.value = value;
      }

      /** Compares using only the variable id. */
      public int compareTo(Variable other)
      {
         this.oid.compareTo(other.oid);
      }
   }

   private String host;
   private Monitor.SNMPVersion version;

   /** Keeps all tracked variables.
    * The first thread will probe repeatedly the remote agent to update the
    * set of variables.
    */
   private Set<Variable> variables;

   /** This timer will manage a thread which will be used to periodically probe
    * variable for new values. */
   private Timer varUpdateTimer = new Timer();

   public RemoteAgent(String host, Monitor.SNMPVersion version)
   {
      assert version < 0 || version > 3;

      this.host    = host;
      this.version = version;
   }

   // Accessers/setters
   public String getHost() { return host; }
   public int getSnmpVersion() { return snmpVersion; }
   public void setSnmpVersion(int version) { snmpVersion = version; }

   private updateVar(Variable var)
   {
      synchronized (this.variables) {
         if (!this.variables.contains(var))
            // The variable has been removed since it was scheduled.
            return;

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
               removeVar(var);
            else
               rescheduleVar(var, var.delay)
         } else {                // Received a new value.
            var.retries = 0;

            // Reschedules the variable according to its change.
            if (var.value != newValue)
               rescheduleVar(var, var.delay / 2);
            else {
               var.value = newValue;
               rescheduleVar(var, var.delay * 2);
            }
         }
      }
   }

   /** Probes the client for the new value of the given variable.
    * Returns null if the query failed. */
   private String probeVar(Variable var)
   {
      var.oid;
   }

   private void removeVar(Variable var)
   {
      synchronized (this.variables) {
         this.variables.remove(var);
      }
   }

   /** Schedules a variable update with the given number of milliseconds as
    * delay. */
   private void rescheduleVar(final Variable var, long delay)
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
