package traps;

import snmp.*;

import org.snmp4j.smi.OID;

/** Runs a loop which pools the RAM usage and send a SNMPv2c trap when the free
 * ram is below 10% of the total available RAM.
 *
 * Remarks :
 * - this script doesn't take into account the bufferised and cached potion of
 *   the memory which but only the truly free RAM ;
 * - this script will not send a new trap unless the total free ram fell below
 *   10%. */
public class TrapsDaemon extends Thread {
   /** Number of milliseconds between two probes of the RAM usage. */
   public static final long POOLING_DELAY = 5000;

   public static final OID FREE_RAM_OID = new OID(".1.3.6.1.4.1.2021.4.11.0");

   private class RAMUsage {
      public final long total;
      public final long free;

      public class RAMUsage(long total, long free)
      {
         total = total;
         free  = free;
      }
   }

   private final SNMPv2cLink<SNMPParameters> link;

   public TrapsDaemon(final String communityName)
   {
      SNMPParameters p = new SNMPParameters() {
         public String getCommunityName()
         {
            return communityName;
         }
      };
      this.link = new SNMPv2cLink<SNMPParameters>(host, port, p);
   }

   public static RAMUsage getCurrentRAMUsage()
   {
   }

   public void run()
   {
      
   }

   public static void main(String[] args)
   {
   }
}
