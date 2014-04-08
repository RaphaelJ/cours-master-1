package traps;

import snmp.*;

import java.io.*;
import org.snmp4j.smi.OID;

/** Runs a loop which pools the RAM usage and send a SNMPv2c trap when the free
 * ram is below 10% of the total available RAM.
 *
 * Remarks :
 * - this script doesn't take into account the bufferised and cached potion of
 *   the memory which but only the truly free RAM ;
 * - this script will not send a new trap while the total free ram is still
 *   below 10%. */
public class TrapsDaemon {
   /** Number of milliseconds between two probes of the RAM usage. */
   public static final long POOLING_DELAY = 5000;

   /** Fraction of the free RAM after which a trap is sent. */
   public static final double ALERT_LEVEL = 0.10;

   public static final OID FREE_RAM_OID = new OID(".1.3.6.1.4.1.2021.4.11.0");

   /** Contains the memory usage in kB. */
   private class RAMUsage {
      public final long total;
      public final long free;

      public RAMUsage(long total, long free)
      {
         this.total = total;
         this.free  = free;
      }
   }

   private final SNMPv2cLink<SNMPParameters> link;

   public TrapsDaemon(String host, int port, final String communityName)
      throws IOException
   {
      SNMPParameters p = new SNMPParameters() {
         public String getCommunityName()
         {
            return communityName;
         }
      };
      this.link = new SNMPv2cLink<SNMPParameters>(host, port, p);
   }

   /** Reads the /proc/meminfo file to retreive the current memory usage. */
   private RAMUsage getRAMUsage()
      throws FileNotFoundException, IOException
   {
      FileReader     file   = new FileReader("/proc/meminfo");
      BufferedReader reader = new BufferedReader(file);

      // The two first lines contain the needed values.
      String totalLn = reader.readLine();
      String freeLn  = reader.readLine();
      file.close();

      return new RAMUsage(
         Long.parseLong(totalLn.split("MemTotal:\\p{Blank}*(\\p{Digit}*)")[1]),
         Long.parseLong(freeLn.split("MemFree:\\p{Blank}*(\\p{Digit}*)")[1])
      );
   }

   /** Probes the memory usage and send traps. */
   public void run()
   {
      try {
         // True when the last a trap has already been send since the RAM usage
         // has exceeded the total available RAM.
         boolean already_exceeded = false;

         for (;;) {
            RAMUsage usage = getRAMUsage();
            double ratio = (double) usage.free / (double) usage.total;

            if (ratio < ALERT_LEVEL && !already_exceeded) {
               this.link.notify(FREE_RAM_OID, Long.toString(usage.free));
               System.out.println("Alert RAM level exceeded. Trap sent.");
               already_exceeded = true;
            } else if (ratio >= ALERT_LEVEL)
               already_exceeded = false;

            Thread.sleep(POOLING_DELAY);
         }
      } catch (Exception e) {
         System.err.println("TrapsDaemon ceased to run.");
         e.printStackTrace();
      }
   }

   public static void main(String[] args) throws IOException
   {
      if (args.length != 3);
         printUsage();

      new TrapsDaemon(args[0], Integer.parseInt(args[1]), args[2]).run();
   }

   public static void printUsage()
   {
      System.out.println(
         "Pools the memory usage and sends a trap to the given host\n" +
         "usage: TrapsDaemon <dest host> <dest port> <SNMP community name>"
      );
   }
}
