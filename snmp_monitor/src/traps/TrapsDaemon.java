package traps;

import snmp.SNMPv2cLink;

import org.snmp4j.smi.OID;

/** Runs a loop which pools the RAM usage and send a SNMPv2c trap when the free
 * ram is below 10% of the total available RAM.
 *
 * Remarks :
 * - this script doesn't take into account the bufferised and cached potion of
 *   the memory which but only the truly free RAM ;
 * - this script will not send a new trap unless the total free ram fell below
 *   10%. */
public class TrapsDaemon {
   /** Number of milliseconds between two probes of the RAM usage. */
   public static final long POOLING_DELAY = 5000;

   public static final OID FREE_RAM_OID = new OID(".1.3.6.1.4.1.2021.4.11.0");

   private final SNMPv2cLink link;

   public TrapsDaemon()
   {
      this.link = new SNMPv2cLink(host, port, p)
   }

   public void sendTrap()
   {
      this.link.send(pdu);
   }
}
