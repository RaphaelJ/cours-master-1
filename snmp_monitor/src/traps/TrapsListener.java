package traps;

import monitor.MonitorParameters;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;

import org.snmp4j.*;
import org.snmp4j.smi.*;
import org.snmp4j.transport.DefaultUdpTransportMapping;

/**
 * Thread for receiving traps.
 * It implements a special interface of SNMP4j to handle traps.
 */
public class TrapsListener implements CommandResponder
{
   private MonitorParameters p;
   private Snmp              s;

   private Writer logger;

   public TrapsListener(MonitorParameters p) throws IOException
   {
      this.p = p;

      UdpAddress udpAddress = new UdpAddress("0.0.0.0/" + p.getTrapPort());
      this.s = new Snmp(new DefaultUdpTransportMapping(udpAddress));
      this.s.addCommandResponder(this);

      // Opens the log file.
      File file = new File(this.p.getOutputDirectory(), "traps.log");
      this.logger = new FileWriter(file, true);
   }

   /** Starts listening for traps. */
   public void start() throws IOException
   {
      this.s.listen();
   }

   // Method to handle a single trap/PDU at a time.
   public synchronized void processPdu(CommandResponderEvent event)
   {
      try
      {
         // Obtains IP address and PDU from this event.
         PDU pdu = event.getPDU();
         String ip = ((IpAddress) event.getPeerAddress()).toString();

         // Checks if pdu is an actual trap.
         if (pdu != null && pdu.isConfirmedPdu() && pdu.getType() == PDU.TRAP)
         {
            VariableBinding bind = pdu.get(0);

            OID    oid   = bind.getOid();
            String value = bind.getVariable().toString();

            Date now = new Date();

            this.logger.write(
               new SimpleDateFormat("yyyy-MM-dd hh:mm:ss").format(now) + ' ' +
               ip + ' ' + oid + ' ' + value + '\n'
            );
         }
      }
      catch (Exception e)
      {
         System.err.println("Error while receiving a trap.");
         e.printStackTrace();
      }
   }
}
