package trap;

import main.Parameters;

import java.io.*;
import java.util.*;

import org.snmp4j.Snmp;
import org.snmp4j.smi.Address;
import org.snmp4j.smi.IpAddress;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.Variable;

/**
 * Class for receiving traps.
 * It implements a special interface of SNMP4j to handle traps.
 */
public class TrapHandler implements CommandResponder
{
   private Parameters p;
   private Snmp snmp;

   public TrapHandler(Parameters p) throws Exception
   {
      this.p = p;
   }

   /** Starts listening for traps. */
   public void start()
   {
      snmp.listen();
   }

   // Method to handle a single trap/PDU at a time.
   public void processPdu(CommandResponderEvent event)
   {
      try
      {
         // Obtains IP address and PDU from this event.
         PDU pdu = event.getPDU();
         String ip = ((IpAddress) event.getPeerAddress()).toString();
         
         // Looks for traps.log; creates it if it does not exist and opens a stream.
         String dest = p.getOutputDirectory();
         File log = null;
         if (dest.endsWith("/"))
            log = new File(dest + "traps.log");
         else
            log = new File(dest + "/traps.log");
         if (!dest.exists())
            dest.createNewFile(
            );
         BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(log));
         
         // Checks if pdu is an actual trap, then explores the variable bindings.
         if (pdu != null && pdu.isConfirmedPdu() && pdu.getType() == PDU.TRAP)
         {
            Vector<VariableBinding> bindings = pdu.getVariableBindings();
            if (bindings != null && !bindings.isEmpty())
            {
               Iterator<VariableBinding> iterator = varBinds.iterator();
               
               // Object to format the current time
               SimpleDateFormat simpleDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
               
               while (iterator.hasNext())
               {
                  VariableBinding var = iterator.next();
                  String oid = var.getOid().toString();
                  String value = var.getVariable().toString();
                  
                  // Adds a new line into the traps.log file.
                  String newLine = simpleDate.format(new Date());
                  newLine += " " + ip + " " + oid + " " + value + "\n";
                  bos.write(newLine.getBytes());
               }
            }
         }
         
         bos.flush();
         bos.close();
      }
      catch (Exception e)
      {
         System.err.println("Error while receiving a trap");
         e.printStackTrace();
      }
   }
}
