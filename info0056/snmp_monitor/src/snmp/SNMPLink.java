package snmp;

import java.io.*;
import org.snmp4j.PDU;
import org.snmp4j.Snmp;
import org.snmp4j.Target;
import org.snmp4j.event.ResponseEvent;
import org.snmp4j.event.ResponseListener;
import org.snmp4j.mp.MessageProcessingModel;
import org.snmp4j.smi.*;
import org.snmp4j.transport.DefaultUdpTransportMapping;

/** This abstract class is a wrapper over SNMP4j to make easier simple
 * comunications with an host. P is the type of the parameters used to
 * to instanciate the class. */
public abstract class SNMPLink<P> {

   /** Number of milliseconds before returning a timeout error. */
   public static final long REQUEST_TIMEOUT = 1000;

   /** Constants to distinguish SNMP versions. */
   public enum SNMPVersion { SNMPv1, SNMPv2c, SNMPv3 };

   private Snmp   s;
   private Target target;

   public SNMPLink(String host, int port, P p) throws IOException
   {
      this.s = new Snmp(new DefaultUdpTransportMapping());

      this.s.getMessageDispatcher()
            .addMessageProcessingModel(this.getMessageProcessingModel(p));

      this.target = this.getTarget(p);
      this.target.setAddress(new UdpAddress(host + "/" + port));
      this.target.setRetries(0);
      this.target.setTimeout(REQUEST_TIMEOUT);
   }

   /** This method is to be overrided by sub-classes to provide a model able
    * to receive their messages. */
   protected abstract MessageProcessingModel getMessageProcessingModel(P p);

   /** This method is to be overrided by sub-classes to instantiate a target
    * used to receive a PDU. */
   protected abstract Target getTarget(P p);

   /** This method is to be overrided by sub-classes to instantiate a PDU. */
   protected abstract PDU getPDU();

   /** Returns a SNMPLink instance of the given SNMP version. */
   public static <P extends SNMPParameters & SNMPv3Parameters>
   SNMPLink<P> getInstance(
      SNMPVersion version, String host, int port, P p
   ) throws IOException
   {
      switch (version) {
      case SNMPv1:  return new SNMPv1Link<P>(host, port, p);
      case SNMPv2c: return new SNMPv2cLink<P>(host, port, p);
      case SNMPv3:  return new SNMPv3Link<P>(host, port, p);
      default:      return null;
      }
   }

   /** Starts listening for SNMP packets. */
   public synchronized void listen() throws IOException
   {
      this.s.listen();
   }

   /** Executes the given OID get request synchronously. */
   public synchronized PDU get(OID oid) throws IOException
   {
      PDU pdu = this.getPDU();
      pdu.add(new VariableBinding(oid));

      ResponseEvent e = this.s.get(pdu, this.target);

      if (e == null)
         throw new IOException("Request timeout.");
      else
         return e.getResponse();
   }

   /** Executes the given OID get request asynchronously. Calls the given
    * callback on error or success. */
   public synchronized void get(OID oid, ResponseListener callback)
      throws IOException
   {
      PDU pdu = this.getPDU();
      pdu.add(new VariableBinding(oid));

      this.s.get(pdu, this.target, null, callback);
   }

   /** Executes the given OID getnext request synchronously. */
   public synchronized PDU getNext(OID oid) throws IOException
   {
      PDU pdu = this.getPDU();
      pdu.add(new VariableBinding(oid));

      ResponseEvent e = this.s.getNext(pdu, this.target);

      if (e == null)
         throw new IOException("Request timeout.");
      else
         return e.getResponse();
   }

   /** Sends a trap with the given variable. */
   public synchronized void notify(OID oid, String value) throws IOException
   {
      PDU pdu = this.getPDU();
      pdu.add(new VariableBinding(oid, new OctetString(value)));

      this.s.notify(pdu, this.target);
   }

   /** Closes the session and frees any allocated resources, i.e. sockets and
    * the internal thread for processing request timeouts. */
   public synchronized void dispose() throws IOException
   {
      this.s.close();
   }

   /** Gives the SNMP version in String format. */
   public static String snmpVersion(SNMPVersion version)
   {
      switch(version) {
      case SNMPv1:  return "SNMPv1";
      case SNMPv2c: return "SNMPv2c";
      case SNMPv3:  return "SNMPv3";
      default:      return null;
      }
   }
}
