package snmp;

import java.io.*;

import org.snmp4j.CommunityTarget;
import org.snmp4j.PDU;
import org.snmp4j.PDUv1;
import org.snmp4j.Target;
import org.snmp4j.mp.MessageProcessingModel;
import org.snmp4j.mp.MPv1;
import org.snmp4j.mp.SnmpConstants;
import org.snmp4j.smi.OctetString;

/** Creates an SNMPv1 link with the given host. */
public class SNMPv1Link<P extends SNMPParameters> extends SNMPLink<P> {

   public SNMPv1Link(String host, int port, P p) throws IOException
   {
      super(host, port, p);
   }

   protected MessageProcessingModel getMessageProcessingModel(P p)
   {
      return new MPv1();
   }

   protected Target getTarget(P p)
   {
      CommunityTarget target = new CommunityTarget();
      target.setVersion(SnmpConstants.version1);
      target.setCommunity(new OctetString(p.getCommunityName()));
      return target;
   }

   protected PDU getPDU()
   {
      return new PDUv1();
   }
}
