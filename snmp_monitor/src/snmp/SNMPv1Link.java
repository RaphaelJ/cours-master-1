package snmp;

import main.Parameters;

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
public class SNMPv1Link extends SNMPLink {

   public SNMPv1Link(String host, Parameters p) throws IOException
   {
      super(host, p);
   }

   protected MessageProcessingModel getMessageProcessingModel(
      Parameters p
   )
   {
      return new MPv1();
   }

   protected Target getTarget(Parameters p)
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
