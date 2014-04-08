package snmp;

import main.Parameters;

import java.io.*;

import org.snmp4j.CommunityTarget;
import org.snmp4j.PDU;
import org.snmp4j.PDUv1;
import org.snmp4j.Target;
import org.snmp4j.mp.MessageProcessingModel;
import org.snmp4j.mp.MPv2c;
import org.snmp4j.mp.SnmpConstants;
import org.snmp4j.smi.OctetString;

/** Creates an SNMPv2c link with the given host. */
public class SNMPv2cLink<P implements SNMPParameters> extends SNMPLink<P> {

   public SNMPv2cLink(String host, int port, SNMPParameters p)
      throws IOException
   {
      super(host, port, p);
   }

   protected MessageProcessingModel getMessageProcessingModel(P p)
   {
      return new MPv2c();
   }

   protected Target getTarget(P p)
   {
      CommunityTarget target = new CommunityTarget();
      target.setVersion(SnmpConstants.version2c);
      target.setCommunity(new OctetString(p.getCommunityName()));
      return target;
   }

   protected PDU getPDU()
   {
      return new PDU();
   }
}
