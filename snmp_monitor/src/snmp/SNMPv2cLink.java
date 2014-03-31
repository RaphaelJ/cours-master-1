package snmp;

import main.Parameters;

import org.snmp4j.CommunityTarget;
import org.snmp4j.PDU;
import org.snmp4j.PDUv1;
import org.snmp4j.SnmpConstants;
import org.snmp4j.Target;
import org.snmp4j.mp.MessageProcessingModel;

/** Creates an SNMPv2c link with the given host. */
public class SNMPv2cLink extends SNMPLink {

   public SNMPv2cLink(String host, Parameters p)
   {
      super(host, p);
   }

   protected abstract MessageProcessingModel getMessageProcessingModel(
      Parameters p
   )
   {
      return new MPv2c();
   }

   protected abstract Target getTarget(Parameters p)
   {
      CommunityTarget target = new CommunityTarget();
      target.setVersion(SnmpConstants.version2c);
      target.setCommunity(new OctetString(p.getCommunityName()));
      return target;
   }

   protected abstract PDU getPDU()
   {
      return new PDU();
   }
}
