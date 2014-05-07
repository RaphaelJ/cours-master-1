package snmp;

import java.io.*;

import org.snmp4j.PDU;
import org.snmp4j.ScopedPDU;
import org.snmp4j.Target;
import org.snmp4j.UserTarget;
import org.snmp4j.mp.MessageProcessingModel;
import org.snmp4j.mp.MPv3;
import org.snmp4j.mp.SnmpConstants;
import org.snmp4j.security.AuthMD5;
import org.snmp4j.security.AuthSHA;
import org.snmp4j.security.PrivAES128;
import org.snmp4j.security.PrivDES;
import org.snmp4j.security.SecurityLevel;
import org.snmp4j.security.SecurityModel;
import org.snmp4j.security.SecurityModels;
import org.snmp4j.security.SecurityProtocols;
import org.snmp4j.security.UsmUser;
import org.snmp4j.security.USM;
import org.snmp4j.smi.OctetString;
import org.snmp4j.smi.OID;

/** Creates an SNMPv3 link with the given host. */
public class SNMPv3Link<P extends SNMPv3Parameters> extends SNMPLink<P> {

   public SNMPv3Link(String host, int port, P p)
      throws IOException
   {
      super(host, port, p);
   }

   protected MessageProcessingModel getMessageProcessingModel(P p)
   {
      OID authProtocol;
      if (p.getAuthProtocol() == SNMPv3Parameters.AuthProtocol.MD5)
         authProtocol = AuthMD5.ID;
      else
         authProtocol = AuthSHA.ID;

      OID privProtocol;
      if (p.getPrivProtocol() == SNMPv3Parameters.PrivacyProtocol.DES)
         privProtocol = PrivDES.ID;
      else
         privProtocol = PrivAES128.ID;

      // Creates a new security model based on the username and the pair of
      // passwords given in the command line for SNMPv3.
      OctetString localEngineID = new OctetString(
         MPv3.createLocalEngineID()
      );
      USM usm = new USM(SecurityProtocols.getInstance(), localEngineID, 0);
      SecurityModels.getInstance().addSecurityModel(usm);
      usm.addUser(
         new OctetString(p.getUserName()),
         new UsmUser(
            new OctetString(p.getUserName()), authProtocol,
            new OctetString(p.getAuthPassword()), privProtocol,
            new OctetString(p.getPrivPassword())
         )
      );

      return new MPv3(usm);
   }

   protected Target getTarget(P p)
   {
      UserTarget target = new UserTarget();
      target.setVersion(SnmpConstants.version3);
      target.setSecurityModel(SecurityModel.SECURITY_MODEL_USM);
      target.setSecurityLevel(SecurityLevel.AUTH_PRIV);
      target.setSecurityName(new OctetString(p.getUserName()));
      return target;
   }

   protected PDU getPDU()
   {
      return new ScopedPDU();
   }
}
