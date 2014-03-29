package snmp;

import main.*;

import org.snmp4j.Snmp;
import org.snmp4j.mp.MessageProcessingModel;
import org.snmp4j.transport.DefaultUdpTransportMapping;

/** This abstract class is a wrapper over SNMP4j to make easier simple
 * comunications with an host. */
public abstract class SNMPLink {

   /** Constants to distinct SNMP versions. */
   public enum SNMPVersion { SNMPv1, SNMPv2c, SNMPv3 };

   private Snmp s;

   public SNMPLink()
   {
      s = new Snmp(new DefaultUdpTransportMapping());
      s.getMessageDispatcher().addMessageProcessingModel(getMessageProcessingModel());

         // Creates a new security model based on the username and the pair of
         // passwords given in the command line fot SNMPv3.
         OctetString localEngineID = new OctetString(
            MPv3.createLocalEngineID()
         );
         USM usm = new USM(SecurityProtocols.getInstance(), localEngineID, 0);
         SecurityModels.getInstance().addSecurityModel(usm);
         usm.addUser(new OctetString(p.getUserName()),
                     new UsmUser(new OctetString(p.getUserName()),
                                 AuthSHA.ID,
                                 new OctetString(p.getAuthPassword()),
                                 PrivAES128.ID,
                                 new OctetString(p.getPrivPassword())));

         // Makes the SNMP listenner able to receive messages from the three
         // versions of the protocol.
         MessageDispatcher disp = s.getMessageDispatcher();
         disp.P(new MPv1());
         disp.addMessageProcessingModel(new MPv2c());
         disp.addMessageProcessingModel(new MPv3(usm));

         // SNMP starts listening.
         s.listen();
   }

   /** This method is to be overrided by subclasses to provide a model able
    * to receive their messages. */
   protected abstract MessageProcessingModel getMessageProcessingModel();
   
   

   public static SNMPLink getInstance(
      SNMPVersion version, String host, Parameters p
   )
   {
      switch (version) {
      case SNMPVersion.SNMPv1:
         return new SNMPv1Link(host, p)
      case SNMPVersion.SNMPv2c:
         return new SNMPv2cLink(host, p)
      case SNMPVersion.SNMPv3:
         return new SNMPv3Link(host, p)
      }
   }

   /** Gives the SNMP version in String format. */
   public static String snmpVersion(SNMPVersion version)
   {
      switch(version) {
      case SNMPVersion.SNMPv1:
         return "SNMPv1";
      case SNMPVersion.SNMPv2c:
         return "SNMPv2c";
      case SNMPVersion.SNMPv3:
         return "SNMPv3";
      }
   }
}