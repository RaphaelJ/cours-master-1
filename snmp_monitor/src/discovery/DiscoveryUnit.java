package discovery;

import main.*;
import database.*;

import org.snmp4j.Snmp;
import org.snmp4j.TransportMapping;
import org.snmp4j.MessageDispatcher;
import org.snmp4j.AbstractTarget;
import org.snmp4j.CommunityTarget;
import org.snmp4j.UserTarget;
import org.snmp4j.PDU;
import org.snmp4j.ScopedPDU;
import org.snmp4j.event.ResponseEvent;
import org.snmp4j.mp.MPv1;
import org.snmp4j.mp.MPv2c;
import org.snmp4j.mp.MPv3;
import org.snmp4j.mp.SnmpConstants;
import org.snmp4j.smi.Integer32;
import org.snmp4j.smi.OID;
import org.snmp4j.smi.OctetString;
import org.snmp4j.smi.UdpAddress;
import org.snmp4j.smi.VariableBinding;
import org.snmp4j.transport.DefaultUdpTransportMapping;
import org.snmp4j.security.USM;
import org.snmp4j.security.UsmUser;
import org.snmp4j.security.PrivAES128;
import org.snmp4j.security.AuthSHA;
import org.snmp4j.security.SecurityProtocols;
import org.snmp4j.security.SecurityModels;
import org.snmp4j.security.SecurityLevel;
import org.snmp4j.security.SecurityModel;

/**
 * Thread checking a single host by sending different PDUs to it in order to
 * verify if that host is running a SNMP agent.
 * To do so, the "unit" uses a Snmp object and send up to 3 PDUs to the host
 * given to it upon constructor. First PDU is in SNMPv3. If the host does not
 * reply, we send a v2 PDU, and we will send a 3rd and last PDU in v1 to see if
 * that host is running SNMPv1. If the host never replies to any of these PDUs,
 * we consider it is not running any SNMP agent.
 */
public class DiscoveryUnit extends Thread
{
   private Parameters p;
   private AgentsPool ap;
   private DiscoveryGuardian dg;
   private String host;
   private Snmp s;

   // Constructor; will initialize Snmp object.
   public DiscoveryUnit(
      Parameters p, AgentsPool ap, DiscoveryGuardian dg, String host
   ) throws Exception
   {
      this.p = p;
      this.ap = ap;
      this.dg = dg;
      this.host = host;

      try
      {
         // Transport mapping
         TransportMapping tm = new DefaultUdpTransportMapping();
         s = new Snmp(tm);

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
      catch (Exception e)
      {
         throw new Exception("Unable to init SNMP : " + e.getMessage());
      }
   }
   
   /**
    * Sends a PDU to a given host. An integer tells which version of SNMP we
    * use (we will first try SNMPv3, then v2c, then v1), because the
    * AbstractTarget and the PDU objects we use differ depending on the SNMP
    * version we are testing. Returns true if we got a response and false
    * otherwise. 
    */
   private boolean sendPDU(String host, SNMPVersion snmpVersion)
   {
      AbstractTarget target;
      if (snmpVersion == Monitor.SNMPVersion.SNMPv3)
      {
         target = new UserTarget();
         target.setVersion(SnmpConstants.version3);
         target.setSecurityModel(SecurityModel.SECURITY_MODEL_USM);
         target.setSecurityLevel(SecurityLevel.AUTH_PRIV);
         target.setSecurityName(new OctetString(p.getUserName()));
      }
      else
      {
         target = new CommunityTarget();
         if (snmpVersion == Monitor.SNMPv2c)
            target.setVersion(SnmpConstants.version2c);
         else
            target.setVersion(SnmpConstants.version1);
         ((CommunityTarget) target).setCommunity(
            new OctetString(p.getCommunityName())
         );
      }

      target.setAddress(new UdpAddress(host + "/" + 161));
      target.setRetries(1);
      target.setTimeout(1000);

      PDU pdu;
      if (snmpVersion == Monitor.SNMPv3)
         pdu = new ScopedPDU();
      else
         pdu = new PDU();
      pdu.add(new VariableBinding(new OID("1.3.6.1.2.1.7.1"))); // udpInDatagrams
      pdu.setType(PDU.GET);
      
      try
      {
         ResponseEvent response = s.get(pdu, target);
         
         if (response != null)
         {
            PDU responsePDU = response.getResponse();
            
            if (responsePDU != null)
            {
               int errorStatus = responsePDU.getErrorStatus();

               if (errorStatus == PDU.noError)
               {
                  return true;
               }
            }
         }
      }
      catch (Exception e)
      {
         // Do nothing in case of exception; method will return false.
      }
      
      return false;
   }

   public void run()
   {
      int discoveredVersion = -1;

      // Tries sequentially v3, v2c and v1 PDUs.
      if (sendPDU(host, Monitor.SNMPv3))
      {
         discoveredVersion = Monitor.SNMPv3;
      }
      else if (sendPDU(host, Monitor.SNMPv2c))
      {
         discoveredVersion = Monitor.SNMPv2c;
      }
      else if (sendPDU(host, Monitor.SNMPv1))
      {
         discoveredVersion = Monitor.SNMPv1;
      }
      
      // If we detected something, add it or update it.
      if (discoveredVersion != -1)
      {
         if (!ap.agentExists(host))
         {
            ap.addAgent(host, discoveredVersion);
            String consoleMsg = "Discovered " + host;
            consoleMsg += " (" + Monitor.snmpVersion(discoveredVersion) + ")";
            System.out.println(consoleMsg);
         }
         else
         {
            RemoteAgent ra = ap.getAgent(host);
            if (ra.getSnmpVersion() != discoveredVersion)
            {
               ra.setSnmpVersion(discoveredVersion);
               String consoleMsg = "Updated " + host + " (now running ";
               consoleMsg += Monitor.snmpVersion(discoveredVersion) + ")";
               System.out.println(consoleMsg);
            }
         }
      }
      // If we detected nothing : was that host previously registered ? If yes, remove it.
      else
      {
         if (ap.agentExists(host))
         {
            ap.removeAgent(host);
            System.out.println(host + " went down");
         }
      }
      
      // When done, inform the guardian to allow new threads.
      dg.signalThreadIsDone();
      
      // Close SNMP and terminates thread.
      try
      {
         s.close();
      }
      catch (Exception e) {}
      return;
   }
}

