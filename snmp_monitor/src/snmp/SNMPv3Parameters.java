package snmp;

/** Defines an interface for classes which contains the parameters needed for an
 * SNMPv3 link. */
public interface SNMPv3Parameters {
   public enum AuthLevel { NOAUTH_NOPRIV, AUTH_NOPRIV, AUTH_PRIV };
   public enum AuthProtocol { MD5, SHA1 };
   public enum PrivacyProtocol { DES, AES };

   public String getUserName();
   public AuthLevel getSecurityLevel();
   public AuthProtocol getAuthProtocol();
   public String getAuthPassword();
   public PrivacyProtocol getPrivProtocol();
   public String getPrivPassword();
}
