package network.protocol;

/** First message from the server.
 * Gives the configuration of the server. */
public class ServerConfigMessage implements Message {

    public final int nPlayers;

    public final int width;
    public final int height;

}