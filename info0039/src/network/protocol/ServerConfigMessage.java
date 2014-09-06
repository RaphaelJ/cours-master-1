package network.protocol;

/** First message from the server.
 * Gives the configuration of the server. */
public class ServerConfigMessage implements Message {

    public final int nPlayers;

    public final int width;
    public final int height;

    public ServerConfigMessage(int nPlayers, int width, int height)
    {
        this.nPlayers = nPlayers;
        this.width    = width;
        this.height   = height;
    }
}