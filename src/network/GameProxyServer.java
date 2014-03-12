package network;

/** This class will listen to game events and will transmit these to its client
 * counterpart. */
public class GameProxyServer extends GamePlayListener {
    private TCPSocket _sock;

    public GameProxyServer(TCPSocket sock, GamePlay game)
    {
        this._sock = sock;

        game.addListener(this);
    }
}