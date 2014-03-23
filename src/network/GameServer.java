package network;

import java.io.*;
import java.net.*;
import java.util.*;

import game.*;
import game.multi.*;
import model.*;
import model.piece.*;
import network.protocol.*;
import network.protocol.actions.*;
import network.protocol.events.*;

/** This class will listen to game events and will transmit these to its client
 * counterparts using a network socket. */
public class GameServer implements GameStateListener {

    /** Manages transmissions with a client.
     * Listens to local game events and transmits it to the clients.
     * Receives client actions and transmits it to the GameServer instance. */
    private class ClientManager extends Thread implements GameListener {
        public final GameServer server;
        public final Socket sock;
        public final GamePlayer game;
        public final int playerId;

        private final ObjectInputStream  _in;
        private final ObjectOutputStream _out;

        public ClientManager(
            GameServer server, Socket sock, GamePlayer game, int nPlayers,
            int playerId
        ) throws IOException
        {
            this.server   = server;
            this.sock     = sock;
            this.game     = game;
            this.playerId = playerId;

            this._in  = new ObjectInputStream(sock.getInputStream());
            this._out = new ObjectOutputStream(sock.getOutputStream());

            // Sends first the server configuration to the client.
            this.sendMessage(
                new ServerConfigMessage(
                    nPlayers, game.getGridWidth(), game.getGridHeight()
                )
            );
        }

        /** Sends a message to the client. */
        public void sendMessage(Message msg)
        {
            synchronized (this._out) {
                try {
                    this._out.writeObject(msg);
                    this._out.flush();
                } catch (IOException e) {
                    System.err.println(
                        "Error while sending a message to the client."
                    );
                    e.printStackTrace();
                }
            }
        }

        /** Runs a loop which listen for every command from the client and which
         * dispatches them. */
        public void run()
        {
            for (;;) {
                try {
                    Message msg = (Message) this._in.readObject();

                    if (msg instanceof NewGameAction)
                        this.server.newGame();
                    else if (msg instanceof PauseAction)
                        this.server.pause();
                    else if (msg instanceof StopAction)
                        this.server.stop();
                    else if (msg instanceof ResetAction)
                        this.server.reset();
                    else if (msg instanceof MoveLeftAction)
                        this.game.moveLeft();
                    else if (msg instanceof MoveRightAction)
                        this.game.moveRight();
                    else if (msg instanceof SoftDropAction)
                        this.game.softDrop();
                    else if (msg instanceof HardDropAction)
                        this.game.hardDrop();
                    else if (msg instanceof RotateAction)
                        this.game.rotate();
                    else if (msg instanceof SetAIAction)
                        this.game.setAI(((SetAIAction) msg).enable);
                } catch (Exception e) {
                    System.err.println(
                        "Error while receiving a message from the client."
                    );
                    e.printStackTrace();
                }
            }
        }

        /****************** Game events ******************/

        public void gridChange(BoardSection section)
        {
            this.server.emitGridChange(this.playerId, section);
        }

        public void newPiece(Piece currentPiece, Piece nextPiece)
        {
            this.server.emitNewPiece(this.playerId, currentPiece, nextPiece);
        }

        public void scoreChange(int newScore)
        {
            this.server.emitScoreChange(this.playerId, newScore);
        }

        public void levelChange(int newLevel)
        {
            this.server.emitLevelChange(this.playerId, newLevel);
        }

        public void clockDelayChange(int newClockDelay)
        {
            this.server.emitClockDelayChange(this.playerId, newClockDelay);
        }

        public void stateChanged(GameManager.GameState newState)
        { // Event already received by the GameServer
        }

        public void timeChanged(long elapsedTime)
        { // Event already received by the GameServer
        }
    }

    private ServerSocket _sock;
    private MultiGame    _multiGame;

    private ArrayList<ClientManager> _clients = null;

    /** Opens a server socket on the given port to host the given multiplayer
     * game. */
    public GameServer(int port, MultiGame multiGame) throws IOException
    {
        this._sock = new ServerSocket(port);
        this._multiGame = multiGame;

        multiGame.addListener(this);
    }

    public void waitForPlayers() throws IOException
    {
        ArrayList<MultiGameProxy> games = this._multiGame.getGames();

        // Waits for every client to connect.
        ArrayList<Socket> client_socks = new ArrayList<Socket>(games.size());
        for (int i = 0; i < games.size(); i++)
            client_socks.add(this._sock.accept());

        // Doesn't need the server socket anymore
        this._sock.close();

        // Creates a GameListener (which captures events on the board) and a
        // ClientManager (which talks to the client) for each player.
        this._clients = new ArrayList<ClientManager>(games.size());
        for (int i = 0; i < games.size(); i++) {
            MultiGameProxy game        = games.get(i);
            Socket         client_sock = client_socks.get(i);

            ClientManager client = new ClientManager(
                this, client_sock, game, games.size(), i
            );
            client.start();

            this._clients.add(client);
        }
    }

    /****************** Manager actions ******************/

    public void newGame()
    {
        this._multiGame.newGame();
    }

    public void pause()
    {
        this._multiGame.pause();
    }

    public void stop()
    {
        this._multiGame.stop();
    }

    public void reset()
    {
        this._multiGame.reset();
    }

    /****************** Observer event triggers ******************/

    // When there is a board change, we need to broadcast the event to every
    // client.

    public void emitGridChange(int playerId, BoardSection section)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new GridChangeEvent(clientPlayerId(i, playerId), section)
            );
        }
    }

    public void emitNewPiece(int playerId, Piece currentPiece, Piece nextPiece)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new NewPieceEvent(
                    clientPlayerId(i, playerId), currentPiece, nextPiece
                )
            );
        }
    }

    public void emitScoreChange(int playerId, int newScore)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new ScoreChangeEvent(clientPlayerId(i, playerId), newScore)
            );
        }
    }

    public void emitLevelChange(int playerId, int newLevel)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new LevelChangeEvent(clientPlayerId(i, playerId), newLevel)
            );
        }
    }

    public void emitClockDelayChange(int playerId, int newClockDelay)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new ClockDelayChangeEvent(
                    clientPlayerId(i, playerId), newClockDelay
                )
            );
        }
    }

    /** As the client always sees itself as player 0, we need to correct/shift
     * the server player id to match the client one.
     * clientId is the client we are talking to and playerId is the client we
     * are talking about. */
    private static int clientPlayerId(int clientId, int playerId)
    {
        if (clientId == playerId) // The client sees itself as player 0
            return 0;
        else if (playerId < clientId)
            return playerId + 1;
        else
            return playerId;
    }

    /****************** GameManager events ******************/

    public void stateChanged(GameManager.GameState newState)
    {
        broadcastMessage(new StateChangedEvent(newState));
    }

    public void timeChanged(long elapsedTime)
    {
        broadcastMessage(new TimeChangedEvent(elapsedTime));
    }

    /** Sends the given message to every client. */
    private void broadcastMessage(Message msg)
    {
        for (ClientManager client : this._clients)
            client.sendMessage(msg);
    }
}