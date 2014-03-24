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

            this._out = new ObjectOutputStream(sock.getOutputStream());
            this._in  = new ObjectInputStream(sock.getInputStream());

            // Sends the server configuration to the client.
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
            try {
                for (;;) {
                    Message msg = (Message) this._in.readObject();

                    if (msg instanceof NewGameAction) {
                        this.server.log(
                            "Received a new game action from player " +
                            this.playerId
                        );
                        this.server.newGame();
                    } else if (msg instanceof PauseAction) {
                        this.server.log(
                            "Received a pause action from player " +
                            this.playerId
                        );
                        this.server.pause();
                    } else if (msg instanceof StopAction) {
                        this.server.log(
                            "Received a stop action from player " +
                            this.playerId
                        );
                        this.server.stop();
                    } else if (msg instanceof ResetAction) {
                        this.server.log(
                            "Received a reset action from player " +
                            this.playerId
                        );
                        this.server.reset();
                    } else if (msg instanceof MoveLeftAction) {
                        this.server.log(
                            "Received a move left action from player " +
                            this.playerId
                        );
                        this.game.moveLeft();
                    } else if (msg instanceof MoveRightAction) {
                        this.server.log(
                            "Received a move right action from player " +
                            this.playerId
                        );
                        this.game.moveRight();
                    } else if (msg instanceof SoftDropAction) {
                        this.server.log(
                            "Received a soft drop action from player " +
                            this.playerId
                        );
                        this.game.softDrop();
                    } else if (msg instanceof HardDropAction) {
                        this.server.log(
                            "Received a hard drop action from player " +
                            this.playerId
                        );
                        this.game.hardDrop();
                    } else if (msg instanceof RotateAction) {
                        this.server.log(
                            "Received a rotate action from player " +
                            this.playerId
                        );
                        this.game.rotate();
                    } else if (msg instanceof SetAIAction) {
                        this.server.log(
                            "Received a set AI action from player " +
                            this.playerId
                        );
                        this.game.setAI(((SetAIAction) msg).enable);
                    }
                }
            } catch (Exception e) {
                System.err.println(
                    "Error while receiving a message from the client."
                );
                e.printStackTrace();
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
    private Writer       _logger;

    private ArrayList<ClientManager> _clients = null;

    /** Opens a server socket on the given port to host the given multiplayer
     * game.
     * Logs every transmitted message to the given logger. */
    public GameServer(int port, MultiGame multiGame, Writer logger)
        throws IOException
    {
        this._logger = logger;

        this._sock = new ServerSocket(port);
        this.log("Started listening on port " + port);

        this._multiGame = multiGame;

        multiGame.addListener(this);
    }

    public void waitForPlayers() throws IOException
    {
        ArrayList<MultiGameProxy> games = this._multiGame.getGames();

        // Waits for every client to connect.
        this.log("Waits for " + games.size() + " players to join the game.");
        ArrayList<Socket> client_socks = new ArrayList<Socket>(games.size());
        for (int i = 0; i < games.size(); i++) {
            Socket client_sock = this._sock.accept();
            client_socks.add(client_sock);

            this.log(
                "Received a connection from " +
                client_sock.getInetAddress().toString() +
                " as player " + i + "."
            );
        }

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
            game.addListener(client);

            this._clients.add(client);
        }

        this.log("All players joined the game.");
    }

    /** Writes a message to the server's log.
     * Adds a new-line character after the message. */
    public synchronized void log(String str)
    {
        try {
            this._logger.append(str);
            this._logger.append('\n');
        } catch (IOException e) {
            System.err.println("Unable to log a message.");
            e.printStackTrace();
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

        this.log("Sent a grid change event of player " + playerId + ".");
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

        this.log("Sent a new piece event of player " + playerId + ".");
    }

    public void emitScoreChange(int playerId, int newScore)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new ScoreChangeEvent(clientPlayerId(i, playerId), newScore)
            );
        }

        this.log("Sent a score change event of player " + playerId +".");
    }

    public void emitLevelChange(int playerId, int newLevel)
    {
        for (int i = 0; i < this._clients.size(); i++) {
            ClientManager client = this._clients.get(i);
            client.sendMessage(
                new LevelChangeEvent(clientPlayerId(i, playerId), newLevel)
            );
        }

        this.log("Sent a level change event of player " + playerId + ".");
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

        this.log("Sent a clock change event of player " + playerId + ".");
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

        this.log("Sent a game state change event to every client.");
    }

    public void timeChanged(long elapsedTime)
    {
        broadcastMessage(new TimeChangedEvent(elapsedTime));

        this.log("Sent a time change event to every client.");
    }

    /** Sends the given message to every client. */
    private void broadcastMessage(Message msg)
    {
        for (ClientManager client : this._clients)
            client.sendMessage(msg);
    }
}