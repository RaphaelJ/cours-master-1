package network;

import game.GameManager;
import game.GameObserver;
import game.GamePlayer;
import game.GameStateListener;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;

import network.protocol.Message;
import network.protocol.ServerConfigMessage;
import network.protocol.actions.NewGameAction;
import network.protocol.events.ManagerEvent;
import network.protocol.events.ObserverEvent;

/** This class will listen to a network socket and will transmit events from and
 * to the views by implementing the GameManager, GameObserver and GamePlayer
 * interfaces. Those interfaces will make appear these instances as local one
 * while they will transmit action and receive events from the network. */
public class GameClient implements GameManager {

    private final ObjectInputStream  _in;
    private final ObjectOutputStream _out;

    private final ServerConfigMessage _config;

    private GameManager.GameState _currentState
        = GameManager.GameState.INITIALIZED;

    private ArrayList<GameStateListener> _listeners
        = new ArrayList<GameStateListener>();

    // Each observer (one for each board) is identified by the server by its
    // index in this array.
    // The first observer (index = 0) is the player (who extends GamePlayer).
    private ArrayList<GameObserverProxy> _players
        = new ArrayList<GameObserverProxy>();

    /** Negociates the connexion with the server. Returns when the game has
     * been started. Spawns a new thread to receive events. */
    public GameClient(String host, int port)
        throws UnknownHostException, IOException
    {
        Socket sock = new Socket(host, port);
        this._in  = new ObjectInputStream(sock.getInputStream());
        this._out = new ObjectOutputStream(sock.getOutputStream());
    }

    public GameClient(InputStream in, OutputStream out)
    {
        this._in  = new ObjectInputStream(in);
        this._out = new ObjectOutputStream(out);

        // The server first send its configuratiob (number of players, the width
        // and the height of the board).
        this._config = (ServerConfigMessage) this._in.readObject();

        // Creates the playable controller for the player.
        this._opponents.add(
            new GamePlayerProxy(this, this._config.width, this._config.height)
        );

        // Creates the observers for the other players.
        for (int i = 1; i < this._config.nPlayers; i++) {
            this._opponents.add(
                new GameObserverProxy(this._config.width, this._config.height)
            );
        }

        // Runs eventLoop() in a new thread.
        new Thread() {
            public void run()
            {
                try {
                    eventLoop();
                } catch (Exception e) {
                    System.err.println(
                        "The client listening connection crashed with the " +
                        "following exception :"
                    );
                    e.printStackTrace();
                }
            }
        }.run();
    }

    /** This is the main loop in which the thread is trapped, listening for
     * every event message from the server, dispatching every message to
     * the corresponding observer, if needed. */
    private void eventLoop()
        throws ClassNotFoundException, IOException, ProtocolException
    {
        for (;;) {
            Message msg = recvMessage();

            if (msg instanceof ManagerEvent) {
                // Manager events need to be broadcasted to every observer.

                if (msg instanceof StateChangedEvent)
                    this.emitStateChanged(((StateChangedEvent) msg).newState);
                else if (msg instanceof TimeChangedEvent)
                    this.emitStateChanged(((TimeChangedEvent) msg).elapsedTime);
            } else if (msg instanceof ObserverEvent) {
                // Events for a given player's board.

                int playerId = ((ObserverEvent) msg).getPlayerId();
                GameObserverProxy observer = this._players.get(playerId);

                if (msg instanceof BoardChangeEvent)
                    observer.emitBoardChange(((BoardChangeEvent) msg).section);
                else if (msg instanceof NewPieceEvent) {
                    NewPieceEvent event = (NewPieceEvent) msg;
                    observer.emitNewPiece(event.currentPiece, event.nextPiece);
                } else if (msg instanceof ScoreChangeEvent)
                    observer.emitScoreChange(((ScoreChangeEvent) msg).newScore);
                else if (msg instanceof LevelChangeEvent)
                    observer.emitLevelChange(((LevelChangeEvent) msg).newLevel);
                else if (msg instanceof ClockDelayChangeEvent) {
                    observer.emitClockDelayChange(
                        ((ClockDelayChange) msg).newClockDelay
                    );
                }
            } else
                throw new ProtocolException("Invalid message received.");
        }
    }

    /** Sends a message to the server. */
    public void sendMessage(Message msg) throws IOException
    {
        synchronized (this._out) {
            this._out.writeObject(msg);
            this._out.flush();
        }
    }

    /** Waits for a message to the server. */
    public Message recvMessage() throws ClassNotFoundException, IOException
    {
        synchronized (this._in) {
            return (Message) this._in.readObject();
        }
    }

    /****************** Getters ******************/

    /** Returns the interface with control the player game. */
    public GamePlayer getPlayer()
    {
        return this._players.get(0);
    }

    /** Returns the set of observers interfaces.
     * The first player (index = 0) is the player (returned by getPlayer())
     * while others are opponents. */
    public ArrayList<GameObserver> getPlayers()
    {
        return this._players;
    }

    /****************** Manager actions ******************/

    /** Starts the game and resets the game if needed. */
    public void newGame()
    {
        this.sendMessage(new NewGameAction());
    }

    /** Pauses/Unpauses the game if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause()
    {
        this.sendMessage(new PauseAction());
    }

    /** Stops the timers and made the game impossible to resume. */
    public void stop()
    {
        this.sendMessage(new StopAction());
    }

    /** Reinitialises the game and the board. Stop the game if it's running. */
    public void reset()
    {
        this.sendMessage(new ResetAction());
    }

    public void addListener(GameStateListener listener)
    {
        this._listeners.add(listener);
    }

    public GameManager.GameState getCurrentState()
    {
        return this._currentState;
    }

    /****************** Manager event triggers ******************/

    private void emitStateChanged(GameManager.GameState newState)
    {
        this._currentState = newState;

        for (GameStateListener listener : this._listeners)
            listener.stateChanged(newState);

        for (GameObserverProxy player : this._players)
            player.emitStateChanged(newState);
    }

    private void emitTimeChanged(long elapsedTime)
    {
        for (GameStateListener listener : this._listeners)
            listener.timeChanged(elapsedTime);

        for (GameObserverProxy player : this._players)
            player.emitTimeChanged(elapsedTime);
    }
}
