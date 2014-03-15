package network;

import java.util;

import game.*;
import network.protocol.*;

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
        = new ArrayList<GameObserver>();

    /** Negociates the connexion with the server. Returns when the game has
     * been started. Spawns a new thread to receive events. */
    public GameClient(String host, int port)
    {
        // TODO
    }

    public GameClient(InputStream in, OutputStream out)
    {
        this._in  = new ObjectInputStream(in);
        this._out = new ObjectOutputStream(out);

        // The server first send an integer containing the number of players,
        // then the width and the height of the board.
        this._config = (ServerConfigMessage) this._in.readObject();

        this._opponents.add(
            this._player = new GamePlayerProxy(
                this, this._config.width, this._config.height
            )
        );

        for (int i = 1; i < this._config.nPlayers; i++) {
            this._opponents.add(
                new GameObserverProxy(this._config.width, this._config.height)
            );
        }

        // TODO: lancer eventLoop() dans un nouveau thread
        // eventLoop();
    }

    /** This is the main loop in which the thread is trapped, listening for
     * every event message from the server, dispatching every message to
     * the corresponding observer, if needed. */
    private eventLoop()
    {
        for (;;) {
            EventMessage msg = recvMessage();

            if (msg instanceof ManagerEvent) {
                // TODO : trouver le bon type d'event (stateChanged
                // ou timeChanged), le transmettre aux _listenners et à
                // l'ensemble des observers.
            }
            else if (msg instanceof ObserverEvent) {
                // Events for a given player's board.
                int playerId = (ObserverEvent msg).getPlayerId();
                GameObserver observer = this._players.get(playerId);

                // TODO : Trouver le bon type d'event et appeller la bonne
                // méthode (emitXXXX()) d'observer :
                //
                // if (msg instanceof GridChangeEvent) { ... }
            }
        }
    }

    /** Sends a message to the server. */
    public void sendMessage(Message msg)
    {
        synchronized (this._out) {
            this._out.writeObject(msg);
            this._out.flush();
        }
    }

    /** Waits for a message to the server. */
    public Message recvMessage()
    {
        synchronized (this._in) {
            return (Message) this._in.readObject();
        }
    }

    /****************** Getters ******************/

    /** Returns the interface with control the player game. */
    public GamePlayer getPlayer();
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

    public GameState getCurrentState()
    {
        return this._currentState;
    }
}
