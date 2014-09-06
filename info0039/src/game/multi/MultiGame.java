package game.multi;

import game.GameManager;
import game.GameObserver;
import game.GameStateListener;
import game.rules.Rule;

import java.util.ArrayList;

import model.Board;
import util.GameTimer;
import util.random.LCGRandom;

/** Applies a rule and manages a multiplayer game. */
public class MultiGame implements GameManager {
    protected ArrayList<MultiGameProxy> _games
        = new ArrayList<MultiGameProxy>();

    private GameManager.GameState _currentState
        = GameManager.GameState.INITIALIZED;

    // This timer is used to track the game's eslaped time.
    private GameTimer _timer;

    private ArrayList<GameStateListener> _listeners =
        new ArrayList<GameStateListener>();

    public MultiGame(
        int width, int height, int nPlayers, Rule.RuleFactory ruleFactory
    )
    {
        for (Board board : this.getBoards(width, height, nPlayers))
            this._games.add(this.getGameProxy(board, ruleFactory));

        this.initTimers();
    }

    /** Creates a board for each player. Use a different random generator for
     * each board.
     * Will be overrided by other game modes. */
    protected ArrayList<Board> getBoards(int width, int height, int nPlayers)
    {
        LCGRandom seedGenerator = new LCGRandom();
        ArrayList<Board> boards = new ArrayList<Board>(nPlayers);

        for (int i = 0; i < nPlayers; i++) {
            boards.add(
                new Board(new LCGRandom(seedGenerator.nextInt()), width, height)
            );
        }
        return boards;
    }

    /** Given a board instance and a rule, constructs a transformed gameplay for
     * the player which applies the multiplayer rules in a synchronized way.
     * Will be overrided by other game modes. */
    protected MultiGameProxy getGameProxy(Board board,
                                          Rule.RuleFactory ruleFactory)
    {
        return new MultiGameProxy(this, board, ruleFactory.construct());
    }

    public synchronized void addListener(GameStateListener listener)
    {
        this._listeners.add(listener);
    }

    /*********************** Users actions ***********************/

    /** Starts the timer which controls the game.
     * Resets the game if needed. */
    public synchronized void newGame()
    {
        if (this._currentState != GameManager.GameState.INITIALIZED)
            this.reset();

        this.setCurrentState(GameManager.GameState.RUNNING);
        this.startTimers();
    }

    /** Pauses/Unpauses the timer if the game is running/in pause.
     * Does nothing otherwise. */
    public synchronized void pause()
    {
        switch (this._currentState) {
        case RUNNING:
            this.stopTimers();
            this.setCurrentState(GameManager.GameState.PAUSED);
            break;
        case PAUSED:
            this.setCurrentState(GameManager.GameState.RUNNING);
            this.startTimers();
            break;
        default:
        }
    }

    public synchronized void stop()
    {
        if (this._currentState == GameManager.GameState.RUNNING
            || this._currentState == GameManager.GameState.PAUSED)
            this.stopTimers();

        this.setCurrentState(GameManager.GameState.STOPPED);
    }

    public synchronized void reset()
    {
        if (this._currentState == GameManager.GameState.RUNNING
            || this._currentState == GameManager.GameState.PAUSED)
            this.stopTimers();

        for (MultiGameProxy game : this._games) {
            game.getBoard().reset();
            game.getRule().reset();
        }

        this.initTimers();

        this.setCurrentState(GameManager.GameState.INITIALIZED);
    }

    /*********************** Boards events ***********************/

    public synchronized void gameOver()
    {
        if (this._currentState == GameManager.GameState.RUNNING
            || this._currentState == GameManager.GameState.PAUSED)
            this.stopTimers();

        this.setCurrentState(GameManager.GameState.GAMEOVER);
    }

    /*********************** Internals ***********************/

    public synchronized void initTimers()
    {
        this._timer = new GameTimer(
            new Runnable() {
                @Override
                public void run()
                {
                    long elapsed = _timer.getElapsedTime();
                    for (GameStateListener listener : _listeners)
                        listener.timeChanged(elapsed);

                    for (MultiGameProxy game : _games)
                        game.emitTimeChanged(elapsed);
                }
            }, 1000
        );

        for (final MultiGameProxy game : this._games) {
            game.setTimer(
                new GameTimer(
                    new Runnable() {
                        @Override
                        public void run()
                        {
                            if (_currentState == GameManager.GameState.RUNNING)
                                game.getBoard().gameTick();
                        }
                    }, game.getRule().getClockDelay()
                )
            );
        }
    }

    public synchronized void startTimers()
    {
        this._timer.start();

        for (MultiGameProxy game : this._games)
            game.getTimer().start();
    }

    public synchronized void stopTimers()
    {
        for (MultiGameProxy game : this._games)
            game.getTimer().stop();

        this._timer.stop();
    }

    /*********************** Getters ***********************/

    /** Returns the instances which manage the game for every player. */
    public ArrayList<MultiGameProxy> getGames()
    {
        return this._games;
    }

    /** Returns the Game instance which can be used to control a given
     * player's game. */
    public MultiGameProxy getPlayerGame(int player)
    {
        return this._games.get(player);
    }

    public GameManager.GameState getCurrentState()
    {
        return this._currentState;
    }

    private void setCurrentState(GameManager.GameState newState)
    {
        if (newState != this._currentState) {
            this._currentState = newState;

            for (GameStateListener listener : this._listeners)
                listener.stateChanged(newState);

            for (MultiGameProxy game : this._games)
                game.emitStateChanged(newState);
        }
    }
}
