package gameplay.multi;

import java.util.*;

import gameplay.*;
import gameplay.rules.*;
import model.Board;
import util.*;

/** Applies a rule to a multiplayer game. */
public class MultiGamePlay {
    protected ArrayList<GamePlay> _games = new ArrayList<GamePlay>();

    private GamePlay.GameState _currentState = GamePlay.GameState.INITIALIZED;

    // This timer is used to track the game's eslaped time.
    private GameTimer _timer;

    private ArrayList<GamePlayListener> _listeners =
        new ArrayList<GamePlayListener>();

    public MultiGamePlay(ArrayList<Board> boards, Rule.RuleFactory ruleFactory)
    {
        for (Board board : boards)
            this._games.add(this.getGamePlayProxy(board, ruleFactory));

        this.initTimers();
    }

    public synchronized void addListener(GamePlayListener listener)
    {
        this._listeners.add(listener);
    }

    /*********************** Users actions ***********************/

    /** Starts the timer which controls the game.
     * Resets the game if needed. */
    public synchronized void newGame()
    {
        if (this._currentState != GamePlay.GameState.INITIALIZED)
            this.reset();

        this.setCurrentState(GamePlay.GameState.RUNNING);
        this.startTimers();
    }

    /** Pauses/Unpauses the timer if the game is running/in pause.
     * Does nothing otherwise. */
    public synchronized void pause()
    {
        switch (this._currentState) {
        case RUNNING:
            this.stopTimers();
            this.setCurrentState(GamePlay.GameState.PAUSED);
            break;
        case PAUSED:
            this.setCurrentState(GamePlay.GameState.RUNNING);
            this.startTimers();
            break;
        default:
        }
    }

    public synchronized void reset()
    {
        if (this._currentState == GamePlay.GameState.RUNNING
            || this._currentState == GamePlay.GameState.PAUSED)
            this.stopTimers();

        for (GamePlay game : this._games) {
            game.getBoard().reset();
            game.getRule().reset();
        }

        this.initTimers();

        this.setCurrentState(GamePlay.GameState.INITIALIZED);
    }

    public synchronized void stop()
    {
        if (this._currentState == GamePlay.GameState.RUNNING
            || this._currentState == GamePlay.GameState.PAUSED)
            this.stopTimers();

        this.setCurrentState(GamePlay.GameState.STOPPED);
    }

    /*********************** Boards events ***********************/

    public synchronized void gameOver()
    {
        if (this._currentState == GamePlay.GameState.RUNNING
            || this._currentState == GamePlay.GameState.PAUSED)
            this.stopTimers();

        this.setCurrentState(GamePlay.GameState.GAMEOVER);
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
                    for (GamePlayListener listener : _listeners)
                        listener.timeChanged(elapsed);

                    for (GamePlay game : _games)
                        ((MultiGamePlayProxy) game).emitTimeChanged(elapsed);
                }
            }, 1000
        );

        for (final GamePlay game : this._games) {
            ((MultiGamePlayProxy) game).setTimer(
                new GameTimer(
                    new Runnable() {
                        @Override
                        public void run()
                        {
                            if (_currentState == GamePlay.GameState.RUNNING)
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

        for (GamePlay game : this._games)
            game.getTimer().start();
    }

    public synchronized void stopTimers()
    {
        for (GamePlay game : this._games)
            game.getTimer().stop();
    }

    /*********************** Getters ***********************/

    /** Returns the instances which manage the game for every player. */
    public ArrayList<GamePlay> getGamePlays()
    {
        return this._games;
    }

    /** Returns tkstenehe GamePlay instance which can be used to control a given
     * player's game. */
    public GamePlay getPlayerGamePlay(int player)
    {
        return this._games.get(player);
    }

    /** Given a board instance and a rule, constructs a transformed gameplay for
     * the player which applies the multiplayer rules in a synchronized way. */
    protected GamePlay getGamePlayProxy(Board board,
                                        Rule.RuleFactory ruleFactory)
    {
        return new MultiGamePlayProxy(this, board, ruleFactory.construct());
    }

    public GamePlay.GameState getCurrentState()
    {
        return this._currentState;
    }

    private void setCurrentState(GamePlay.GameState newState)
    {
        if (newState != this._currentState) {
            this._currentState = newState;

            for (GamePlayListener listener : this._listeners)
                listener.stateChanged(newState);

            for (GamePlay game : this._games)
                ((MultiGamePlayProxy) game).emitStateChanged(newState);
        }
    }
}
