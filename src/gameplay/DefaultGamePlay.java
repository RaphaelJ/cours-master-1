package gameplay;

import java.util.*;

import ai.ArtificialIntelligence;
import gameplay.rules.*;
import model.Board;
import util.*;

/** Provides a base class for single player gameplays which use a timer to
 * controll the game. */
public class DefaultGamePlay implements GamePlay, RuleListener {

    protected Board _board;
    protected Rule _rule;

    private GamePlay.GameState _currentState = GamePlay.GameState.INITIALIZED;

    // For the game "ticks".
    private GameTimer _game_timer;

    // For the elapsed time.
    private GameTimer _timer;

    private ArtificialIntelligence _ai = null;

    private ArrayList<GamePlayListener> _listeners =
        new ArrayList<GamePlayListener>();

    public DefaultGamePlay(Board board, Rule rule)
    {
        this._board = board;
        board.setGamePlay(this);

        this._rule = rule;
        rule.addListener(this);

        this.initTimers();
    }

    public synchronized void addListener(GamePlayListener listener)
    {
        this._listeners.add(listener);
    }

    /*********************** User actions ***********************/

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

        this._board.reset();
        this._rule.reset();

        this.initTimers();

        this.setCurrentState(GamePlay.GameState.INITIALIZED);
    }

    public synchronized void stop()
    {
        if (this._currentState == GamePlay.GameState.RUNNING
            || _currentState == GamePlay.GameState.PAUSED)
            this.stopTimers();

        this.setCurrentState(GamePlay.GameState.STOPPED);
    }

    public synchronized void moveLeft()
    {
        if (this._currentState == GamePlay.GameState.RUNNING)
            this._board.moveLeft();
    }

    public synchronized void moveRight()
    {
        if (this._currentState == GamePlay.GameState.RUNNING)
            this._board.moveRight();
    }

    /** Push the piece one line down. */
    public synchronized void softDrop()
    {
        if (this._currentState == GamePlay.GameState.RUNNING)
            this._board.softDrop();
    }

    /** Push the piece down to the last free line. */
    public synchronized void hardDrop()
    {
        if (this._currentState == GamePlay.GameState.RUNNING)
            this._board.hardDrop();
    }

    /** Tries to rotate the piece. */
    public synchronized void rotate()
    {
        if (this._currentState == GamePlay.GameState.RUNNING)
            this._board.rotate();
    }

    /** Enable/disable the Artificial intelligence. Disabled by default. */
    public synchronized void setAI(boolean enable)
    {
        if (this._ai == null)
            this._ai = new ArtificialIntelligence(this);

        this._ai.setActive(enable);
    }

    /*********************** Board events ***********************/

    /** Will be called by the board when asking to clear a set of lines.
     * The default method calls the removeLine() method of the board for each
     * line.
     * The gameplay instance must be previously associated with the board using
     * the Board.setBoard() method. */
    public synchronized void clearLines(LinkedList<Integer> linesIndices)
    {
        for (Integer i : linesIndices)
            this._board.removeLine(i.intValue());

        this._rule.clearLines(linesIndices.size());
    }

    public synchronized void gameOver()
    {
        if (this._currentState == GamePlay.GameState.RUNNING
            || _currentState == GamePlay.GameState.PAUSED)
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
                }
            }, 1000
        );

        this._game_timer = new GameTimer(
            new Runnable() {
                @Override
                public void run()
                {
                    if (_currentState == GamePlay.GameState.RUNNING)
                        _board.gameTick();
                }
            }, this._rule.getClockDelay()
        );
    }

    public synchronized void startTimers()
    {
        this._timer.start();
        this._game_timer.start();
    }

    public synchronized void stopTimers()
    {
        this._timer.stop();
        this._game_timer.stop();
    }

    public void scoreChange(int newScore) { }

    public void levelChange(int newLevel) { }

    public void clockDelayChange(int newClockDelay)
    {
        this._game_timer.changeSpeed(newClockDelay);
    }

    /*********************** Getters ***********************/

    public Board getBoard()
    {
        return this._board;
    }

    public Rule getRule()
    {
        return this._rule;
    }

    public GameTimer getTimer()
    {
        return this._timer;
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
        }
    }
}
