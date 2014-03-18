package game;

import game.rules.Rule;
import game.rules.RuleListener;

import java.util.ArrayList;
import java.util.LinkedList;

import model.Board;
import model.FullBoardSection;
import model.piece.Piece;
import util.GameTimer;
import ai.ArtificialIntelligence;

/** Provides a base class for single player games which use a timer to control
 * the game behavior. */
public class Game implements GameManager, GamePlayer, RuleListener {

    protected Board _board;
    protected Rule _rule;

    private GameManager.GameState _currentState
        = GameManager.GameState.INITIALIZED;

    // For the game "ticks".
    private GameTimer _game_timer;

    // For the elapsed time.
    private GameTimer _timer;

    private ArtificialIntelligence _ai = null;

    private ArrayList<GameStateListener> _listeners
        = new ArrayList<GameStateListener>();

    public Game(Board board, Rule rule)
    {
        this._board = board;
        board.setGamePlayer(this);

        this._rule = rule;
        rule.addListener(this);

        this.initTimers();
    }

    public synchronized void addListener(GameListener listener)
    {
        this._board.addListener(listener);
        this._rule.addListener(listener);

        this._listeners.add(listener);
    }

    public synchronized void addListener(GameStateListener listener)
    {
        this._listeners.add(listener);
    }

    /*********************** User actions ***********************/

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
            || _currentState == GameManager.GameState.PAUSED)
            this.stopTimers();

        this.setCurrentState(GameManager.GameState.STOPPED);
    }

    public synchronized void reset()
    {
        if (this._currentState == GameManager.GameState.RUNNING
            || this._currentState == GameManager.GameState.PAUSED)
            this.stopTimers();

        this._board.reset();
        this._rule.reset();

        this.initTimers();

        this.setCurrentState(GameManager.GameState.INITIALIZED);
    }

    public synchronized void moveLeft()
    {
        if (this._currentState == GameManager.GameState.RUNNING)
            this._board.moveLeft();
    }

    public synchronized void moveRight()
    {
        if (this._currentState == GameManager.GameState.RUNNING)
            this._board.moveRight();
    }

    /** Push the piece one line down. */
    public synchronized void softDrop()
    {
        if (this._currentState == GameManager.GameState.RUNNING)
            this._board.softDrop();
    }

    /** Push the piece down to the last free line. */
    public synchronized void hardDrop()
    {
        if (this._currentState == GameManager.GameState.RUNNING)
            this._board.hardDrop();
    }

    /** Tries to rotate the piece. */
    public synchronized void rotate()
    {
        if (this._currentState == GameManager.GameState.RUNNING)
            this._board.rotate();
    }

    /** Enable/disable the Artificial intelligence. Disabled by default. */
    public synchronized void setAI(boolean enable)
    {
        if (this._ai == null)
            this._ai = new ArtificialIntelligence(this._board);

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
        if (this._currentState == GameManager.GameState.RUNNING
            || _currentState == GameManager.GameState.PAUSED)
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
                }
            }, 1000
        );

        this._game_timer = new GameTimer(
            new Runnable() {
                @Override
                public void run()
                {
                    if (_currentState == GameManager.GameState.RUNNING)
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

    public FullBoardSection getGrid()
    {
        return this._board.getGrid();
    }

    public int getGridWidth()
    {
        return this._board.getWidth();
    }

    public int getGridHeight()
    {
        return this._board.getHeight();
    }

    public Piece getCurrentPiece()
    {
        return this._board.getCurrentPiece();
    }

    public Piece getNextPiece()
    {
        return this._board.getNextPiece();
    }

    public int getScore()
    {
        return this._rule.getScore();
    }

    public int getLevel()
    {
        return this._rule.getLevel();
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
        }
    }
}
