package gameplay;

import java.util.*;

import model.Board;
import model.Board.GameState;

/** Provides a base class for gameplays which use a timer to controll the game.
 * Most concrete gameplays should derive from this class whereas gameplays which
 * are relying on another gameplay, as the proxies used in multiplayer games,
 * are using the timer of the transformed gameplay and thus shouldn't derive 
 * from this class.
 */
public abstract class DefaultGamePlay implements GamePlay {

    protected Board _board;

    private int _speed;
    private Timer _timer = null;

    private ArrayList<GamePlayListener> _listeners
        = new ArrayList<GamePlayListener>();

    public DefaultGamePlay(Board board, int speed)
    {
        this._board = board;
        this._speed = speed;
    }

    /*********************** User actions ***********************/

    /** Starts the timer which controls the game.
     * Resets the game if needed. */
    public synchronized void newGame()
    {
        if (this._board.getCurrentState() != Board.GameState.INITIALIZED)
            this.reset();

        this.startTimer();

        this._board.setCurrentState(Board.GameState.RUNNING);
        this._board.gameTick();
    }

    /** Pauses/Unpauses the timer if the game is running/in pause.
     * Does nothing otherwise. */
    public synchronized void pause()
    {
        switch (this._board.getCurrentState()) {
        case RUNNING:
            this._timer.cancel();
            this._board.setCurrentState(Board.GameState.PAUSED);
            break;
        case PAUSED:
            this._board.setCurrentState(Board.GameState.RUNNING);
            this.startTimer();
            break;
        default:
        }
    }

    public void moveLeft()
    {
        this._board.moveLeft();
    }

    public void moveRight()
    {
        this._board.moveRight();
    }

    public void softDrop()
    {
        this._board.softDrop();
    }

    public void hardDrop()
    {
        this._board.hardDrop();
    }

    public void rotate()
    {
        this._board.rotate();
    }

    public synchronized void reset()
    {
        if (this._board.getCurrentState() == Board.GameState.RUNNING)
            this._timer.cancel();

        this._board.reset();
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
    }

    /*********************** Internals ***********************/

    private synchronized void startTimer()
    {
        TimerTask task = new TimerTask() {
            @Override
            public void run()
            {
                if (!_board.gameTick())
                    gameOver();
            }
        };

        this._timer = new Timer();
        this._timer.scheduleAtFixedRate(
            task, this._speed, this._speed
        );
    }

    private synchronized void gameOver()
    {
        this._timer.cancel();
    }

    /*********************** Getters/Setters and events ***********************/

    public void addListener(GamePlayListener listener)
    {
        this._listeners.add(listener);
    }

    public void emitScoreChange(int newScore)
    {
        for (GamePlayListener listener : this._listeners)
            listener.scoreChange(newScore);
    }

    public void emitLevelChange(int newLevel)
    {
        for (GamePlayListener listener : this._listeners)
            listener.levelChange(newLevel);
    }

    public void emitSpeedChange(int newClockSpeed)
    {
        for (GamePlayListener listener : this._listeners)
            listener.speedChange(newClockSpeed);
    }

    public Board getBoard()
    {
        return this._board;
    }

    public abstract int getScore();

    public abstract int getLevel();

    public int getSpeed()
    {
        return this._speed;
    }

    /** Changes the speed of the game. Will restart the timer if the game is
     * running. */
    public synchronized void setSpeed(int newClockSpeed)
    {
        this._speed = newClockSpeed;

        if (this._board.getCurrentState() == GameState.RUNNING) {
            this._timer.cancel();
            this.startTimer();
        }

        this.emitSpeedChange(newClockSpeed);
    }
}
