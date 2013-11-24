package gameplay;

import java.util.*;

import model.Board;
import model.Board.GameState;
import model.piece.Piece;

/** Provides an interface for "rules" which manage the dynamic aspect of the
 * game (timer, score, speed, levels ...).
 * GamePlay instances change the game behaviour when receiving events from
 * controllers by changing their timer and by controlling their associed Board.
 */
public abstract class GamePlay {

    protected Board _board;

    private int _speed;
    private Timer _timer = null;

    private ArrayList<GamePlayListener> _listeners
        = new ArrayList<GamePlayListener>();

    public GamePlay(Board board, int speed)
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

    /** Reinitialises the game and the board. Stop the game if it's running. */
    public synchronized void reset()
    {
        if (this._board.getCurrentState() == Board.GameState.RUNNING)
            this._timer.cancel();

        this._board.reset();
    }

    /*********************** Board events ***********************/

    /** Will be called by the board when n lines have been removed by the
     * player. */
    public void clearedLines(int n)
    {
    }

    /** Will be called by the board when a new piece has been randomly chosen
     * but not yet introduced in the grid. */
    public void newPiece(Piece piece)
    {
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

    /** Returns the current delay between two "ticks" in milliseconds. */
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
