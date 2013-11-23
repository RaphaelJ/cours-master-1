package gameplay;

import model.Board;
import view.GameView;

/** Provides an interface for "rules" which controls how the game is running (
 * score, speed, levels ...).
 * GamePlay instances emits events to change the game behaviour. */
public class GamePlay {

    /** "Ticks" duration in milliseconds. */
    public static final int DEFAULT_SPEED = 1000;


    protected Board _board;

    private Timer _timer;
    private int _clockSpeed;


    public void addListener(GamePlayListener listener);

    /** Starts the timer which controls the game.
     * Resets the game if needed. */
    public synchronized void newGame()
    {
        if (this._board.getCurrentState() != Board.GameState.INITIALIZED)
            this.reset();

        this.startTimer();

        this._board.changeState(Board.GameState.RUNNING);
        this._board.gameTick();
    }

    /** Pauses/Unpauses the timer if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause()
    {
        switch (this._board.getCurrentState()) {
        case RUNNING:
            this._board.changeState(Board.GameState.PAUSED);
            break;
        case PAUSED:
            this._board.changeState(Board.GameState.RUNNING);
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

        this._clockSpeed = 0;

        this._board.reset();
    }

    private synchronized void startTimer()
    {
        TimerTask task = new TimerTask() {
            @Override
            public void run()
            {
                gameTick();
            }
        };

        this._timer = new Timer();
        this._timer.scheduleAtFixedRate(
            task, this._clockSpeed, this._clockSpeed
        );
    }

    /** Will be called by the board when n lines have been removed by the
     * player. */
    public void clearedLines(int n);

    /** Will be called by the board when a new piece has been randomly chosen
     * but not yet introduced in the grid. */
    public void newPiece(Piece piece);

    public int getScore();

    public int getLevel();

    /** Returns the current delay between two "ticks" in milliseconds. */
    public int getSpeed();

    public void setSpeed(int newClockSpeed)
    {
        this._clockSpeed = newClockSpeed;

        if (this._board.getCurrentState() == GameState.RUNNING) {
            this._timer.cancel();
            this.startTimer();
        }
    }
}
