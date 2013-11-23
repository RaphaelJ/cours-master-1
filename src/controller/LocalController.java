package controller;

import gameplay.GamePlay;
import gameplay.GamePlayListener;

import java.util.Timer;
import java.util.TimerTask;

import model.Board;
import model.Board.GameState;

/** Basic controller which tramits event to the game's board and manages
 * a timer. */
public class LocalController implements GameController, GamePlayListener {

	/** "Ticks" duration in milliseconds. */
    public static final int DEFAULT_SPEED = 1000;
	
    private Board _board;
    private GamePlay _gameplay;
    private Timer _clock;
    private int _clockSpeed;

    public LocalController(Board board, GamePlay gameplay)
    {
        this._board = board;
        this._gameplay = gameplay;
        this._clockSpeed = DEFAULT_SPEED;
        
        this._gameplay.addListener(this);
        this._board.addView(this._gameplay);
    }

    public void newGame()
    {
    	if (this._board.getCurrentState() == GameState.RUNNING)
            this._clock.cancel();
    	
        this._board.newGame();
        this.startTimer();
    }
    
    private synchronized void startTimer()
    {
        TimerTask task = new TimerTask() {
            @Override
            public void run()
            {
                _board.gameTick();
                
                if(_board.getCurrentPiece() == null)
                	gameover();
            }
        };

        this._clock = new Timer();
        this._clock.scheduleAtFixedRate(
            task, this._clockSpeed, this._clockSpeed
        );
    }

    /** Pauses/Unpauses the timer if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause()
    {
        switch (this._board.getCurrentState()) {
        case RUNNING:
        	this._board.changeState(GameState.PAUSED);
            this._clock.cancel();
            break;
        case PAUSED:
        	this._board.changeState(GameState.RUNNING);
            this.startTimer();
            break;
        case INITIALIZED:
        case GAMEOVER:
            break;
        }
    }
    
    public void gameover()
    {
    	this._clock.cancel();
        this._board.changeState(GameState.GAMEOVER);
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
    
    public void scoreChange(int newScore) { }

    public void levelChange(int newLevel) { }

    public void speedChange(int newClockSpeed)
    {
        this.setClockSpeed(newClockSpeed);
    }
    
    public int getClockSpeed()
    {
        return this._clockSpeed;
    }

    /** Changes the speed of the game. */
    public void setClockSpeed(int clockSpeed)
    {
        this._clockSpeed = clockSpeed;

        if (this._board.getCurrentState() == GameState.RUNNING) {
            this._clock.cancel();
            this.startTimer();
        }
    }
}
