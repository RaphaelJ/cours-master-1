package controller;

import java.util.Timer;
import java.util.TimerTask;

import model.Board;

/** Basic controller which tramits event to the game's board and manages
 * a timer. */
public class LocalController implements GameController {

    private Board _board;
    private Timer _timer;
    private TimerTask _tickTask;
    private int _clockSpeed;

    public LocalController(Board board, int clockSpeed)
    {
        this._board = board;
        this._timer = new Timer();
        this._tickTask = new TimerTask() {
			
			@Override
			public void run() {
				_board.gameTick();
			}
		};
		this._clockSpeed = clockSpeed;
    }

    public void newGame()
    {
        /* timer.cancel(); */
        /* this._board.resetBoard(); */
        /* new Timer({ this._board.gameTick() }).start(); */
    	this._timer.scheduleAtFixedRate(this._tickTask, 0, this._clockSpeed);
    }
}
