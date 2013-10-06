package controller;

import util.timer.Task;
import util.timer.Timer;
import model.Board;

/** Basic controller which tramits event to the game's board and manages
 * a timer. */
public class LocalController implements GameController {

    private Board _board;
    private Timer _timer;

    public LocalController(Board board, int clockSpeed)
    {
        this._board = board;
        
        Task task = new Task(true) {
			
			@Override
			public void run() {
				_board.gameTick();
			}
		};
		
        this._timer = new Timer(task, clockSpeed);
    }

    public void newGame()
    {
        /* timer.cancel(); */
        /* this._board.resetBoard(); */
        /* new Timer({ this._board.gameTick() }).start(); */
    	this._timer.start();
    }
}
