package controller;

import model.Board;

/** Basic controller which tramits event to the game's board and manages
 * a timer. */
public class LocalController implements GameController {

    private Board _board;

    public LocalController(Board board)
    {
        this._board = board;
    }

    public void newGame()
    {
        this._board.start();
    }
}
