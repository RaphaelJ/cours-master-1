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
        this._board.resetBoard();
        this._board.start();
    }

    public void left()
    {
        this._board.moveLeft();
    }

    public void right()
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
}
