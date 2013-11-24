package controller;

import gameplay.GamePlay;

/** Basic controller which tramits event to the game's board and manages
 * a timer. */
public class LocalController implements GameController {

    private GamePlay _game;

    public LocalController(GamePlay game)
    {
        this._game = game;
    }

    public void newGame()
    {
        this._game.newGame();
    }

    public void pause()
    {
        this._game.pause();
    }

    public void moveLeft()
    {
        this._game.moveLeft();
    }

    public void moveRight()
    {
        this._game.moveRight();
    }

    public void softDrop()
    {
        this._game.softDrop();
    }

    public void hardDrop()
    {
        this._game.hardDrop();
    }

    public void rotate()
    {
        this._game.rotate();
    }
}
