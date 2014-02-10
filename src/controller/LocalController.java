package controller;

import gameplay.GamePlay;

/** Basic controller which tramits events to the game's manager. */
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
        this._game.getBoard().moveLeft();
    }

    public void moveRight()
    {
        this._game.getBoard().moveRight();
    }

    public void softDrop()
    {
        this._game.getBoard().softDrop();
    }

    public void hardDrop()
    {
        this._game.getBoard().hardDrop();
    }

    public void rotate()
    {
        this._game.getBoard().rotate();
    }
}
