package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Creates a proxy for the player gameplay but with stopping the opponent game
 * when the pause() method is called. */
public class DualGamePlayProxy implements GamePlay {

    protected DualGamePlay _dualGame;
    protected GamePlay _player, _opponent;

    public DualGamePlayProxy(DualGamePlay dualGame, GamePlay player,
                             GamePlay opponent)
    {
        this._dualGame = dualGame;
        this._player = player;
        this._opponent = opponent;
    }

    public void newGame()
    {
        System.out.println(this._dualGame);
        synchronized (this._dualGame) { // Avoid both players to press pause
                                        // at the same time.
            this._player.newGame();
            this._opponent.newGame();
        }
    }

    public synchronized void pause()
    {
        synchronized (this._dualGame) {
            this._player.pause();
            this._opponent.pause();
        }
    }

    public void moveLeft()
    {
        this._player.moveLeft();
    }

    public void moveRight()
    {
        this._player.moveRight();
    }

    public void softDrop()
    {
        this._player.softDrop();
    }

    public void hardDrop()
    {
        this._player.hardDrop();
    }

    public void rotate()
    {
        this._player.rotate();
    }

    public void reset()
    {
        this._player.reset();
    }

    public void clearLines(LinkedList<Integer> lines)
    {
        this._player.clearLines(lines);
    }

    public void addListener(GamePlayListener listener)
    {
        this._player.addListener(listener);
    }

    public Board getBoard()
    {
        return this._player.getBoard();
    }

    public int getScore()
    {
        return this._player.getScore();
    }

    public int getLevel()
    {
        return this._player.getLevel();
    }

    public int getSpeed()
    {
        return this._player.getSpeed();
    }

    public void setSpeed(int newClockSpeed)
    {
        this._player.setSpeed(newClockSpeed);
    }
}