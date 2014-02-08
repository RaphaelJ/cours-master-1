package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Creates a proxy for the player gameplay but with stopping the opponent game
 * when the pause() method is called. */
public class MultiGamePlayProxy implements GamePlay {

    protected MultiGamePlay _dualGame;
    protected GamePlay _player;
    protected ArrayList<GamePlay> _opponents;

    public MultiGamePlayProxy(MultiGamePlay dualGame, GamePlay player,
                             ArrayList<GamePlay> opponents)
    {
        this._dualGame = dualGame;
        this._player = player;
        this._opponents = opponents;
    }

    public void newGame()
    {
        synchronized (this._dualGame) { // Avoid both players to press pause
                                        // at the same time.
            this._player.newGame();
            
            for(GamePlay opponent : this._opponents)
            	opponent.newGame();
        }
    }

    public void pause()
    {
        synchronized (this._dualGame) {
            this._player.pause();
            
            for(GamePlay opponent : this._opponents)
            	opponent.pause();
        }
    }
    
    public void stop()
    {
        this._player.stop();
    }

    public void moveLeft()
    {
        synchronized (this._dualGame) {
            this._player.moveLeft();
        }
    }

    public void moveRight()
    {
        synchronized (this._dualGame) {
            this._player.moveRight();
        }
    }

    public void softDrop()
    {
        synchronized (this._dualGame) {
            this._player.softDrop();
        }
    }

    public void hardDrop()
    {
        synchronized (this._dualGame) {
            this._player.hardDrop();
        }
    }

    public void rotate()
    {
        synchronized (this._dualGame) {
            this._player.rotate();
        }
    }

    public void reset()
    {
        synchronized (this._dualGame) {
            this._player.reset();
        }
    }

    public void clearLines(LinkedList<Integer> lines)
    {
        synchronized (this._dualGame) {
            this._player.clearLines(lines);
        }
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
        synchronized (this._dualGame) {
            this._player.setSpeed(newClockSpeed);
        }
    }
}
