package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Creates a proxy for the player gameplay but with stopping the opponent game
 * when the pause() method is called. */
public class MultiGamePlayProxy implements GamePlay {

    protected MultiGamePlay _multiGame;
    protected GamePlay _player;
    protected ArrayList<GamePlay> _opponents;

    public MultiGamePlayProxy(MultiGamePlay multiGame, GamePlay player,
                             ArrayList<GamePlay> opponents)
    {
        this._multiGame = multiGame;
        this._player = player;
        this._opponents = opponents;
    }

    public void newGame()
    {
        synchronized (this._multiGame) { // Avoid both players to press pause
                                         // at the same time.
            this._player.newGame();

            for(GamePlay opponent : this._opponents)
                opponent.newGame();
        }
    }

    public void pause()
    {
        synchronized (this._multiGame) {
            this._player.pause();

            for(GamePlay opponent : this._opponents)
                opponent.pause();
        }
    }

    public void stop()
    {
        this._player.stop();
    }

    public void reset()
    {
        synchronized (this._multiGame) {
            this._player.reset();
        }
    }

    public void clearLines(LinkedList<Integer> lines)
    {
        synchronized (this._multiGame) {
            this._player.clearLines(lines);
        }
    }

    public void gameOver()
    {
        synchronized (this._multiGame) {
            for(GamePlay opponent : this._opponents)
                opponent.gameOver();
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
        synchronized (this._multiGame) {
            this._player.setSpeed(newClockSpeed);
        }
    }
}
