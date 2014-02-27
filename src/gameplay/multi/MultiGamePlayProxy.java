package gameplay.multi;

import java.util.*;

import ai.ArtificialIntelligence;
import gameplay.*;
import gameplay.rules.*;
import model.Board;
import util.*;

/** GamePlay which is a proxy for a multiplayer gameplay : actions from this
 * object are forwarded to the associated MultiGamePlay. */
public class MultiGamePlayProxy implements GamePlay, RuleListener {

    protected MultiGamePlay _multiGame;
    protected Board _board;
    protected Rule _rule;

    // Each player has its own timer as the speed of the game will change
    // independently for each player.
    protected GameTimer _timer;

    private ArtificialIntelligence _ai = null;

    private ArrayList<GamePlayListener> _listeners =
        new ArrayList<GamePlayListener>();

    public MultiGamePlayProxy(MultiGamePlay multiGame, Board board, Rule rule)
    {
        this._multiGame = multiGame;

        this._board = board;
        board.setGamePlay(this);

        this._rule = rule;
        rule.addListener(this);
    }

    public void addListener(GamePlayListener listener)
    {
        this._listeners.add(listener);
    }

    public void newGame()
    {
        this._multiGame.newGame();
    }

    public void pause()
    {
        this._multiGame.pause();
    }

    public void reset()
    {
        this._multiGame.reset();
    }

    public void stop()
    {
        this._multiGame.stop();
    }

    public void moveLeft()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState() == GamePlay.GameState.RUNNING)
                this._board.moveLeft();
        }
    }

    public void moveRight()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState() == GamePlay.GameState.RUNNING)
                this._board.moveRight();
        }
    }

    /** Push the piece one line down. */
    public void softDrop()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState() == GamePlay.GameState.RUNNING)
                this._board.softDrop();
        }
    }

    /** Push the piece down to the last free line. */
    public void hardDrop()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState() == GamePlay.GameState.RUNNING)
                this._board.hardDrop();
        }
    }

    /** Tries to rotate the piece. */
    public void rotate()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState() == GamePlay.GameState.RUNNING)
                this._board.rotate();
        }
    }

    /** Enable/disable the Artificial intelligence. Disabled by default. */
    public void setAI(boolean enable)
    {
        synchronized (this._multiGame) {
            if (this._ai == null)
                this._ai = new ArtificialIntelligence(this);

            this._ai.setActive(enable);
        }
    }

    public void clearLines(LinkedList<Integer> lines)
    {
        for (Integer i : lines)
            this._board.removeLine(i.intValue());

        this._rule.clearLines(lines.size());
    }

    public void gameOver()
    {
        this._multiGame.gameOver();
    }

    public void scoreChange(int newScore) { }

    public void levelChange(int newLevel) { }

    public void clockDelayChange(int newClockDelay)
    {
        this._timer.changeSpeed(newClockDelay);
    }

    public Board getBoard()
    {
        return this._board;
    }

    public Rule getRule()
    {
        return this._rule;
    }

    public GameTimer getTimer()
    {
        return this._timer;
    }

    public void setTimer(GameTimer timer)
    {
        this._timer = timer;
    }

    public GamePlay.GameState getCurrentState()
    {
        return this._multiGame.getCurrentState();
    }

    /** Is used by the MultiGamePlay to broadcast state change events to every
     * sinfle player gameplay listeners. */
    public void emitStateChanged(GamePlay.GameState newState)
    {
        for (GamePlayListener listener : this._listeners)
            listener.stateChanged(newState);
    }

    /** Is used by the MultiGamePlay to broadcast time changed events to every
     * sinfle player gameplay listeners. */
    public void emitTimeChanged(long elapsedTime)
    {
        for (GamePlayListener listener : this._listeners)
            listener.timeChanged(elapsedTime);
    }
}
