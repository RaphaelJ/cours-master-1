package game.multi;

import game.GameListener;
import game.GameManager;
import game.GameObserver;
import game.GamePlayer;
import game.GameStateListener;
import game.rules.Rule;
import game.rules.RuleListener;

import java.util.ArrayList;
import java.util.LinkedList;

import model.Board;
import model.FullBoardSection;
import model.piece.Piece;
import util.GameTimer;
import ai.ArtificialIntelligence;

/** GamePlayer which is a single player proxy for a multiplayer game : actions
 * from this object are forwarded to the associated MultiGame */
public class MultiGameProxy implements GamePlayer, RuleListener {

    protected MultiGame _multiGame;
    protected Board _board;
    protected Rule _rule;

    // Each player has its own timer as the speed of the game will change
    // independently for each player.
    protected GameTimer _timer;

    private ArtificialIntelligence _ai = null;

    private ArrayList<GameStateListener> _listeners
        = new ArrayList<GameStateListener>();

    public MultiGameProxy(MultiGame multiGame, Board board, Rule rule)
    {
        this._multiGame = multiGame;

        this._board = board;
        board.setGamePlayer(this);

        this._rule = rule;
        rule.addListener(this);
    }

    public void addListener(GameListener listener)
    {
        this._board.addListener(listener);
        this._rule.addListener(listener);

        this._listeners.add(listener);
    }

    public void addListener(GameStateListener listener)
    {
        this._listeners.add(listener);
    }

    /*********************** User actions ***********************/

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
            if (this._multiGame.getCurrentState()
                == GameManager.GameState.RUNNING)
                this._board.moveLeft();
        }
    }

    public void moveRight()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState()
                == GameManager.GameState.RUNNING)
                this._board.moveRight();
        }
    }

    /** Push the piece one line down. */
    public void softDrop()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState()
                == GameManager.GameState.RUNNING)
                this._board.softDrop();
        }
    }

    /** Push the piece down to the last free line. */
    public void hardDrop()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState()
                == GameManager.GameState.RUNNING)
                this._board.hardDrop();
        }
    }

    /** Tries to rotate the piece. */
    public void rotate()
    {
        synchronized (this._multiGame) {
            if (this._multiGame.getCurrentState()
                == GameManager.GameState.RUNNING)
                this._board.rotate();
        }
    }

    /** Enable/disable the Artificial intelligence. Disabled by default. */
    public void setAI(boolean enable)
    {
        synchronized (this._multiGame) {
            if (this._ai == null)
                this._ai = new ArtificialIntelligence(this._board);

            this._ai.setActive(enable);
        }
    }

    /*********************** Board events ***********************/

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

    /*********************** Internals ***********************/

    public void scoreChange(int newScore) { }

    public void levelChange(int newLevel) { }

    public void clockDelayChange(int newClockDelay)
    {
        this._timer.changeSpeed(newClockDelay);
    }

    /*********************** Getters ***********************/

    public FullBoardSection getGrid()
    {
        return this._board.getGrid();
    }

    public int getGridWidth()
    {
        return this._board.getWidth();
    }

    public int getGridHeight()
    {
        return this._board.getHeight();
    }

    public int getScore()
    {
        return this._rule.getScore();
    }

    public int getLevel()
    {
        return this._rule.getLevel();
    }

    public Piece getCurrentPiece()
    {
        return this._board.getCurrentPiece();
    }

    public Piece getNextPiece()
    {
        return this._board.getNextPiece();
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

    public GameManager.GameState getCurrentState()
    {
        return this._multiGame.getCurrentState();
    }

    /** Is used by the MultiGamePlay to broadcast state change events to every
     * single player game listeners. */
    public void emitStateChanged(GameManager.GameState newState)
    {
        for (GameStateListener listener : this._listeners)
            listener.stateChanged(newState);
    }

    /** Is used by the MultiGame to broadcast time changed events to every
     * single player game listeners. */
    public void emitTimeChanged(long elapsedTime)
    {
        for (GameStateListener listener : this._listeners)
            listener.timeChanged(elapsedTime);
    }
}
