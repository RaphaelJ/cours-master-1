package network;

import game.*;
import model.*;
import model.piece.*;

/** A GameObserver proxy which will transmit events it receives from the
 * GameClient to the views. It doesn't transmit anything to the network, only
 * listenning. */
public class GameObserverProxy implements GameObserver {

    private int _width;
    private int _height;
    private Piece[][] _grid;

    private Piece _currentPiece = null;
    private Piece _nextPiece = null;

    private int score = 0;
    private int level = 0;

    private ArrayList<GameStateListener> _listeners
        = new ArrayList<GameStateListener>();

    public GameObserverProxy(int width, int height)
    {
        this._width  = width;
        this._height = height;

        this._grid = new Piece[height][width];
    }

    public void addListener(GameListener listener)
    {
        this.addListener(listener);
    }

    public FullBoardSection getGrid()
    {
        return new FullBoardSection(this._width, this._height) {
            public Piece get(int y, int x)
            {
                return _grid[y][x];
            }
        }
    }

    public int getGridWidth()
    {
        return this._width;
    }

    public int getGridHeight()
    {
        return this._height;
    }

    public Piece getCurrentPiece()
    {
        return this._currentPiece;
    }

    public Piece getNextPiece()
    {
        return this._nextPiece;
    }

    public int getScore()
    {
        return this._score;
    }

    public int getLevel()
    {
        return this._level;
    }

    public void emitBoardChange(BoardSection section)
    {
        for (GameListener listener : this._listeners)
            listener.boardChange(section)
    }

    public void emitNewPiece(Piece currentPiece, Piece nextPiece)
    {
        // TODO
    }

    public void emitScoreChange(int newScore)
    {
        // TODO
    }

    public void emitLevelChange(int newLevel)
    {
        // TODO
    }

    public void emitClockDelayChange(int newClockDelay)
    {
        // TODO
    }

    public void emitStateChanged(GameManager.GameState newState)
    {
        // TODO
    }

    public void emitTimeChanged(long elapsedTime)
    {
        // TODO
    }
}
