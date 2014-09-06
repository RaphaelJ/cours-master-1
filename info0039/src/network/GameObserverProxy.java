package network;

import game.GameListener;
import game.GameManager;
import game.GameObserver;
import game.GameStateListener;

import java.util.ArrayList;

import model.BoardSection;
import model.FullBoardSection;
import model.piece.Piece;

/** A GameObserver proxy which will transmit events it receives from the
 * GameClient to the views. It doesn't transmit anything to the network, only
 * listenning. */
public class GameObserverProxy implements GameObserver {

    protected GameClient _client;

    private int _width;
    private int _height;
    private Piece[][] _grid;

    private Piece _currentPiece = null;
    private Piece _nextPiece = null;

    private int _score = 0;
    private int _level = 0;

    private ArrayList<GameListener> _listeners = new ArrayList<GameListener>();

    public GameObserverProxy(GameClient client, int width, int height)
    {
        this._client = client;
        this._width  = width;
        this._height = height;

        this._grid = new Piece[height][width];
    }

    public void addListener(GameListener listener)
    {
        this._listeners.add(listener);
    }

    public FullBoardSection getGrid()
    {
        return new FullBoardSection(this._width, this._height) {
            public Piece get(int y, int x)
            {
                return _grid[y][x];
            }
        };
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

    public GameManager.GameState getCurrentState()
    {
        return this._client.getCurrentState();
    }

    // Methods used by the GameClient to trigger events :

    public void emitGridChange(BoardSection section)
    {
        for (int i = 0; i < section.getHeight(); i++) {
            int y = section.getY() + i;
            for (int j = 0; j < section.getWidth(); j++) {
                this._grid[y][section.getX() + j] = section.get(i, j);
            }
        }

        for (GameListener listener : this._listeners)
            listener.gridChange(section);
    }

    public void emitNewPiece(Piece currentPiece, Piece nextPiece)
    {
        this._currentPiece = currentPiece;
        this._nextPiece = nextPiece;

        for (GameListener listener : this._listeners)
            listener.newPiece(currentPiece, nextPiece);
    }

    public void emitScoreChange(int newScore)
    {
        this._score = newScore;

        for (GameListener listener : this._listeners)
            listener.scoreChange(newScore);
    }

    public void emitLevelChange(int newLevel)
    {
        this._level = newLevel;

        for (GameListener listener : this._listeners)
            listener.levelChange(newLevel);
    }

    public void emitClockDelayChange(int newClockDelay)
    {
        for (GameListener listener : this._listeners)
            listener.clockDelayChange(newClockDelay);
    }

    public void emitStateChanged(GameManager.GameState newState)
    {
        for (GameListener listener : this._listeners)
            listener.stateChanged(newState);
    }

    public void emitTimeChanged(long elapsedTime)
    {
        for (GameListener listener : this._listeners)
            listener.timeChanged(elapsedTime);
    }
}
