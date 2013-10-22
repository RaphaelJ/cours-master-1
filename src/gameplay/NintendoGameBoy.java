package gameplay;

import model.Board;
import view.GameView;

/** Implements the traditional Marathon mode from the Game Boy tetris.
 * Read http://tetris.wikia.com/wiki/Tetris_(Game_Boy) for a complete
 * documentation. */
public class NintendoGameBoy implements GamePlay, GameView {
    /** Clock's speed for each level. */
    private static int[] _levels = {
        883, 817, 750, 683, 617, 550, 467, 367, 283, 183, 167, 150, 133, 117,
        100, 100, 83, 83, 67, 67, 50
    };

    private Board _board;

    private int _score;
    private int _nClearedLines;

    private ArrayList<GamePlayListener> _listeners
        = new ArrayList<GamePlayListener>();

    public NintendoGameBoy()
    {
        this.initGame();
    }

    private void initGame()
    {
        this._score = 0;
        this._nClearedLines = 0;
    }

    public void setGame(Board board)
    {
        this._board = board;
    }

    public void addListener(GamePlayListener listener)
    {
        this._listener.add(listener);
    }

    public void stateChange(GameState newState)
    {
        switch (newState) {
        case INITIALIZED:
            this.initGame();
            break;
        }
    }

    public void gridChange() { }

    public void clearedLines(int n)
    {
        // Advances by one level every 10 lines.
        int oldClearedLines = this._nClearedLines;
        this._nClearedLines += n;

        if (this._nClearedLines % 10 > oldClearedLines % 10) {
            for (GamePlayListener listener : this._listeners) {
                listener.levelChange(this.getLevel());
                listener.speedChange(this.getSpeed());
            }
        }
    }

    public void newPiece(Piece piece) { }

    public int getScore()
    {
        return this._score;
    }

    public int getLevel()
    {
        return this._nClearedLines / 10;
    }

    public int getSpeed()
    {
        return _levels[this.getLevel()];
    }
}
