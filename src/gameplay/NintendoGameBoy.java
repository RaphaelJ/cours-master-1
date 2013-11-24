package gameplay;

import model.Board;

/** Implements the traditional Marathon mode from the Game Boy tetris.
 * Read http://tetris.wikia.com/wiki/Tetris_(Game_Boy) for a complete
 * documentation. */
public class NintendoGameBoy extends GamePlay {
    /** Clock's speed for each level. */
    private static int[] _levels = {
        883, 817, 750, 683, 617, 550, 467, 367, 283, 183, 167, 150, 133, 117,
        100, 100, 83, 83, 67, 67, 50
    };

    private int _score;
    private int _nClearedLines;

    public NintendoGameBoy(Board board)
    {
        super(board, _levels[0]);
        this.initGame();
    }

    private synchronized void initGame()
    {
        this._score = 0;
        this._nClearedLines = 0;
    }

    @Override
    public synchronized void reset()
    {
        super.reset();

        this.initGame();

        this.emitScoreChange(this._score);
        this.emitLevelChange(this.getLevel());
        this.setSpeed(_levels[0]);
    }

    @Override
    public synchronized void clearedLines(int n)
    {
        // Advances by one level every 10 lines.
        int oldClearedLines = this._nClearedLines;
        this._nClearedLines += n;
        if (this._nClearedLines / 10 > oldClearedLines / 10) {
            int newLevel = this.getLevel();
            this.emitLevelChange(newLevel);
            this.emitSpeedChange(_levels[newLevel - 1]);
        }

        // Gives more points when groups of lines are erased.
        int[] points = { 40, 100, 300, 1200 };
        this._score += points[n - 1] * this.getLevel();

        this.emitScoreChange(this._score);
    }

    @Override
    public int getScore()
    {
        return this._score;
    }

    @Override
    public int getLevel()
    {
        return this._nClearedLines / 10 + 1;
    }
}
