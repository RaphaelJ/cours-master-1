package gameplay.rules;

import java.util.*;

import gameplay.*;

/** Implements the traditional Marathon mode from the Game Boy tetris.
 * Read http://tetris.wikia.com/wiki/Tetris_(Game_Boy) for a complete
 * documentation. */
public class NintendoGameBoy extends DefaultRule {
    /** Clock's delay for each level. */
    private static int[] _levels = {
        883, 817, 750, 683, 617, 550, 467, 367, 283, 183, 167, 150, 133, 117,
        100, 100, 83, 83, 67, 67, 50
    };

    private int _score = 0;
    private int _nClearedLines = 0;

    public NintendoGameBoy()
    {
    }

    public void reset()
    {
        this._score = 0;
        this._nClearedLines = 0;
    }

    public synchronized void clearLines(int n)
    {
        // Advances by one level every 10 lines.
        int oldClearedLines = this._nClearedLines;
        this._nClearedLines += n;
        if (this._nClearedLines / 10 > oldClearedLines / 10) {
            int newLevel = this.getLevel();
            this.emitLevelChange(newLevel);
            this.emitClockDelayChange(_levels[newLevel - 1]);
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

    @Override
    public int getClockDelay()
    {
        return _levels[this.getLevel() - 1];
    }
}
