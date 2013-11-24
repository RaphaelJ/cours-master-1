package gameplay;

/** Applies a gameplay rule to a two independant players game. */
public class DualGamePlay {
    protected GamePlay _gameplay1, _gameplay2:

    public DualGamePlay(GamePlayFactory innerGamePlay)
    {
        this._gameplay1 = innerGamePlay.construct();
        this._gameplay2 = innerGamePlay.construct();
    }

    /** Broadcast the new game event to both players. */
    public synchronized void newGame()
    {
        this._gameplay1.newGame();
        this._gameplay2.newGame();
    }

    /** Broadcast the pause event to both players. */
    public synchronized void pause()
    {
        this._gameplay1.pause();
        this._gameplay2.pause();
    }

    /** Returns the GamePlay instance which can be used to controll the first
     * player's game. */
    public GamePlay getPlayer1GamePlay()
    {
        return this._gameplay1;
    }

    /** Returns the GamePlay instance which can be used to controll the second
     * player's game. */
    public GamePlay getPlayer2GamePlay()
    {
        return this._gameplay2;
    }
}
