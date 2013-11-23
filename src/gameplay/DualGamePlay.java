package gameplay;

/** Applies an internal gameplay algorithm to a two independant players game. */
public class DualGamePlay implements GamePlay {
    protected GamePlay _gameplay1, _gameplay2:

    public DualGamePlay(GamePlayFactory innerGamePlay)
    {
        this._gameplay1 = innerGamePlay.construct();
        this._gameplay2 = innerGamePlay.construct();
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
