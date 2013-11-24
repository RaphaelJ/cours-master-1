package gameplay.multi;

import gameplay.*;
import model.Board;

/** Applies a gameplay rule to a two independant players game. */
public class DualGamePlay {
    protected GamePlay _gameplay1, _gameplay2;

    public DualGamePlay(GamePlayFactory innerGamePlay, Board board1,
                        Board board2)
    {
        GamePlay player1 = innerGamePlay.construct(board1)
               , player2 = innerGamePlay.construct(board2);

        this._gameplay1 = this.wrapGamePlay(player1, player2);
        this._gameplay2 = this.wrapGamePlay(player2, player1);
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

    /** Given a player gameplay instance and its opponent one, constructs the
     * transformed gameplay for the player which applies the multiplayer rules.
     * This instance returns the inner gameplay wrapped with the pause method
     * which pauses both games. */
    protected GamePlay wrapGamePlay(GamePlay player, GamePlay opponent)
    {
        return new DualGamePlayProxy(this, player, opponent);
    }
}
