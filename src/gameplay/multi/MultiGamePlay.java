package gameplay.multi;

import java.util.ArrayList;

import gameplay.*;
import model.Board;

/** Applies a gameplay rule to a two independant players game. */
public class MultiGamePlay {
    protected ArrayList<GamePlay> _gameplays;

    public MultiGamePlay(GamePlayFactory innerGamePlay, ArrayList<Board> boards)
    {
        this._gameplays = new ArrayList<GamePlay>();

        ArrayList<GamePlay> tmpGamePlays = new ArrayList<GamePlay>();

        for(Board board : boards) {
            GamePlay gameplay = innerGamePlay.construct(board);
            tmpGamePlays.add(gameplay);
        }

        for(int i = 0; i < tmpGamePlays.size(); i++) {
            GamePlay player = tmpGamePlays.get(i);
            ArrayList<GamePlay> opponents = new ArrayList<GamePlay>();

            for(int j = 0; j < tmpGamePlays.size(); j++) {
                if(j != i)
                    opponents.add(tmpGamePlays.get(j));
            }

            this._gameplays.add(this.wrapGamePlay(player, opponents));
        }
    }

    /** Broadcast the new game event to both players. */
    public synchronized void newGame()
    {
        for(GamePlay gameplay : this._gameplays)
            gameplay.newGame();
    }

    /** Broadcast the pause event to both players. */
    public synchronized void pause()
    {
        for(GamePlay gameplay : this._gameplays)
            gameplay.pause();
    }

    /** Returns the GamePlay instance which can be used to controll the nbPlayer
     * player's game. */
    public GamePlay getPlayerGamePlay(int nbPlayer)
    {
        return this._gameplays.get(nbPlayer);
    }

    /** Given a player gameplay instance and its opponent one, constructs the
     * transformed gameplay for the player which applies the multiplayer rules.
     * This instance returns the inner gameplay wrapped with the pause method
     * which pauses both games. */
    protected GamePlay wrapGamePlay(GamePlay player,
                                    ArrayList<GamePlay> opponents)
    {
        return new MultiGamePlayProxy(this, player, opponents);
    }
}
