package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Applies a gameplay rule to a two players game where (n-1) lines are sent to
 * the opponent when the player removes n lines. */
public class DualClassic extends DualGamePlay {

    public DualClassic(GamePlayFactory innerGamePlay, Board board1,
                       Board board2)
    {
        super(innerGamePlay, board1, board2);
    }

    /** Wraps the inner gameplay in a proxy so (n-1) lines are added to the
     * opponent grid when the player clears n lines. */
    @Override
    protected GamePlay wrapGamePlay(GamePlay player, GamePlay opponent)
    {
        return new DualGamePlayProxy(this, player, opponent) {
            @Override
            public void clearLines(LinkedList<Integer> lines)
            {
                synchronized (this._dualGame) {
                    this._player.clearLines(lines);

                    for (int i = 0; i < lines.size() - 1; i++)
                        this._opponent.getBoard().addLine();
                }
            }
        };
    }
}
