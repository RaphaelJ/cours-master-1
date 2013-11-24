package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Applies a gameplay rule to a two players game where lines are only removed
 * if the line is also complete on the opponent grid. */
public class DualCooperative extends DualGamePlay {

    public DualCooperative(GamePlayFactory innerGamePlay, Board board1,
                           Board board2)
    {
        super(innerGamePlay, board1, board2);
    }

    /** Wraps the inner gameplay in a proxy so lines are removed only if they
     * are complete for both players. */
    @Override
    protected GamePlay wrapGamePlay(GamePlay player, GamePlay opponent)
    {
        return new DualGamePlayProxy(this, player, opponent) {
            @Override
            public synchronized void clearLines(LinkedList<Integer> lines)
            {
                this._player.clearLines(lines);

                Board opponentBoard = this._opponent.getBoard();

                for (Integer i : lines) {
                    if (opponentBoard.getGrid()[i.intValue()].isComplete()) {
                        this._player.getBoard().removeLine(i.intValue());
                        opponentBoard.removeLine(i.intValue());
                    }
                }
            }
        };
    }
}
