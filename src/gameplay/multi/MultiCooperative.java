package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Applies a gameplay rule to a two players game where lines are only removed
 * if the line is also complete on the opponent grid. */
public class MultiCooperative extends MultiGamePlay {

    public MultiCooperative(GamePlayFactory innerGamePlay,
                            ArrayList<Board> boards)
    {
        super(innerGamePlay, boards);
    }

    /** Wraps the inner gameplay in a proxy so lines are removed only if they
     * are complete for all other players. */
    @Override
    protected GamePlay wrapGamePlay(GamePlay player, ArrayList<GamePlay> opponents)
    {
        return new MultiGamePlayProxy(this, player, opponents) {
            @Override
            public void clearLines(LinkedList<Integer> lines)
            {
                synchronized (this._multiGame) {
                    LinkedList<Integer> toRemove = new LinkedList<Integer>();

                    linesFor:
                        for (Integer i : lines) {
                            for(GamePlay opponent : this._opponents) {
                                Board board = opponent.getBoard();
                                boolean complete = board.getGrid()[i.intValue()]
                                                        .isComplete();
                                if (!complete) // Doesn't remove this line.
                                    continue linesFor;
                            }
                            toRemove.add(i);
                        }


                    if (toRemove.size() > 0) {
                        this._player.clearLines(toRemove);
                        for(GamePlay opponent : this._opponents)
                            opponent.clearLines(toRemove);
                    }
                }
            }
        };
    }
}
