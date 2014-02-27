package gameplay.multi;

import java.util.*;

import gameplay.*;
import gameplay.rules.*;
import model.Board;

/** Applies a gameplay rule to a two players game where lines are only removed
 * if the line is also complete on the opponent grid. */
public class MultiCooperative extends MultiGamePlay {

    public MultiCooperative(ArrayList<Board> boards,
                            Rule.RuleFactory ruleFactory)
    {
        super(boards, ruleFactory);
    }

    /** Wraps the inner gameplay in a proxy so lines are removed only if they
     * are complete for all other players. */
    @Override
    protected GamePlay getGamePlayProxy(Board board,
                                        Rule.RuleFactory ruleFactory)
    {
        return new MultiGamePlayProxy(this, board, ruleFactory.construct()) {
            @Override
            public void clearLines(LinkedList<Integer> lines)
            {
                synchronized (this._multiGame) {
                    LinkedList<Integer> toRemove = new LinkedList<Integer>();

                    linesFor:
                        for (Integer i : lines) {
                            for (GamePlay opponent 
                                 : this._multiGame.getGamePlays()) {
                                if (opponent != this) {
                                    Board board = opponent.getBoard();
                                    boolean complete =
                                        board.getGrid()[i.intValue()]
                                                      .isComplete();

                                    if (!complete) // Doesn't remove this line.
                                        continue linesFor;
                                }
                            }
                            toRemove.add(i);
                        }

                    if (toRemove.size() > 0) {
                        for (GamePlay game : this._multiGame.getGamePlays()) {
                            for (Integer i : toRemove)
                                game.getBoard().removeLine(i.intValue());

                            game.getRule().clearLines(toRemove.size());
                        }
                    }
                }
            }
        };
    }
}
