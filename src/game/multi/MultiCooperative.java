package game.multi;

import game.rules.Rule;

import java.util.ArrayList;
import java.util.LinkedList;

import model.Board;
import model.Row;

/** Applies a game rule to a multi players game where lines are only removed if
 * the line is also complete on the opponent grid. */
public class MultiCooperative extends MultiGame {

    public MultiCooperative(ArrayList<Board> boards,
                            Rule.RuleFactory ruleFactory)
    {
        super(boards, ruleFactory);
    }

    /** Wraps the inner gameplay in a proxy so lines are removed only if they
     * are complete for all other players. */
    @Override
    protected MultiGameProxy getGameProxy(Board board,
                                          Rule.RuleFactory ruleFactory)
    {
        return new MultiGameProxy(this, board, ruleFactory.construct()) {
            @Override
            public void clearLines(LinkedList<Integer> lines)
            {
                synchronized (this._multiGame) {
                    LinkedList<Integer> toRemove = new LinkedList<Integer>();

                    linesFor:
                        for (Integer i : lines) {
                            for (MultiGameProxy opponent
                                 : this._multiGame.getGames()) {
                                if (opponent != this) {
                                    Row row = opponent.getBoard()
                                                      .getRow(i.intValue());

                                    if (!row.isComplete())
                                        // Doesn't remove this line.
                                        continue linesFor;
                                }
                            }
                            toRemove.add(i);
                        }

                    if (toRemove.size() > 0) {
                        for (MultiGameProxy game : this._multiGame.getGames()) {
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
