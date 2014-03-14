package game.multi;

import java.util.*;

import game.*;
import game.rules.*;
import model.Board;

/** Applies a gameplay rule to a two players game where (n-1) lines are sent to
 * the opponent when the player removes n lines. */
public class MultiClassic extends MultiGame {

    /** Position of the empty block in added lines. */
    protected int posHole;

    public MultiClassic(ArrayList<Board> boards, Rule.RuleFactory ruleFactory,
                        int posHole)
    {
        super(boards, ruleFactory);

        this.posHole = posHole;
    }

    /** Wraps the inner gameplay in a proxy so (n-1) lines are added to the
     * opponent grids when the player clears n lines. */
    @Override
    protected MultiGameProxy getGameProxy(Board board,
                                          Rule.RuleFactory ruleFactory)
    {
        return new MultiGameProxy(this, board, ruleFactory.construct()) {
            @Override
            public void clearLines(LinkedList<Integer> lines)
            {
                synchronized (this._multiGame) {
                    super.clearLines(lines);

                    for (MultiGameProxy opponent : this._multiGame.getGames()) {
                        if (opponent != this) {
                            for (int i = 0; i < lines.size() - 1; i++)
                                opponent.getBoard().addLine(posHole);
                        }
                    }
                }
            }
        };
    }
}
