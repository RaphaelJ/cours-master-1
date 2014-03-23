package game.multi;

import game.rules.Rule;

import java.util.ArrayList;
import java.util.LinkedList;

import model.Board;
import util.random.LCGRandom;

/** Applies a gameplay rule to a two players game where (n-1) lines are sent to
 * the opponent when the player removes n lines. */
public class MultiClassic extends MultiGame {

    /** Position of the empty block in added lines. */
    protected int posHole;

    public MultiClassic(
        int width, int height, int nPlayers, Rule.RuleFactory ruleFactory,
        int posHole
    )
    {
        super(width, height, nPlayers, ruleFactory);

        this.posHole = posHole;
    }

    /** Creates boards using random number generators initialized with the same
     * seed. */
    @Override
    protected ArrayList<Board> getBoards(int width, int height, int nPlayers)
    {
        ArrayList<Board> boards = new ArrayList<Board>(nPlayers);
        long commonSeed = new LCGRandom().getSeed();

        for(int i = 0; i < nPlayers; i++) {
            boards.add(new Board(new LCGRandom(commonSeed), width, height));
        }

        return boards;
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
