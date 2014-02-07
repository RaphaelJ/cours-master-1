package gameplay.multi;

import java.util.*;

import gameplay.*;
import model.Board;

/** Applies a gameplay rule to a two players game where (n-1) lines are sent to
 * the opponent when the player removes n lines. */
public class MultiClassic extends MultiGamePlay {
	
	protected int posHole;

    public MultiClassic(GamePlayFactory innerGamePlay, ArrayList<Board> boards,
    		int posHole)
    {
        super(innerGamePlay, boards);
        
        this.posHole = posHole;
    }

    /** Wraps the inner gameplay in a proxy so (n-1) lines are added to the
     * opponents grid when the player clears n lines. */
    @Override
    protected GamePlay wrapGamePlay(GamePlay player, ArrayList<GamePlay> opponents)
    {
        return new MultiGamePlayProxy(this, player, opponents) {
            @Override
            public void clearLines(LinkedList<Integer> lines)
            {
                synchronized (this._dualGame) {
                	for(GamePlay opponent : this._opponents) {
	                    this._player.clearLines(lines);
	
	                    for (int i = 0; i < lines.size() - 1; i++)
	                        opponent.getBoard().addLine(posHole);
                	}
                }
            }
        };
    }
}
