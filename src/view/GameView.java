package view;

import java.util.*;

import model.piece.Piece;

/** Provides an interface for views which are object which listen to game's
 * board changes. */
public interface GameView extends EventListener {
    /** Event triggered when a piece move inside the grid. */
    public void gridChange();

    /** Event triggered when n lines have been removed by the player. */
    public void clearedLines(int n);

    /** When a new piece has been randomly chosen (not yet introduced in the
     * grid). */
    public void newPiece(Piece piece);

    public void gameOver();

    public void reset();
}
