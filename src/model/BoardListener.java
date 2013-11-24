package model;

import java.awt.Rectangle;
import java.util.*;

import model.piece.Piece;

/** Provides an interface for views and other objects which listen to game's
 * board changes. */
public interface BoardListener extends EventListener {
    public void stateChange(Board.GameState newState);

    /** Event triggered when a some cells of the grid have been modified. */
    public void gridChange(Rectangle bounds);

    /** When a new piece has been randomly chosen (not yet introduced in the
     * grid). */
    public void newPiece(Piece piece);
}
