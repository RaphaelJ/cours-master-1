package model;

import java.util.EventListener;

import model.piece.Piece;

/** Provides an interface for views and other objects which listen to game's
 * board changes. */
public interface BoardListener extends EventListener {
    /** Event triggered when a some cells of the grid have been modified. */
    public void gridChange(BoardSection section);

    /** When a new piece has been randomly chosen. */
    public void newPiece(Piece currentPiece, Piece nextPiece);
}
