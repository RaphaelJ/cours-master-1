package model;

import model.piece.Piece;

/** This interface is used to access to section of the board.
 * Defines a rectangle in witch every modified cell resides and gives a getter
 * method to get the content of one of these cells.
 * This is defined as an interface as some implementations will use a delayed
 * representation (get() is just an intermediate call to the real board) whereas
 * others will copy the board's content (for network instances). This avoid
 * useless copy of the grid content in the case of local views. */
public interface BoardSection {

    public int getX();
    public int getY();

    public int getWidth();
    public int getHeight();

    /** Returns the piece at the given index in the rectangle (not in the
     * board). */
    public Piece get(int y, int x);
}
