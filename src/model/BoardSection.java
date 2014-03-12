package model;

/** This class is used by board's events to signal cells which have been
 * modified. */
class BoardSection {
    public final int x;
    public final int y;
    public final int width;
    public final int height;

    public final Piece[][] grid;

    public BoardSection(int x, int y, int width, int height, Piece[][] grid)
    {
        
    }

    public BoardSection(int x, int y, int width, int height, Board board)
    {
    }
}