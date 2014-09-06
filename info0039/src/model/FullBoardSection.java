package model;

import model.piece.Piece;

/** Implements a board section for the entire grid (x = 0, y = 0,
 * width = board's width and height = board's height). */
public abstract class FullBoardSection implements BoardSection {

    private int _width;
    private int _height;

    public FullBoardSection(int width, int height)
    {
        this._width = width;
        this._height = height;
    }

    public int getX()
    {
        return 0;
    }

    public int getY()
    {
        return 0;
    }

    public int getWidth()
    {
        return this._width;
    }

    public int getHeight()
    {
        return this._height;
    }

    public abstract Piece get(int y, int x);
}