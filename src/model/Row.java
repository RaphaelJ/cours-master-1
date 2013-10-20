package model;

import java.util.Arrays;

import model.piece.Piece;

/** Maps every cell of a line to an piece. */
public class Row {

    private final int _width;
    private int _nbBlocks;
    private Piece[] _pieces;

    public Row(int width)
    {
        this._width = width;
        this._nbBlocks = 0;
        this._pieces = new Piece[width];
    }

    public Piece[] getPieces()
    {
        return this._pieces;
    }

    public Piece getPiece(int column)
    {
        return this._pieces[column];
    }

    /** Changes the state of a cell.
     * A piece with null as value frees the cell. */
    public void setPiece(Piece piece, int column)
    {
        if(piece != null)
            this._nbBlocks++;
        else
            this._nbBlocks--;

        this._pieces[column] = piece;
    }

    public boolean isComplete()
    {
        return this._nbBlocks == this._width;
    }
}
