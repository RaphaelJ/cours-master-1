package model.piece;

import model.Coordinates;

public class PieceSFactory implements Piece.PieceFactory {
    public int getExtent()
    {
        return 3;
    }

    public Piece construct(Coordinates topLeft, int currentState)
    {
        return new PieceS(topLeft, currentState);
    }
}
