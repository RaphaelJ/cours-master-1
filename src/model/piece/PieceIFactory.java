package model.piece;

import model.Coordinates;

public class PieceIFactory implements Piece.PieceFactory {
    public int getExtent()
    {
        return 4;
    }

    public Piece construct(Coordinates topLeft, int currentState)
    {
        return new PieceI(topLeft, currentState);
    }
}
