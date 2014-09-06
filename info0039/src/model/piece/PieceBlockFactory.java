package model.piece;

import model.Coordinates;

public class PieceBlockFactory implements Piece.PieceFactory {
    public int getExtent()
    {
        return 1;
    }

    public Piece construct(Coordinates topLeft, int currentState)
    {
        return new PieceBlock(topLeft, currentState);
    }
}
