package model.piece;

import model.Coordinates;

public class PieceTFactory implements Piece.PieceFactory {
    public int getExtent()
    {
        return 3;
    }

    public Piece construct(Coordinates topLeft, int currentState)
    {
        return new PieceT(topLeft, currentState);
    }
}
