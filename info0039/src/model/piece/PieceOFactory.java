package model.piece;

import model.Coordinates;

public class PieceOFactory implements Piece.PieceFactory {
    public int getExtent()
    {
        return 2;
    }

    public Piece construct(Coordinates topLeft, int currentState)
    {
        return new PieceO(topLeft, currentState);
    }
}
