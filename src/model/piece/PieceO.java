package model.piece;

public class PieceO extends Piece {
    private class PieceOFactory extends Piece.PieceFactory {
        public int getExtent()
        {
            return 2;
        }

        public Piece construct(Coordinates topLeft, int currentState)
        {
            return new PieceO(topLeft, currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { true, true },
            { true, true }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceOFactory factory = new PieceOFactory();
    protected final PieceFactory _factory = factory;

    public PieceO(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
