package model.piece;

public class PieceZ extends Piece {
    private class PieceZFactory extends Piece.PieceFactory {
        public int getExtent()
        {
            return 3;
        }

        public Piece construct(Coordinates topLeft, int currentState)
        {
            return new PieceZ(topLeft, currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, false, false },
            { true,  true,  false },
            { false, true,  true  }
        }, {
            { false, false, true },
            { false, true,  true },
            { false, true,  false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceZFactory factory = new PieceZFactory();
    protected final PieceFactory _factory = factory;

    public PieceZ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
