package model.piece;

public class PieceI extends Piece {
    private class PieceIFactory extends Piece.PieceFactory {
        public int getExtent()
        {
            return 4;
        }

        public Piece construct(Coordinates topLeft, int currentState)
        {
            return new PieceI(topLeft, currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, true, false, false },
            { false, true, false, false },
            { false, true, false, false },
            { false, true, false, false }
        }, {
            { false, false, false, false },
            { true,  true,  true,  true  },
            { false, false, false, false },
            { false, false, false, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceIFactory factory = new PieceIFactory();
    protected final PieceFactory _factory = factory;

    public PieceI(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
