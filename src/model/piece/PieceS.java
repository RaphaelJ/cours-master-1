package model.piece;

public class PieceS extends Piece {
    private class PieceSFactory extends Piece.PieceFactory {
        public int getExtent()
        {
            return 3;
        }

        public Piece construct(Coordinates topLeft, int currentState)
        {
            return new PieceS(topLeft, currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, false, false },
            { false, true,  true  },
            { true,  true,  false }
        }, {
            { false, true,  false },
            { false, true,  true  },
            { false, false, true  }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceSFactory factory = new PieceSFactory();
    protected final PieceFactory _factory = factory;

    public PieceS(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
