package model.piece;

public class PieceL extends Piece {
    private class PieceLFactory extends Piece.PieceFactory {
        public int getExtent()
        {
            return 3;
        }

        public Piece construct(Coordinates topLeft, int currentState)
        {
            return new PieceL(topLeft, currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, false, false },
            { true,  true,  true  },
            { true,  false, false }
        }, {
            { false, true, false },
            { false, true, false },
            { false, true, true  }
        }, {

            { true,  true, false },
            { false, true, false },
            { false, true, false }
        }, {
            { false, false, true  },
            { true,  true,  true  },
            { false, false, false }
        }, 
    };
    protected final Boolean[][][] _states = states;

    private static final PieceLFactory factory = new PieceLFactory();
    protected final PieceFactory _factory = factory;

    public PieceL(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
