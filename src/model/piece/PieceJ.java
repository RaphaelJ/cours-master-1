package model.piece;

public class PieceJ extends Piece {
    private class PieceJFactory extends Piece.PieceFactory {
        public int getExtent()
        {
            return 3;
        }

        public Piece construct(Coordinates topLeft, int currentState)
        {
            return new PieceJ(topLeft, currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, false, false },
            { true,  true,  true  },
            { false, false, true }
        }, {
            { false, true, true  },
            { false, true, false },
            { false, true, false }
        }, {
            { true,  false, false },
            { true,  true,  true  },
            { false, false, false }
        }
        }, {
            { false, true, false },
            { false, true, false },
            { true,  true, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceJFactory factory = new PieceJFactory();
    protected final PieceFactory _factory = factory;

    public PieceJ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
