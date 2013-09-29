package model.piece;

public class PieceI extends Piece {
    public class PieceIFactory extends Piece.PieceFactory {
        public int extent = 4;

        public Piece construct(int currentState)
        {
            return new PieceI(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false, false },
            { true,  true,  true,  true  },
            { false, false, false, false },
            { false, false, false, false }
        }, {
            { false, false, true, false },
            { false, false, true, false },
            { false, false, true, false },
            { false, false, true, false }
        }, {
            { false, false, false, false },
            { false, false, false, false },
            { true,  true,  true,  true  },
            { false, false, false, false }
        }, {
            { false, true, false, false },
            { false, true, false, false },
            { false, true, false, false },
            { false, true, false, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceIFactory factory = new PieceIFactory();
    protected final PieceFactory _factory = factory;

    public PieceI(int currentState)
    {
        super(currentState);
    }
}
