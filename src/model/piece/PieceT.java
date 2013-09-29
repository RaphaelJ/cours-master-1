package model.piece;

public class PieceT extends Piece {
    public class PieceTFactory extends Piece.PieceFactory {
        public int extent = 3;

        public Piece construct(int currentState)
        {
            return new PieceT(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, true,  false },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { false, true, false },
            { false, true, true  },
            { false, true, false }
        }, {
            { false, false, false },
            { true,  true,  true  },
            { false, true,  false }
        }, {
            { false, true, false },
            { true,  true, false },
            { false, true, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceTFactory factory = new PieceTFactory();
    protected final PieceFactory _factory = factory;

    public PieceT(int currentState)
    {
        super(currentState);
    }
}
