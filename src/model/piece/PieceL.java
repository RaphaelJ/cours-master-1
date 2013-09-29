package model.piece;

public class PieceL extends Piece {
    public class PieceLFactory extends Piece.PieceFactory {
        public int extent = 3;

        public Piece construct(int currentState)
        {
            return new PieceL(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, false, true  },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { false, true, false },
            { false, true, false },
            { false, true, true  }
        }, {
            { false, false, false },
            { true,  true,  true  },
            { true,  false, false }
        }, {
            { true,  true, false },
            { false, true, false },
            { false, true, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceLFactory factory = new PieceLFactory();
    protected final PieceFactory _factory = factory;

    public PieceL(int currentState)
    {
        super(currentState);
    }
}
