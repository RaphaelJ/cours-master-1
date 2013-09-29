package model.piece;

public class PieceS extends Piece {
    public class PieceSFactory extends Piece.PieceFactory {
        public int extent = 3;

        public Piece construct(int currentState)
        {
            return new PieceS(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { false, true,  true  },
            { true,  true,  false },
            { false, false, false }
        }, {
            { false, true,  false },
            { false, true,  true  },
            { false, false, true  }
        }, {
            { false, false, false },
            { false, true,  true  },
            { true,  true,  false }
        }, {
            { true,  false, false },
            { true,  true,  false },
            { false, true,  false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceSFactory factory = new PieceSFactory();
    protected final PieceFactory _factory = factory;

    public PieceS(int currentState)
    {
        super(currentState);
    }
}
