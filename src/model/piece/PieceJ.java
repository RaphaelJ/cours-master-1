package model.piece;

public class PieceJ extends Piece {
    public class PieceJFactory extends Piece.PieceFactory {
        public int extent = 3;

        public Piece construct(int currentState)
        {
            return new PieceJ(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { true,  false, false },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { false, true, true  },
            { false, true, false },
            { false, true, false }
        }, {
            { false, false, false },
            { true,  true,  true  },
            { false, false, true }
        }, {
            { false, true, false },
            { false, true, false },
            { true,  true, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceJFactory factory = new PieceJFactory();
    protected final PieceFactory _factory = factory;

    public PieceJ(int currentState)
    {
        super(currentState);
    }
}
