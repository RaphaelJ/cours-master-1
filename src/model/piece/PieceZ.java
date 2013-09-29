package model.piece;

public class PieceZ extends Piece {
    public class PieceZFactory extends Piece.PieceFactory {
        public int extent = 3;

        public Piece construct(int currentState)
        {
            return new PieceZ(currentState);
        }
    }

    // Shares the states between each instances of the Pieces.
    private static final states = { {
            { true,  true,  false },
            { false, true,  true  },
            { false, false, false }
        }, {
            { false, false, true },
            { false, true,  true },
            { false, true,  false }
        }, {
            { false, false, false },
            { true,  true,  false },
            { false, true,  true  }
        }, {
            { false, true,  false },
            { true,  true,  false },
            { true,  false, false }
        }
    };
    protected final Boolean[][][] _states = states;

    private static final PieceZFactory factory = new PieceZFactory();
    protected final PieceFactory _factory = factory;

    public PieceZ(int currentState)
    {
        super(currentState);
    }
}
