package model.piece;

import model.Coordinates;

public class PieceS extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false },
            { false, true,  true  },
            { true,  true,  false }
        }, {
            { false, true,  false },
            { false, true,  true  },
            { false, false, true  }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceSFactory factory = new PieceSFactory();
    protected final PieceFactory _factory = factory;

    public PieceS(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
