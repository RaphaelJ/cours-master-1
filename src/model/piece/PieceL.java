package model.piece;

import model.Coordinates;

public class PieceL extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
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
    protected final boolean[][][] _states = states;

    public static final PieceLFactory factory = new PieceLFactory();
    protected final PieceFactory _factory = factory;

    public PieceL(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
