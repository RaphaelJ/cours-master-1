package model.piece;

import model.Coordinates;

public class PieceJ extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
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
        }, {
            { false, true, false },
            { false, true, false },
            { true,  true, false }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceJFactory factory = new PieceJFactory();
    protected final PieceFactory _factory = factory;

    public PieceJ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
