package model.piece;

import model.Coordinates;

public class PieceI extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, true, false, false },
            { false, true, false, false },
            { false, true, false, false },
            { false, true, false, false }
        }, {
            { false, false, false, false },
            { true,  true,  true,  true  },
            { false, false, false, false },
            { false, false, false, false }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceIFactory factory = new PieceIFactory();
    protected final PieceFactory _factory = PieceI.factory;

    public PieceI(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
