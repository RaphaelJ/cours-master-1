package model.piece;

import model.Coordinates;

public class PieceZ extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  false },
            { false, true,  true  }
        }, {
            { false, false, true },
            { false, true,  true },
            { false, true,  false }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceZFactory factory = new PieceZFactory();
    protected final PieceFactory _factory = factory;

    public PieceZ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }
}
