package model.piece;

import model.Coordinates;

public class PieceZ extends Piece {

    public static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  false },
            { false, true,  true  }
        }, {
            { false, false, true },
            { false, true,  true },
            { false, true,  false }
        }
    };

    public static final PieceZFactory factory = new PieceZFactory();

    public PieceZ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    @Override
    public boolean[][][] getStates()
    {
        return states;
    }

    @Override
    public PieceFactory getFactory()
    {
        return factory;
    }
}
