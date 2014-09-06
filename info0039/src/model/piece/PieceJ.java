package model.piece;

import model.Coordinates;

public class PieceJ extends Piece {

    public static final boolean[][][] states = { {
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

    public static final PieceJFactory factory = new PieceJFactory();

    public PieceJ(Coordinates topLeft, int currentState)
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
