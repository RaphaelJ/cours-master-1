package model.piece;

import model.Coordinates;

public class PieceT extends Piece {
    public static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  true  },
            { false, true,  false }
        }, {
            { false, true, false },
            { false, true, true  },
            { false, true, false }
        }, {
            { false, true,  false },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { false, true, false },
            { true,  true, false },
            { false, true, false }
        }
    };

    public static final PieceTFactory factory = new PieceTFactory();

    public PieceT(Coordinates topLeft, int currentState)
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
