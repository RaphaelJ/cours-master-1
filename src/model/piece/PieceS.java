package model.piece;

import model.Coordinates;

public class PieceS extends Piece {

	private static final long serialVersionUID = -6985133238593306441L;

	public static final boolean[][][] states = { {
            { false, false, false },
            { false, true,  true  },
            { true,  true,  false }
        }, {
            { false, true,  false },
            { false, true,  true  },
            { false, false, true  }
        }
    };

    public static final PieceSFactory factory = new PieceSFactory();

    public PieceS(Coordinates topLeft, int currentState)
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
