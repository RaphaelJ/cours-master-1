package model.piece;

import model.Coordinates;

public class PieceL extends Piece {

	private static final long serialVersionUID = -5806121015981081465L;

	public static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  true  },
            { true,  false, false }
        }, {
            { false, true, false },
            { false, true, false },
            { false, true, true  }
        }, {
            { false, false, true  },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { true,  true, false },
            { false, true, false },
            { false, true, false }
        },
    };

    public static final PieceLFactory factory = new PieceLFactory();

    public PieceL(Coordinates topLeft, int currentState)
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
