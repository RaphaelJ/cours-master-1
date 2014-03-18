package model.piece;

import model.Coordinates;

public class PieceZ extends Piece {

	private static final long serialVersionUID = -4504181434220476284L;

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
