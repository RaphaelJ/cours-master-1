package model.piece;


import model.Coordinates;

public class PieceI extends Piece {

	private static final long serialVersionUID = -2378608939503131589L;

	public static final boolean[][][] states = { {
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

    public static final PieceIFactory factory = new PieceIFactory();

    public PieceI(Coordinates topLeft, int currentState)
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
