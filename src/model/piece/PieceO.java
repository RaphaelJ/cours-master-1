package model.piece;

import model.Coordinates;

public class PieceO extends Piece {
    public static final boolean[][][] states = { {
            { true, true },
            { true, true }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceOFactory factory = new PieceOFactory();

    public PieceO(Coordinates topLeft, int currentState)
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
