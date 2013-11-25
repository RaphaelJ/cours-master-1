package model.piece;

import model.Coordinates;

/** A piece which only use a single block. This is used to create new lines
 * which are added in multiplayer games by the opponent. */
public class PieceBlock extends Piece {
    public static final boolean[][][] states = { {
            { true }
        }
    };

    public static final PieceBlockFactory factory = new PieceBlockFactory();

    public PieceBlock(Coordinates topLeft, int currentState)
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
