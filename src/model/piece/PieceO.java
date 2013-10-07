package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceO extends Piece {
    public static final boolean[][][] states = { {
            { true, true },
            { true, true }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceOFactory factory = new PieceOFactory();

    public static final Lazy<BufferedImage> tile = Piece.getTile("yellow.png");

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
    public BufferedImage getTile() throws Exception
    {
        return tile.get();
    }

    @Override
    public PieceFactory getFactory()
    {
        return factory;
    }
}
