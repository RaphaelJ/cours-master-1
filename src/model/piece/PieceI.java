package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceI extends Piece {
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

    public static final Lazy<BufferedImage> tile = Piece.getTile("cyan.png");

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
