package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceS extends Piece {
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

    public static final Lazy<BufferedImage> tile = Piece.getTile("green.png");

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
