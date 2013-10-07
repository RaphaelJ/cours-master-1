package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceJ extends Piece {
    public static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  true  },
            { false, false, true }
        }, {
            { false, true, true  },
            { false, true, false },
            { false, true, false }
        }, {
            { true,  false, false },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { false, true, false },
            { false, true, false },
            { true,  true, false }
        }
    };

    public static final PieceJFactory factory = new PieceJFactory();

    public static final Lazy<BufferedImage> tile = Piece.getTile("blue.png");

    public PieceJ(Coordinates topLeft, int currentState)
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
