package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceT extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  true  },
            { false, true,  false }
        }, {
            { false, true, false },
            { false, true, true  },
            { false, true, false }
        }, {
            { false, true,  false },
            { true,  true,  true  },
            { false, false, false }
        }, {
            { false, true, false },
            { true,  true, false },
            { false, true, false }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceTFactory factory = new PieceTFactory();
    protected final PieceFactory _factory = factory;

    public static final Lazy<BufferedImage> tile = Piece.getTile("purple.png");

    public PieceT(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    public BufferedImage getTile() throws Exception
    {
        return PieceT.tile.get();
    }
}
