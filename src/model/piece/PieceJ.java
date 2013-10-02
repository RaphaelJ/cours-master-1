package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceJ extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
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
    protected final boolean[][][] _states = states;

    public static final PieceJFactory factory = new PieceJFactory();
    protected final PieceFactory _factory = factory;

    public static final Lazy<BufferedImage> tile = Piece.getTile("blue.png");

    public PieceJ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    public BufferedImage getTile() throws Exception
    {
        return PieceJ.tile.get();
    }
}
