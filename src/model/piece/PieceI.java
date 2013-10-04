package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceI extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
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

    public BufferedImage getTile() throws Exception
    {
        return PieceI.tile.get();
    }

	@Override
	public boolean[][] getCurrentState() {
		
        return states[this._currentState];
	}

	@Override
	public PieceFactory getFactory() {
		return factory;
	}
}
