package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceS extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false },
            { false, true,  true  },
            { true,  true,  false }
        }, {
            { false, true,  false },
            { false, true,  true  },
            { false, false, true  }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceSFactory factory = new PieceSFactory();
    protected final PieceFactory _factory = factory;

    public static final Lazy<BufferedImage> tile = Piece.getTile("green.png");

    public PieceS(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    public BufferedImage getTile() throws Exception
    {
        return PieceS.tile.get();
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
