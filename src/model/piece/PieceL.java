package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceL extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  true  },
            { true,  false, false }
        }, {
            { false, true, false },
            { false, true, false },
            { false, true, true  }
        }, {

            { true,  true, false },
            { false, true, false },
            { false, true, false }
        }, {
            { false, false, true  },
            { true,  true,  true  },
            { false, false, false }
        }, 
    };
    protected final boolean[][][] _states = states;

    public static final PieceLFactory factory = new PieceLFactory();
    protected final PieceFactory _factory = factory;

    public static final Lazy<BufferedImage> tile = Piece.getTile("gray.png");

    public PieceL(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    public BufferedImage getTile() throws Exception
    {
        return PieceL.tile.get();
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
