package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceZ extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { false, false, false },
            { true,  true,  false },
            { false, true,  true  }
        }, {
            { false, false, true },
            { false, true,  true },
            { false, true,  false }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceZFactory factory = new PieceZFactory();
    protected final PieceFactory _factory = factory;

    public static final Lazy<BufferedImage> tile = Piece.getTile("red.png");

    public PieceZ(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    public BufferedImage getTile() throws Exception
    {
        return PieceZ.tile.get();
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
