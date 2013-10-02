package model.piece;

import java.awt.image.BufferedImage;

import model.Coordinates;
import util.Lazy;

public class PieceO extends Piece {
    // Shares the states between each instances of the Pieces.
    private static final boolean[][][] states = { {
            { true, true },
            { true, true }
        }
    };
    protected final boolean[][][] _states = states;

    public static final PieceOFactory factory = new PieceOFactory();
    protected final PieceFactory _factory = factory;

    public static final Lazy<BufferedImage> tile = Piece.getTile("yellow.png");

    public PieceO(Coordinates topLeft, int currentState)
    {
        super(topLeft, currentState);
    }

    public BufferedImage getTile() throws Exception
    {
        return PieceO.tile.get();
    }
}
