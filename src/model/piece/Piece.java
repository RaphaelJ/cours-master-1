package model.piece;

import java.awt.image.BufferedImage;
import java.io.File;
import java.util.concurrent.Callable;
import javax.imageio.ImageIO;

import model.Coordinates;
import util.Lazy;

/** Contains the position and the state (orientation) of a piece.
 * The class is immutable to allow non destructive updates. That is, rotate
 * returns a new Piece object without updating the current one.
 * This is useful to check if a piece can be rotated or moved before applying
 * the transformation. */
public abstract class Piece {
    public static final Piece.PieceFactory[] AVAILABLE_PIECES = {
        PieceI.factory, PieceJ.factory, PieceL.factory, PieceO.factory,
        PieceS.factory, PieceT.factory, PieceZ.factory
    };

    /** Size of images which will be used by graphical interfaces to paint
     * cells. */
    public static final int TILES_SIZE = 24;

    public static final File TILES_DIR = new File("resources");

    /** Provides a factory method to create new instances of a piece with
     * different orientations and positions. */
    public interface PieceFactory {
        /** Extent of the square matrix which contains the state of the piece.
         */
        public int getExtent();

        public Piece construct(Coordinates topLeft, int currentState);
    }

    /** Coordinates of the top-left corner of the piece's state matrix. */
    private final Coordinates _topLeft;

    /** Cycles between each state when a rotation is applied to the piece. */
    protected final int _currentState;

    protected Piece(Coordinates topLeft)
    {
        this(topLeft, 0);
    }

    protected Piece(Coordinates topLeft, int currentState)
    {
        this._topLeft = topLeft;
        this._currentState = currentState;
    }

    /** Returns the set of matrices for each orientations of the piece.
     * Each matrix contains booleans to indicate if the cell is occupied by the
     * piece or not. Each matrix must be a square matrix and they all must have
     * the same extent. */
    public abstract boolean[][][] getStates();

    /** Performs a rotation of the piece by cycling to the next state.
     * Returns a new Piece to allow non-destructive rotations. */
    public Piece rotate()
    {
        return this.getFactory().construct(
            this._topLeft, (this._currentState + 1) % this.getStates().length
        );
    }

    public Piece translate(int dX, int dY)
    {
        return this.getFactory().construct(
            new Coordinates(
                this._topLeft.getX() + dX, this._topLeft.getY() + dY
            ), this._currentState
        );
    }

    public Coordinates getTopLeft()
    {
        return this._topLeft;
    }

    /** Returns the boolean matrix with contains the current state of the
     * piece. */
    public boolean[][] getCurrentState()
    {
        return this.getStates()[this._currentState];
    }

    /** Returns the corresponding image of a cell of the piece. */
    public abstract BufferedImage getTile() throws Exception;

    /** Returns the tile corresponding to the given name.
     * Doesn't load the image until its first accessed as some interfaces
     * (CLI) don't use them. */
    public static Lazy<BufferedImage> getTile(final String name)
    {
        return new Lazy<BufferedImage>(
            new Callable<BufferedImage>() {
                public BufferedImage call() throws Exception
                {
                    return ImageIO.read(new File(TILES_DIR, name));
                }
            }
        );
    }

    /** Provides the factory which can be used to create new instances of the
     * current piece.
     * This is needed to create new pieces from the same type but at a different
     * position or orientation in this abstract class, as the methods can't
     * update the current piece.
     */
    public abstract PieceFactory getFactory();
}
