package model.piece;

import java.io.Serializable;

import model.Coordinates;

/** Contains the position and the state (orientation) of a piece.
 * The class is immutable to allow non destructive updates. That is, rotate
 * returns a new Piece object without updating the current one.
 * This is useful to check if a piece can be rotated or moved before applying
 * the transformation. */
public abstract class Piece implements Serializable {

    public static final Piece.PieceFactory[] AVAILABLE_PIECES = {
        PieceI.factory, PieceJ.factory, PieceL.factory, PieceO.factory,
        PieceS.factory, PieceT.factory, PieceZ.factory
    };

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

    public int numStates()
    {
        return this.getStates().length;
    }

    /** Performs a rotation of the piece by cycling to the next state.
     * Returns a new Piece to allow non-destructive rotations. */
    public Piece rotate()
    {
        return this.getFactory().construct(
            this._topLeft, (this._currentState + 1) % this.numStates()
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

    /** Returns true if the piece has been completely introduced in the grid
     * (i.e. no block is above the first line of the board).
     * This is used to check for game over when the piece is blocked. */
    public boolean isFullyIntroduced()
    {
        for (int i = 0; i < -this._topLeft.getY(); i++) {
            boolean[] line = this.getCurrentState()[i];

            for (int j = 0; j < line.length; j++) {
                if (line[j])
                    return false;
            }
        }

        return true;
    }

    /** Provides the factory which can be used to create new instances of the
     * current piece.
     * This is needed to create new pieces from the same type but at a different
     * position or orientation in this abstract class, as the methods can't
     * update the current piece.
     */
    public abstract PieceFactory getFactory();
}
