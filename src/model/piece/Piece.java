package model.piece;

import model.Coordinates;

/** Contains the position and the state (orientation) of a piece.
 * The class is immutable to allow non destructive updates. That is, rotate
 * returns a new Piece object without updating the current one.
 * This is useful to check if a piece can be rotated or moved before applying
 * the transformation. */
public abstract class Piece {
    /** Provides a factory method to create new instances of a piece with
     * different orientations and positions. */
    public abstract PieceFactory {
        /** Extent of the square matrix which contains the state of the piece.
         */
        public final int extent;

        public Piece construct(int x, int y, int currentState);
    }

    /** Coordinates of the top-left corner of the piece's state matrix. */
    private Coordinates _topLeft;

    /** Contains the set of matrices for each orientations of the piece.
     * Each matrix contains booleans to indicate if the cell is occupied by the
     * piece or not. Each matrix must be a square matrix and they all must have
     * the same extent. */
    protected final Boolean[][][] _states;

    /** Cycles between each state when a rotation is applied to the piece. */
    private final int _currentState;

    /** Provides a factory to creates new instances of the current piece.
     * This is needed to create new pieces from the same type but at a different
     * position or orientation, as the methods can't update the current piece.
     */
    protected final Piece.PieceFactory _factory;

    protected Piece(Coordinates topLeft)
    {
        this(topLeft, 0);
    }

    protected Piece(Coordinates topLeft, int currentState)
    {
        this._topLeft = topLeft;
        this._currentState = currentState;
    }

    /** Performs a rotation of the piece by cycling to the next state.
     * Returns a new Piece to allow non-destructive rotations. */
    public Piece rotate()
    {
        return this._factory.construct(
            (this._currentState + 1) % this._states.length
        );
    }

    public Coordinates getTopLeft()
    {
        return this._topLeft;
    }

    /** Returns the boolean matrix with contains the current state of the
     * piece. */
    public Boolean[][] getCurrentState()
    {
        return this._states[this._currentState];
    }
}
