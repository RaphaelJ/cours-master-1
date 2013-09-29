package model.piece;

import model.Coordinates;

/** Contains the position and the state (orientation) of a piece.
 * The class is immutable to allow non destructive updates. That is, rotate
 * returns a new Piece object without updating the current one.
 * This is useful to check if a piece can be rotated or moved before applying
 * the transformation. */
public abstract class Piece {
    public static final Piece.PieceFactory[] AVAILABLE_PIECES = {
        new PieceI.PieceIFactory(), new PieceJ.PieceJFactory(),
        new PieceL.PieceLFactory(), new PieceO.PieceOFactory(),
        new PieceS.PieceSFactory(), new PieceT.PieceTFactory(),
        new PieceZ.PieceZFactory()
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

    /** Contains the set of matrices for each orientations of the piece.
     * Each matrix contains booleans to indicate if the cell is occupied by the
     * piece or not. Each matrix must be a square matrix and they all must have
     * the same extent. */
    protected final Boolean[][][] _states;

    /** Cycles between each state when a rotation is applied to the piece. */
    private final int _currentState;

    /** Provides the factory which can be used to create new instances of the
     * current piece.
     * This is needed to create new pieces from the same type but at a different
     * position or orientation in this abstract class, as the methods can't
     * update the current piece.
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
            this._topLeft, (this._currentState + 1) % this._states.length
        );
    }

    public Piece translate(int dX, int dY)
    {
        return this._factory.construct(
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
    public Boolean[][] getCurrentState()
    {
        return this._states[this._currentState];
    }
}
