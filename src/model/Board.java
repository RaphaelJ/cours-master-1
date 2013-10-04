package model;

import java.util.*;

import model.piece.Piece;
import view.GameView;

/** Saves the current status of the board and communicates with views to share
 * changes and game events with the user. */
public class Board {

    public static final int DEFAULT_WIDTH  = 10;
    public static final int DEFAULT_HEIGHT = 22;

    private final Random _rand;

    private final int _width;
    private final int _height;

    /** Contains a matrix which maps each cell of the board to the piece which
     * is there, if any.
     * The first cell of the grid is the top-left point of the board. */
    private final Piece[][] _grid;

    private Piece _current = null;

    private ArrayList<GameView> _views = new ArrayList<GameView>();

    public Board()
    {
        this._rand = new Random();

        this._width = DEFAULT_WIDTH;
        this._height = DEFAULT_HEIGHT;

        this._grid = new Piece[this._height][this._width];
    }

    /** Initializes an empty board with a specified seed for the random
     * generator. Using a common seed for two Board instances ensures that
     * pieces will come in the same order. i.e. can avoid some synchronization
     * between two remote processes. */
    public Board(long seed)
    {
        this(seed, DEFAULT_WIDTH, DEFAULT_HEIGHT);
    }

    public Board(long seed, int width, int height)
    {
        this._rand = new Random(seed);

        this._width = width;
        this._height = height;

        this._grid = new Piece[height][width];
    }

    /** Removes every pieces from the grid and emits the reset event. */
    public void resetBoard()
    {
        for (Piece[] line : this._grid)
                Arrays.fill(line, null);

        this._current = null;

        for (GameView view : this._views)
            view.reset();
    }

    public void addView(GameView view)
    {
        this._views.add(view);
    }

    /** Runs one step of the game: moves the current piece.
      * Returns the current piece or null if the game is over. */
    public Piece gameTick()
    {
        if (this._current == null) // First piece.
            this._current = this.nextPiece();
        else { // Moves the piece downward.
            this._current = this.movePiece(this._current);

            if (this._current == null) // Introduces a new piece.
                this._current = this.nextPiece();
        }

        return this._current;
    }

    /** Places a piece on the grid and emits the grid change event. */
    private void placesPiece(Piece piece)
    {
    }

    /** Returns the next random piece and places it at the top of the grid.
     * Returns null and emits a game over event if the piece can't be placed in
     * the grid. */
    private Piece nextPiece()
    {
        Piece.PieceFactory factory = Piece.AVAILABLE_PIECES[
            this._rand.nextInt(Piece.AVAILABLE_PIECES.length)
        ];

        // Puts the last line of the piece on the top-most line of the board.
        Coordinates coords = new Coordinates(
            (this._width - factory.getExtent()) / 2, 1 - factory.getExtent()
        );
        Piece piece = factory.construct(coords , 0);

        // Checks if the last line is blocked by some piece.
        Coordinates topLeft = piece.getTopLeft();
        boolean[][] state = piece.getCurrentState();
        boolean[] line = state[state.length - 1];

        for (int j = 0; j < line.length; j++) {
        	Piece cell = this._grid[0][j + topLeft.getX()];
            if (line[j] && cell != null) {
                for (BoardListener listener : this._listeners)
                    listener.gameOver();
                return null;
            }
        }

        return piece;
    }

    /** Returns the new translated piece if it doesn't overlap with another
     * piece or if it is out of the board.
     * Returns null if an overlap occurs. */
    private Piece movePiece(Piece piece)
    {
        Coordinates topLeft = piece.getTopLeft();
        boolean[][] state = piece.getCurrentState();

        Piece newPiece = piece.translate(0, 1);
        Coordinates newTopLeft = newPiece.getTopLeft();
        boolean[][] newState = newPiece.getCurrentState();

        // Checks if it hits the bottom of the grid.
        if (newTopLeft.getY() + newState.length > this._height)
            return null;

        // Checks if the new piece overlap another piece.
        // Only checks coordinates of the piece which are inside the grid.
        int i = newTopLeft.getY() < 0 ? -newTopLeft.getY() : 0;
        for (; i < newState.length; i++) {
            boolean[] line = newState[i];

            for (int j = 0; j < line.length; j++) {
                Piece cell = this._grid[i + newTopLeft.getY()]
                                       [j + newTopLeft.getX()];
                if (line[j] && cell != null && cell != piece)
                    return null;
            }
        }

        // Removes the old piece from the grid.
        i = topLeft.getY() < 0 ? -topLeft.getY() : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];

            for (int j = 0; j < line.length; j++) {
                if (line[j])
                    this._grid[i + topLeft.getY()][j + topLeft.getX()] = null;
            }
        }

        // Places the new piece on the grid.
        i = newTopLeft.getY() < 0 ? -newTopLeft.getY() : 0;
        for (; i < newState.length; i++) {
            boolean[] line = newState[i];
	
            for (int j = 0; j < line.length; j++) {
                if (line[j]) {
                    this._grid[i + newTopLeft.getY()][j + newTopLeft.getX()]
                        = newPiece;
                }
            }
        }

        return newPiece;
    }

    public int getWidth()
    {
        return this._width;
    }

    public int getHeight()
    {
        return this._height;
    }

    public Piece[][] getGrid()
    {
        return this._grid;
    }
}
