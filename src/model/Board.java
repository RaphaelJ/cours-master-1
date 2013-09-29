package model;

/** Saves the current status of the board and communicates with views to share
 * changes and game events with the user. */
public class Board {
    /** Provides an interface to listen to game's board changes. */
    public interface BoardListener {
        /** Event triggered when a piece move in the grid. */
        public void gameChange();

        /** Event triggered when n lines have been removed by the player. */
        public void newClearedLines(int n);

        public void gameOver();
    }

    public static final int DEFAULT_WIDTH   = 10;
    public static final int DEFAULT_HEIGHT  = 22;

    private final Random _rand;

    private final int _width;
    private final int _height;

    /** Contains a matrix which maps each cell of the board to the piece which
     * is there, if any.
     * The first cell of the grid is the top-left point of the board. */
    private final Piece[][] _grid;

    private Piece _current = null;

    private ArrayList<> _listeners = new ArrayList<BoardListener>();

    public Board();
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

    public Board(int seed, int width, int height)
    {
        this._rand = new Random(seed);

        this._width = width;
        this._height = height;

        this._grid = new Piece[height][width];
    }

    /** Returns the next random piece and places it at the top of the grid.
     * Returns null if the piece can't be placed in the grid (game over). */
    private Piece nextPiece()
    {
        int factory = Piece.AVAILABLE_PIECES[
            this._rand.nextInt(Piece.AVAILABLE_PIECES.length)
        ];

        // Puts the last line of the piece on the top-most line of the board.
        Piece piece = factory.construct(
            (this._width - factory.getExtent()) / 2, 1 - factory.getExtent(), 0
        );

        // Checks if the last line is blocked by some piece.
        Coordinates topLeft = piece.getTopLeft();
        boolean[][] state = piece.getCurrentState();
        boolean line = state[state.length - 1];

        for (int j = 0; j < line.length; j++) {
            Piece cell = this._grid[0][j + topLeft.getX()];
            if (line[j] && cell != null)
                return null;
        }

        return piece;
    }

    /** Returns true if the piece doesn't overlap with another piece of the
     * board. */
    private Piece movePiece(Piece piece)
    {
        Piece newPiece = piece.translate(0, 1);

        Coordinates topLeft = newPiece.getTopLeft();
        boolean[][] state = newPiece.getCurrentState();

        // Only checks coordinates of the piece which are inside the grid.
        int i = topLeft.getY() < 0 ? -topLeft.getY() : 0;

        for (; i < state.length; i++) {
            boolean line = state[i];

            for (int j = 0; j < line.length; j++) {
                Piece cell = this._grid[i + topLeft.getY()][j + topLeft.getX()];
                if (line[j] && cell != null && cell != piece)
                    return null;
            }
        }

        return newPiece;
    }

    public void addListener(BoardListener listener)
    {
        this._listeners.add(listener);
    }

    /** Runs one step of the game: moves the current piece. */
    public void gameTick()
    {
        if (this._current) { // Needs to introduce a new piece in the grid.
            Piece piece = this.nextPiece();

            if (piece == null) {
                for (BoardListener listener : this._listeners)
                    listener.gameOver();
            } else {

            }
        } else { // Moves the piece downward.
            Piece piece = this.movePiece();

            if (piece == null) {
                for (BoardListener listener : this._listeners)
                    listener.gameOver();
            } else {

            }
        }
    }

    public int getWidth()
    {
        return this._width;
    }

    public int getHeight()
    {
        return this._height;
    }
}
