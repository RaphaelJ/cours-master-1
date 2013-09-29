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

    public static final Piece.PieceFactory[] AVAILABLE_PIECES = {
        new PieceI.PieceIFactory(), new PieceJ.PieceJFactory(),
        new PieceL.PieceLFactory(), new PieceO.PieceOFactory(),
        new PieceS.PieceSFactory(), new PieceT.PieceTFactory(),
        new PieceZ.PieceZFactory()
    };

    private final Random _rand;

    private final int _width;
    private final int _height;

    /** Contains a matrix which maps each cell of the board to the piece which
     * is there, if any. */
    private final Piece[][] _grid;

    private Piece _current;

    private ArrayList<> _listeners = new ArrayList<BoardListener>();

    public Board();
    {
        this._rand = new Random();

        this._width = DEFAULT_WIDTH;
        this._height = DEFAULT_HEIGHT;

        this._grid = new Piece[this._height][this._width];

        this._current = this.nextPiece();
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

        this._current = this.nextPiece();
    }

    /** Returns the next random piece and places it at the top of the grid. */
    private Piece nextPiece()
    {
        int factory = AVAILABLE_PIECES[
            this._rand.nextInt(AVAILABLE_PIECES.length)
        ];

        return factory.construct((this._width - factory.extent) / 2, 0, 0);
    }

    /** Returns true if the piece doesn't overlap with another piece of the
     * board. */
    private boolean checkPiece(Piece piece)
    {
        Coordinates topLeft = piece.getTopLeft();
        boolean[][] state = piece.getCurrentState();

        for (int i = 0; i < state.length; i++) {
            boolean line = state[i];

            for (int j = 0; j < line.length; j++) {
                if (line[j] && this._grid[i + topLeft.y][j + topLeft.x] != null)
                    return false;
            }
        }

        return true;
    }

    public void addBoardListener(BoardListener listener)
    {
        this._listeners.add(listener);
    }

    /** Runs one step of the game: moves the current piece. */
    public void gameTick()
    {
        
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
