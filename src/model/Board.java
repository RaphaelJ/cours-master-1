package model;

import java.awt.Rectangle;
import java.util.ArrayList;

import model.piece.Piece;
import util.random.LCGRandom;
import util.random.Random;
import view.GameView;

/** Saves the current status of the board and communicates with views to share
 * changes and game events with the user. */
public class Board {

    public enum GameState {
          INITIALIZED // The board is empty and the timer hasn't been started.
                      // This is the initial state when the Board is instanced.
        , RUNNING     // The timer and the game are running.
        , PAUSED      // The game is running but the timer has been stopped.
        , GAMEOVER    // The game as been finished. The board need to be
                      // reinitialised before being started.
    }

    public static final int DEFAULT_WIDTH  = 10;
    public static final int DEFAULT_HEIGHT = 22;

    private final Random _rand;


    private final int _width;
    private final int _height;

    /** Contains a matrix which maps each cell of the board to the piece which
     * is there, if any.
     * The first row is the top-most row of the board.
     * Complete rows can be easily removed from the grid by applying a shift
     * of references. */
    private final Row[] _grid;

    private GameState _currentState;

    private Piece _current = null;
    private Piece _next = null;

    private ArrayList<GameView> _views = new ArrayList<GameView>();

    private long _startTime;

    public Board()
    {
        this._rand = new LCGRandom();

        this._width = DEFAULT_WIDTH;
        this._height = DEFAULT_HEIGHT;

        this._grid = new Row[this._height];
        this.initBoard();
    }

    /** Initializes an empty board with a specified seed for the random
     * generator. Using a common seed for two Board instances ensures that
     * pieces will come in the same order. i.e. can avoid some synchronization
     * between two remote processes. */
    public Board(Random rand)
    {
        this(rand, DEFAULT_WIDTH, DEFAULT_HEIGHT);
    }

    public Board(Random rand, int width, int height)
    {
        this._rand = rand;

        this._width = width;
        this._height = height;

        this._grid = new Row[this._height];
        this.initBoard();
    }

    public void addView(GameView view)
    {
        this._views.add(view);
    }

    /*********************** User actions ***********************/

    /** Starts the timer which controls the game.
     * Resets the game if needed. */
    public synchronized void newGame()
    {
        if (this._currentState != GameState.INITIALIZED)
            this.reset();
        
        this._startTime = System.currentTimeMillis();

        this.changeState(GameState.RUNNING);
        this.gameTick();
    }

    /** Reinitialises the grid and stops the game if needed. */
    public synchronized void reset()
    {
        this.initBoard();

        this.emitGridChange(new Rectangle(0, 0, this._width, this._height));
        this.changeState(GameState.INITIALIZED);
    }

    /** Runs one step of the game: moves the current piece.
     * Usually called by the timer. */
    public synchronized void gameTick()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        if (this._current == null) // First piece.
            this._current = this.nextPiece();
        else { // Moves the piece downward.
            Piece newPiece = this._current.translate(0, 1);

            if (this.pieceCollide(newPiece, this._current)) {
                if (!this._current.isFullyIntroduced())
                    this._current = null;
                else { // Introduces a new piece.
                    this.clearLines();
                    this._current = this.nextPiece();
                }
            } else {
                this.removePiece(this._current);
                this._current = newPiece;
                this.placePiece(this._current);
            }
        }
    }

    public void moveLeft()
    {
        this.moveCurrentPiece(-1, 0);
    }

    public void moveRight()
    {
        this.moveCurrentPiece(1, 0);
    }

    /** Push the piece one line down. */
    public synchronized void softDrop()
    {
        this.moveCurrentPiece(0, 1);
    }

    /** Push the piece down to the last free line. */
    public synchronized void hardDrop()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece finalPiece = this._current;
        Piece movedPiece = this._current;

        do {
            finalPiece = movedPiece;
            movedPiece = movedPiece.translate(0, 1);
        } while (!this.pieceCollide(movedPiece, this._current));

        if (finalPiece != this._current) {
            this.removePiece(this._current);
            this._current = finalPiece;
            this.placePiece(this._current);
        }
    }

    /** Tries to rotate the piece. Does nothing if a collision occurs. */
    public synchronized void rotate()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece rotatedPiece = this._current.rotate();

        if (this.pieceCollide(rotatedPiece, this._current))
            return;

        this.removePiece(this._current);
        this.placePiece(rotatedPiece);

        this._current = rotatedPiece;
    }

    /*********************** Internals ***********************/

    /** Fills the board with empty lines. */
    private synchronized void initBoard()
    {
        for (int i = 0; i < this._height; i++)
            this._grid[i] = new Row(this._width);

        this._currentState = GameState.INITIALIZED;

        this._current = null;
        this._next = this.getRandomPiece();
    }

    /** Choose the next random piece without placing it on the grid. */
    private synchronized Piece getRandomPiece()
    {
        Piece.PieceFactory factory = Piece.AVAILABLE_PIECES[
            this._rand.nextInt(Piece.AVAILABLE_PIECES.length)
        ];

        Coordinates coords = new Coordinates(
            (this._width - factory.getExtent()) / 2, 1 - factory.getExtent()
        );

        return factory.construct(coords, 0);
    }

    public synchronized void changeState(GameState newState)
    {
        this._currentState = newState;
        this.emitStateChange(newState);
    }

    /** Returns the next random piece and places it at the top of the grid.
     * Returns null if the piece can't be placed in the grid. */
    private synchronized Piece nextPiece()
    {
        Piece piece = this._next;
        this._next = getRandomPiece();

        // Checks if the last line is blocked by some piece.
        Coordinates topLeft = piece.getTopLeft();
        boolean[][] state = piece.getCurrentState();
        boolean[] line = state[state.length - 1];

        for (int j = 0; j < line.length; j++) {
            Piece cell = this._grid[0].getPiece(j + topLeft.getX());
            if (line[j] && cell != null)
                return null;
        }

        this.placePiece(piece);

        this.emitNewPiece(this._next);

        return piece;
    }

    /** Moves the current piece by the given offset.
     * Does nothing if an collision occurs. */
    private synchronized void moveCurrentPiece(int dX, int dY)
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece newPiece = this._current.translate(dX, dY);

        if (this.pieceCollide(newPiece, this._current))
            return;

        this.removePiece(this._current);
        this._current = newPiece;
        this.placePiece(this._current);

    }

    /** Returns true if the piece collide with the left/right/bottom border or
     * with another piece. Ignore oldPiece collisions. */
    private synchronized boolean pieceCollide(Piece newPiece, Piece oldPiece)
    {
        Coordinates topLeft = newPiece.getTopLeft();
        boolean[][] state = newPiece.getCurrentState();

        // Checks if the new piece overlap another piece or touch a border.
        int topX = topLeft.getX(), topY = topLeft.getY();
        for (int i = 0; i < state.length; i++) {
            boolean[] line = state[i];
            int y = topY + i;

            for (int j = 0; j < line.length; j++) {
                if (line[j]) {
                    // Checks the left, right and bottom border.
                    int x = topX + j;
                    if (x < 0 || x >= this._width || y >= this._height)
                        return true;

                    // Checks if the cell is free, for cells which have been
                    // inserted inside the board.
                    if (y >= 0) {
                        Piece cell = this._grid[y].getPiece(x);
                        if (cell != null && cell != oldPiece)
                            return true;
                    }
                }
            }
        }

        return false;
    }

    /** Places a new piece on the grid. Doesn't check for collisions. */
    private synchronized void placePiece(Piece piece)
    {
        int topX = piece.getTopLeft().getX()
          , topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        // Only draws coordinates of the piece which are inside the grid.
        int minX = topX < 0 ? -topX : 0
          , maxX = Math.min(this._width, topX + state.length) - topX
          , minY = topY < 0 ? -topY : 0
          , maxY = Math.min(this._height, topY + state.length) - topY;
        for (int i = minY; i < maxY; i++) {
            boolean[] line = state[i];

            for (int j = minX; j < maxX; j++) {
                if (line[j])
                    this._grid[topY + i].setPiece(piece, topX + j);
            }
        }

        this.emitGridChange(
            new Rectangle(
                topX + minX, topY + minY, maxX, maxY
            )
        );
    }

    /** Removes a piece from the grid. */
    private synchronized void removePiece(Piece piece)
    {
        int topX = piece.getTopLeft().getX()
          , topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        // Only clean coordinates of the piece which are inside the grid.
        int minX = topX < 0 ? -topX : 0
          , maxX = Math.min(this._width, topX + state.length) - topX
          , minY = topY < 0 ? -topY : 0
          , maxY = Math.min(this._height, topY + state.length) - topY;
        for (int i = minY; i < maxY; i++) {
            boolean[] line = state[i];

            for (int j = minX; j < maxX; j++) {
                if (line[j])
                    this._grid[topY + i].setPiece(null, topX + j);
            }
        }

        this.emitGridChange(
            new Rectangle(
                topX + minX, topY + minY, maxX, maxY
            )
        );
    }

    /** Checks every line where the current piece is to be cleared.
     * Emits the clear lines event when some lines have been removed. */
    private synchronized void clearLines()
    {
        int topX = this._current.getTopLeft().getX()
          , topY = this._current.getTopLeft().getY();

        boolean[][] state = this._current.getCurrentState();

        int nLines = 0;
        int lastRemoved = 0;

        // Only clean coordinates of the piece which are inside the grid.
        int minX = topX < 0 ? -topX : 0
          , maxX = Math.min(this._width, topX + state.length) - topX
          , minY = topY < 0 ? -topY : 0
          , maxY = Math.min(this._height, topY + state.length) - topY;
        for (int i = minY; i < maxY; i++) {
            boolean[] line = state[i];

            // Checks each line where the piece occupes a cell.
            for (int j = minX; j < maxX; j++) {
                if (line[j]) {
                    if (this._grid[i + topY].isComplete()) {
                        lastRemoved = i + topY;

                        // Shifts line references to the bottom and adds a new
                        // line at the top of the grid.
                        for (int k = i + topY; k > 0; k--)
                            this._grid[k] = this._grid[k-1];

                        this._grid[0] = new Row(this._width);
                        nLines++;
                    }

                    break;
                }
            }
        }

        if (nLines > 0) {
            this.emitClearedLines(nLines);
            this.emitGridChange(
                new Rectangle(0, 0, this._width, lastRemoved + 1)
            );
        }
    }

    /*********************** Events ***********************/

    private void emitStateChange(GameState newState)
    {
        for (GameView view : _views)
            view.stateChange(newState);
    }

    private void emitGridChange(Rectangle bounds)
    {
        for (GameView view : _views)
            view.gridChange(bounds);
    }

    private void emitClearedLines(int n)
    {
        for (GameView view : _views)
            view.clearedLines(n);
    }

    private void emitNewPiece(Piece piece)
    {
        for (GameView view : _views)
            view.newPiece(piece);
    }

    /*********************** Getters/Setters ***********************/

    public int getWidth()
    {
        return this._width;
    }

    public int getHeight()
    {
        return this._height;
    }

    public Row[] getGrid()
    {
        return this._grid;
    }

    public GameState getCurrentState()
    {
        return this._currentState;
    }
    
    public Piece getCurrentPiece()
    {
    	return this._current;
    }

    public Piece getNextPiece()
    {
        return this._next;
    }

    public int getElapsedTimeInSeconds() {
    	return (int) ((System.currentTimeMillis() - this._startTime) / 1000);
    }
}
