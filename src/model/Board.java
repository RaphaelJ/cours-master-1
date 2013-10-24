package model;

import java.util.ArrayList;
import java.util.Timer;
import java.util.TimerTask;

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

    /** "Ticks" duration in milliseconds. */
    public static final int DEFAULT_SPEED = 1000;

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

    private Timer _timer;
    private int _clockSpeed;

    public Board()
    {
        this._rand = new LCGRandom();

        this._width = DEFAULT_WIDTH;
        this._height = DEFAULT_HEIGHT;

        this._clockSpeed = DEFAULT_SPEED;

        this._grid = new Row[this._height];
        this.initBoard();
    }

    /** Initializes an empty board with a specified seed for the random
     * generator. Using a common seed for two Board instances ensures that
     * pieces will come in the same order. i.e. can avoid some synchronization
     * between two remote processes. */
    public Board(Random rand)
    {
        this(rand, DEFAULT_WIDTH, DEFAULT_HEIGHT, DEFAULT_SPEED);
    }

    public Board(Random rand, int width, int height, int clockSpeed)
    {
        this._rand = rand;

        this._width = width;
        this._height = height;

        this._clockSpeed = clockSpeed;

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
    public void newGame()
    {
        if (this._currentState != GameState.INITIALIZED)
            this.reset();

        this.startTimer();

        this.changeState(GameState.RUNNING);
    }

    /** Pauses/Unpauses the timer if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause()
    {
        switch (this._currentState) {
        case RUNNING:
            this._timer.cancel();
            this.changeState(GameState.PAUSED);
            break;
        case PAUSED:
            this.startTimer();
            this.changeState(GameState.RUNNING);
            break;
        case INITIALIZED:
        case GAMEOVER:
        	break;
        }
    }

    /** Reinitialises the grid and stops the game if needed. */
    public void reset()
    {
        if (this._currentState == GameState.RUNNING)
            this._timer.cancel();

        this.initBoard();

        this.emitGridChange();
        this.changeState(GameState.INITIALIZED);
    }

    /** Runs one step of the game: moves the current piece.
     * Usually called by the timer. */
    public void gameTick()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        if (this._current == null) // First piece.
            this._current = this.nextPiece();
        else { // Moves the piece downward.
            Piece newPiece = this.movePiece(this._current, 0, 1);

            if (newPiece == null) { // Piece blocked.
                if (!this._current.isFullyIntroduced())
                    this._current = null;
                else // Introduces a new piece.
                    this._current = this.nextPiece();
            } else
                this._current = newPiece;
        }

        if (this._current != null)
            this.emitGridChange();
        else
            this.gameOver();
    }

    public void moveLeft()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece movedPiece = this.movePiece(this._current, -1, 0);

        if (movedPiece != null) {
            this._current = movedPiece;
            this.emitGridChange();
        }
    }

    public void moveRight()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece movedPiece = this.movePiece(this._current, 1, 0);

        if (movedPiece != null) {
            this._current = movedPiece;
            this.emitGridChange();
        }
    }

    /** Push the piece one line down. */
    public void softDrop()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece movedPiece = this.movePiece(this._current, 0, 1);

        if (movedPiece != null) {
            this._current = movedPiece;
            this.emitGridChange();
        }
    }

    /** Push the piece down to the last free line. */
    public void hardDrop() 
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece finalPiece = this._current;
        Piece movedPiece = this._current;

        do {
            movedPiece = this.movePiece(movedPiece, 0, 1);

            if (movedPiece != null)
                finalPiece = movedPiece;
        } while (movedPiece != null);

        if (finalPiece != this._current) {
            this._current = finalPiece;
            this.emitGridChange();
        }
    }

    /** Tries to rotate the piece. Does nothing if a collision occurs. */
    public void rotate()
    {
        if (this._currentState != GameState.RUNNING)
            return;

        Piece rotatedPiece = this._current.rotate();

        if (this.pieceCollide(rotatedPiece, this._current))
            return;

        this.removePiece(this._current);
        this.placePiece(rotatedPiece);

        this._current = rotatedPiece;
        this.emitGridChange();
    }

    /*********************** Internals ***********************/

    /** Fills the board with empty lines. */
    private void initBoard()
    {
        for (int i = 0; i < this._height; i++)
            this._grid[i] = new Row(this._width);

        this._currentState = GameState.INITIALIZED;

        this._current = null;
        this._next = this.getRandomPiece();
    }

    /** Choose the next random piece without placing it on the grid. */
    private Piece getRandomPiece()
    {
        Piece.PieceFactory factory = Piece.AVAILABLE_PIECES[
            this._rand.nextInt(Piece.AVAILABLE_PIECES.length)
        ];

        Coordinates coords = new Coordinates(
            (this._width - factory.getExtent()) / 2, 1 - factory.getExtent()
        );

        return factory.construct(coords, 0);
    }

    private void startTimer()
    {
        TimerTask task = new TimerTask() {
            @Override
            public void run()
            {
                gameTick();
            }
        };

        this._timer = new Timer();
        this._timer.scheduleAtFixedRate(
            task, this._clockSpeed, this._clockSpeed
        );
    }

    private void gameOver()
    {
        this._timer.cancel();
        this.changeState(GameState.GAMEOVER);
    }

    private void changeState(GameState newState)
    {
        this._currentState = newState;
        this.emitStateChange(newState);
    }

    /** Returns the next random piece and places it at the top of the grid.
     * Returns null and emits a game over event if the piece can't be placed in
     * the grid. */
    private Piece nextPiece()
    {
        Piece piece = this._next;
        this._next = getRandomPiece();

        // Checks if the last line is blocked by some piece.
        Coordinates topLeft = piece.getTopLeft();
        boolean[][] state = piece.getCurrentState();
        boolean[] line = state[state.length - 1];

        for (int j = 0; j < line.length; j++) {
            Piece cell = this._grid[0].getPiece(j + topLeft.getX());
            if (line[j] && cell != null) {
                this.gameOver();
                return null;
            }
        }

        this.placePiece(piece);

        this.emitNewPiece(this._next);

        return piece;
    }

    /** Returns the new translated piece if it doesn't overlap with another
     * piece or if it is out of the board.
     * Returns null and does nothing if an collision occurs. */
    private Piece movePiece(Piece piece, int dX, int dY)
    {
        Piece newPiece = piece.translate(dX, dY);

        if (this.pieceCollide(newPiece, piece)) {
            this.clearLines();
            return null;
        }

        this.removePiece(piece);
        this.placePiece(newPiece);

        return newPiece;
    }

    /** Returns true if the piece collide with the left/right/bottom border or
     * with another piece. Ignore oldPiece collisions. */
    private boolean pieceCollide(Piece newPiece, Piece oldPiece)
    {
        Coordinates topLeft = newPiece.getTopLeft();
        boolean[][] state = newPiece.getCurrentState();

        // Checks if the new piece overlap another piece.
        // Only checks coordinates of the piece which are inside the grid as the
        // top-most line can still be out of the board.
        int topX = topLeft.getX(), topY = topLeft.getY();
        int i = topY < 0 ? -topY : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];
            int y = topY + i;

            for (int j = 0; j < line.length; j++) {
                if (line[j]) {
                    // Checks the left, right and bottom border.
                    int x = topX + j;
                    if (x < 0 || x >= this._width || y >= this._height)
                        return true;

                    // Checks if the cell is free.
                    Piece cell = this._grid[y].getPiece(x);
                    if (cell != null && cell != oldPiece)
                        return true;
                }
            }
        }

        return false;
    }

    /** Places a new piece on the grid. Doesn't check for collisions. */
    private void placePiece(Piece piece)
    {
        int topX = piece.getTopLeft().getX()
          , topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        // Only draws coordinates of the piece which are inside the grid.
        int i = topY < 0 ? -topY : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];

            for (int j = 0; j < line.length; j++) {
                if (line[j])
                    this._grid[topY + i].setPiece(piece, topX + j);
            }
        }
    }

    /** Removes a piece from the grid. */
    private void removePiece(Piece piece)
    {
        int topX = piece.getTopLeft().getX()
          , topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        // Only clean coordinates of the piece which are inside the grid.
        int i = topY < 0 ? -topY : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];

            for (int j = 0; j < line.length; j++) {
                if (line[j])
                    this._grid[topY + i].setPiece(null, topX + j);
            }
        }
    }

    /** Checks every line where the current piece is to be cleared.
     * Emits the clear lines event when some lines have been removed. */
    private void clearLines()
    {
        int topY = this._current.getTopLeft().getY();

        boolean[][] state = this._current.getCurrentState();

        int nLines = 0;

        // Only clean coordinates of the piece which are inside the grid.
        int i = topY < 0 ? -topY : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];

            // Checks for at least one occupied cell.
            for (int j = 0; j < line.length; j++) {
                if (line[j]) {
                    if (this._grid[i + topY].isComplete()) {
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

        if (nLines > 0)
            this.emitClearedLines(nLines);
    }

    /*********************** Events ***********************/

    private void emitStateChange(GameState newState)
    {
        for (GameView view : _views)
            view.stateChange(newState);
    }

    private void emitGridChange()
    {
        for (GameView view : _views)
            view.gridChange();
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

    public Piece getNextPiece()
    {
        return this._next;
    }

    public int getClockSpeed()
    {
        return this._clockSpeed;
    }

    /** Changes the speed of the game. */
    public void setClockSpeed(int clockSpeed)
    {
        this._clockSpeed = clockSpeed;
        this._timer.cancel();
        this.startTimer();
    }
}
