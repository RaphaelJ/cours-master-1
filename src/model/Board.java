package model;

import java.util.*;

import model.piece.Piece;
import view.GameView;

/** Saves the current status of the board and communicates with views to share
 * changes and game events with the user. */
public class Board {

    public static final int DEFAULT_WIDTH  = 10;
    public static final int DEFAULT_HEIGHT = 22;

    /** "Ticks" duration in milliseconds. */
    public static final int DEFAULT_SPEED = 1000;

    private final Random _rand;

    private final int _width;
    private final int _height;

    /** Contains a matrix which maps each cell of the board to the piece which
     * is there, if any.
     * The first cell of the grid is the top-left point of the board. */
    private final Row[] _grid;

    private Piece _current = null;
    private Piece _next;

    private ArrayList<GameView> _views = new ArrayList<GameView>();

    private Timer _timer;
    private int _clockSpeed;

    public Board()
    {
        this._rand = new Random();

        this._width = DEFAULT_WIDTH;
        this._height = DEFAULT_HEIGHT;

        this._clockSpeed = DEFAULT_SPEED;

        this._grid = new Row[this._height];
        
        for(int i = 0; i < this._height; i++)
        	this._grid[i] = new Row(this._width);
        
        this._next = this.getRandomPiece();
    }

    /** Initializes an empty board with a specified seed for the random
     * generator. Using a common seed for two Board instances ensures that
     * pieces will come in the same order. i.e. can avoid some synchronization
     * between two remote processes. */
    public Board(long seed)
    {
        this(seed, DEFAULT_WIDTH, DEFAULT_HEIGHT, DEFAULT_SPEED);
    }

    public Board(long seed, int width, int height, int clockSpeed)
    {
        this._rand = new Random(seed);

        this._width = width;
        this._height = height;

        this._clockSpeed = clockSpeed;

        this._grid = new Row[this._height];
        
        for(int i = 0; i < this._height; i++)
        	this._grid[i] = new Row(this._width);
        
        this._next = this.getRandomPiece();
    }

    /** Removes every pieces from the grid and emits the reset event. */
    public void resetBoard()
    {

    	for(int i = 0; i < this._height; i++)
        	this._grid[i] = new Row(this._width);

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
            this._current = this.movePiece(this._current, new Coordinates(0, 1));

            if (this._current == null) // Introduces a new piece.
                this._current = this.nextPiece();
        }

        for(GameView view : _views)
            view.gridChange();

        return this._current;
    }
    
    private Piece getRandomPiece() {
    	
    	Piece.PieceFactory factory = Piece.AVAILABLE_PIECES[
            this._rand.nextInt(Piece.AVAILABLE_PIECES.length)
        ];
    	
    	Coordinates coords = new Coordinates(
            (this._width - factory.getExtent()) / 2, 1 - factory.getExtent()
        );
    	
    	return factory.construct(coords, 0);
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
                gameOver();
                return null;
            }
        }

        this.placePiece(piece, topLeft, state);

        updateViewsNewPiece(this._next);
        
        return piece;
    }

    /** Returns the new translated piece if it doesn't overlap with another
     * piece or if it is out of the board.
     * Returns null if an overlap occurs. */
    private Piece movePiece(Piece piece, Coordinates offset)
    {
        Piece newPiece = piece.translate(offset.getX(), offset.getY());

        if(isPieceCollide(newPiece, piece)) {
        	clearLines();
        	return null;
        }

        // Removes the old piece from the grid.
        this.placePiece(null, piece.getTopLeft(), piece.getCurrentState());

        // Places the new piece on the grid.
        this.placePiece(newPiece, newPiece.getTopLeft(), newPiece.getCurrentState());

        return newPiece;
    }
    
    private boolean isPieceCollide(Piece newPiece, Piece oldPiece) {
    	
    	Coordinates topLeft = newPiece.getTopLeft();
    	boolean[][] state = newPiece.getCurrentState();
    	
    	// Checks if the new piece overlap another piece.
        // Only checks coordinates of the piece which are inside the grid.
    	int i = topLeft.getY() < 0 ? -topLeft.getY() : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];

            for (int j = 0; j < line.length; j++) {
            	if(line[j]) {
            		
            		if(i + topLeft.getY() >= this._height ||
            				j + topLeft.getX() < 0 ||
            				j + topLeft.getX() >= this._width)
            			return true; // TODO : Handle this separately
            		
	                Piece cell = this._grid[i + topLeft.getY()].getPiece(j + topLeft.getX());
	                if (cell != null && cell != oldPiece)
	                    return true;
            	}
            }
        }
        
        return false;
    }
    
    public void moveLeft() {
    	
    	Piece movedPiece = movePiece(this._current, new Coordinates(-1, 0));
    	
    	if(movedPiece != null) {
    		this._current = movedPiece;
	    	this.updateViewsGridChange();
    	}
    }
    
    public void moveRight() {
    	
    	Piece movedPiece = movePiece(this._current, new Coordinates(1, 0));
    	
    	if(movedPiece != null) {
    		this._current = movedPiece;
	    	this.updateViewsGridChange();
    	}
    }
    
    public void softDrop() {
    	
		Piece movedPiece = movePiece(this._current, new Coordinates(0, 1));
    	
    	if(movedPiece != null) {
    		this._current = movedPiece;
	    	this.updateViewsGridChange();
    	}
    }
    
    public void hardDrop() {
    	
    	Piece finalPiece = this._current;
    	Piece movedPiece = this._current;
    	
    	
    	do {
    		movedPiece = movePiece(movedPiece, new Coordinates(0, 1));
    		
    		if(movedPiece != null)
    			finalPiece = movedPiece;
		
    	} while (movedPiece != null);
    	
    	this._current = finalPiece;
    	this.updateViewsGridChange();
    }
    
    public void rotate() {
    	
    	Piece rotatedPiece = this._current.rotate();
    	
    	if(isPieceCollide(rotatedPiece, this._current))
    		return;
    	
    	// Removes the old piece from the grid.
        this.placePiece(null, this._current.getTopLeft(), this._current.getCurrentState());

        // Places the new piece on the grid.
        this.placePiece(rotatedPiece, rotatedPiece.getTopLeft(), rotatedPiece.getCurrentState());

    	
    	this._current = rotatedPiece;
    	this.updateViewsGridChange();
    }
    
    private void clearLines() {
    	
    	Coordinates topLeft = this._current.getTopLeft();
    	boolean[][] state = this._current.getCurrentState();
    	
    	int i = topLeft.getY() < 0 ? -topLeft.getY() : 0;
        for (; i < state.length; i++) {
        	
        	boolean[] line = state[i];

            for (int j = 0; j < line.length; j++) {
        	
	        	if(line[j] && this._grid[i + topLeft.getY()].isRowComplete()) {
	        		
	        		for (int k = i + topLeft.getY(); k > 0; k--)
	        			this._grid[k] = this._grid[k-1];
	        		
	        		this._grid[0] = new Row(this._width);
	        	}
            }
        }
    }

    /** Places a new piece on the grid. */
    private void placePiece(Piece piece, Coordinates topLeft, boolean[][] state)
    {
        // Only draws coordinates of the piece which are inside the grid.
        int i = topLeft.getY() < 0 ? -topLeft.getY() : 0;
        for (; i < state.length; i++) {
            boolean[] line = state[i];

            for (int j = 0; j < line.length; j++) {
                if (line[j])
                    this._grid[i + topLeft.getY()].setPiece(piece, j + topLeft.getX());
            }
        }
    }

    public void start()
    {
        this.resetBoard();

        TimerTask task = new TimerTask() {
            @Override
            public void run()
            {
                gameTick();
            }
        };

        if(this._timer != null)
            this.stop();

        this._timer = new Timer();
        this._timer.scheduleAtFixedRate(task, 0, this._clockSpeed);
    }

    public void stop()
    {
        this._timer.cancel();
    }

    private void gameOver()
    {
        for (GameView view : this._views)
            view.gameOver();

        this.stop();
    }
    
    private void updateViewsGridChange() {
    	
    	for(GameView view : _views)
            view.gridChange();
    }
    
    private void updateViewsNewPiece(Piece piece) {
    	
    	for(GameView view : _views)
            view.newPiece(piece);
    }

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

    public int getClockSpeed()
    {
        return this._clockSpeed;
    }

    public void setClockSpeed(int clockSpeed)
    {
        this._clockSpeed = clockSpeed;
    }
    
    public Piece getNextPiece() {
    	return this._next;
    }
}
