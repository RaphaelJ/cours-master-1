package ai;

import java.awt.Rectangle;
import gameplay.*;
import model.*;
import model.piece.*;

/** Creates an artificial intelligence which controls the game in place of the
 * user. The AI is configured with three variables. */ 
public class ArtificialIntelligence implements BoardListener {

    private class PieceMove {
        public final Piece piece;
        public final double penality;

        public PieceMove(Piece piece, double penality)
        {
            this.piece = piece;
            this.penality = penality;
        }
    }

    private final GamePlay _game;

    private boolean _active = false;

    private final double _α;
    private final double _β;
    private final double _ln_γ;

    public ArtificialIntelligence(GamePlay game, double α, double β,
                                  double γ) {
        this._game = game;
        this._α = α;
        this._β = β;
        this._ln_γ = Math.log(γ);

        game.getBoard().addListener(this);
    }

    public boolean getActive()
    {
        return this._active;
    }

    public void setActive(boolean active)
    {
        this._active = active;

        if (active) {
            Board board = this._game.getBoard();
            synchronized (board) {
                Piece current = board.getCurrentPiece();
                if (current != null)
                    this.newPiece(current, board.getNextPiece());
            }
        }
    }

    public void stateChange(Board.GameState newState) { }

    public void gridChange(Rectangle bounds) { }

    /** When a new piece is chosen, compares every possible column and rotation
     * to select the best one. */
    public void newPiece(Piece current, Piece next)
    {
        if (!this._active)
            return;

        Board board = this._game.getBoard();
        synchronized (board) {
            // Warning : loops which will follow are terrible as hell.
            // This is because loops are NOT as expressive as recursive
            // functions, and thus the over-usage of additional break & continue
            // keywords.
            // I *know* how to write those loops in a more elegant way, using
            // recursion BUT Java's designers decided to do make the usage of
            // recursion as hard as possible (by not optimising tail-calls and
            // by not providing nested-functions and co-routines. These
            // "experts" don't seem to be aware of some great '60s stuff).
            // So I implore you to be tolerant here. This mess is
            // James Gosling's fault, not mine.

            // Tries to rotate the piece in its first position.
            PieceMove bestMove = null;
            Piece rotated = current;
            for (int rot = 0; rot < current.getStates().length; rot++) {
                if (board.pieceCollide(rotated, current))
                    continue;
                bestMove = tryMove(rotated, bestMove);

                // Tries to move the piece horizontally.

                // Move left
                for (int dx = -1; ; dx--) {
                    Piece translated = rotated.translate(dx, 0);
                    if (board.pieceCollide(translated, current))
                        break;
                    bestMove = tryMove(translated, bestMove);
                }

                // Move right
                for (int dx = 1; ; dx++) {
                    Piece translated = rotated.translate(dx, 0);
                    if (board.pieceCollide(translated, current))
                        break;
                    bestMove = tryMove(translated, bestMove);
                }

                rotated = rotated.rotate();
            }

            if (bestMove != null) {
                board.removePiece(current);
                board.setCurrentPiece(bestMove.piece);
                board.placePiece(bestMove.piece);
                board.hardDrop();
            }
        }
    }

    /** Infers the score of the given piece by simulating its drop to the last
     * free position.
     * Returns a new move if this position is better than the given previous
     * best. Returns best otherwise. */
    public PieceMove tryMove(Piece piece, PieceMove best)
    {
        // Hard drop the piece to the last free space.
        Piece finalPiece = piece;
        Piece movedPiece = piece;

        Board board = this._game.getBoard();
        synchronized (board) {
            Piece current = board.getCurrentPiece();
            do {
                finalPiece = movedPiece;
                movedPiece = movedPiece.translate(0, 1);
            } while (!board.pieceCollide(movedPiece, current));
        }

        if (finalPiece.isFullyIntroduced()) {
            double penality = this.movePenality(finalPiece);
            if (best == null || penality < best.penality) {
                return new PieceMove(piece, penality);
            } else
                return best;
        } else
            return best;
    }

    /** Returns the "penality" of a move. */
    private double movePenality(Piece piece)
    {
        Board board = this._game.getBoard();
        synchronized (board) {
            return this._α * this.bockedCells(piece)
                 - this._β * this.completedRows(piece)
                 + Math.log(board.getHeight() - this.topIndex(piece))
                   / this._ln_γ;
        }
    }

    /** Returns the number of cells which will be impossible to be filled
     * because of the given piece without removing at least one line first. */
    private int bockedCells(Piece piece)
    {
        int count = 0;

        int topX = piece.getTopLeft().getX()
          , topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        Board board = this._game.getBoard();
        synchronized (board) {
            Row[] grid = board.getGrid();
            Piece current = board.getCurrentPiece();

            // Counts empty cells which are directly below the piece.

            // Only checks columns of the piece's state inside the grid.
            int minX = topX < 0 ? -topX : 0
              , maxX = Math.min(board.getWidth(), topX + state.length) - topX;

            for (int j = minX; j < maxX; j++) {
                for (int i = state.length - 1; i >= 0; i--) {
                    if (state[i][j]) {
                        // Adds the number of empty cells of this column.
                        for (int k = i + topY + 1; k < grid.length; k++) {
                            Piece other = grid[k].getPiece(j + topX);
                            if (other == null || other == current)
                                count++;
                            else
                                break;
                        }

                        break;
                    }
                }
            }
        }

        return count;
    }

    /** Returns the y-index of the top-most cell of the piece. */
    private int topIndex(Piece piece)
    {
        int topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        for (int i = 0; i < state.length - 1; i++) {
            for (boolean cell : state[i]) {
                if (cell)
                    return topY + i;
            }
        }
        return state.length - 1;
    }

    /** Returns the number of rows that the given piece will complete. */
    private int completedRows(Piece piece)
    {
        int count = 0;

        int topX = piece.getTopLeft().getX()
          , topY = piece.getTopLeft().getY();

        boolean[][] state = piece.getCurrentState();

        Board board = this._game.getBoard();
        synchronized (board) {
            Row[] grid = board.getGrid();

            int minX = topX < 0 ? -topX : 0
              , maxX = Math.min(board.getWidth(), topX + state.length) - topX
              , minY = topY < 0 ? -topY : 0
              , maxY = Math.min(board.getHeight(), topY + state.length) - topY;
            for (int i = minY; i < maxY; i++) {
                boolean[] line = state[i];

                // Checks every line where the piece occupies at least a cell.
                for (int j = minX; j < maxX; j++) {
                    if (line[j]) {
                        if (grid[i + topY].isComplete())
                            count++;

                        break;
                    }
                }
            }
        }

        return count;
    }
}
