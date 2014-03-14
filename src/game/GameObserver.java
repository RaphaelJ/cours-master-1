package game;

import model.*;
import model.piece.*;

/** Provides an interface to survey a player's game behaviour.
 * This interface is used as is in GUI which doesn't provide a way to interact
 * with the game (remote player games in a network game). */
public interface GameObserver extends GameManager {

    public void addListener(GameListener listener);

    /*********************** Getters ***********************/

    // Observers give read-only access the grid but don't provide direct access
    // to the Board instance. This is needed for observers which are proxies to
    // a remote game (running on a server) : they can't provide the
    // Board instance (which is running on another host) but can provide the
    // state of the grid.
    // This albso prohibit users to directly change the grid, keeping the
    // semantic of a simple "observer".

    /** Returns a BoardSection for the entire grid. */
    public FullBoardSection getGrid();

    public int getGridWidth();

    public int getGridHeight();

    public Piece getCurrentPiece();

    public Piece getNextPiece();

    public int getScore();

    public int getLevel();
}
