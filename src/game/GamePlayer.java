package game;

import java.util.LinkedList;

/** Provides an interface to survey and manage a game.
 * Extends GameObserver by providing actions.
 * GamePlay instances also manage the game (timer, etc) and change the game
 * behavior when receiving events from the board (game over, clear lines) or
 * opponents from a multiplayer game. */
public interface GamePlayer extends GameObserver {

    /*********************** User actions ***********************/

    public void moveLeft();

    public void moveRight();

    /** Push the piece one line down. */
    public void softDrop();

    /** Push the piece down to the last free line. */
    public void hardDrop();

    /** Tries to rotate the piece. */
    public void rotate();

    /** Enable/disable the Artificial intelligence. Disabled by default. */
    public void setAI(boolean enable);

    /*********************** Board events ***********************/

    /** Will be called by the board when asking to clear a set of lines.
     * Is needed to synchronise clear-lines events over multiplayer games. */
    public void clearLines(LinkedList<Integer> linesIndices);

    /** Will be called by the board when asking to stop the game. */
    public void gameOver();
}
