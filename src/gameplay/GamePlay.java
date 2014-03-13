package gameplay;

import java.util.*;

import gameplay.rules.*;
import model.Board;
import util.*;

/** Manages the dynamic aspect of the game (timer, score, speed, levels ...).
 * GamePlay instances change the game behaviour when receiving events from
 * controllers. Catches game over over and clear lines events from the board. */
public interface GamePlay {

    public enum GameState {
          INITIALIZED // The board is empty and the timer hasn't been started.
                      // This is the initial state when the Board is instanced.
        , RUNNING     // The timer and the game are running.
        , PAUSED      // The game is running but the timer has been stopped.
        , GAMEOVER    // The game as been finished. The board(s) need to be
                      // reinitialised before being started.
        , STOPPED     // The game is finished because another player has a
                      // GAMEOVER state.
    }

    public void addListener(GameListener listener);

    /*********************** User actions ***********************/

    /** Starts the game and resets the game if needed. */
    public void newGame();

    /** Pauses/Unpauses the game if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause();

    /** Reinitialises the game and the board. Stop the game if it's running. */
    public void reset();

    public void stop();

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

    /*********************** Getters ***********************/

    public Row[] getGrid();

    public int getScore();

    public int getLevel();

    public GameState getCurrentState();
}
