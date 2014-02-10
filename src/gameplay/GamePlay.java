package gameplay;

import java.util.*;

import model.Board;

/** Provides an interface for "rules" which manage the dynamic aspect of the
 * game (timer, score, speed, levels ...).
 * GamePlay instances change the game behaviour when receiving events from
 * controllers by controlling their associated Board.
 * Gameplay are proxies between the controller and the board.
 */
public interface GamePlay {

    /*********************** Controller events ***********************/

    /** Starts the game and resets the game if needed. */
    public void newGame();

    /** Pauses/Unpauses the game if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause();

    /** Stop the game without resetting the game and the board. */
    public void stop();

    /** Reinitialises the game and the board. Stop the game if it's running. */
    public void reset();

    /*********************** Board events ***********************/

    /** Will be called by the board when asking to clear a set of lines. */
    public void clearLines(LinkedList<Integer> linesIndices);

    public void gameOver();

    /*********************** Getters/Setters and events ***********************/

    public void addListener(GamePlayListener listener);

    public Board getBoard();

    public int getScore();

    public int getLevel();

    /** Returns the current delay between two "ticks" in milliseconds. */
    public int getSpeed();

    /** Changes the speed of the game. */
    public void setSpeed(int newClockSpeed);
}
