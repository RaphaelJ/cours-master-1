package views;

/** Provides an interface for views which are object which listen to game's
 * board changes. */
public interface GameView extends EventListener {
    /** Event triggered when a piece move inside the grid. */
    public void gridChange();

    /** Event triggered when n lines have been removed by the player. */
    public void clearedLines(int n);

    public void gameOver();

    public void reset();
}
