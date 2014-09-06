package game;

/** Provides an interface to change the global state of a game. */
public interface GameManager {

    public enum GameState {
          INITIALIZED // The board is empty and the timer hasn't been started.
                      // This is the initial state when the Board is instanced.
        , RUNNING     // The timer and the game are running.
        , PAUSED      // The game is running but the timer has been stopped.
        , GAMEOVER    // The game as been finished. The board(s) need to be
                      // reinitialized before being started.
        , STOPPED     // The game is finished because another player has a
                      // GAMEOVER state.
    }

    /** Starts the game and resets the game if needed. */
    public void newGame();

    /** Pauses/Unpauses the game if the game is running/in pause.
     * Does nothing otherwise. */
    public void pause();

    /** Stops the timers and made the game impossible to resume. */
    public void stop();

    /** Reinitialises the game and the board. Stop the game if it's running. */
    public void reset();

    public void addListener(GameStateListener listener);

    public GameState getCurrentState();
}