package gameplay;

import java.util.*;

/** Provides a way to listen to game's state changes. */
public interface GamePlayListener extends EventListener {
    public void stateChanged(GamePlay.GameState newState);

    /** Is called ~ every second with the elapsed number of milliseconds since
     * the game has been started (without pauses). */
    public void timeChanged(long elapsedTime);
}
