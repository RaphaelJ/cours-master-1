package gameplay;

import java.util.*;

/** Provides a way to listen to game's state change.
 * This interface distinguish itself from GameListener by only listening to
 * common events around every player (i.e. game time/state changes are common to
 * every player but board events are different for each player). */
public interface GameStateListener {
    public void stateChanged(GamePlay.GameState newState);

    /** Is called ~ every second with the elapsed number of milliseconds since
     * the game has been started (without pauses). */
    public void timeChanged(long elapsedTime);
}
