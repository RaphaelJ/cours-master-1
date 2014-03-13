package gameplay;

import java.util.*;

/** Combines three interfaces for a full awareness of the game progress.
 * This interface distinguish itself from GameStateListener because it listens
 * to events linked to a single player (board and score changes ...). */
public interface GameListener extends BoardListener, RuleListener
                                    , GameStateListener {
}
