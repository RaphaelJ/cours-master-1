package gameplay;

import java.util.*;

/** Combines three interfaces for a full awareness of the game progress. */
public interface GameListener extends BoardListener, RuleListener
                                    , GameStateListener {
}
