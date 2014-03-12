package gameplay;

import java.util.*;

/** Combines three interfaces for a full awareness of the game status. */
public interface GameListener extends BoardListener, RuleListener
                                    , GameStateListener {
}
