package game;

import java.util.*;

import game.rules.*;
import model.*;

/** Combines three interfaces for a full awareness of the game progress.
 * This interface distinguish itself from GameStateListener because it listens
 * to events linked to a single player (board and score changes ...), not only
 * to the global state of a multiplayer game (time, state, ...). */
public interface GameListener extends BoardListener, RuleListener
                                    , GameStateListener {
}
