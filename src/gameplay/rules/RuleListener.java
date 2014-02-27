package gameplay.rules;

import java.util.*;

/** Provides a way to listen to game play events. */
public interface RuleListener extends EventListener {
    public void scoreChange(int newScore);

    public void levelChange(int newLevel);

    public void clockDelayChange(int newClockDelay);
}
