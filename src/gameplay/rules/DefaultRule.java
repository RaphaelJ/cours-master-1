package gameplay.rules;

import java.util.*;

/** Provides a default abstract rule which manages events. */
public abstract class DefaultRule implements Rule {

    protected ArrayList<RuleListener> _listeners
        = new ArrayList<RuleListener>();

    public void addListener(RuleListener listener)
    {
        this._listeners.add(listener);
    }

    public void emitScoreChange(int newScore)
    {
        for (RuleListener listener : this._listeners)
            listener.scoreChange(newScore);
    }

    public void emitLevelChange(int newLevel)
    {
        for (RuleListener listener : this._listeners)
            listener.levelChange(newLevel);
    }

    public void emitClockDelayChange(int newClockDelay)
    {
        for (RuleListener listener : this._listeners)
            listener.clockDelayChange(newClockDelay);
    }

    public abstract void reset();

    public abstract void clearLines(int n);

    public abstract int getScore();

    public abstract int getLevel();

    public abstract int getClockDelay();
}
