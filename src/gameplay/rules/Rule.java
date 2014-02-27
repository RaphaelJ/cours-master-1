package gameplay.rules;

/** Manages how the score an the game's speed is determined, implementing set of
 * rules. */
public interface Rule {

    /** Provides a factory method to create new instance of a given rule.
     * This is used for mutliplayer games which need to creates as many instance
     * of the rule as the number of players. */
    public interface RuleFactory {
        public Rule construct();
    }

    public void reset();

    /** Will be called by the GamePlay when a set of lines have been cleared. */
    public void clearLines(int n);

    public int getScore();

    public int getLevel();

    /** Returns the current delay between two "ticks" in milliseconds. */
    public int getClockDelay();

    public void addListener(RuleListener listener);
}
