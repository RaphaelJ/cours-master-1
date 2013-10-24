package gameplay;

import model.Board;

/** Provides an interface for "rules" which controls how the game is running (
 * score, speed, levels ...).
 * GamePlay instances emits events to change the game behaviour. */
public interface GamePlay {
    /** Events a GamePlay instance can emit to change the game behaviour. */
    public interface GamePlayListener {
        public void scoreChange(int newScore);

        public void levelChange(int newLevel);

        public void speedChange(int newClockSpeed);
    }

    public void setGame(Board board);

    public void addListener(GamePlayListener listener);

    public int getScore();

    public int getLevel();

    public int getSpeed();
}
