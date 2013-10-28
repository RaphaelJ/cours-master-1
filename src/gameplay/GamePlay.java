package gameplay;

import model.Board;
import view.GameView;

/** Provides an interface for "rules" which controls how the game is running (
 * score, speed, levels ...).
 * GamePlay instances emits events to change the game behaviour. */
public interface GamePlay extends GameView {
    public void setGame(Board board);

    public void addListener(GamePlayListener listener);

    public int getScore();

    public int getLevel();

    public int getSpeed();
}
