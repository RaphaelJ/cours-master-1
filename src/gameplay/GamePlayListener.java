package gameplay;

import java.util.*;

import model.Board;
import view.GameView;

/** Provides a way to listen to game play events. */
public interface GamePlayListener extends EventListener {
    public void scoreChange(int newScore);

    public void levelChange(int newLevel);

    public void speedChange(int newClockSpeed);
}
