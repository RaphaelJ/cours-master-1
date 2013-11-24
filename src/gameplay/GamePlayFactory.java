package gameplay;

import model.Board;

/** Provides a factory method to create new instances of a gameplay.
 * This is mainly used by multi-players gameplay because they need to accept
 * an "inner gameplay" as argument. */
public interface GamePlayFactory {
    public GamePlay construct(Board board);
}
