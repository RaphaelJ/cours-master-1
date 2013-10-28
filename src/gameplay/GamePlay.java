package model;

/** Provides an interface for different game rules. */
public interface GamePlay extends GameView {
    public int currentLevel();

    public int score();
}
