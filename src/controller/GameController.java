package controller;

/** Provides an interface for controllers which can be glued to views to listen
 * to user actions.
 * The most basic controller broadcast view's events to the model but some more
 * complex controllers can transmit events over the network, for example. */
public interface GameController extends EventListener {
    public void newGame();

    /*
    public void left();
    public void right();
    public void down();
    public void rotate();
    */
}
