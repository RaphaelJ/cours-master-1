package controller;

import java.util.*;

/** Provides an interface for controllers which can be glued to views to listen
 * to user actions.
 * The most basic controller broadcast view's events to the model but some more
 * complex controllers might transmit events over the network, for example. */
public interface GameController extends EventListener {
    public void newGame();

    public void pause();

    public void moveLeft();

    public void moveRight();

    /** Push the piece one line down. */
    public void softDrop();

    /** Push the piece down to the last free line. */
    public void hardDrop();

    public void rotate();
}
