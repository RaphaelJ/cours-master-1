package network;

import java.util.*;

import game.GamePlayer;
import network.protocol.actions.*;

/** A GamePlayer proxy which will transmit events it receives from the
 * GameClient to the views and will transmit user actions to the network. */
public class GamePlayerProxy extends GameObserverProxy implements GamePlayer {

    public GamePlayerProxy(GameClient client, int width, int height)
    {
        super (client, width, height);
    }

    public void moveLeft()
    {
        this._client.sendMessage(new MoveLeftAction());
    }

    public void moveRight()
    {
        this._client.sendMessage(new MoveRightAction());
    }

    public void softDrop()
    {
        this._client.sendMessage(new SoftDropAction());
    }

    public void hardDrop()
    {
        this._client.sendMessage(new HardDropAction());
    }

    public void rotate()
    {
        this._client.sendMessage(new RotateAction());
    }

    public void setAI(boolean enable)
    {
        this._client.sendMessage(new SetAIAction(enable));
    }

    public void clearLines(LinkedList<Integer> linesIndices)
    { // Doesn't apply for this multiplayer proxy.
    }

    public void gameOver()
    { // Doesn't apply for this multiplayer proxy.
    }
}
