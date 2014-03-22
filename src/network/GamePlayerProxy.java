package network;

import game.GamePlayer;

import java.util.LinkedList;

/** A GamePlayer proxy which will transmit events it receives from the
 * GameClient to the views and will transmit user actions to the network. */
public class GamePlayerProxy extends GameObserverProxy implements GamePlayer {

    private GameClient _client;

    public GamePlayerProxy(GameClient client, int width, int height)
    {
        super (width, height);

        this._client = client;
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
