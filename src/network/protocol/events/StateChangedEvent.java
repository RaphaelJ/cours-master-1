package network.protocol.events;

import game.GameManager;

public class StateChangedEvent implements ManagerEvent {
    public final GameManager.GameState newState;

    public StateChangedEvent(GameManager.GameState newState)
    {
        this.newState = newState;
    }
}
