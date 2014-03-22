package network.protocol.events;

public class StateChangedEvent implements ManagerEvent {
    public final GameManager.GameState newState;

    public StateChangedEvent(GameManager.GameState newState)
    {
        this.newState = newState;
    }
}
