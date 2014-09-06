package network.protocol.events;

public class LevelChangeEvent extends ObserverEvent {

    public final int newLevel;

    public LevelChangeEvent(int playerId, int newLevel)
    {
        super(playerId);
        this.newLevel = newLevel;
    }
}
