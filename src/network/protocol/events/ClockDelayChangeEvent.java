package network.protocol.events;

public class ClockDelayChangeEvent extends ObserverEvent {

    public final int newClockDelay;

    public ClockDelayChangeEvent(int playerId, int newClockDelay)
    {
        super(playerId);
        this.newClockDelay = newClockDelay;
    }
}
