package network.protocol.events;

public class TimeChangedEvent implements ManagerEvent {
    public final long elapsedTime;

    public TimeChangedEvent(long elapsedTime)
    {
        this.elapsedTime = elapsedTime;
    }
}
