package network.protocol.events;

public class ScoreChangeEvent extends ObserverEvent {

    public final int newScore;

    public ScoreChangeEvent(int playerId, int newScore)
    {
        super(playerId);
        this.newScore = newScore;
    }
}
