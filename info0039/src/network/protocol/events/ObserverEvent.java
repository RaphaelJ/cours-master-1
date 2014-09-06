package network.protocol.events;

/** Is used as a base abstract class for every event message from the server to
 * the client which signal a change in the state of a player's board. */
public abstract class ObserverEvent implements EventMessage {

    /** Gives the number of the player for who the event has been triggered.
     * 0 is the player whereas positive numbers are for opponents. */
    public final int playerId;

    public ObserverEvent(int playerId)
    {
        this.playerId = playerId;
    }
}
