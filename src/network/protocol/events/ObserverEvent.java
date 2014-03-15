package network.protocol.events;

/** Is used as a base interface for every event message from the server to the
 * client which signal a change in the state of a player's board. */
public interface ObserverEvent extends EventMessage {

    /** Returns the number of the player for who the event has been triggered.
     * 0 is the player whereas positive numbers are for opponents. */
    public int getPlayerId();
}