package network.protocol.actions;

public class SetAIAction implements ActionMessage {
    public final boolean enable;

    public SetAIAction(boolean enable)
    {
        this.enable = enable;
    }
}
