package game.rules;

public class NintendoGameBoyFactory implements Rule.RuleFactory {
    public Rule construct()
    {
        return new NintendoGameBoy();
    }
}
