package model.config;

import java.util.ArrayList;

import view.keyboard.KeySet;

public class LocalConfig extends Config {

    private int _nbPlayersMulti;
    private ArrayList<KeySet> _keySets;

    public LocalConfig()
    {
        super();

        this._nbPlayersMulti = 2;
        this._keySets = new ArrayList<KeySet>();

        for(int i = 0; i < this._nbPlayersMulti; i++)
            this._keySets.add(KeySet.getKeySet(i));
    }

    public int getNbPlayersMulti()
    {
        return this._nbPlayersMulti;
    }
    
    public KeySet getKeySet(int i) {
    	return this._keySets.get(i);
    }

    public void increaseNbPlayersMulti()
    {
        this._nbPlayersMulti++;
        this.addKeySet(KeySet.getKeySet(this._nbPlayersMulti-1));
    }

    public void decreaseNbPlayersMulti()
    {
        this._nbPlayersMulti--;
        this.removeKeySet();
    }

    private void addKeySet(KeySet keySet)
    {
        this._keySets.add(keySet);
    }

    private void removeKeySet()
    {
        this._keySets.remove(this._keySets.size()-1); // Remove  the last KeySet
    }
}
