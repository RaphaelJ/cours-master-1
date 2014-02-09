package view;

import java.util.ArrayList;

public class Configuration {

	private int _boardWidth;
	private int _boardHeight;
	
	private int _nbPlayersMulti;
	private ArrayList<KeySet> _keySets;
	
	public Configuration() {
		
		this._boardWidth = 10;
		this._boardHeight = 22;
		this._nbPlayersMulti = 2;
		this._keySets = new ArrayList<KeySet>();
		
		for(int i = 0; i < this._nbPlayersMulti; i++)
			this._keySets.add(KeySet.getKeySet(i));
	}
	
	public int getBoardWidth() {
		return this._boardWidth;
	}
	
	public int getBoardHeight() {
		return this._boardHeight;
	}
	
	public int getNbPlayersMulti() {
		return this._nbPlayersMulti;
	}
	
	public KeySet getKeySet(int numPlayer) {
		return this._keySets.get(numPlayer);
	}
	
	public void setBoardWidth(int boardWidth) {
		this._boardWidth = boardWidth;
	}
	
	public void setBoardHeight(int boardHeight) {
		this._boardHeight = boardHeight;
	}
	
	public void increaseNbPlayersMulti() {
		this._nbPlayersMulti++;
		this.addKeySet(KeySet.getKeySet(this._nbPlayersMulti-1));
	}
	
	public void decreaseNbPlayersMulti() {
		this._nbPlayersMulti--;
		this.removeKeySet();
	}
	
	private void addKeySet(KeySet keySet) {
		this._keySets.add(keySet);
	}
	
	private void removeKeySet() {
		this._keySets.remove(this._keySets.size()-1); // Remove last KeySet
	}
}
