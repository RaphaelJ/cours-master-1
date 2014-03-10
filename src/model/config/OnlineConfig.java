package model.config;

public class OnlineConfig extends Config {

	public enum GameMode {
		SIMPLE,
		CLASSIC,
		COOPERATIVE
	}
	
	private GameMode _mode;
	
	public OnlineConfig() {
		super();
		
		this._mode = GameMode.SIMPLE;
	}
	
	public OnlineConfig(LocalConfig localConf) {
		super();
		
		this.setBoardWidth(localConf.getBoardWidth());
		this.setBoardHeight(localConf.getBoardHeight());
		
		this._mode = GameMode.SIMPLE;
	}
	
	public GameMode getGameMode() {
		return this._mode;
	}
	
	public void setGameMode(GameMode mode) {
		this._mode = mode;
	}
}
