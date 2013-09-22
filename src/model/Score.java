package model;

public abstract class Score {

	protected int points = 0;
	protected int totalClearedLines = 0;
	
	public int getPoints() {
		return points;
	}
	
	public int getTotalClearedLines() {
		return totalClearedLines;
	}
	
	public abstract void increase(int clearedLines, int level) throws Exception;
}
