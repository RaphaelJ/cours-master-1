package model;

public class Nintendo extends Gameplay {
	@Override
	public void increase(int clearedLines, int level) throws Exception {
		
		int basePoints = 0;
		
		switch(clearedLines) {
			
			case 1:
				basePoints = 40;
				break;
				
			case 2:
				basePoints = 100;
				break;
				
			case 3:
				basePoints = 300;
				break;
				
			case 4:
				basePoints = 1200;
				break;
				
			default:
				// TODO: Create proper exception class.
				throw new Exception("Invalid number of cleared lines.");
		}
		
		points = basePoints * (level + 1);
		totalClearedLines += clearedLines;
	}
}
