package view;

public class PieceViewCLI extends PieceView {
	
	@Override
	public void display(boolean[][] blocks) {
		
		for(boolean[] rows : blocks) {
			for(boolean cols : rows) {
				
				if(cols)
					System.out.print("#");
				else
					System.out.print(" ");
			}
			
			System.out.println();
		}
	}
}
