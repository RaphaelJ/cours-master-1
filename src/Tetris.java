import controller.PieceController;
import view.PieceViewCLI;
import model.*;


public class Tetris {

	public static void main(String[] args) {
		
		Piece piece = new PieceJ();
		PieceViewCLI view = new PieceViewCLI();
		PieceController controller = new PieceController(piece, view);
		
		controller.printPiece();
		System.out.println("-------------------");
		controller.rotatePiece();
		controller.printPiece();
		System.out.println("-------------------");
		controller.rotatePiece();
		controller.printPiece();
		System.out.println("-------------------");
		controller.rotatePiece();
		controller.printPiece();
		System.out.println("-------------------");
		controller.rotatePiece();
		controller.printPiece();
		System.out.println("-------------------");
	}
}
