package controller;

import java.util.Random;

import util.timer.TaskImpl;
import util.timer.Timer;
import view.SwingView;
import model.Board;
import model.Coordinates;
import model.piece.Piece;

public class GameController {
	
	private Random _rand;
	
	private Board _board;
	private Piece _fallingPiece;
	private Piece _nextPiece;
	
	private Timer _timer;
	
	private SwingView _gui;
	
	public GameController() {
		
		_rand = new Random();
		
		_board = new Board();
		_fallingPiece = generateRandomPiece();
		_nextPiece = generateRandomPiece();
		
		_timer = new Timer(new TaskImpl(true, this, "softDrop", null, null), 1000);
		_timer.start();
		
		_gui = new SwingView(_board);
		_gui.run();
	}
	
	public Piece generateRandomPiece() {
		
		int i = _rand.nextInt(Piece.AVAILABLE_PIECES.length - 1);
		return Piece.AVAILABLE_PIECES[i].construct(new Coordinates(0 ,0), 0);
	}
	
	public void softDrop() {
		_fallingPiece.translate(0, 1);
	}
}
