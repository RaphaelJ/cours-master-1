package controller;

import java.util.Random;

import util.timer.TaskImpl;
import util.timer.Timer;
import view.SwingView;
import model.Board;
import model.Coordinates;
import model.piece.Piece;

public class GameController {
	
	private Board _board;
	private Timer _timer;
	private SwingView _gui;
	
	public GameController() {
		
		_board = new Board();
	}
}
