package controller;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import util.timer.Task;
import util.timer.Timer;
import view.SwingView;
import model.Board;
import model.piece.Piece;

public class GameController implements ActionListener {
	
	private Board _board;
	private Timer _timer;
	private SwingView _gui;
	
	public GameController() {
		
		_board = new Board();
		_gui = new SwingView(_board, this);
		_gui.run();
		
		Task task = new Task(true) {
			@Override
			public void run() {
				Piece piece = _board.gameTick();
				
				if(piece == null)
					_gui.gameOver();
				else
					_gui.gridChange();
			}
		};
		
		_timer = new Timer(task, 1000);
	}

	@Override
	public void actionPerformed(ActionEvent e) {

		if("new game".equals(e.getActionCommand()))
			_timer.start();
	}
}
