package view;

import model.Board;
import model.Row;
import model.piece.Piece;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.beans.PropertyChangeListener;
import java.util.*;
import javax.swing.*;

import controller.GameController;

@SuppressWarnings("serial")
public class SwingView extends JFrame implements GameView, KeyListener {

	private Board _board;
    private JPanel _playPanel;
    private JLabel _score;
    private JPanel _nextPiecePanel;

    private ArrayList<GameController> _controllers
        = new ArrayList<GameController>();

    public SwingView(Board board)
    {
        super("Tetris MVC");
        this._board = board;
        initComponents();

        this.setFocusable(true);
        this.addKeyListener(this);
    }

    private void initComponents()
    {
        this._playPanel = new JPanel();
        JPanel rightPanel = new JPanel();
        JButton newGame = new JButton("Start a new game");
        JLabel scoreTitle = new JLabel("Score :");
        this._score = new JLabel("");
        this._nextPiecePanel = new JPanel();

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this._playPanel.setBackground(new java.awt.Color(255, 255, 255));
        this._playPanel.setPreferredSize(
            new Dimension(
                this._board.getWidth() * Piece.TILES_SIZE,
                this._board.getHeight() * Piece.TILES_SIZE
            )
        );
        
        rightPanel.setBorder(
            BorderFactory.createLineBorder(new Color(0, 0, 0))
        );
        
        this._nextPiecePanel.setBackground(new java.awt.Color(255, 255, 255));
        this._nextPiecePanel.setPreferredSize(
            new Dimension(
                4 * Piece.TILES_SIZE,
                4 * Piece.TILES_SIZE
            )
        );

        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        rightPanel.add(newGame);
        rightPanel.add(scoreTitle);
        rightPanel.add(this._score);
        rightPanel.add(this._nextPiecePanel);

        this.setLayout(new BorderLayout());
        this.add(this._playPanel, BorderLayout.CENTER);
        this.add(rightPanel, BorderLayout.EAST);

        newGame.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt)
            {
                for (GameController controller : _controllers)
                    controller.newGame();
                
                requestFocus();
            }
        });

        this.setResizable(false);

        pack();
    }

    public void run()
    {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run()
            {
                setVisible(true);
            }
        });
    }

    public void addController(GameController controller)
    {
        this._controllers.add(controller);
    }

    public void gridChange()
    {
        Row[] grid = this._board.getGrid();
        Graphics g = this._playPanel.getGraphics();

        // Draw the grid
        int i = 0;
        for (Row row : grid) {
            int j = 0;
            for (Piece piece : row.getPieces()) {
                if (piece != null) {
                    try {
                        g.drawImage(
                            piece.getTile(), j * Piece.TILES_SIZE,
                            i * Piece.TILES_SIZE, this
                        );
                    } catch (Exception e) { // Unable to load the tile.
                    }
                } else {
                    g.setColor(Color.WHITE);
                    g.fillRect(
                        j * Piece.TILES_SIZE, i * Piece.TILES_SIZE,
                        Piece.TILES_SIZE, Piece.TILES_SIZE
                    );
                }
                j++;
            }
            i++;
        }

        g.finalize();
    }

    public void clearedLines(int n)
    {
    }

    public void gameOver()
    {
        JOptionPane.showMessageDialog(
            this, "Game over !", "Game Over", JOptionPane.INFORMATION_MESSAGE
        );
    }

    public void reset()
    {
    }
    
    public void newPiece(Piece piece)
    {
    	Graphics g = this._nextPiecePanel.getGraphics();
    	
    	int dimension = piece.getFactory().getExtent();
    	boolean[][] state = piece.getCurrentState();
    	
    	// Erase the next piece panel
    	for(int i = 0; i < 4; i++) {
    		for(int j = 0; j < 4; j++) {
    			g.setColor(Color.WHITE);
                g.fillRect(
                    j * Piece.TILES_SIZE, i * Piece.TILES_SIZE,
                    Piece.TILES_SIZE, Piece.TILES_SIZE
                );
    		}
    	}

    	// Draw the next piece
    	for(int i = 0; i < dimension; i++) {
    		for(int j = 0; j < dimension; j++) {
    			if(state[i][j])
    				try {
						g.drawImage(
								piece.getTile(),
								((4 / dimension) - 1 + j) * Piece.TILES_SIZE,
								((4 / dimension) - 1 + i) * Piece.TILES_SIZE,
								this
		                );
    				} catch (Exception e) { // Unable to load the tile.
                    }
    		}
    	}
    }

	@Override
	public void keyPressed(KeyEvent event) {
		
		switch(event.getKeyCode()) {
		
			case KeyEvent.VK_LEFT:
				_board.moveLeft();
				break;
				
			case KeyEvent.VK_RIGHT:
				_board.moveRight();
				break;
				
			case KeyEvent.VK_UP:
				_board.rotate();
				break;
				
			case KeyEvent.VK_DOWN:
				_board.softDrop();
				break;
				
			case KeyEvent.VK_SPACE:
				_board.hardDrop();
				break;
		}
	}

	@Override
	public void keyReleased(KeyEvent event) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void keyTyped(KeyEvent event) {
		// TODO Auto-generated method stub
		
	}
}
