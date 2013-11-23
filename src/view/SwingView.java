package view;

import gameplay.*;
import model.Board;
import model.Board.GameState;
import model.Row;
import model.piece.Piece;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import javax.swing.*;

import controller.GameController;

@SuppressWarnings("serial")
public class SwingView extends JFrame
                       implements GameView, GamePlayListener, KeyListener {
    private Board _board;
    private JPanel _playPanel;
    private JLabel _time;
    private JLabel _score;
    private JLabel _level;
    private JPanel _nextPiecePanel;

    private ArrayList<GameController> _controllers
        = new ArrayList<GameController>();

    public SwingView(Board board)
    {
        super("Tetris MVC");
        this._board = board;
        this._board.getGameplay().addListener(this);
        initComponents();

        this.setFocusable(true);
        this.addKeyListener(this);
    }

    private void initComponents()
    {
        this._playPanel = new JPanel();
        JPanel rightPanel = new JPanel();

        GamePlay gameplay = this._board.getGameplay();
        JLabel timeTitle = new JLabel("Time elapsed :");
        this._time = new JLabel("00:00:00");
        
        JLabel scoreTitle = new JLabel("Score :");
        this._score = new JLabel(Integer.toString(gameplay.getScore()));

        JLabel levelTitle = new JLabel("Level :");
        this._level = new JLabel(Integer.toString(gameplay.getLevel()));

        this._nextPiecePanel = new JPanel();
        JButton newGame = new JButton("Start a new game");

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
            new Dimension(4 * Piece.TILES_SIZE, 4 * Piece.TILES_SIZE)
        );

        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        rightPanel.add(timeTitle);
        rightPanel.add(this._time);
        rightPanel.add(scoreTitle);
        rightPanel.add(this._score);
        rightPanel.add(levelTitle);
        rightPanel.add(this._level);
        rightPanel.add(this._nextPiecePanel);
        rightPanel.add(newGame);

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

    public void stateChange(GameState newState)
    {
        switch (newState) {
        case INITIALIZED:
            this.cleanBoards();
            break;
        case RUNNING:
            // Redraws the entire grid.
            this.gridChange(
                new Rectangle(0, 0, this._board.getWidth(),
                this._board.getHeight())
            );
            this.newPiece(this._board.getNextPiece());
            break;
        case PAUSED:
            this.cleanBoards();
            this.drawPauseString();
            break;
        case GAMEOVER:
            JOptionPane.showMessageDialog(
                this, "Game over !", "Game Over",
                JOptionPane.INFORMATION_MESSAGE
            );
            break;
        }
    }

    public void gridChange(Rectangle bounds)
    {  	
    	this.updateElapsedTime();
    	
        // Update the grid
    	Row[] grid = this._board.getGrid();
        Graphics g = this._playPanel.getGraphics();

        for (int i = bounds.y; i < bounds.y + bounds.height; i++) {
            Row row = grid[i];
            for (int j = bounds.x; j < bounds.x + bounds.width; j++) {
                Piece piece = row.getPiece(j);
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
            }
        }

        g.finalize();
    }
    
    private void updateElapsedTime() {
    	
    	long delta = this._board.getElapsedTimeInSeconds();
    	int elapsedHours = (int) (delta / 3600);
    	delta = delta % 3600;
 
		int elapsedMinutes = (int) (delta / 60);
		delta = delta % 60;
 
		int elapsedSeconds = (int) delta;
		
        this._time.setText(String.format("%02d:%02d:%02d",
        		elapsedHours, elapsedMinutes, elapsedSeconds));
    }

    public void clearedLines(int n) { }

    public void newPiece(Piece piece)
    {
        Graphics g = this._nextPiecePanel.getGraphics();

        int dimension = piece.getFactory().getExtent();
        boolean[][] state = piece.getCurrentState();

        // Erases the next piece panel
        g.setColor(Color.WHITE);
        g.fillRect(0, 0, Piece.TILES_SIZE * 4, Piece.TILES_SIZE * 4);

        // Draws the next piece at the center of the panel.
        int offset = (Piece.TILES_SIZE * 4 - Piece.TILES_SIZE * dimension) / 2;
        for (int i = 0; i < dimension; i++) {
            for (int j = 0; j < dimension; j++) {
                if (state[i][j]) {
                    try {
                        g.drawImage(
                            piece.getTile(), offset + j * Piece.TILES_SIZE,
                            offset + i * Piece.TILES_SIZE, this
                        );
                    } catch (Exception e) { // Unable to load the tile.
                    }
                }
            }
        }

        g.finalize();
    }

    public void scoreChange(int newScore)
    {
        this._score.setText(Integer.toString(newScore));
    }

    public void levelChange(int newLevel)
    {
        this._level.setText(Integer.toString(newLevel));
    }

    public void speedChange(int newClockSpeed) { }

    /** Removes every drawing for the board and the next piece panels. */
    private void cleanBoards()
    {
        // Hides the board.
        Graphics g = this._playPanel.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(
            0, 0, this._board.getWidth() * Piece.TILES_SIZE,
            this._board.getHeight() * Piece.TILES_SIZE
        );

        // Hides the next piece panel.
        g = this._nextPiecePanel.getGraphics();
        g.setColor(Color.WHITE);
        g.fillRect(0, 0, Piece.TILES_SIZE * 4, Piece.TILES_SIZE * 4);

        g.finalize();
    }

    /** Draws the "Game paused" text at the center of the board. */
    private void drawPauseString()
    {
        String text = "Game paused";

        Graphics g = this._playPanel.getGraphics();
        FontMetrics metrics = g.getFontMetrics();

        int width  = metrics.stringWidth(text)
          , height = metrics.getHeight();

        g.drawString(
            text, (this._board.getWidth() * Piece.TILES_SIZE - width) / 2,
            (this._board.getHeight() * Piece.TILES_SIZE - height) / 2
        );

        g.finalize();
    }

    @Override
    public void keyPressed(KeyEvent event)
    {
        switch(event.getKeyCode()) {
        case KeyEvent.VK_P:
            for (GameController controller : this._controllers)
                controller.pause();
            break;
        case KeyEvent.VK_LEFT:
            for (GameController controller : this._controllers)
                controller.moveLeft();
            break;
        case KeyEvent.VK_RIGHT:
            for (GameController controller : this._controllers)
                controller.moveRight();
            break;
        case KeyEvent.VK_DOWN:
            for (GameController controller : this._controllers)
                controller.softDrop();
            break;
        case KeyEvent.VK_ENTER:
            for (GameController controller : this._controllers)
                controller.hardDrop();
            break;
        case KeyEvent.VK_UP:
            for (GameController controller : this._controllers)
                controller.rotate();
            break;
        }
    }

    @Override
    public void keyReleased(KeyEvent e) { }

    @Override
    public void keyTyped(KeyEvent e) { }
}
