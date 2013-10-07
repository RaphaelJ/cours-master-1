package view;

import model.Board;
import model.piece.Piece;

import java.awt.*;
import java.util.*;
import javax.swing.*;

import controller.GameController;

public class SwingView extends JFrame implements GameView {

	private static final long serialVersionUID = -6459070709025180654L;
	private Board _board;
    private JPanel _playPanel;
    private JLabel _score;

    private ArrayList<GameController> _controllers
        = new ArrayList<GameController>();

    public SwingView(Board board)
    {
        super("Tetris MVC");
        this._board = board;
        initComponents();
    }

    private void initComponents()
    {
        this._playPanel = new JPanel();
        JPanel rightPanel = new JPanel();
        JButton newGame = new JButton("Start a new game");
        JLabel scoreTitle = new JLabel("Score :");
        this._score = new JLabel("");

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this._playPanel.setBackground(new java.awt.Color(255, 255, 255));
        this._playPanel.setBorder(
            BorderFactory.createLineBorder(new Color(0, 0, 0))
        );
        this._playPanel.setPreferredSize(
            new Dimension(
                this._board.getWidth() * Piece.TILES_SIZE,
                this._board.getHeight() * Piece.TILES_SIZE
            )
        );

        rightPanel.setLayout(new BoxLayout(rightPanel, BoxLayout.Y_AXIS));
        rightPanel.add(newGame);
        rightPanel.add(scoreTitle);
        rightPanel.add(this._score);

        this.setLayout(new BorderLayout());
        this.add(this._playPanel, BorderLayout.CENTER);
        this.add(rightPanel, BorderLayout.EAST);

        newGame.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt)
            {
                for (GameController controller : _controllers)
                    controller.newGame();
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
        Piece[][] grid = this._board.getGrid();

        Graphics g = this._playPanel.getGraphics();

        int i = 0;
        for (Piece[] row : grid) {
            int j = 0;
            for (Piece piece : row) {
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
    	JOptionPane.showMessageDialog(this, "Game over !", "Game Over",
    			JOptionPane.INFORMATION_MESSAGE);
    }

    public void reset()
    {
    }
}
