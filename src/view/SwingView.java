package view;

import gameplay.*;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import javax.swing.*;

import controller.GameController;

@SuppressWarnings("serial")
public class SwingView extends JFrame
                       implements KeyListener {
    
	private ArrayList<GamePlay> _games;
    private ArrayList<GamePanel> _gamePanels = new ArrayList<GamePanel>();
    private JLabel _time;

    private ArrayList<GameController> _controllers
        = new ArrayList<GameController>();

    public SwingView(ArrayList<GamePlay> games, boolean useImages)
    {
        super("Tetris MVC");

        this._games = games;
        
        for(GamePlay game : games)
        	_gamePanels.add(new GamePanel(game, useImages));

        initComponents();

        this.setFocusable(true);
        this.addKeyListener(this);
    }

    private void initComponents()
    {
    	JLabel timeTitle = new JLabel("Time elapsed :");
        this._time = new JLabel("00:00:00");
        // TODO: Place the time somewhere
    	
    	JButton newGame = new JButton("Start a new game");
    	// TODO: Place the new game button somewhere
    	
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

    private void updateElapsedTime() 
    {
    	// TODO: Where does the time should come from ?
        /*long delta = this._game.getBoard().getElapsedTime() / 1000;
        int elapsedHours = (int) (delta / 3600);
        delta = delta % 3600;
 
        int elapsedMinutes = (int) (delta / 60);
        delta = delta % 60;
 
        int elapsedSeconds = (int) delta;

        this._time.setText(
            String.format(
                "%02d:%02d:%02d", elapsedHours, elapsedMinutes, elapsedSeconds
            )
        );*/
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
