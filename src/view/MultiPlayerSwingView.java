package view;

import gameplay.*;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import controller.GameController;

public class MultiPlayerSwingView extends SwingView implements KeyListener {

    private ArrayList<GamePlay> _games;
    private ArrayList<GamePanel> _panels;

    private Configuration _config;
    private boolean _useImages;
    private ArrayList<ArrayList<GameController>> _controllers;
    private Set<Integer> _activeKeys;
    private ArrayList<KeyboardHandler> _keyboardHandlers;

    public MultiPlayerSwingView(JFrame parent, ArrayList<GamePlay> games,
    		Configuration config, boolean useImages)
    {
        super(parent, games.get(0).getBoard());

        this._games = games;
        this._panels = new ArrayList<GamePanel>();
        
        this._config = config;
        this._useImages = useImages;
        this._activeKeys = new HashSet<Integer>();
        this._controllers = new ArrayList<ArrayList<GameController>>();
        this._keyboardHandlers = new ArrayList<KeyboardHandler>();
        
        initComponents();
        
        for(int i = 0; i < this._config.getNbPlayersMulti(); i++) {
        	this._keyboardHandlers.add(
        			new KeyboardHandler(
        					this._activeKeys,
        					this._config.getKeySet(i),
        					this._controllers.get(i)));
        }
    }
    
    private void initComponents() {
    	
    	for(GamePlay game : this._games) {
        	GamePanel panel = new GamePanel(this, game, this._useImages);
        	this._panels.add(panel);
        	this._controllers.add(new ArrayList<GameController>());
        	this.playPanel.add(panel);
        }
        
        this.addKeyListener(this);
        this.addWindowListener(new WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt){
				for(GamePlay game : _games)
					game.stop();
			}
		});

        this.pack();
    }

    public void addControllerPlayer(int nbPlayer, GameController controller)
    {
        this._controllers.get(nbPlayer).add(controller);
    }

    @Override
    protected void newGame()
    {
        for (ArrayList<GameController> playerControllers : this._controllers)
        	for(GameController controller : playerControllers)
        		controller.newGame();
    }

    @Override
    protected long getElapsedTime()
    {
        return this._games.get(0).getBoard().getElapsedTime();
    }

    @Override
    public void keyPressed(KeyEvent event)
    {
        _activeKeys.add(event.getKeyCode());

        // Use the first player to propagate the pause event to all controllers.
        if(_activeKeys.contains(KeyEvent.VK_P))
            for (GameController controller : this._controllers.get(0))
                controller.pause();

        for(KeyboardHandler handler : this._keyboardHandlers)
        	handler.checkKeys();
    }

    @Override
    public void keyReleased(KeyEvent e) {
        _activeKeys.remove(e.getKeyCode());
    }

    @Override
    public void keyTyped(KeyEvent e) { }
    
    @Override
    public void gameOver() {
    	int numWinner = 0;
    	int scoreWinner = this._games.get(0).getScore();
    	
    	for(int i = 0; i < this._games.size(); i++) {
    		int score = this._games.get(i).getScore();
    		
    		if(score > scoreWinner) {
    			numWinner = i;
    			scoreWinner = score;
    		}
    	}
    	
    	for(GamePlay game : this._games)
    		game.stop();
    	
    	JOptionPane.showMessageDialog(
    			this,
    			"Player " + (numWinner+1) + " wins the game with " +
    					scoreWinner + " points !",
				"Game Over",
				JOptionPane.INFORMATION_MESSAGE);
    	
    	int choice = 0;
    	choice = JOptionPane.showConfirmDialog(
    			this,
    			"Would you like to retry ?",
    			"Game Over",
    			JOptionPane.YES_NO_OPTION);
    	
    	if(choice == 0)
    		newGame();
    	else
    		this.dispatchEvent(new WindowEvent(this,
    				WindowEvent.WINDOW_CLOSING));
    }
}