package view;

import gameplay.*;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.*;
import javax.swing.*;

import controller.GameController;

public class SwingView extends JFrame
                       implements KeyListener {
    
    private JPanel infoPanel;
    private JPanel playPanel;
    private JButton startButton;
    private JLabel time;
    private JLabel timeTitle;
    
    private ArrayList<GamePlay> _games;
    private ArrayList<GamePanel> _gamePanels = new ArrayList<GamePanel>();
    

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
        this.infoPanel = new JPanel();
        this.startButton = new JButton();
        this.timeTitle = new JLabel();
        this.time = new JLabel();
        this.playPanel = new JPanel();
        for(GamePanel panel : this._gamePanels) {
            this.playPanel.add(panel);
        }

        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        this.startButton.setText("Start");
        this.startButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt)
            {
                for (GameController controller : _controllers)
                    controller.newGame();

                requestFocus();
            }
        });

        this.timeTitle.setText("Time elapsed :");

        this.time.setText("00:00:00");

        javax.swing.GroupLayout infoPanelLayout = new GroupLayout(infoPanel);
        infoPanel.setLayout(infoPanelLayout);
        infoPanelLayout.setHorizontalGroup(
            infoPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(GroupLayout.Alignment.TRAILING,
                    infoPanelLayout.createSequentialGroup()
                .addComponent(timeTitle)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(time)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, 327,
                        Short.MAX_VALUE)
                .addComponent(startButton)
                .addContainerGap())
        );
        infoPanelLayout.setVerticalGroup(
            infoPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(GroupLayout.Alignment.TRAILING,
                    infoPanelLayout.createSequentialGroup()
                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(infoPanelLayout.createParallelGroup(
                        GroupLayout.Alignment.BASELINE)
                    .addComponent(startButton)
                    .addComponent(timeTitle)
                    .addComponent(time))
                .addContainerGap())
        );

        GroupLayout playPanelLayout = new GroupLayout(playPanel);
        playPanel.setLayout(playPanelLayout);
        playPanelLayout.setHorizontalGroup(
            playPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        playPanelLayout.setVerticalGroup(
            playPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGap(0, 349, Short.MAX_VALUE)
        );

        GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addComponent(playPanel, GroupLayout.DEFAULT_SIZE,
                    GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(infoPanel, GroupLayout.Alignment.TRAILING,
                    GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE,
                    Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(playPanel, GroupLayout.DEFAULT_SIZE,
                        GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(infoPanel, GroupLayout.PREFERRED_SIZE,
                        GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
        );

        pack();

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
        long delta = this._game.getBoard().getElapsedTime() / 1000;
        int elapsedHours = (int) (delta / 3600);
        delta = delta % 3600;
 
        int elapsedMinutes = (int) (delta / 60);
        delta = delta % 60;
 
        int elapsedSeconds = (int) delta;

        this.time.setText(
            String.format(
                "%02d:%02d:%02d", elapsedHours, elapsedMinutes, elapsedSeconds
            )
        );
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
