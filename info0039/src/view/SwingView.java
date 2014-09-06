package view;

import game.GameManager;
import game.GameStateListener;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;

import javax.swing.BoxLayout;
import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.LayoutStyle;
import javax.swing.WindowConstants;

/** Serves as a baseclass for the game's windows.
 * Provides an empty JPanel (this.gamePanel) which will be used by subclasses
 * to implement the game's board user interface(s). */
public abstract class SwingView extends JFrame implements GameStateListener {

    protected JFrame parent;

    protected JPanel gamePanel;

    protected JPanel infoPanel;
    protected JLabel time;
    protected JLabel timeTitle;
    protected JButton newGameButton;
    protected JLabel status;

    public SwingView(JFrame parent, GameManager game)
    {
        super("Tetris MVC");

        game.addListener(this);

        this.parent = parent;

        initComponents();

        this.setFocusable(true);
    }

    private void initComponents()
    {
        this.gamePanel = new JPanel();
        this.gamePanel.setLayout(new BoxLayout(gamePanel, BoxLayout.X_AXIS));

        this.infoPanel = new JPanel();
        this.timeTitle = new JLabel();
        this.time = new JLabel();

        this.newGameButton = new JButton("New game");
        this.newGameButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                newGame();
                requestFocus();
            }
        });

        this.status = new JLabel();

        setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);

        this.addWindowListener(new WindowAdapter() {
                public void windowClosing(java.awt.event.WindowEvent evt){
                        parent.setVisible(true);
                }
        });

        this.timeTitle.setText("Time elapsed :");

        this.time.setText("00:00:00");

        javax.swing.GroupLayout infoPanelLayout = new GroupLayout(this.infoPanel);
        this.infoPanel.setLayout(infoPanelLayout);
        infoPanelLayout.setHorizontalGroup(
            infoPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(GroupLayout.Alignment.TRAILING,
                    infoPanelLayout.createSequentialGroup()
                .addComponent(this.timeTitle)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(this.time)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(this.newGameButton)
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addContainerGap())
            .addGroup(GroupLayout.Alignment.TRAILING,
                    infoPanelLayout.createSequentialGroup()
                .addComponent(this.status))
        );
        infoPanelLayout.setVerticalGroup(
            infoPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(GroupLayout.Alignment.TRAILING,
                    infoPanelLayout.createSequentialGroup()
                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(infoPanelLayout.createParallelGroup(
                        GroupLayout.Alignment.CENTER)
                    .addComponent(this.status))
                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(infoPanelLayout.createParallelGroup(
                        GroupLayout.Alignment.BASELINE)
                    .addComponent(this.timeTitle)
                    .addComponent(this.time)
                    .addComponent(this.newGameButton))
                .addContainerGap())
        );

        this.setLayout(new BorderLayout());
        this.add(this.gamePanel, BorderLayout.CENTER);
        this.add(this.infoPanel, BorderLayout.SOUTH);

        this.setResizable(false);
    }

    public void run()
    {
        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run()
            {
                setVisible(true);
                newGame();
                requestFocus();
            }
        });
    }

    protected abstract void newGame();

    public abstract void stateChanged(GameManager.GameState newState);

    public void timeChanged(long elapsedTime)
    {
        long delta = elapsedTime / 1000;
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
}
