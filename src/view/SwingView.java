package view;

import java.awt.*;
import java.awt.event.WindowAdapter;

import javax.swing.*;

import gameplay.*;
import model.*;
import model.piece.*;
import util.*;

/** Serves as a baseclass for the game's windows.
 * Provides an empty JPanel (this.gamePanel) which will be used by subclasses
 * to implement the game's board user interface(s). */
public abstract class SwingView extends JFrame implements GamePlayListener {

    protected JFrame parent;

    protected JPanel gamePanel;

    private JPanel infoPanel;
    private JLabel time;
    private JLabel timeTitle;

    public SwingView(JFrame parent)
    {
        super("Tetris MVC");

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

        setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);

        this.addWindowListener(new WindowAdapter() {
                public void windowClosing(java.awt.event.WindowEvent evt){
                        parent.setVisible(true);
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
                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                .addContainerGap())
        );
        infoPanelLayout.setVerticalGroup(
            infoPanelLayout.createParallelGroup(GroupLayout.Alignment.LEADING)
            .addGroup(GroupLayout.Alignment.TRAILING,
                    infoPanelLayout.createSequentialGroup()
                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addGroup(infoPanelLayout.createParallelGroup(
                        GroupLayout.Alignment.BASELINE)
                    .addComponent(timeTitle)
                    .addComponent(time))
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

    public abstract void stateChanged(GamePlay.GameState newState);

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
