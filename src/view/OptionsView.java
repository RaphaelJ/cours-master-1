package view;


import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/** Main configuration menu. Used to set the number of players and the size of
 * the boards. */
public class OptionsView extends JFrame
        implements ChangeListener, ActionListener {

    private JPanel _formPanel;
    private JPanel _playersPanel;
    private JPanel _buttonsPanel;

    private JLabel _boardWidthTitle;
    private JSpinner _boardWidth;

    private JLabel _boardHeightTitle;
    private JSpinner _boardHeight;

    private JLabel _nbPlayersMultiTitle;
    private JSpinner _nbPlayersMulti;

    private JButton _okButton;

    private JFrame _parent;
    private Configuration _config;

    public OptionsView(JFrame parent, Configuration config)
    {
        super("Options");

        this._parent = parent;
        this._config = config;

        initComponents();
    }

    private void initComponents()
    {
        // Initialize the panels
        this._formPanel = new JPanel(new GridBagLayout());
        this._playersPanel = new JPanel();
        this._playersPanel.setLayout(new BoxLayout(this._playersPanel,
                        BoxLayout.Y_AXIS));
        this._buttonsPanel = new JPanel(new FlowLayout());

        // Initialize the labels
        this._boardWidthTitle = new JLabel("Board width :");
        this._boardHeightTitle = new JLabel("Board height :");
        this._nbPlayersMultiTitle = new JLabel("Nb players in multi :");

        // Initialize other components
        this._boardWidth = new JSpinner(
                        new SpinnerNumberModel(
                                        this._config.getBoardWidth(), 4, 50, 1));
        this._boardWidthTitle.setLabelFor(this._boardWidth);

        this._boardHeight = new JSpinner(
                        new SpinnerNumberModel(
                                        this._config.getBoardHeight(), 10, 30, 1));
        this._boardHeightTitle.setLabelFor(this._boardHeight);

        this._nbPlayersMulti = new JSpinner(
                        new SpinnerNumberModel(
                                        this._config.getNbPlayersMulti(), 2, 4, 1));
        this._nbPlayersMultiTitle.setLabelFor(this._nbPlayersMulti);
        this._nbPlayersMulti.addChangeListener(this);

        this._okButton = new JButton("OK");
        this._okButton.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                        setVisible(false);
                        dispose();
                        afterWindowClosed();
                }
        });

        // Add components to the formPanel
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(2,2,2,2);
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.anchor = GridBagConstraints.EAST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._boardWidthTitle, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._boardWidth, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._boardHeightTitle, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._boardHeight, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._nbPlayersMultiTitle, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._nbPlayersMulti, gbc);

        // Add components to playerPanel
        for(int i = 0; i < this._config.getNbPlayersMulti(); i++)
            this.addPlayerInPlayerPanel(i);

        this._buttonsPanel.add(this._okButton);

        // Setup the frame
        this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        this.setResizable(false);
        this.setLayout(new BorderLayout(6, 6));

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt){
                    afterWindowClosed();
            }
        });

        // Add components to the frame
        this.add(this._formPanel, BorderLayout.NORTH);
        this.add(this._playersPanel, BorderLayout.CENTER);
        this.add(this._buttonsPanel, BorderLayout.SOUTH);

        this.pack();
    }

    private void afterWindowClosed()
    {
        this._config.setBoardWidth((Integer) this._boardWidth.getValue());
        this._config.setBoardHeight((Integer) this._boardHeight.getValue());

        if(this._parent != null)
            this._parent.setVisible(true);
    }

    private void addPlayerInPlayerPanel(int numPlayer)
    {
        JPanel panel = new JPanel(new FlowLayout());
        JLabel label = new JLabel("Player " + (numPlayer+1) + " : ");
        JButton button = new JButton("Keys");

        button.setName("buttonKeysPlayer_"+numPlayer);
        button.addActionListener(this);

        label.setLabelFor(button);

        panel.add(label);
        panel.add(button);

        this._playersPanel.add(panel);
    }

    public static void main(String[] argv)
    {
        OptionsView ov = new OptionsView(null, new Configuration());
        ov.setVisible(true);
    }

    @Override
    public void stateChanged(ChangeEvent e)
    {
        if(e.getSource() == this._nbPlayersMulti) {
            int oldNbPlayersValue = this._config.getNbPlayersMulti();
            int newNbPlayersValue = (Integer) this._nbPlayersMulti.getValue();

            if(oldNbPlayersValue < newNbPlayersValue) {
                    this.addPlayerInPlayerPanel(newNbPlayersValue-1);
                    this._config.increaseNbPlayersMulti();
            } else {
                    this._playersPanel.remove(newNbPlayersValue);
                    this._config.decreaseNbPlayersMulti();
            }

            this.pack();
        }
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();

        // If a "Keys" button has been pressed
        if(source instanceof JButton
           && ((JButton) source).getName().startsWith("buttonKeysPlayer_")) {
            // Extract the number of the player to modify the keys
            JButton button = (JButton) source;
            String name = button.getName();
            String number = name.substring(name.indexOf("_")+1);

            int numPlayer = Integer.parseInt(number);
            KeySet keySet = this._config.getKeySet(numPlayer);

            KeySetDialog dialog = new KeySetDialog(this, true, keySet);
            dialog.setVisible(true);
        }
    }
}
