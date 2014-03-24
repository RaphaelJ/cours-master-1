package view.online;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import view.MultiPlayerSwingView;
import model.config.LocalConfig;
import network.*;

public class JoinServerView extends JFrame {

    private JFrame _parent;
    private JPanel _formPanel;

    private JLabel _ipLabel;
    private JLabel _portLabel;

    private JTextField _ipTextField;
    private JTextField _portTextField;

    private JButton _joinButton;

    private LocalConfig _config;

    public JoinServerView(JFrame parent, LocalConfig config) 
    {
        super("Join server");
        
        this._config = config;
        this._parent = parent;

        initComponents();
    }

    private void initComponents()
    {
        this._formPanel = new JPanel(new GridBagLayout());

        this._ipLabel = new JLabel("IP :");
        this._portLabel = new JLabel("Port :");

        this._ipTextField = new JTextField();
        this._portTextField = new JTextField();

        this._joinButton = new JButton("Join");
        this._joinButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                joinServer();
            }
        });

        // Add components to the formPanel
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(2,2,2,2);
        gbc.anchor = GridBagConstraints.NORTH;

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._ipLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._ipTextField, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._portLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._portTextField, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 0.5;
        gbc.anchor = GridBagConstraints.SOUTH;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._joinButton, gbc);

        // Setup the frame
        this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        this.setLayout(new BorderLayout(6, 6));
        this.setSize(350, 150);
        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt){
                    _parent.setVisible(true);
            }
        });

        // Add components to the frame
        this.add(this._formPanel, BorderLayout.CENTER);
    }

    private void joinServer()
    {
        try {
            String host = this._ipTextField.getText();
            int    port = Integer.parseInt(this._portTextField.getText());

            this.setTitle("Waiting for other players to join ...");

            this.setEnabled(false);

            // Connects itself to the server.
            GameClient client = new GameClient(host, port);

            this.setTitle("");
            this.setVisible(false);
            this.setEnabled(true);

            // Start the view of the game.
            MultiPlayerSwingView mpsv = new MultiPlayerSwingView(
                this._parent, client, this._config
            );
            mpsv.setVisible(true);
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this, "Unable to start the client.",
                    "Error", JOptionPane.ERROR_MESSAGE);
            System.err.println("Unable to start the client.");
            e.printStackTrace();

            this.setTitle("");
            this.setVisible(false);
            this.setEnabled(true);
        }
    }
}
