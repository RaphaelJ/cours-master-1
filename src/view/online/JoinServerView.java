package view.online;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import view.MultiPlayerSwingView;
import model.config.LocalConfig;
import network.*;

public class JoinServerView extends JFrame {

    private JPanel _formPanel;

    private JLabel _ipLabel;
    private JLabel _portLabel;

    private JTextField _ipTextField;
    private JTextField _portTextField;
    private JButton _joinButton;
    
    private LocalConfig _config;

    public JoinServerView(LocalConfig config) 
    {
        this._config = config;
        
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
        this.setSize(200, 150);

        // Add components to the frame
        this.add(this._formPanel, BorderLayout.CENTER);
    }

    private void joinServer()
    {
        try {
            String host = this._ipTextField.getText();
            int    port = Integer.parseInt(this._portTextField.getText());

            // Connects itself to the server.
            GameClient client = new GameClient(host, port);

            // Start the view of the game.
            MultiPlayerSwingView mpsv = new MultiPlayerSwingView(this, client,
                    this._config, true);
            mpsv.setVisible(true);
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this, "Unable to start the client.",
                    "Error", JOptionPane.ERROR_MESSAGE);
            System.err.println("Unable to start the client.");
            e.printStackTrace();
        }
    }

    public static void main(String[] args)
    {
        JoinServerView jsv = new JoinServerView(new LocalConfig());
        jsv.setDefaultCloseOperation(EXIT_ON_CLOSE);
        jsv.setVisible(true);
    }
}
