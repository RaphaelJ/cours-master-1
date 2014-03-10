package view.online;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import model.config.OnlineConfig;
import model.config.OnlineConfig.GameMode;


public class ServerOptionsView extends JFrame {

	private JPanel _formPanel;
	
	private JLabel _logFileLabel;
	private JLabel _gameModeLabel;
	private JLabel _portLabel;
	
	private JTextField _logFileTextField;
	private JButton _browseButton;
	private JComboBox _gameModeComboBox;
	private JTextField _portTextField;
	private JButton _startButton;
	
	private OnlineConfig _config;
	private String _logFile;
	private int _port;
	
	public ServerOptionsView(OnlineConfig config) {
		
		this._config = config;
		
		initComponents();
	}
	
	private void initComponents() {
		
		this._formPanel = new JPanel(new GridBagLayout());
		
		this._logFileLabel = new JLabel("Log file :");
		this._gameModeLabel = new JLabel("Game mode :");
		this._portLabel = new JLabel("Port number :");
		
		this._logFileTextField = new JTextField();
		
		this._browseButton = new JButton("Browse");
		this._browseButton.setName("browseButton");
		this._browseButton.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				browseLogFile();
			}
		});
		
		this._gameModeComboBox = new JComboBox(new DefaultComboBoxModel(
				new Object[] {
						"Simple",
						"Classic",
						"Cooperative"
				}
		));
		this._gameModeComboBox.addItemListener(new ItemListener() {
			
			@Override
			public void itemStateChanged(ItemEvent e) {
				
				if(e.getStateChange() == ItemEvent.SELECTED) {
				
					String item = e.getItem().toString();
					
					if(item.compareTo("Simple") == 0)
						_config.setGameMode(GameMode.SIMPLE);
					
					else if(item.compareTo("Classic") == 0)
						_config.setGameMode(GameMode.CLASSIC);
					
					else if(item.compareTo("Cooperative") == 0)
						_config.setGameMode(GameMode.COOPERATIVE);
				}
			}
		});
		
		this._portTextField = new JTextField();
		
		this._startButton = new JButton("Start server");
		this._startButton.setName("startButton");
		this._startButton.addActionListener(new ActionListener() {
			
			@Override
			public void actionPerformed(ActionEvent e) {
				startServer();
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
        this._formPanel.add(this._logFileLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 0.50;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._logFileTextField, gbc);
        
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._browseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._gameModeLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._gameModeComboBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.NONE;
        this._formPanel.add(this._portLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 0.75;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._portTextField, gbc);
        
        gbc.gridx = 2;
        gbc.gridy = 3;
        gbc.weightx = 0.25;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        this._formPanel.add(this._startButton, gbc);
        
        // Setup the frame
        this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        this.setLayout(new BorderLayout(6, 6));
        
        // Add components to the frame
        this.add(this._formPanel, BorderLayout.NORTH);
        
        this.pack();
	}
	
	private void browseLogFile() {
		JFileChooser chooser = new JFileChooser(new File("."));
		int returnVal = chooser.showSaveDialog(this);
		
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			_logFileTextField.setText(
					new File(
							chooser.getCurrentDirectory().toString(),
							chooser.getSelectedFile().getName()
							).getAbsolutePath());
		}
	}
	
	private void startServer() {
		// TODO : Launch server listening and display lobby view
	}
	
	public static void main(String[] args) {
		ServerOptionsView sov = new ServerOptionsView(new OnlineConfig());
		sov.setDefaultCloseOperation(EXIT_ON_CLOSE);
		sov.setVisible(true);
	}
}
