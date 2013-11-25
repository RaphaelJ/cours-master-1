package view;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.HashSet;
import java.util.Set;

public class MultiKeyListener implements KeyListener {

	private Set<Character> _activeKeys = new HashSet<Character>();
	
	@Override
	public void keyPressed(KeyEvent e) {
		this._activeKeys.add(e.getKeyChar());
	}

	@Override
	public void keyReleased(KeyEvent e) {
		this._activeKeys.remove(e.getKeyChar());
	}

	@Override
	public void keyTyped(KeyEvent e) {
	}

	public Set<Character> getActiveKeys() {
		return this._activeKeys;
	}
}
