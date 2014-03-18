package network;

import game.GameManager.GameState;
import game.GameStateListener;

/** This class will listen to game events and will transmit these to its client
 * counterpart using a network socket. */
public class GameServer implements GameStateListener {
    // TODO : créer la classe MultiGameFactory
    public GameServer(String ip, int port, MultiGameFactory multiGameFactory)
    {
        // TODO : initialiser la socket serveur en écoute.
    }

    public void run()
    {
        // TODO :
        // Le GameServer doit se mettre en écoute sur le socket, attendre les
        // connections des clients et emettre un event lorsqu'un client s'est
        // connecté (pour signaler le nombre de clients connectés à la GUI,
        // c'est pour cela que ça doit être fait dans run() et non dans le
        // constructeur : sans ça, la GUI ne s'aurait pas écouter des events
        // du GameServer qui ne serait pas encore construit).
        //
        // Lorsque tous les clients sont connectés, il leur envoie la
        // configuration du serveur (ServerConfigMessage), crée le MultiGame et
        // se met en écoute sur celui-ci.
        //
        // En se mettant en écoute sur le MultiGame (et sur ses MultiGameProxy),
        // il va capturer tous les événements des jeux et les transmettre par
        // le réseau au(x) GameClient(s) correspondants.
    }

	@Override
	public void stateChanged(GameState newState) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void timeChanged(long elapsedTime) {
		// TODO Auto-generated method stub
		
	}
}