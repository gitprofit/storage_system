package storage.client.ui;

import storage.client.core.*;
import storage.client.core.indexer.JNotifyIndexer;
import storage.client.core.watcher.JNotifyWatcher;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpAuthException;

import javafx.beans.property.*;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;


///
///
///
///
/// TODO REFACTOR THAT SPAGHETTI :E
/// (MainController, Storage, FileSystemWatcher)
///
///
///


/**
 *
 * @author Michal
 */
public class MainController implements Initializable {
	
	private final BooleanProperty mounted = new SimpleBooleanProperty(false);
	public BooleanProperty mountedProperty() { return mounted; }
	public boolean getMounted() { return mounted.get(); }
	public void setMounted(boolean value) { mounted.set(value); }
	
	@FXML private TextField txtGatewayNode;
	@FXML private TextField txtUserId;
	@FXML private TextField txtMountPoint;
	
	@FXML private ListView<String> lvLog;
	@FXML private TextArea tarLog;
	
	//FileSystemWatcher watcher = null;
	JNotifyWatcher watcher = null;
	//CommonsWatcher watcher = null;
	
	Storage storage = null;
	
	public MainController()
	{
		//
	}
    
	JNotifyIndexer index;
	@FXML
	private void handleMount(ActionEvent event)
	{
		String mountPoint = txtMountPoint.getText();
		String gateway = txtGatewayNode.getText();
		
		Path root = Paths.get(mountPoint);
		
		log("Mounting: " + mountPoint);
		/*
		
		try {
			storage = new Storage("cl01@PROFIT-PC", "moje_ciastko");
			storage.setUserId(txtUserId.getText());
			storage.setGatewayNode(gateway);
			
		} catch(IOException | OtpAuthException e) {
			
			log("Mounting failed: Cannot instantiate Storage");
			e.printStackTrace();
			return;
		}
		*/
		
		try {
			index = new JNotifyIndexer(root);
			index.update();
			
			//watcher = new FileSystemWatcher(root, index);
			watcher = new JNotifyWatcher(root, index);
			//watcher = new CommonsWatcher(root);
			
			watcher.start();
			
		} catch (IOException e) {
			
			log("Mounting failed: Cannot instantiate FileSystemWatcher");
			e.printStackTrace();
			return;
		}
		
		log("Storage mounted at: " + mountPoint);
		log("System accessed via: " + gateway);
		setMounted(true);
	}
	
	@FXML
	private void handleUnmount(ActionEvent event)
	{
		watcher.interrupt();
		
		index.dump();
		
		setMounted(false);
	}
	
	@FXML
	private void handleSync(ActionEvent event)
	{
		log("User files:");
		logSep();
	}
	
	@FXML
	private void handleClear(ActionEvent event)
	{
		tarLog.setText("");
	}

	@Override
	public void initialize(URL url, ResourceBundle rb)
	{
	}
	
	
	public void log(String message) {
		tarLog.appendText(message + "\n");
	}
	
	public void logSep() {
		log("-----------------------");
	}
}
