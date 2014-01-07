package storage.client.ui;

import storage.client.core.*;

import java.io.IOException;
import java.net.URL;
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
	
	FileSystemWatcher watcher = null;
	Storage storage = null;
	
	public MainController()
	{
		//
	}
    
	@FXML
	private void handleMount(ActionEvent event)
	{
		String mountPoint = txtMountPoint.getText();
		String gateway = txtGatewayNode.getText();
		
		log("Mounting: " + mountPoint);
		
		
		try {
			storage = new Storage("cl01@PROFIT-PC", "moje_ciastko");
			storage.setUserId(txtUserId.getText());
			storage.setGatewayNode(gateway);
			
		} catch(IOException | OtpAuthException e) {
			
			log("Mounting failed: Cannot instantiate Storage");
			e.printStackTrace();
			return;
		}
		
		
		try {
			watcher = new FileSystemWatcher(this, storage, txtMountPoint.getText());
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
		
		setMounted(false);
	}
	
	@FXML
	private void handleSync(ActionEvent event)
	{
		log("User files:");
		
		//storage.sync();

		for(Map.Entry<String, String> kv : storage.getLocalSys().entrySet()) {
			log(kv.getKey() + " is " + kv.getValue());
		}
		
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
