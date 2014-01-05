package storage.client.ui;

import storage.client.core.*;

import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ResourceBundle;

import javafx.beans.property.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;

/**
 *
 * @author Michal
 */
public class MainController implements Initializable {
	
	private final BooleanProperty mounted = new SimpleBooleanProperty(false);
	public BooleanProperty mountedProperty() { return mounted; }
	public boolean getMounted() { return mounted.get(); }
	public void setMounted(boolean value) { mounted.set(value); }
	
	@FXML private TextField txtUserId;
	@FXML private TextField txtMountPoint;
	
	@FXML private ListView<String> lvLog;
	@FXML private TextArea tarLog;
	
	FileSystemWatcher watcher = null;
	
	public MainController()
	{
		//
	}
    
	@FXML
	private void handleMount(ActionEvent event)
	{
		String mountPoint = txtMountPoint.getText();
		log("Mounting: " + mountPoint);
		
		try {
			watcher = new FileSystemWatcher(this, txtMountPoint.getText());
			watcher.start();
			
			log("Storage mounted at: " + mountPoint);
			setMounted(true);
			
		} catch (IOException e) {
			
			log("Mounting failed: IOException");
			e.printStackTrace();
		}
	}
	
	@FXML
	private void handleUnmount(ActionEvent event)
	{
		System.out.println("Unmounting!");
		
		watcher.interrupt();
		
		setMounted(false);
	}
	
	@FXML
	private void handleSync(ActionEvent event)
	{
		System.out.println("Syncing!");
	}
	
	@FXML
	private void handleClear(ActionEvent event)
	{
		tarLog.setText("");
	}

	@Override
	public void initialize(URL url, ResourceBundle rb)
	{
		//
	}
	
	public void log(String message) {
		tarLog.appendText(message + "\n");
	}
}
