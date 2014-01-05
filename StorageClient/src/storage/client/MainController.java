package storage.client;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.beans.property.*;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;

/**
 *
 * @author Michal
 */
public class MainController implements Initializable {
    
    public final StringProperty userId = new SimpleStringProperty("");
    public final StringProperty mountPoint = new SimpleStringProperty("");
	public StringProperty mountPointProperty() { return mountPoint; }
    
	public MainController()
	{
		//
	}
    
	@FXML
    private void handleMount(ActionEvent event)
	{
		System.out.println("Mounting: " + mountPoint);
		
	}
	
	@FXML
    private void handleUnmount(ActionEvent event)
	{
		System.out.println("Unmounting!");
	}

	@Override
    public void initialize(URL url, ResourceBundle rb)
	{
		//
	}    
}
