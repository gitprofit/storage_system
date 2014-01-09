package storage.client;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import storage.client.core.LocalFileSystem;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

/**
 *
 * @author Michal
 */
public class ApplicationRoot extends Application {
    
    @Override
    public void start(Stage stage) throws Exception {
    	
        Parent root = FXMLLoader.load(getClass().getResource("ui/MainView.fxml"));
        
        Scene scene = new Scene(root);
        
        stage.setScene(scene);
		stage.setTitle("Storage Client");
		stage.initStyle(StageStyle.DECORATED);
        stage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
