package storage.client.core;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;

import storage.client.ui.MainController;
import static java.nio.file.StandardWatchEventKinds.*;
import static com.sun.nio.file.ExtendedWatchEventModifier.*;

/**
*
* @author Michal
*/
public class FileSystemWatcher extends Thread {
	
	// logging purposes only
	private MainController c;
	
	private Storage storage;

	private Path watchPath = null;
	private WatchService watcher = null;

	public FileSystemWatcher(MainController ctrl, Storage stor, String watchDir) throws IOException {
		
		c = ctrl;
		storage = stor;
		
		watchPath = Paths.get(watchDir);
		watcher = watchPath.getFileSystem().newWatchService();
		watchPath.register(watcher,
				new WatchEvent.Kind<?>[] { ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY }, FILE_TREE);
		
		storage.sync();
	}

	@Override
	public void run() {

		WatchKey key;

		while(!currentThread().isInterrupted()) {

			try {
				key = watcher.take();
			} catch (InterruptedException x) {
				return;
			}

			for(WatchEvent<?> event : key.pollEvents()) {
				WatchEvent.Kind<?> kind = event.kind();

				if(kind == OVERFLOW) continue;

				@SuppressWarnings("unchecked")
				WatchEvent<Path> ev = (WatchEvent<Path>)event;
				Path path = ev.context();
				Path absPath = watchPath.resolve(path);
				
				// handle file-ops only
				if(! new File(absPath.toString()).isFile())
					continue;
				
				if(kind == ENTRY_CREATE) {
					c.log("creation of: " + path);
					storage.create(absPath.toString(), path);
				}
				else if(kind == ENTRY_DELETE) {
					c.log("deletion of: " + path);
				}
				else if (kind == ENTRY_MODIFY) {
					c.log("modify of: " + path);
				}
			}
			
			// key reset !!!!
			boolean valid = key.reset();
			if (!valid) {
				break;
			}
			
		}
	}
}
