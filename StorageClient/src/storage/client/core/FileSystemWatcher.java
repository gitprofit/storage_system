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
		
		String lastCreated = "";
		
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
				
				
				
				
				if(kind == ENTRY_DELETE) {
					// delete only indexed files
					if(!storage.getLocalSys().containsKey(path.toString()))
						continue;
				}
				// handle file-ops only
				else if(! new File(absPath.toString()).isFile()) {
					System.out.println("consuming " + kind.name() + " of " + path.toString());
					continue;
				}
				
				if(kind == ENTRY_CREATE) {
					c.log("creation of: " + path);
					lastCreated = path.toString();
					storage.create(absPath.toString(), path);
				}
				else if(kind == ENTRY_DELETE) {
					c.log("deletion of: " + path);
					storage.delete(absPath.toString(), path);
				}
				else if (kind == ENTRY_MODIFY) {
					
					System.out.println("modify:");
					System.out.println("\tlast: " + lastCreated);
					System.out.println("\tcurr: " + path.toString());
					
					//
					// TODO FIX !!!!!!!!!!!!!!!!!!!!!!!
					// works, but looks like crap
					//
					
					if(path.toString().equals(lastCreated)) {
						lastCreated = "";
						System.out.println("skipping");
						continue;
					}
					
					c.log("modify of: " + path);
					storage.write(absPath.toString(), path);
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
