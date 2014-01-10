package storage.client.core;

import java.io.*;
import java.nio.file.*;

import storage.client.core.indexer.JNotifyIndexer;
import storage.client.core.watcher.*;


/**
*
* @author Michal
*/
public class LocalFileSystem {
	
	private final Path root;
	
	private final JNotifyIndexer index;
	private final Watcher watcher;
	
	public LocalFileSystem(Path mountPoint) throws IOException {
		root = mountPoint;
		index = new JNotifyIndexer(root);
		watcher = new JNotifyWatcher(root, index);
		
		scan();
	}
	
	public void mount() {
		
		watcher.start();
	}
	
	public void unmount() {
		
		watcher.stop();
	}
	
	private void scan() throws IOException {
		index.update();
	}

}
