package storage.client.core;

import java.io.*;
import java.nio.file.*;


/**
*
* @author Michal
*/
public class LocalFileSystem {
	
	private final Path root;
	private final Index index;
	private final FileSystemWatcher watcher;
	
	public LocalFileSystem(Path mountPoint) throws IOException {
		root = mountPoint;
		index = new Index(root);
		watcher = new FileSystemWatcher(root, index);
		
		scan();
	}
	
	public void mount() {
		
		watcher.start();
	}
	
	public void unmount() {
		
		watcher.interrupt();
	}
	
	private void scan() throws IOException {
		index.update();
	}

}
