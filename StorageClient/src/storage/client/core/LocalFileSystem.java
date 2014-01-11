package storage.client.core;

import java.io.*;
import java.nio.file.*;

import com.ericsson.otp.erlang.OtpAuthException;

import storage.client.core.executor.*;
import storage.client.core.indexer.*;
import storage.client.core.watcher.*;


/**
*
* @author Michal
*/
public class LocalFileSystem {
	
	private final Path root;
	
	private final Indexer index;
	private final Watcher watcher;
	private final Executor executor;
	
	public LocalFileSystem(Path mountPoint)
			throws IOException, OtpAuthException {
		
		root = mountPoint;
		executor = new SequentialExecutor("usr123", "cl01@PROFIT-PC", "ds001@PROFIT-PC");
		index = new JNotifyIndexer(root, executor);
		watcher = new JNotifyWatcher(root, index);
		
		
		//scan();
	}
	
	public void mount() {
		
		watcher.start();
	}
	
	public void unmount() {
		
		watcher.stop();
	}
	
	/*
	private void scan() throws IOException {
		try {
			index.update();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}*/

}
