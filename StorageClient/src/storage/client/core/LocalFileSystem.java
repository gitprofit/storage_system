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
	
	private final String userId;
	
	
	public LocalFileSystem(Path mountPoint)
			throws IOException, OtpAuthException {
		
		userId = "usr123";
		
		root = mountPoint;
		executor = new SequentialExecutor(userId, "cl01@PROFIT-PC", "ds001@PROFIT-PC");
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
	
	public void sync() {
		
		// TODO dont stop the watcher thread, just ignore pulled files
		watcher.stop();
		
		try {
			index.update();
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		watcher.start();
	}
}
