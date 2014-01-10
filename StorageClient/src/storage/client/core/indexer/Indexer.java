package storage.client.core.indexer;

import java.nio.file.Path;
import storage.client.core.action.FileAction;

public interface Indexer {
	
	void insert(Path path);
	void remove(Path path);
	void rename(Path from, Path to);
	
	boolean isFile(Path path);
	boolean isDirectory(Path path);
	
	void update() throws Exception;
	
	FileAction poll();
}
