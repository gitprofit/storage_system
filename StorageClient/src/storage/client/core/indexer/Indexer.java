package storage.client.core.indexer;

import java.nio.file.Path;

public interface Indexer {
	
	void insert(Path path);
	void remove(Path path);
	void rename(Path from, Path to);
	void modify(Path path);
	
	boolean isFile(Path path);
	boolean isDirectory(Path path);
	
	void update() throws Exception;
}
