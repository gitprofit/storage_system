package storage.client.core.action;

public interface FileAction {
	
	void execute();
	boolean reducedBy(FileAction reductor);
	FileAction reduceWith(FileAction reductor);
}
