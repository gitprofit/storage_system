package storage.client.core.action;

public interface Action {
	
	// TODO Merge Create & Modify into Push
	// TODO Move Rename to Alter
	
	ErlangNodeCall prepareCall();
	
	boolean reducedBy(Action reductor);
	Action reduceWith(Action reductor);
}
