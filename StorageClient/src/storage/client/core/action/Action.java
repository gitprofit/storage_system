package storage.client.core.action;

public interface Action {
	
	ErlangNodeCall prepareCall();
	
	boolean reducedBy(Action reductor);
	Action reduceWith(Action reductor);
}
