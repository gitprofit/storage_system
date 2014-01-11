package storage.client.core.executor;

import storage.client.core.action.Action;

public interface Executor {
	
	void execute(Action action);
}
