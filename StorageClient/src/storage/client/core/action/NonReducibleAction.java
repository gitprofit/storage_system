package storage.client.core.action;

public abstract class NonReducibleAction implements Action {

	@Override
	public boolean reducedBy(Action reductor) {
		throw new RuntimeException("Reduction not implemented");
	}

	@Override
	public Action reduceWith(Action reductor) {
		throw new RuntimeException("Reduction not supported");
	}
}
