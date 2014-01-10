package storage.client.core;

/**
 * Generic file exception.
 * @author Michal
 * @deprecated Currently unused
 */
@Deprecated
public class InvalidFileException extends Exception {
	private static final long serialVersionUID = 1L;

	public InvalidFileException(String message) {
		super(message);
	}
}
