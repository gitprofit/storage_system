package storage.client.core.action;

import java.nio.file.Path;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class DeleteAction extends NonReducibleAction {

	private final Path path;
	
	public DeleteAction(Path path) {
		this.path = path;
	}
	
	@Override
	public ErlangNodeCall prepareCall() {
		
		OtpErlangObject[] args = new OtpErlangObject[] {
				new OtpErlangString(path.toString())
		};
		
		return new ErlangNodeCall("storage_client", "delete2", args);
	}
}
