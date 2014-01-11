package storage.client.core.action;

import java.nio.file.Path;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class CreateAction extends NonReducibleAction {
	
	private final Path path;
	
	public CreateAction(Path path) {
		this.path = path;
	}
	
	@Override
	public ErlangNodeCall prepareCall() {
		
		OtpErlangObject[] args = new OtpErlangObject[] {
				new OtpErlangString(path.toString())
		};
		
		return new ErlangNodeCall("storage_client", "create", args);
	}

}
