package storage.client.core.action;

import java.nio.file.Path;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ModifyAction extends NonReducibleAction {
	
	private final Path path;
	
	public ModifyAction(Path path) {
		this.path = path;
	}
	
	@Override
	public ErlangNodeCall prepareCall() {
		
		OtpErlangObject[] args = new OtpErlangObject[] {
				new OtpErlangString(path.toString())
		};
		
		return new ErlangNodeCall("storage_client", "write", args);
	}

}
