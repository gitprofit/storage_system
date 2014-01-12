package storage.client.core.action;

import java.nio.file.Path;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class RenameAction extends NonReducibleAction {
	
	private final Path pathFrom;
	private final Path pathTo;
	
	public RenameAction(Path pathFrom, Path pathTo) {
		this.pathFrom = pathFrom;
		this.pathTo = pathTo;
	}
	
	@Override
	public ErlangNodeCall prepareCall() {
		
		OtpErlangObject[] args = new OtpErlangObject[] {
				new OtpErlangString(pathFrom.toString()),
				new OtpErlangString(pathTo.toString())
		};
		
		return new ErlangNodeCall("storage_client", "rename2", args);
	}

}