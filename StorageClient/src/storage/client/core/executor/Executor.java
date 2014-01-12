package storage.client.core.executor;

import com.ericsson.otp.erlang.OtpErlangObject;

import storage.client.core.action.Action;
import storage.client.core.action.ErlangNodeCall;

public interface Executor {
	
	// TODO merge both methods ??
	void execute(Action action);
	OtpErlangObject execute(ErlangNodeCall call);
}
