package storage.client.core.executor;

import java.io.IOException;
import java.net.UnknownHostException;
import java.util.UUID;

import storage.client.core.action.ErlangNodeCall;
import storage.client.core.action.Action;

import com.ericsson.otp.erlang.*;

public class SequentialExecutor implements Executor {

	private static final String ERL_COOKIE = "moje_ciastko";
	
	private final OtpSelf locJava;
	private final OtpPeer locErlg;
	private final OtpConnection connection;
	
	@SuppressWarnings("unused")
	private final String userId;
	@SuppressWarnings("unused")
	private final String gatewayNode;
	
	public SequentialExecutor(String userId, String localNode, String gatewayNode)
			throws UnknownHostException, OtpAuthException, IOException {
		
		this.userId = userId;
		this.gatewayNode = gatewayNode;
		
		locJava = new OtpSelf(UUID.randomUUID().toString(), ERL_COOKIE);
		locErlg = new OtpPeer(localNode);
		connection = locJava.connect(locErlg);
	}
	
	@Override
	public void execute(Action action) {
		
		ErlangNodeCall call = action.prepareCall();
		@SuppressWarnings("unused")
		OtpErlangObject result = call.executeWith(connection);
		
		// TODO check returned status here
	}
	
	@Override
	public OtpErlangObject execute(ErlangNodeCall call) {
		return call.executeWith(connection);
	}
}
