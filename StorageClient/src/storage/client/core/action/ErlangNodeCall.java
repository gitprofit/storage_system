package storage.client.core.action;

import java.io.IOException;

import com.ericsson.otp.erlang.*;

/**
 * RMI on Erlang Node
 * @author Michal
 */
public class ErlangNodeCall {
	
	private final String moduleName;
	private final String functionName;
	private final OtpErlangObject[] args;
	
	public ErlangNodeCall(String moduleName, String functionName, OtpErlangObject[] args) {
		this.moduleName = moduleName;
		this.functionName = functionName;
		this.args = args;
	}
	
	public OtpErlangObject executeWith(OtpConnection connection) {

		try {
			
			System.out.println("executing " + moduleName + ":" + functionName + " ...");
			
			connection.sendRPC(moduleName, functionName, args);
			OtpErlangObject response = connection.receiveMsg().getMsg();
			System.out.println("execution result: " + response.toString());
			return extractResult(response);
			
		} catch (OtpErlangDecodeException | OtpErlangExit | OtpAuthException
				| IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	/**
	 * Extracts result from received tuple { ok, Result } -> Result
	 * @param rawResult
	 * @return
	 */
	private OtpErlangObject extractResult(OtpErlangObject rawResult) {
		OtpErlangTuple tuple = (OtpErlangTuple) rawResult;
		return tuple.elementAt(1);
	}
}
