package storage.client.core;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.ericsson.otp.erlang.*;

/**
*
* @author Michal
*/
public class Storage {
	
	private OtpSelf client;
	private OtpPeer server;
	private OtpConnection connection;
	
	private String userId;
	private String gatewayNode;
	
	private Map<String, String> localSys;
	
	public void setUserId(String value) {
		userId = value;
	}
	
	public void setGatewayNode(String value) {
		gatewayNode = value;
	}
	
	public Storage(String clientNode, String cookie)
			throws IOException, OtpAuthException {
		
		localSys = new HashMap<>();
		
		client = new OtpSelf(UUID.randomUUID().toString(), cookie);
		server = new OtpPeer(clientNode);
		connection = client.connect(server);
	}
	
	public synchronized Map<String, String> getLocalSys() {
		///TODO return deep copy !!!!!
		return localSys;
	}
	
	public synchronized void sync() {
		localSys = grab_ids();
	}
	
	public Map<String, String> grab_ids() {

		OtpErlangObject[] args = new OtpErlangObject[] {
				new OtpErlangAtom(gatewayNode),
				new OtpErlangString(userId)
		};
		
		Map<String, String> res = new HashMap<>();
		
		try {
			connection.sendRPC("storage_client", "grab_ids", args);
			OtpErlangObject response = connection.receiveMsg().getMsg();
			
			for(OtpErlangObject obj : (OtpErlangList)extractResult(response)) {
				OtpErlangTuple tup = (OtpErlangTuple)obj;
				OtpErlangString f = (OtpErlangString)tup.elementAt(0);
				OtpErlangString s = (OtpErlangString)tup.elementAt(1);
				res.put(f.stringValue(), s.stringValue());
			}
			
		} catch (IOException | 
				OtpAuthException |
				OtpErlangExit |
				OtpErlangDecodeException e) {
			e.printStackTrace();
		}
		
		return res;
	}
	
	public String create(String fullPath, Path vPath) {
		
		fullPath = fullPath.replace('\\', '/');
		
		OtpErlangObject[] args = new OtpErlangObject[] {
				new OtpErlangAtom(gatewayNode),
				new OtpErlangString(fullPath),
				new OtpErlangString(userId)
		};

		try {
			connection.sendRPC("storage_client", "create", args);
			OtpErlangObject response = connection.receiveMsg().getMsg();
			OtpErlangString newId = (OtpErlangString)extractResult(response);
			
			synchronized(this) {
				localSys.put(vPath.toString(), newId.stringValue());
			}
			
			return newId.stringValue();
			
		} catch (IOException | 
				OtpAuthException |
				OtpErlangExit |
				OtpErlangDecodeException e) {
			e.printStackTrace();
			return "_error_";
		}
	}
	
	public void read(String fileId) {
		
		
	}
	
	private OtpErlangObject extractResult(OtpErlangObject rawResult) {
		OtpErlangTuple tuple = (OtpErlangTuple) rawResult;
		return tuple.elementAt(1);
	}
}
