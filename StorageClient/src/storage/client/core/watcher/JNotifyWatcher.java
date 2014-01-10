package storage.client.core.watcher;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import storage.client.core.indexer.JNotifyIndexer;

import net.contentobjects.jnotify.JNotify;
import net.contentobjects.jnotify.JNotifyException;
import net.contentobjects.jnotify.JNotifyListener;

/**
* Notifies (updates) local index according to filesystem events.
* @author Michal
*/
public class JNotifyWatcher implements Watcher {
	
	private final String path;
	private final int mask;
	private final boolean watchSubtree;
	
	private final JNotifyIndexer index;
	
	private int watchID;
	
	public JNotifyWatcher(Path root, JNotifyIndexer index) throws IOException {
		
		
		path = root.toString();
		
		mask = JNotify.FILE_CREATED  | 
	               JNotify.FILE_DELETED  | 
	               JNotify.FILE_MODIFIED | 
	               JNotify.FILE_RENAMED;

	    watchSubtree = true;
	    
	    this.index = index;
	}
	
	@Override
	public void start() {
		 try {
			watchID = JNotify.addWatch(path, mask, watchSubtree, new Listener(index));
		} catch (JNotifyException e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public void stop() {
		try {
			JNotify.removeWatch(watchID);
		} catch (JNotifyException e) {
			e.printStackTrace();
		}
	}
}

class Listener implements JNotifyListener {
	
	private final JNotifyIndexer index;
	
	public Listener(JNotifyIndexer index) {
		this.index = index;
	}
	
    public void fileRenamed(int wd, String rootPath, String oldName,
        String newName) {
      print(wd + ": renamed " + rootPath + " : " + oldName + " -> " + newName);
      
      index.rename(Paths.get(oldName), Paths.get(newName));
    }
    
    public void fileModified(int wd, String rootPath, String name) {
      print(wd + ": modified " + rootPath + " : " + name);
      
      
    }
    
    public void fileDeleted(int wd, String rootPath, String name) {
      print(wd + ": deleted " + rootPath + " : " + name);
      
      index.remove(Paths.get(name));
      
      
    }
    
    public void fileCreated(int wd, String rootPath, String name) {
      print(wd + ": created " + rootPath + " : " + name);
      
      
      index.insert(Paths.get(name));
      
      /*
      try {
  		index.insert(Paths.get(name));
  	} catch (InvalidFileException e) {
  		// TODO Auto-generated catch block
  		e.printStackTrace();
  	}*/
      
      
    }
    
    void print(String msg) {
      System.err.println(msg);
    }
    
  }