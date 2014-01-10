package storage.client.core.watcher;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.*;
import java.util.Set;

import storage.client.core.indexer.JNotifyIndexer;
import storage.client.ui.MainController;
import static java.nio.file.StandardWatchEventKinds.*;
import static com.sun.nio.file.ExtendedWatchEventModifier.*;

/**
*
* @author Michal
* @deprecated Use JNotifyWatcher instead
*/
@SuppressWarnings("unused")
@Deprecated
public class FileSystemWatcher extends Thread {
	
	private final JNotifyIndexer index;
	private final Path watchPath;
	private final WatchService watcher;

	public FileSystemWatcher(Path watchDir, JNotifyIndexer index) throws IOException {
		
		this.index = index;
		watchPath = watchDir;
		watcher = watchPath.getFileSystem().newWatchService();
		watchPath.register(watcher,
				new WatchEvent.Kind<?>[] { ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY }, FILE_TREE);
	}

	@Override
	public void run() {

		WatchKey key;

		while(!currentThread().isInterrupted()) {

			try {
				key = watcher.take();
			} catch (InterruptedException x) {
				return;
			}
			
			
			for(WatchEvent<?> event : key.pollEvents()) {
				WatchEvent.Kind<?> kind = event.kind();

				if(kind == OVERFLOW) continue;

				@SuppressWarnings("unchecked")
				WatchEvent<Path> ev = (WatchEvent<Path>)event;
				Path path = ev.context();
				//Path absPath = watchPath.resolve(path);
				
				// consume directory ops
				
				//if(index.isDirectory(path))
				//	continue;
				
				/*
				
				
				if(kind == ENTRY_DELETE) {
					// delete only indexed files
					if(!storage.getLocalSys().containsKey(path.toString()))
						continue;
				}
				// handle file-ops only
				else if(! new File(absPath.toString()).isFile()) {
					System.out.println("consuming " + kind.name() + " of " + path.toString());
					continue;
				}
				
				*/
				
				if(kind == ENTRY_CREATE) {
					
					
					/*
					try {
						index.insert(path);
					} catch (InvalidFileException e) {
						e.printStackTrace();
					}*/
					
					if(index.isFile(path))
						System.out.println("create: " + path.toString());
					
					//c.log("creation of: " + path);
					
					
					
					/*
					FileChannel channel = null;
					RandomAccessFile raf = null;
					try {
						raf = new RandomAccessFile(absPath.toString(), "rw");
						channel = raf.getChannel();
						
						FileLock lock = channel.tryLock();
						
						if(lock == null) {
							c.log("locking failed");
						} else
							c.log("file locked!");
						
						lock.release();
						
						
						
						
					} catch (IOException e) {
						e.printStackTrace();
					} finally {
						try {
							channel.close();
							raf.close();
						} catch (IOException e) {
							e.printStackTrace();
						}
						
					}
*/
					
					//lastCreated = path.toString();
					//storage.create(absPath.toString(), path);
				}
				else if(kind == ENTRY_DELETE) {
					
					/*
					if(index.isDirectory(path)) {
						Set<Path> p = index.removeSubtreeFiles(path);
						
						for(Path f : p) {
							System.out.println("\tdelete sub: " + f.toString());
						}
					}
					*/
					if(index.isFile(path))
						System.out.println("delete: " + path.toString());
					
					index.remove(path);
					
					//c.log("deletion of: " + path);
					//storage.delete(absPath.toString(), path);
				}
				else if (kind == ENTRY_MODIFY) {
					
					if(index.isFile(path))
						System.out.println("modify: " + path.toString());
					
					//System.out.println("\tlast: " + lastCreated);
					//System.out.println("\tcurr: " + path.toString());
					
					//
					// TODO FIX !!!!!!!!!!!!!!!!!!!!!!!
					// works, but looks like crap
					//
					
					/*
					
					if(path.toString().equals(lastCreated)) {
						lastCreated = "";
						System.out.println("skipping");
						continue;
					}
					*/
					
					//c.log("modify of: " + path);
					//storage.write(absPath.toString(), path);
				}
			}
			
			// key reset !!!!
			boolean valid = key.reset();
			if (!valid) {
				break;
			}
			
		}
	}
}
