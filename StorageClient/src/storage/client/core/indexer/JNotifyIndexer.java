package storage.client.core.indexer;

import java.io.*;
import java.nio.file.*;
import java.sql.Timestamp;
import java.util.*;
import java.nio.file.attribute.*;

import com.ericsson.otp.erlang.*;

import storage.client.core.action.*;
import storage.client.core.executor.Executor;


/**
* Collection of all paths in root subtree. Designed to work with JNotifyWatcher
* @author Michal
* TODO extract interface
*/
public class JNotifyIndexer implements Indexer {
	
	private final Path root;			// mointpoint of the filesystem
	private Set<Path> files;			// all inxedex paths are relative to the root
	private Set<Path> dirs;
	
	private final Executor executor;
	
	/*
	 * More formally, sets contain no pair of elements e1 and e2 such that e1.equals(e2)
	 * ~ http://docs.oracle.com/javase/7/docs/api/java/util/Set.html
	 * 
	 * Path p1 = Paths.get("P:/");
	 * Path p2 = Paths.get("P:/");
	 * 
	 * p1.equals(p2) -> true
	 * p1 == p2 -> false
	 */
	
	
	public JNotifyIndexer(Path mountPoint, Executor executor) throws IOException {
		root = mountPoint;
		files = new HashSet<>();
		dirs = new HashSet<>();
		this.executor = executor;
	}
	
	public synchronized void insert(Path path) {
		
		Path absolutePath = root.resolve(path);
		Path relativePath = root.relativize(absolutePath);
		
		File file = absolutePath.toFile();
		
		if(file.isFile()) {
			files.add(relativePath);
			executor.execute(new CreateAction(relativePath));
		}
		else if(file.isDirectory()) dirs.add(relativePath);
	}
	
	public synchronized void remove(Path path) {
		if(files.remove(path)) {
			executor.execute(new DeleteAction(path));
		}
		else
			dirs.remove(path);
	}
	
	public synchronized void modify(Path path) {
		if(isFile(path))
			executor.execute(new ModifyAction(path));
	}
	
	public synchronized void rename(Path from, Path to) {

		if(isDirectory(from)) {
			
			Set<Path> newFiles = new HashSet<>();
			Set<Path> newDirs = new HashSet<>();
			
			dirs.remove(from);
			newDirs.add(to);
			
			// rename subtree
			
			for(Path file : files)
				if(file.startsWith(from)) {
					Path constPart = from.relativize(file);
					to.resolve(constPart);
					System.out.println("renaming file " + file + " to " + to.resolve(constPart));
					executor.execute(new RenameAction(file, to.resolve(constPart)));
					newFiles.add(to.resolve(constPart));
				} else {
					System.out.println("leaving file " + file);
					newFiles.add(file);
				}

			for(Path file : dirs)
				if(file.startsWith(from)) {
					Path constPart = from.relativize(file);
					to.resolve(constPart);
					System.out.println("renaming dir " + file + " to " + to.resolve(constPart));
					newDirs.add(to.resolve(constPart));
				} else {
					System.out.println("leaving dir " + file);
					newDirs.add(file);
				}
			
			files = newFiles;
			dirs = newDirs;
			
		}
		else if(isFile(from)) {
			executor.execute(new RenameAction(from, to));
			files.remove(from);
			files.add(to);
		}
	}
	
	public synchronized boolean isFile(Path path) {
		return files.contains(path);
	}
	
	public synchronized boolean isDirectory(Path path) {
		return dirs.contains(path);
	}
	
	@Deprecated
	public synchronized void dump() {
		
		System.out.println("dirs:");
		for(Path p : dirs)
			System.out.println("\t" + p.toString());
		
		System.out.println("files:");
		for(Path p : files)
			System.out.println("\t" + p.toString());
	}
	
	
	
	public synchronized void update() throws IOException {
		
		// scanning remote
		
		final Map<Path, Long> remoteStatus = new HashMap<Path, Long>();
		final Set<Action> syncQueue = new HashSet<Action>();
		
		ErlangNodeCall call = new ErlangNodeCall("storage_client", "scan", new OtpErlangObject[]{});
		OtpErlangObject res = executor.execute(call);
		OtpErlangTuple resp = (OtpErlangTuple)res;
		
		for(OtpErlangObject obj : (OtpErlangList)resp.elementAt(1)) {
			OtpErlangTuple tup = (OtpErlangTuple)obj;
			String vpath = ((OtpErlangString)tup.elementAt(0)).stringValue();
			Long time = ((OtpErlangLong)tup.elementAt(1)).longValue();
			
			remoteStatus.put(Paths.get(vpath), time/1000);
			
			//System.out.println("file " + vpath + ", lastModified " + new Timestamp(time/1000));
		}
		
		
		
		files.clear();
		dirs.clear();
		
		Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
			
			@Override
			public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
				
				Path relPath = root.relativize(path);
				
				Long remoteTime = remoteStatus.get(relPath);
				
				if(remoteTime != null) {

					// current local file is stored in remote node
					files.add(relPath);
					remoteStatus.remove(relPath);
					
					Long localTime = path.toFile().lastModified();
					
					
					System.out.println("processing " + relPath);
					System.out.println("\t local iz: " + new Timestamp(localTime));
					System.out.println("\tremote iz: " + new Timestamp(remoteTime));
					
					
					if(remoteTime > localTime) { // TODO add some delta (~30s) ??
						// remote is better, pull
						System.out.println("pulling remote " + relPath);
						syncQueue.add(new PullAction(relPath));
						
					} else {
						// local is newer, keep and push to remote
						System.out.println("pushing local " + relPath);
						syncQueue.add(new ModifyAction(relPath));
					}
					
				} else {
					System.out.println("ignoring local-only " + relPath);
					// ignore local-only files
				}
				
				return FileVisitResult.CONTINUE;
			}
			
			@Override
			public FileVisitResult preVisitDirectory(Path path, BasicFileAttributes attrs) throws IOException {
				dirs.add(root.relativize(path));
				return super.preVisitDirectory(path, attrs);
			}
		});
		
		// there are only new files in remoteStatus left
		// pull all and ensure dirs
		
		for(Path path : remoteStatus.keySet()) {
			
			System.out.println("remote-only pull " + path);
			syncQueue.add(new PullAction(path));
			System.out.println("name count iz " + path.getNameCount());
			
			// add dirs
			for(int i=1; i<path.getNameCount(); ++i) {
				System.out.println("adding subpath " + path.subpath(0, i));
				dirs.add(path.subpath(0, i));
			}
		}
		
		for(Action action : syncQueue) {
			executor.execute(action);
		}
	}
}
