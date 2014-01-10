package storage.client.core.indexer;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.nio.file.attribute.*;

import storage.client.core.action.FileAction;


/**
* Collection of all paths in root subtree. Designed to work with JNotifyWatcher
* @author Michal
* TODO extract interface
*/
public class JNotifyIndexer implements Indexer {
	
	private final Path root;			// mointpoint of the filesystem
	private Set<Path> files;			// all inxedex paths are relative to the root
	private Set<Path> dirs;
	
	private List<FileAction> actions;	// queue, random access needed internally
	
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
	
	
	public JNotifyIndexer(Path mountPoint) throws IOException {
		root = mountPoint;
		files = new HashSet<>();
		dirs = new HashSet<>();
		actions = new ArrayList<>();
	}
	
	public synchronized void insert(Path path) { //throws InvalidFileException {
		
		Path absolutePath = root.resolve(path);
		Path relativePath = root.relativize(absolutePath);
		
		File file = absolutePath.toFile();
		
		if(file.isFile())       files.add(relativePath);
		else if(file.isDirectory()) dirs.add(relativePath);
		
		//else throw new InvalidFileException(absolutePath.toString());
	}
	
	public synchronized void remove(Path path) {
		files.remove(path);
		dirs.remove(path);
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
		
		files.clear();
		dirs.clear();
		
		Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
			
			@Override
			public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) {
				files.add(root.relativize(path));
				return FileVisitResult.CONTINUE;
			}
			
			@Override
			public FileVisitResult preVisitDirectory(Path path, BasicFileAttributes attrs) throws IOException {
				dirs.add(root.relativize(path));
				return super.preVisitDirectory(path, attrs);
			}
		});
	}
	
	private void offer(FileAction action) {
		
		// reducing
		boolean reduced = false;
		for(int i=actions.size()-1; i>=0; --i) {
			if(actions.get(i).reducedBy(action)) {
				actions.set(i, actions.get(i).reduceWith(action));
				reduced = true;
				break;
			}
		}
		if(!reduced)
			actions.add(action);
	}

	@Override
	public synchronized FileAction poll() {
		
		while(!actions.isEmpty())
			try { this.wait(); }
			catch(InterruptedException e) { }

		return actions.remove(0);
	}
}
