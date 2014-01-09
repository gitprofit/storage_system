package storage.client.core;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.nio.file.attribute.*;


/**
* Collection of all paths in root subtree.
* @author Michal
*/
public class Index {
	
	private final Path root;		// mointpoint of the filesystem
	private final Set<Path> files;	// all inxedex paths are relative to the root
	private final Set<Path> dirs;
	
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
	
	
	public Index(Path mountPoint) throws IOException {
		root = mountPoint;
		files = new HashSet<>();
		dirs = new HashSet<>();
	}
	
	public synchronized void insert(Path path) throws InvalidFileException {
		
		Path absolutePath = root.resolve(path);
		Path relativePath = root.relativize(absolutePath);
		
		File file = absolutePath.toFile();
		
		if(file.isFile()) files.add(relativePath);
		else if(file.isDirectory()) dirs.add(relativePath);
		else throw new InvalidFileException(absolutePath.toString());
	}
	
	public synchronized void remove(Path path) {
		files.remove(path);
		dirs.remove(path);
	}
	
	public synchronized Set<Path> removeSubtreeFiles(Path path) {
		
		Set<Path> res = new HashSet<>();
		
		for(Path file : files)
			if(file.startsWith(path))
				res.add(file);
		
		for(Path file : res)
			files.remove(file);
		
		return Collections.unmodifiableSet(res);
	}
	
	public synchronized boolean isFile(Path path) {
		return files.contains(path);
	}
	
	public synchronized boolean isDirectory(Path path) {
		return dirs.contains(path);
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
	
	private Path relativize(Path path) {
		return null;
	}
}
