package storage.client.core.watcher;

import java.io.File;
import java.nio.file.*;
import java.util.Date;

import org.apache.commons.io.monitor.FileAlterationListener;
import org.apache.commons.io.monitor.FileAlterationMonitor;
import org.apache.commons.io.monitor.FileAlterationObserver;

@SuppressWarnings("unused")
public class CommonsWatcher {
	
	private final FileAlterationMonitor monitor;
	
	public CommonsWatcher(Path root) {
		
		
		// Change this to match the environment you want to watch.
        final File directory = new File(root.toString());
        FileAlterationObserver fao = new FileAlterationObserver(directory);
        fao.addListener(new FileAlterationListenerImpl());
        monitor = new FileAlterationMonitor();
        monitor.addObserver(fao);
	}
	
	public void start() {
		try {
			monitor.start();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void interrupt() {
		try {
			monitor.stop();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}


/**
*
* @author John Yeary
* @version 1.0
* @deprecated Use JNotifyWatcher instead
*/
@Deprecated
class FileAlterationListenerImpl implements FileAlterationListener {

   /**
    * {@inheritDoc}
    */
   @Override
   public void onStart(final FileAlterationObserver observer) {
       System.out.println("The WindowsFileListener has started on " + observer.getDirectory().getAbsolutePath());
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onDirectoryCreate(final File directory) {
       System.out.println(directory.getAbsolutePath() + " was created.");
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onDirectoryChange(final File directory) {
       System.out.println(directory.getAbsolutePath() + " wa modified");
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onDirectoryDelete(final File directory) {
       System.out.println(directory.getAbsolutePath() + " was deleted.");
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onFileCreate(final File file) {
       System.out.println(file.getAbsoluteFile() + " was created.");
       /*System.out.println("----------> length: " + file.length());
       System.out.println("----------> last modified: " + new Date(file.lastModified()));
       System.out.println("----------> readable: " + file.canRead());
       System.out.println("----------> writable: " + file.canWrite());
       System.out.println("----------> executable: " + file.canExecute());*/
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onFileChange(final File file) {
       System.out.println(file.getAbsoluteFile() + " was modified.");
       /*Sstem.out.println("----------> length: " + file.length());
       System.out.println("----------> last modified: " + new Date(file.lastModified()));
       System.out.println("----------> readable: " + file.canRead());
       System.out.println("----------> writable: " + file.canWrite());
       System.out.println("----------> executable: " + file.canExecute());*/
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onFileDelete(final File file) {
       System.out.println(file.getAbsoluteFile() + " was deleted.");
       
       /*
       if(file.isDirectory())
    	   System.out.println("\titz dir!");
       else if(file.isFile())
    	   System.out.println("\titz file!");
       else
    	   System.out.println("\titz eror!");
    	   */
   }

   /**
    * {@inheritDoc}
    */
   @Override
   public void onStop(final FileAlterationObserver observer) {
       System.out.println("The WindowsFileListener has stopped on " + observer.getDirectory().getAbsolutePath());
   }
}
