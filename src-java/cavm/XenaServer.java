package cavm;

import java.io.File;

public interface XenaServer {
	boolean load(File file);
	String publicUrl();
	String localUrl();
}
