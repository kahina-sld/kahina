package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.optimization;

import java.io.File;
import java.io.FileFilter;

public class ChooseInstance implements IChooseInstance {
	
	ChooseInstance(String directory){
		File dir = new File(directory);

		if (dir.isDirectory()){

			//Finde Alle Dateien im Verzeichnis.
			File[] fileArray = dir.listFiles(new FileFilter(){
				@Override
				public boolean accept(File arg0) {
					return arg0.isFile() && arg0.getName().toLowerCase().endsWith(".cnf");
				}});

			CDCLFitnessFunctionMultiThreaded.instances = fileArray;
		}
	}

	@Override
	public void chooseInstances(String directory, int i) {

	}

}
