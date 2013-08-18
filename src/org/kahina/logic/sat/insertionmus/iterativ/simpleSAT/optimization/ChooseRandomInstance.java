package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.optimization;

import java.io.File;
import java.io.FileFilter;
import java.util.Random;

public class ChooseRandomInstance implements IChooseInstance {

	private static final int INSTANCES_FACTOR = 1;
	private static final int ANZ_INSTANCES = 4;
	private static Random r = new Random();

	@Override
	public void chooseInstances(String directory, int i) {

		File dir = new File(directory);

		if (dir.isDirectory()){

			//Finde Alle Dateien im Verzeichnis.
			File[] fileArray = dir.listFiles(new FileFilter(){
				@Override
				public boolean accept(File arg0) {
					return arg0.isFile() && arg0.getName().toLowerCase().endsWith(".cnf");
				}});
			int anz = ANZ_INSTANCES + INSTANCES_FACTOR*i;
			CDCLFitnessFunctionMultiThreaded.instances = new File[anz];

			for (int j = 0; j < anz; j++){
				 CDCLFitnessFunctionMultiThreaded.instances[j] = fileArray[r.nextInt(fileArray.length)];
			}

		}else{
			System.err.println(dir.getName() + " is not a directory.");
		}
	}
}
