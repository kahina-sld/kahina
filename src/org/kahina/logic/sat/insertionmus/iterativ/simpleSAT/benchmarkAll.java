package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;




public class benchmarkAll {

	static float time = 0f;
	static final String path = "smallCNF/";
//	static final String path = "../cnf/test";
	public static void main(String[] arg0) throws IOException{
		System.in.read();
		start(path, new FileFilter(){
			@Override
			public boolean accept(File arg0) {
				return arg0.isFile();
			}});
		System.out.println("Total time: " + time + "sec");
	}


	public static void start(String args, FileFilter filter) throws NumberFormatException, IOException {    
		File f = new File(args);
		if (f.isDirectory()){

			//Finde Alle Dateien im Verzeichnis.
			File[] fileArray = f.listFiles(filter);

			//Starte fuer alle gefundenenn Dateien das Entsprechende Programm
			for (int i = 0; i < fileArray.length; i++){
				//					instance.startInstance.start(fileArray[i], args);
				System.out.println(fileArray[i].getAbsolutePath());
				ClauseSet set = new ClauseSet();
				set.pars(fileArray[i].getAbsolutePath() );
				
				long start = System.currentTimeMillis();
//				CDCL cdcl = new CDCL(set, new RandomHeapActivity(set), new UnitPropagation(set), new LearnMoreAnalyseConflict());
				CDCL cdcl = new CDCL(set);
				System.out.println(cdcl.solve(120000));
				long stop = System.currentTimeMillis();
				time += ((stop - start)/1000f);
			}

		}else{
			System.err.println(f.getName() + " is not a directory.");
		}
	}
}
