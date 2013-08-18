package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;





public class testAll {
	protected FileFilter filter = new FileFilter(){
		@Override
		public boolean accept(File pathname) {
			return true;
		}};


		static final String path = "../cnf/";
		public static void main(String[] arg0) throws IOException{
			start(path, new FileFilter(){
				@Override
				public boolean accept(File arg0) {
					return arg0.getName().contains("yes");
				}});
			
			System.in.read();
			
			start(path, new FileFilter(){
				@Override
				public boolean accept(File arg0) {
					return arg0.getName().contains("no");
				}});
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
					set.pars(fileArray[i].getAbsolutePath());
					CDCL cdcl = new CDCL(set);
					System.out.println(cdcl.solve(120000));

				}

			}else{
				System.err.println(f.getName() + " is not a directory.");
			}
		}
}
