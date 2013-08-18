package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.DefaultFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.ICDCLFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.factorys.LearnMoreFactory;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.CDCL;
import org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main.ClauseSet;






public class benchmarkLearnMoreVsDefault {

	static Float timedefault = 0f;
	static Float timeLearnMore = 0f;
//	static final String path = "../cnf/SWGCP-allSat-scrambled-first1000";
	static final String path = "../cnf/10";
	public static void main(String[] arg0) throws IOException{
		System.in.read();
		FileFilter filter = new FileFilter(){
			@Override
			public boolean accept(File arg0) {
				//				return arg0.isFile() && arg0.getName().contains("aim");
				return arg0.isFile() ;
			}};
			timedefault = start(path, filter, new DefaultFactory(), "Default.txt");
			System.out.println();
			System.out.println("NEXT SOLVER");
			System.out.println();
			timeLearnMore = start(path, filter, new LearnMoreFactory(), "LearnMore.txt");

			System.out.println("Default total time: " + timedefault + "sec");
			System.out.println("Learn more total time: " + timeLearnMore + "sec");
	}


	public static float start(String directory, FileFilter filter, ICDCLFactory factory, String path) throws NumberFormatException, IOException {    

		File file = new File(directory + path);
		if (!file.exists()) file.createNewFile();
		
		FileReader fr = new FileReader(file);
		BufferedReader br = new BufferedReader(fr);

		FileWriter fw = new FileWriter(file, true);
		PrintWriter pw = new PrintWriter(fw);

		File dir = new File(directory);
		float time = 0;
		if (dir.isDirectory()){

			//Finde Alle Dateien im Verzeichnis.
			File[] fileArray = dir.listFiles(filter);

			//Starte fuer alle gefundenenn Dateien das Entsprechende Programm
			for (int i = 0; i < fileArray.length; i++){
				String line = br.readLine();
//				System.out.println(line);
				if(line != null && line.contains(fileArray[i].getName())){

					String[] split = line.split(" ");
					
					System.out.println(fileArray[i].getName() + " was skipped. (alrdy calculated) time was: " + split[1] + " summed time: " + time);
					
					time += Float.valueOf(split[1]);
				}else{
					//					instance.startInstance.start(fileArray[i], args);
					System.out.println(fileArray[i].getAbsolutePath());
					ClauseSet set = new ClauseSet( );
					set.pars(fileArray[i].getAbsolutePath());

					long start = System.currentTimeMillis();
					CDCL cdcl = factory.getCDCL(set);
					System.out.println(cdcl.solve(120000));
					long stop = System.currentTimeMillis();
					float localTime = ((stop - start)/1000f);
					System.out.println(localTime + " summed time: " + time);
					time += localTime;
					
					pw.println(fileArray[i].getName() + " " + localTime);
					pw.flush();
				}
			}

		}else{
			System.err.println(dir.getName() + " is not a directory.");
		}
		return time;
	}
}
