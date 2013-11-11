package org.kahina.logic.sat.insertionmus.iterativ.simpleSAT.main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
 * A Class to Pars a DIMACS-File
 * The File will be converted into my Classes Clause and ContentTable
 * 
 
 * @author Paul Seitz
 *
 */

public class Parser {

	
	private final BufferedReader br;
	protected final Integer anzVariables;
	protected final Integer anzClauses;

	public Parser(String file) throws IOException{
		this.br = new BufferedReader(new FileReader(new File(file)));

		//readHeader
		String line; 
		while(! (line = br.readLine()).startsWith("p cnf")){}
		
		//the last line is splitted into 4 parts. The 3rd part is the number of variables in this instance.
		String[] splittedLine = line.split(" ");

		this.anzClauses = Integer.valueOf(splittedLine[3]);
		this.anzVariables = Integer.valueOf(splittedLine[2]);
	}


	/**
	 * @return the next Vector of Integer if the file has another line. else it returns null
	 * @throws IOException 
	 * @throws NumberFormatException 
	 */
	public final ArrayList<Integer> next() throws NumberFormatException, IOException {
		
		String line;
		if (!((line = br.readLine()) == null)){
			final String[] splittedLine = line.split("\\s+");
			final int length = splittedLine.length-1;
			final ArrayList<Integer> ret = new ArrayList<Integer>(length);
			
			
			for (int i = 0; i < length; i++){
				if (splittedLine[i].length() > 0){
					final int val = Integer.parseInt(splittedLine[i]);
					if (!ret.contains(val))
						ret.add(val);
				}
			}
			return ret;
		}
		return null;
	}
}
