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
 * @LICENSE
 * 			FREE BEER LICENSE VERSION 1.02

The free beer license is a license to give free software to you and free
beer (in)to the author(s).

Your rights are :

0. You can use this piece of software in anyway you like.

1. You can redistribute this piece of software in source form or in 
   compiled form. 

2. You can alter the source to your needs and redistribute the altered 
   source in source form or in compiled form.

However :

0. This program is provided without warranty of any kind. So, if it 
   breaks anything, for example itself, it is up to you. 

1. If you redistribute this piece of software, you are not allowed to 
   charge money for this piece of software itself.

2. If you redistribute this pieces of software in binary form, you must 
   supply the source code as well.

3. If you redistribute this software, modified or not, you must 
   redistribute it under this license and you must include the name of 
   the original author(s) and you must point out where the original 
   source can be obtained.

4. If you use this piece of software frequently, and you think it is 
   worth a couple of euros, you are not allowed to send the author 
   anything else than beer or means that provide facilities to get beer 
   into the author(s) (i.e. openers, glasses).
   
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
