package org.kahina.logic.sat.insertionmus.io;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.kahina.logic.sat.io.minisat.FreezeFile;

public class ResultReader {
	/**
	 * Reads the result of a searched map and saves the result ready for a freeze file.
	 */
	public static final void readAssignment(int[] res, File result) throws IOException{
		for (int i = res.length-1; i >= 0; i--){
			res[i] = FreezeFile.FREEZE;
		}
		FileReader fr = new FileReader(result);
		BufferedReader br = new BufferedReader(fr);
		br.readLine(); //First line can be discarded
		final int bufferSize = 1024;
		char[] cbuf = new char[bufferSize];
		int l = 0;

		do {
			l = br.read(cbuf);
			int index = 0;
			boolean pos = true;
			for (int i = 0; i < l; i++){
//				System.out.println(cbuf[i]);
				if (cbuf[i] == ' '){
					if (index == 0){
						return;
					}else if (index < res.length){
						if (pos){
							res[index] = FreezeFile.UNFREEZE;
						}else{
							res[index] = FreezeFile.FREEZE;
						}
					}
					index = 0;
					pos = true;
				}else if (cbuf[i] == '-'){
					pos = false;
				}
				else{
					index*=10;
					index += cbuf[i]-'0';
				}
			}
		}while(l >= bufferSize);
	}
}
