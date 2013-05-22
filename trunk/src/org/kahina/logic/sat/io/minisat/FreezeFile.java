package org.kahina.logic.sat.io.minisat;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class FreezeFile {


	public static final int UNFREEZE = 1;
	public static final int FREEZE = -1;
	
	/**
	 * Creates a FreezeFile that also sets all variables in 'unitClauses' to their inverted values
	 */
	public static void createFreezeFile(int[] freezeVariables, File freezeFile, int offsetID, List<Integer> unitClauses){


		StringBuffer freezeBuffer = new StringBuffer("");
		for (int l: unitClauses){
			freezeBuffer.append(-1*l +  " ");
		}
		writeFreezeVariables(freezeVariables, offsetID, freezeBuffer);
		try
		{
			BufferedWriter out = new BufferedWriter(new FileWriter(freezeFile));
			out.write("" + freezeBuffer + " 0\n");
			out.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.err.println("IO error: failed to create temporary freeze file");
		}
	}

	public static void createFreezeFile(int[] freezeVariables, File freezeFile, int offsetID)
	{
		StringBuffer freezeBuffer = new StringBuffer("");
		writeFreezeVariables(freezeVariables, offsetID, freezeBuffer);
		try
		{
			BufferedWriter out = new BufferedWriter(new FileWriter(freezeFile));
			out.write("" + freezeBuffer + " 0\n");
			out.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
			System.err.println("IO error: failed to create temporary freeze file");
		}
	}

	/**
	 * Writes the freezeVariables intto the freezeBuffer
	 * @param freezeVariables 
	 * @param offsetID starting ID for the freeze variable
	 * @param freezeBuffer
	 */
	private static void writeFreezeVariables(int[] freezeVariables,
			int offsetID, StringBuffer freezeBuffer) {
		for (int i = 0; i < freezeVariables.length; i++)
		{
			if (freezeVariables[i] == FREEZE)
			{
				if (i < (freezeVariables.length - 1))
				{
					freezeBuffer.append("-" + (offsetID + i) + " ");
				}
				else
				{
					freezeBuffer.append("-" + (offsetID + i));
				}
			}
			else if (freezeVariables[i] == UNFREEZE)
			{
				if (i < (freezeVariables.length - 1))
				{
					freezeBuffer.append("" + (offsetID + i) + " ");
				}
				else
				{
					freezeBuffer.append("" + (offsetID + i));
				}
			}
		}
	}

}
