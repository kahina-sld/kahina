package org.kahina.tralesld.bridge;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.SwingUtilities;

import se.sics.jasper.*;

/**
 * This class uses the Jasper interface to run and maintain a second instance of TRALE.
 * This parallel instance is used to generate MGUs and MGSs for FSs on the workbench.
 * 
 * @author jdellert
 */

public class AuxiliaryTraleInstance extends Thread
{
	SICStus sp;
	boolean newInstance;
	
	String instruction;
	String toProcess;
	String result;
	
	/**
	 * Crudely gains access to some SICStus instance (caller or new) and stores it for operations.
	 * Creating a second instance requires the environment variable PROLOGMAXSIZE to be set
	 */
	public AuxiliaryTraleInstance(boolean newInstance)
	{
		this.newInstance = newInstance;
		this.setName("AuxiliaryTraleThread");
		
		this.instruction = "";
		this.toProcess = "";
		this.result = "";
	}
	
	@Override
	public void run() 
	{
		//testing whether SICStus can be loaded (to be removed later)
		//SICStus.main(new String[] {});
		try 
		{
			sp = SICStus.getCaller();
			if (sp == null || newInstance)
			{
				sp = new SICStus();
			}
		    sp.load("/opt/trale2/startup.pl");
		    synchronized(instruction)
		    {
		    	try
		    	{
		    		instruction.wait();
		    	}
		    	catch (InterruptedException e)
		    	{
		    		
		    	}
		    	
		    	if (instruction.equals("mgs"))
		    	{
		    		result = executeMGS(toProcess);
		    		instruction = null;
		    		result.notify();
		    	}
		    }
		}
		catch ( Exception e )
		{
			e.printStackTrace();
		}	
	}
	
	/**
	 * Lets the instance compile a grammar in the current working directory.
	 * @param fileName the file name
	 * @return true if compilation succeeded, false in case of exception
	 */
	public boolean compileGrammar(String fileName)
	{
		try 
		{
			SPPredicate compileGramPred = new SPPredicate(sp, "compile_gram", 1, "");
			//TODO: find a way to set the environment from inside this class
			SPTerm pathTerm = new SPTerm(sp, fileName);
			SPQuery compileQuery = sp.openQuery(compileGramPred, new SPTerm[] { pathTerm });	      
			while (compileQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance compiled theory!");
			}
			return true;
		}
		catch ( Exception e )
		{
			e.printStackTrace();
			return false;
		}
	}
	
	/**
	 * For testing purposes: Loads another instance of Kahina inside the embedding instance.
	 * The environment must be configured with TRALE_ACTIVATE_DEBUGGER set to true.
	 */
	public void loadEmbeddedKahinaInstance()
	{
		try
		{
			SPPredicate dgoPred = new SPPredicate(sp, "dgo", 0, "");
		    SPQuery dgoQuery = sp.openQuery(dgoPred, new SPTerm[] {});	      
		    while (dgoQuery.nextSolution())
		    {
		    	System.err.println("Embedded Kahina closed, back in AuxiliaryTraleInstance!");
		    }
		}
		catch (Exception e)
		{
			System.err.println("Failed to load an embedded Kahina instance.");
			e.printStackTrace();
		}
	}
	
	public String descToMgsGrisu(String descString)
	{
		synchronized(instruction)
		{
			instruction = "mgs";
			toProcess = descString;
			instruction.notify();
		}
		
	    synchronized(result)
	    {
	    	try
	    	{
	    		result.wait();
	    	}
	    	catch (InterruptedException e)
	    	{
	    		
	    	} 	
	    	return result;
	    }
		//stub behavior for now: return GRISU string for trivial structure
		//return "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
	}
	
	private String executeMGS(String descString)
	{
		//generate theory file around descString
		//TODO: clean out the atomic values that TRALE refuses to accept as part of the signature
		String theoryString = "sign *> " + descString + ".";
		try
		{
			FileWriter writer = new FileWriter(new File("aux_theory.pl"));
			writer.append(theoryString);
			writer.flush();
		}
		catch (IOException e)
		{
			System.err.println("WARNING: could not create auxiliary theory file!");
		}
		//TODO: let the instance compile the theory and output MGS in GRISU format to temporary file
		compileGrammar("aux_theory.pl");

		//TODO: read in temporary file to retrieve GRISU string
		//stub behavior for now: return GRISU string for trivial structure
		return "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
	}
	
	public static void main(String[] args)
	{
		//the following environment is necessary for this demo to emulate the startup script:
		// * working directory set to where the files theory.pl and signature reside
		// * TRALE_HOME pointing to the root directory
		AuxiliaryTraleInstance trale = new AuxiliaryTraleInstance(true);
		trale.start();
		try
		{
			trale.join();
		}
		catch (InterruptedException e)
		{
			
		}
		trale.compileGrammar("theory.pl");
	}
}
