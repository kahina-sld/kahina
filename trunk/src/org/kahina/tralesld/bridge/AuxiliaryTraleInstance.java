package org.kahina.tralesld.bridge;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
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
	
	TraleTask task;
	
	/**
	 * Crudely gains access to some SICStus instance (caller or new) and stores it for operations.
	 * Creating a second instance requires the environment variable PROLOGMAXSIZE to be set
	 */
	public AuxiliaryTraleInstance(boolean newInstance)
	{
		this.newInstance = newInstance;
		this.setName("AuxiliaryTraleThread");
		
		this.task = new TraleTask();
	}
	
	@Override
	public void run() 
	{
		try 
		{
			sp = SICStus.getCaller();
			if (sp == null || newInstance)
			{
				sp = new SICStus();
			}
		    sp.load("/opt/trale2/startup.pl");
			compileTrivialGrammar();
		    while (true)
		    {
			    synchronized(task)
			    {
			    	while (task.getInstruction() == null)
			    	{
				    	try
				    	{
				    		task.wait();
				    	}
				    	catch (InterruptedException e)
				    	{
				    		
				    	}
			    	}
			    	if (task.getInstruction().equals("mgs"))
			    	{
			    		task.setResult(executeMGS(task.getToProcess()));
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("compile"))
			    	{
			    		task.setResult(compileTraleGrammar(task.getToProcess()) + "");
			    		task.setInstruction(null);
			    		task.notify();
			    	}
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
		synchronized(task)
		{
			task.setInstruction("compile");
			task.setToProcess(fileName);
			task.notify();
	    	try
	    	{
	    		task.wait();
	    	}
	    	catch (InterruptedException e)
	    	{
	    		
	    	} 	
	    	return task.getResult().equals("true");
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
		synchronized(task)
		{
			task.setInstruction("mgs");
			task.setToProcess(descString);
			task.notify();
	    	try
	    	{
	    		task.wait();
	    	}
	    	catch (InterruptedException e)
	    	{
	    		
	    	} 	
	    	return task.getResult();
		}
		
		//stub behavior for now: return GRISU string for trivial structure
		//return "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
	}
	
	private boolean compileTrivialGrammar()
	{
		try 
		{
			//abolish *> clauses to avoid Prolog-side warning message when a new grammar is compiled
			//TODO: see whether this can be done automatically when executing compile_gram/0
			SPPredicate abolishPred = new SPPredicate(sp, "abolish_user_preds", 1, "");
			SPTerm consTerm = new SPTerm(sp, "cons");
			SPQuery abolishQuery = sp.openQuery(abolishPred, new SPTerm[] { consTerm });	      
			while (abolishQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance discarded old *> database.");
			}
			//generate empty theory file (against signature file in current dir)
			String theoryString = "signature(signature).\n";
			try
			{
				FileWriter writer = new FileWriter(new File("empty_theory.pl"));
				writer.append(theoryString);
				writer.flush();
			}
			catch (IOException e)
			{
				System.err.println("WARNING: could not create empty theory file!");
			}
			SPPredicate compileGramPred = new SPPredicate(sp, "compile_gram", 1, "");
			//TODO: find a way to set the environment from inside this class
			SPTerm pathTerm = new SPTerm(sp, "empty_theory");
			SPQuery compileQuery = sp.openQuery(compileGramPred, new SPTerm[] { pathTerm });	      
			while (compileQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance compiled empty theory.");
			}
			return true;
		}
		catch ( Exception e )
		{
			e.printStackTrace();
			return false;
		}
	}
	
	private boolean compileTraleGrammar(String fileName)
	{
		try 
		{
			//abolish *> clauses to avoid Prolog-side warning message when a new grammar is compiled
			//TODO: see whether this can be done automatically when executing compile_gram/0
			SPPredicate abolishPred = new SPPredicate(sp, "abolish_user_preds", 1, "");
			SPTerm consTerm = new SPTerm(sp, "cons");
			SPQuery abolishQuery = sp.openQuery(abolishPred, new SPTerm[] { consTerm });	      
			while (abolishQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance discarded old *> database.");
			}
			SPPredicate compileGramPred = new SPPredicate(sp, "compile_gram", 1, "");
			//TODO: find a way to set the environment from inside this class
			SPTerm pathTerm = new SPTerm(sp, fileName);
			SPQuery compileQuery = sp.openQuery(compileGramPred, new SPTerm[] { pathTerm });	      
			while (compileQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance compiled theory.");
			}
			return true;
		}
		catch ( Exception e )
		{
			e.printStackTrace();
			return false;
		}
	}
	
	//new version (does not compile anything, always uses the current theory)
	//does not work because SPTerm can only be constructed out of atoms, not out of strings
	/*private String executeMGS(String descString)
	{
		//handle the atomic values (such as "cruel") that TRALE refuses to accept as part of a description
		//FUTURE SOLUTION: instead of just "cruel", GraleJ should produce (a_ cruel)
		//HACK FOR NOW: manipulate the phon value accordingly
		//TODO: extend this to more than one list element
		int phonPosition = descString.indexOf("phon");
		if (phonPosition != -1)
		{
			int leftOfList = descString.indexOf("[",phonPosition);
			descString = descString.substring(0, leftOfList + 1) + "(a_ " + descString.substring(leftOfList + 1);
			int rightOfList = descString.indexOf("]",phonPosition);
			descString = descString.substring(0, rightOfList) + ")" + descString.substring(rightOfList);
		}
		//let TRALE output MGS in GRISU format to temporary file
		try
		{
			SPPredicate mgsPred = new SPPredicate(sp, "mgs_to_tempfile", 2, "");
			SPTerm descTerm = new SPTerm(sp, descString);
			SPTerm fileNameTerm = new SPTerm(sp, "grisu.tmp");
			SPQuery mgsQuery = sp.openQuery(mgsPred, new SPTerm[] { descTerm, fileNameTerm });	      
			if (mgsQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance stored MGS in temporary file.");
			}
		}
		catch (SPException e)
		{
			System.err.println("SPException: " + e.toString());
			e.printStackTrace();
			//TODO: useful error handling
			return "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
		}
		//read in temporary file to retrieve GRISU string
		String grisu = null;
		try
		{
			grisu = slurpFile("grisu.tmp");
		}
		catch (IOException e)
		{
			System.err.println("Could not read grisu.tmp! Returning default GRISU string!");
			//stub behavior for now: return GRISU string for trivial structure
			grisu = "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
		}
		return grisu;
	}*/
	
	//old version, being superseded at the moment
	private String executeMGS(String descString)
	{
		//handle the atomic values (such as "cruel") that TRALE refuses to accept as part of a description
		//FUTURE SOLUTION: instead of just "cruel", GraleJ should produce (a_ cruel)
		//HACK FOR NOW: manipulate the phon value accordingly
		//TODO: extend this to more than one list element
		int phonPosition = descString.indexOf("phon");
		if (phonPosition != -1)
		{
			int leftOfList = descString.indexOf("[",phonPosition);
			descString = descString.substring(0, leftOfList + 1) + "(a_ " + descString.substring(leftOfList + 1);
			int rightOfList = descString.indexOf("]",phonPosition);
			descString = descString.substring(0, rightOfList) + ")" + descString.substring(rightOfList);
		}
		//generate theory file around descString
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
		//let the instance compile the theory
		compileTraleGrammar("aux_theory.pl");
		//let TRALE output MGS in GRISU format to temporary file
		try
		{
			SPPredicate mgsPred = new SPPredicate(sp, "mgs_to_tempfile", 2, "");
			SPTerm descTerm = new SPTerm(sp, "sign");
			SPTerm fileNameTerm = new SPTerm(sp, "grisu.tmp");
			SPQuery mgsQuery = sp.openQuery(mgsPred, new SPTerm[] { descTerm, fileNameTerm });	      
			if (mgsQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance stored MGS in temporary file.");
			}
		}
		catch (SPException e)
		{
			System.err.println("SPException: " + e.toString());
			e.printStackTrace();
			//TODO: useful error handling
			return "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
		}
		//read in temporary file to retrieve GRISU string
		String grisu = null;
		try
		{
			grisu = slurpFile("grisu.tmp");
		}
		catch (IOException e)
		{
			System.err.println("Could not read grisu.tmp! Returning default GRISU string!");
			//stub behavior for now: return GRISU string for trivial structure
			grisu = "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
		}
		return grisu;
	}
	
	private String slurpFile(String file) throws IOException 
	{
	    BufferedReader reader = new BufferedReader( new FileReader (file));
	    String line  = null;
	    StringBuilder stringBuilder = new StringBuilder();
	    String ls = System.getProperty("line.separator");
	    while( ( line = reader.readLine() ) != null ) {
	        stringBuilder.append( line );
	        stringBuilder.append( ls );
	    }
	    return stringBuilder.toString();
	 }

	
	public static void main(String[] args)
	{
		//the following environment is necessary for this demo to emulate the startup script:
		// * working directory set to where the files theory.pl and signature reside
		// * TRALE_HOME pointing to the Trale root directory
		AuxiliaryTraleInstance trale = new AuxiliaryTraleInstance(true);
		trale.start();
		trale.compileGrammar("theory");
	}
	
	private class TraleTask
	{
		String instruction = null;
		String toProcess = null;
		String result = null;
		
		public String getInstruction() 
		{
			return instruction;
		}
		
		public void setInstruction(String instruction) 
		{
			this.instruction = instruction;
		}
		
		public String getToProcess() 
		{
			return toProcess;
		}
		
		public void setToProcess(String toProcess) 
		{
			this.toProcess = toProcess;
		}
		
		public String getResult() 
		{
			return result;
		}
		
		public void setResult(String result) 
		{
			this.result = result;
		}	
	}
}
