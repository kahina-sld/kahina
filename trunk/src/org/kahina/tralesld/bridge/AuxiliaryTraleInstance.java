package org.kahina.tralesld.bridge;

import se.sics.jasper.*;

/**
 * This class uses the Jasper interface to run and maintain a second instance of TRALE.
 * This parallel instance is used to generate MGUs and MGSs for FSs on the workbench.
 * 
 * @author jdellert
 */

public class AuxiliaryTraleInstance 
{
	SICStus sp;
	
	/**
	 * crudely gains access to some SICStus instance (caller or new)
	 * severe problem with the embedded version: the caller instance does not allow
	 * access by this thread (it is occupied running Kahina), and no second instance is allowed
	 */
	public AuxiliaryTraleInstance(boolean newInstance)
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
			SPTerm pathTerm = new SPTerm(sp, "theory.pl");
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
		//TODO: generate theory file around descString
		//TODO: let the instance compile the theory and output MGS in GRISU format to temporary file
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
		trale.compileGrammar("theory.pl");
	}
}
