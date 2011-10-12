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
	
	public AuxiliaryTraleInstance()
	{
		//testing whether SICStus can be loaded (to be removed later)
		SICStus.main(new String[] {});
		try 
		{
			sp = new SICStus();
		    sp.load("/opt/trale2/startup.pl");
		    SPPredicate compileGramPred = new SPPredicate(sp, "compile_gram", 1, "");
		    //TODO: find a way to set the environment from inside this class
		    SPTerm pathTerm = new SPTerm(sp, "theory.pl");
		    SPQuery compileQuery = sp.openQuery(compileGramPred, new SPTerm[] { pathTerm });	      
		    while (compileQuery.nextSolution())
		    {
		    	System.out.println("Theory compiled, back in AuxiliaryTraleInstance!");
		    }	    
		    		    
		    //TEST: loading another instance of Kahina from inside the embedding instance
		    /*SPPredicate dgoPred = new SPPredicate(sp, "dgo", 0, "");
		    SPQuery dgoQuery = sp.openQuery(dgoPred, new SPTerm[] {});	      
		    while (dgoQuery.nextSolution())
		    {
		    	System.out.println("Embedded Kahina closed, back in AuxiliaryTraleInstance!");
		    }*/
		}
		catch ( Exception e )
		{
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args)
	{
		//the following environment is necessary for this demo to emulate the startup script:
		// * working directory set to where the files theory.pl and signature reside
		// * TRALE_HOME pointing to the root directory
		//for the embedding demo, the environment must be configured with
		// * TRALE_ACTIVATE_DEBUGGER set to true
		AuxiliaryTraleInstance trale = new AuxiliaryTraleInstance();
	}
}
