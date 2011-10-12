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
		    //TODO: find out how TRALE handles paths, help it to find the signature
		    /*SPTerm pathTerm = new SPTerm(sp, "/home/kahina/pro/test/nomi/theory.pl");
		    SPQuery compileQuery = sp.openQuery(compileGramPred, new SPTerm[] { pathTerm });	      
		    while (compileQuery.nextSolution())
		    {
		    	System.out.println("Theory compiled, back in AuxiliaryTraleInstance!");
		    }*/
		    SPPredicate dgoPred = new SPPredicate(sp, "dgo", 0, "");
		    SPQuery dgoQuery = sp.openQuery(dgoPred, new SPTerm[] {});	      
		    while (dgoQuery.nextSolution())
		    {
		    	System.out.println("Embedded Kahina closed, back in AuxiliaryTraleInstance!");
		    }
		}
		catch ( Exception e )
		{
			e.printStackTrace();
		}
	}
	
	public static void main(String[] args)
	{
		AuxiliaryTraleInstance trale = new AuxiliaryTraleInstance();
	}
}
