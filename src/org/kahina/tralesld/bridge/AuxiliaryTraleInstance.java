package org.kahina.tralesld.bridge;

import gralej.om.IAny;
import gralej.om.IEntity;
import gralej.om.IFeatureValuePair;
import gralej.om.IList;
import gralej.om.ITag;
import gralej.om.IType;
import gralej.om.ITypedFeatureStructure;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.regex.Pattern;

import javax.swing.SwingUtilities;

import org.kahina.core.io.util.FileUtil;
import org.kahina.tralesld.data.signature.TraleSLDSignature;
import org.kahina.tralesld.visual.fs.GraleJUtility;

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
			sp.load(System.getenv("TRALE_HOME") + "/startup.pl");
		    //compileTrivialGrammar();
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
			    	else if (task.getInstruction().equals("tmgs"))
			    	{
			    		task.setResult(executeTheoryMGS(task.getEntity1()));
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("tmgu"))
			    	{
			    		task.setResult(executeTheoryMGU(task.getEntity1(), task.getEntity2()));
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("lex"))
			    	{
			    		task.setResult(executeLex(task.getToProcess()));
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("signature"))
			    	{
			    		task.setSignatureResult(extractSignature(task.getToProcess()));
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("compile"))
			    	{
			    		task.setResult(compileTraleGrammar(task.getToProcess()) + "");
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("discard"))
			    	{
			    		discardTraleGrammar();
			    		task.setInstruction(null);
			    		task.notify();
			    	}
			    	else if (task.getInstruction().equals("lemmata"))
			    	{
			    		task.setResult(extractLemmata());
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
	 * Lets the instance discard the currently compiled grammar.
	 */
	public void discardGrammar()
	{
		synchronized(task)
		{
			task.setInstruction("discard");
			task.notify();
	    	try
	    	{
	    		task.wait();
	    	}
	    	catch (InterruptedException e)
	    	{
	    		
	    	}
	    	return;
		}
	}
	
	/**
	 * Returns all the lemmata defined in the current theory.
	 * @return a String of lemma strings separated by colons
	 */
	public String getLemmata()
	{
		synchronized(task)
		{
			task.setToProcess(null);
			task.setInstruction("lemmata");
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
	}
	
	/**
	 * Lets the instance compile and return a signature from a file.
	 * @param fileName the file name
	 * @return a TraleSLDSignature object representing the signature in the file.
	 */
	public TraleSLDSignature getSignature(String fileName)
	{
		synchronized(task)
		{
			task.setToProcess(fileName);
			task.setInstruction("signature");
			task.notify();
	    	try
	    	{
	    		task.wait();
	    	}
	    	catch (InterruptedException e)
	    	{
	    		
	    	} 	
	    	return task.getSignatureResult();
		}
	}
	
	/**
	 * Reads out the current signature (usually after theory compilation).
	 * @return a TraleSLDSignature object representing the current signature;
	 */
	public TraleSLDSignature getCurrentSignature()
	{
		return getSignature(null);
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
	
	public String lexEntryGrisu(String lemma)
	{
		synchronized(task)
		{
			task.setInstruction("lex");
			task.setToProcess(lemma);
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
	
	public String entToMgsGrisu(IEntity ent)
	{
		synchronized(task)
		{
			task.setInstruction("tmgs");
			task.setEntity1(ent);
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
	}
	
	public String entsToMguGrisu(IEntity ent1, IEntity ent2)
	{
		synchronized(task)
		{
			task.setInstruction("tmgu");
			task.setEntity1(ent2);
			task.setEntity2(ent1);
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
	}
	
	private TraleSLDSignature extractSignature(String fileName)
	{
		//we can retrieve the current signature by handing over a null argument
		if (fileName != null)
		{
			//let TRALE compile the signature
			try 
			{
				//abolish sig clauses to avoid Prolog-side warning message when a new signature is compiled
				//TODO: see whether this can be done automatically when executing compile_sig/1
				SPPredicate abolishPred = new SPPredicate(sp, "abolish_user_preds", 1, "");
				SPTerm consTerm = new SPTerm(sp, "sig");
				SPQuery abolishQuery = sp.openQuery(abolishPred, new SPTerm[] { consTerm });	      
				while (abolishQuery.nextSolution())
				{
					System.err.println("AuxiliaryTraleInstance discarded old sig database.");
				}
				SPPredicate compileSigPred = new SPPredicate(sp, "compile_sig", 1, "");
				//TODO: find a way to set the environment from inside this class
				SPTerm pathTerm = new SPTerm(sp, fileName);
				SPQuery compileQuery = sp.openQuery(compileSigPred, new SPTerm[] { pathTerm });	      
				while (compileQuery.nextSolution())
				{
					System.err.println("AuxiliaryTraleInstance compiled signature.");
				}
			}
			catch ( Exception e )
			{
				e.printStackTrace();
			}
		}
		//construct the TraleSLDSignature object by reading out the compiled signature
		TraleSLDSignature signature = new TraleSLDSignature();
		LinkedList<String> agenda = new LinkedList<String>();
		agenda.add("bot");
		while (agenda.size() > 0)
		{
			String type = agenda.removeFirst();
			List<String> subtypes = immediateSubtypes(type);
			for (String subtype : subtypes)
			{
				//do not process atoms of form a_(_NUM)
				if (!isAtomicTypeRepresentation(subtype))
				{
					signature.addSubtypeRelation(type, subtype);
				}	
			}
			agenda.addAll(subtypes);
			List<String> feats = appropriateFeatures(type);
			for (String feat : feats)
			{
				signature.addAppropriateFeature(type, feat, valueRestriction(type,feat));
			}
		}
		return signature;
	}
	
	//recognize Trale-specific format for types from the atom hierarchy
	private boolean isAtomicTypeRepresentation(String type)
	{
		return Pattern.matches("a_\\(_[0-9]+\\)", type);
	}
	
	private String extractLemmata()
	{
		String lemmata = "";
		try
		{
			SPPredicate subtypePred = new SPPredicate(sp, "lex", 2, "");
			SPTerm lemmaVar = new SPTerm(sp).putVariable();
			SPTerm irrelevantVar = new SPTerm(sp).putVariable();
			SPQuery subtypeQuery = sp.openQuery(subtypePred, new SPTerm[] { lemmaVar, irrelevantVar });	
			while (subtypeQuery.nextSolution())
			{
				lemmata += lemmaVar.toString() + ":";
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return lemmata;
	}
	
	private List<String> immediateSubtypes(String type)
	{
		LinkedList<String> subtypes = new LinkedList<String>();
		try
		{
			SPPredicate subtypePred = new SPPredicate(sp, "immed_subtypes", 2, "");
			SPTerm typeTerm = new SPTerm(sp, type);
			SPTerm subtypeVar = new SPTerm(sp).putVariable();
			SPQuery subtypeQuery = sp.openQuery(subtypePred, new SPTerm[] { typeTerm, subtypeVar });	
			while (subtypeQuery.nextSolution())
			{
				if (!subtypeVar.toString().equals("[]"))
				{
					SPTerm[] subtypeTerms = subtypeVar.toTermArray();
					for (SPTerm term : subtypeTerms)
					{
						subtypes.add(term.toString());
					}
				}
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return subtypes;
	}
	
	private List<String> appropriateFeatures(String type)
	{
		LinkedList<String> feats = new LinkedList<String>();
		try
		{
			SPPredicate featsPred = new SPPredicate(sp, "approp_feats", 2, "");
			SPTerm typeTerm = new SPTerm(sp, type);
			SPTerm featsVar = new SPTerm(sp).putVariable();
			SPQuery featsQuery = sp.openQuery(featsPred, new SPTerm[] { typeTerm, featsVar });	
			while (featsQuery.nextSolution())
			{
				if (!featsVar.toString().equals("[]"))
				{
					SPTerm[] featTerms = featsVar.toTermArray();
					for (SPTerm feat : featTerms)
					{
						feats.add(feat.toString());
					}
				}
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return feats;
	}
	
	private String valueRestriction(String type, String feat)
	{
		String restr = "bot";
		try
		{
			SPPredicate restrPred = new SPPredicate(sp, "approp", 3, "");
			SPTerm featTerm = new SPTerm(sp, feat);
			SPTerm typeTerm = new SPTerm(sp, type);
			SPTerm restrVar = new SPTerm(sp).putVariable();
			SPQuery restrQuery = sp.openQuery(restrPred, new SPTerm[] { featTerm, typeTerm, restrVar });	
			while (restrQuery.nextSolution())
			{
				restr = restrVar.toString();
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return restr;
	}
	
	private boolean compileTraleGrammar(String fileName)
	{
		try 
		{
			//abolish *> clauses to avoid Prolog-side warning message when a new grammar is compiled
			//TODO: see whether this can be done automatically when executing compile_gram/1
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
	
	private void discardTraleGrammar()
	{
		try 
		{
			//abolish *> clauses to remove the compiled grammar information from the instance
			SPPredicate abolishPred = new SPPredicate(sp, "abolish_user_preds", 1, "");
			SPTerm consTerm = new SPTerm(sp, "cons");
			SPQuery abolishQuery = sp.openQuery(abolishPred, new SPTerm[] { consTerm });	      
			while (abolishQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance discarded old *> database.");
			}
		}
		catch ( Exception e )
		{
			e.printStackTrace();
		}
	}
	
	private String executeLex(String lemma)
	{
		//let TRALE output MGS in GRISU format to temporary file
		try
		{
			SPPredicate mgsPred = new SPPredicate(sp, "lex_to_tempfile", 2, "");
			SPTerm descTerm = new SPTerm(sp, lemma);
			SPTerm fileNameTerm = new SPTerm(sp, "lex.grisu");
			SPQuery mgsQuery = sp.openQuery(mgsPred, new SPTerm[] { descTerm, fileNameTerm });	      
			if (mgsQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance stored lex entry in temporary file.");
			}
			else
			{
				return "ERROR: Lexical entry not found!";
			}
		}
		catch (SPException e)
		{
			return "ERROR: SPException " + e.toString();
		}
		//read in temporary file to retrieve GRISU string
		String grisu = null;
		try
		{
			grisu = FileUtil.slurpFile("lex.grisu");
		}
		catch (IOException e)
		{
			grisu = "ERROR: Could not read lex.grisu!";
		}
		return grisu;
	}
	
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
			if (leftOfList != -1)
			{
				descString = descString.substring(0, leftOfList + 1) + "(a_ " + descString.substring(leftOfList + 1);
				int rightOfList = descString.indexOf("]",phonPosition);
				descString = descString.substring(0, rightOfList) + ")" + descString.substring(rightOfList);
			}
		}
		//HACK: extract head type from the description (necessary for theory)
		//      (alternatively, one could hand over the type as a second argument)
		//		(but that would mean extracting and handing over information twice
		//cannot use "bot" as default because Trale refuses to accept constraints on bot
		String typeString = "sign"; 
		int commaPosition = descString.indexOf(",");
		if (commaPosition != -1)
		{
			typeString = descString.substring(0,commaPosition);
		}
		else
		{
			typeString = descString;
		}
		if (typeString.startsWith("(")) typeString = typeString.substring(1);
		if (typeString.endsWith(")")) typeString = typeString.substring(0,typeString.length() - 1);
		//generate theory file around descString
		String theoryString = typeString + " *> " + descString + ".";
		try
		{
			FileWriter writer = new FileWriter(new File("aux_theory.pl"));
			writer.append(theoryString);
			writer.flush();
		}
		catch (IOException e)
		{
			return "ERROR: Could not create aux_theory.pl!";
		}
		//let the instance compile the theory
		compileTraleGrammar("aux_theory.pl");
		//let TRALE output MGS in GRISU format to temporary file
		try
		{
			SPPredicate mgsPred = new SPPredicate(sp, "mgs_to_tempfile", 2, "");
			SPTerm descTerm = new SPTerm(sp, typeString);
			SPTerm fileNameTerm = new SPTerm(sp, "tmp.grisu");
			SPQuery mgsQuery = sp.openQuery(mgsPred, new SPTerm[] { descTerm, fileNameTerm });	      
			if (mgsQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance stored MGS in temporary file.");
			}
			else
			{
				return "ERROR: Modified description was not satisfiable!";
			}
		}
		catch (SPException e)
		{
			return "ERROR: SPException " + e.toString();
		}
		//read in temporary file to retrieve GRISU string
		String grisu = null;
		try
		{
			grisu = FileUtil.slurpFile("tmp.grisu");
		}
		catch (IOException e)
		{
			grisu = "ERROR: Could not read tmp.grisu!";
		}
		return grisu;
	}
	
	private String executeTheoryMGS(IEntity ent)
	{
		//let TRALE output MGS in GRISU format to temporary file
		try
		{
			SPPredicate mgsPred = new SPPredicate(sp, "mgs_to_tempfile", 2, "");
			SPTerm descTerm = graleJToDescTerm(ent, new HashMap<Integer,SPTerm>());
			SPTerm fileNameTerm = new SPTerm(sp, "tmp.grisu");
			SPQuery mgsQuery = sp.openQuery(mgsPred, new SPTerm[] { descTerm, fileNameTerm });	      
			if (mgsQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance stored MGS in temporary file.");
			}
			else
			{
				return "ERROR: No MGS could be found!";
			}
		}
		catch (SPException e)
		{
			return "ERROR: SPException: " + e.toString();
		}
		//read in temporary file to retrieve GRISU string
		String grisu = null;
		try
		{
			grisu = FileUtil.slurpFile("tmp.grisu");
		}
		catch (IOException e)
		{
			grisu = "ERROR: Could not read tmp.grisu! ";
		}
		return grisu;
	}
	
	private String executeTheoryMGU(IEntity ent1, IEntity ent2)
	{
		//let TRALE output MGS in GRISU format to temporary file
		try
		{
			SPPredicate mgsPred = new SPPredicate(sp, "mgs_to_tempfile", 2, "");
			SPTerm desc1Term = graleJToDescTerm(ent1, new HashMap<Integer,SPTerm>());
			SPTerm desc2Term = graleJToDescTerm(ent2, new HashMap<Integer,SPTerm>());
			SPTerm descTerm = (SPTerm) sp.newTerm(",", new Term[] {desc1Term, desc2Term});
			SPTerm fileNameTerm = new SPTerm(sp, "tmp.grisu");
			SPQuery mgsQuery = sp.openQuery(mgsPred, new SPTerm[] { descTerm, fileNameTerm });	      
			if (mgsQuery.nextSolution())
			{
				System.err.println("AuxiliaryTraleInstance stored MGS in temporary file.");
			}
			else
			{
				return "ERROR: Unification failed!";
			}
		}
		catch (SPException e)
		{
			return "ERROR: SPException: " + e.toString();
		}
		//read in temporary file to retrieve GRISU string
		String grisu = null;
		try
		{
			grisu = FileUtil.slurpFile("tmp.grisu");
		}
		catch (IOException e)
		{
			grisu = "ERROR: Could not read tmp.grisu! ";
		}
		return grisu;
	}
	
	private SPTerm graleJToDescTerm(IEntity ent, Map<Integer,SPTerm> tagVariables)
	{
		SPTerm result = (SPTerm) sp.newTerm();
		if (ent instanceof IType)
		{
			try
			{
				String tyName = ((IType) ent).typeName();
				result = (SPTerm) sp.newTerm(tyName);
				if (task.getSignatureResult().getSupertypes(tyName) == null)
				{
					result = (SPTerm) sp.newTerm("a_", new Term[] { result });
				}
			}
			catch (IllegalTermException e)
			{		
				System.err.println("Illegal term while translating IType!");
			}
			catch (ConversionFailedException e)
			{		
				System.err.println("Conversion failed while translating IType!");
			}
		}
		else if (ent instanceof ITag)
		{
			try
			{
				ITag tag = (ITag) ent;
				SPTerm var = tagVariables.get(tag.number());
				if (var == null)
				{
					var = (SPTerm) sp.newVariable();
					tagVariables.put(tag.number(), var);
					SPTerm targetTerm = graleJToDescTerm(tag.target(), tagVariables);
					result = (SPTerm) sp.newTerm(",", new Term[] {var, targetTerm});
				}
				else
				{
					result = var;
				}
			}
			catch (IllegalTermException e)
			{		
				System.err.println("Illegal term while translating ITag!");
			}
			catch (ConversionFailedException e)
			{		
				System.err.println("Conversion failed while translating ITag!");
			}
		}
		else if (ent instanceof IList)
		{
			try
			{	
				List<SPTerm> elementTermStack = new LinkedList<SPTerm>();
				IList ls = (IList) ent;
				for (IEntity el : ls.elements()) 
				{
                    elementTermStack.add(0,graleJToDescTerm(el, tagVariables));
                }
                if (ls.tail() != null) 
                {
                    result = graleJToDescTerm(ls.tail(), tagVariables);
                }
                while (elementTermStack.size() > 0)
                {
                	result.consList(elementTermStack.remove(0), result);
                }
			}
			catch (IllegalTermException e)
			{			
				System.err.println("Illegal term while translating IList!");
			}
			catch (ConversionFailedException e)
			{		
				System.err.println("Conversion failed while translating IList!");
			}
		}
		else if (ent instanceof IAny)
		{
			System.err.println("Translating any!");
			try
			{
				result = (SPTerm) sp.newTerm("a_", new Term[] { new SPTerm(sp,((IAny) ent).value())});
				//result = (SPTerm) sp.newTerm("a_", new Term[] { new SPTerm(sp,"_" + ((IAny) ent).value())});
			}
			catch (IllegalTermException e)
			{			
				System.err.println("Illegal term while translating IAny!");
			}
			catch (ConversionFailedException e)
			{		
				System.err.println("Conversion failed while translating IAny!");
			}
		}
		else if (ent instanceof ITypedFeatureStructure)
		{
			try
			{
				ITypedFeatureStructure tfs = (ITypedFeatureStructure) ent;
				int numFVs = tfs.featureValuePairs().size();
				if (tfs.type() != null) 
				{
					String tyName = tfs.typeName();
					result = (SPTerm) sp.newTerm(tyName);
					if (task.getSignatureResult().getSupertypes(tyName) == null)
					{
						result = (SPTerm) sp.newTerm("a_", new Term[] { result });
					}
	                if (numFVs == 0 && tfs.type().typeName().startsWith("mgsat"))
	            	{
	                	//anonymous variable
	            		result = (SPTerm) sp.newVariable();
	            	}
	                if (numFVs > 0)
	                {
	                	for (IFeatureValuePair fv : tfs.featureValuePairs()) 
	                	{
	                        result = (SPTerm) sp.newTerm(",", new Term[] {result, graleJToDescTerm(fv, tagVariables)});
	                    }
	                }
	            }
			}
			catch (IllegalTermException e)
			{			
				System.err.println("Illegal term while translating ITypedFeatureStructure!");
			}
			catch (ConversionFailedException e)
			{		
				System.err.println("Conversion failed while translating ITypedFeatureStructure!");
			}
		}
		else if (ent instanceof IFeatureValuePair)
		{
			try
			{
				IFeatureValuePair fv = (IFeatureValuePair) ent;
				result = (SPTerm) sp.newTerm(":", new Term[] {sp.newTerm(fv.feature()),graleJToDescTerm(fv.value(), tagVariables)});
			}
			catch (IllegalTermException e)
			{			
				System.err.println("Illegal term while translating IFeatureValuePair!");
			}
			catch (ConversionFailedException e)
			{		
				System.err.println("Conversion failed while translating IFeatureValuePair!");
			}
		}
		return result;
	}
	
	//new version (does not compile anything, always uses the current theory)
	//does not work because Jasper cannot build SPTerms out of descriptions by default
	//TODO: use this to achieve speedup: let GraleJ generate descriptions as SPTerms 
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
			SPTerm descTerm = sp.readFromString(descString);
			SPTerm fileNameTerm = new SPTerm(sp, "tmp.grisu");
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
			grisu = slurpFile("tmp.grisu");
		}
		catch (IOException e)
		{
			System.err.println("Could not read grisu.tmp! Returning default GRISU string!");
			//stub behavior for now: return GRISU string for trivial structure
			grisu = "!newdata \"cruel\" (S1(0\"mgsat\"))(T2 \"head_subject:cruel\" 1)\n";
		}
		return grisu;
	}*/
	
	//not useful at the moment, will be needed once the new version of executeMGS works
	/*private boolean compileTrivialGrammar()
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
	}*/

	
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
		IEntity entity1 = null;
		IEntity entity2 = null;

		String result = null;
		TraleSLDSignature signatureResult = null;

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
		
		public IEntity getEntity1() 
		{
			return entity1;
		}

		public void setEntity1(IEntity entity) 
		{
			this.entity1 = entity;
		}
		
		public IEntity getEntity2() 
		{
			return entity2;
		}

		public void setEntity2(IEntity entity) 
		{
			this.entity2 = entity;
		}
		
		public String getResult() 
		{
			return result;
		}
		
		public void setResult(String result) 
		{
			this.result = result;
		}	
		
		public TraleSLDSignature getSignatureResult() 
		{
			return signatureResult;
		}

		public void setSignatureResult(TraleSLDSignature signatureResult) 
		{
			this.signatureResult = signatureResult;
		}
	}
}
