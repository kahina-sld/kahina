package org.kahina.logic.sat.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public class NecessaryClauseStatistics
{
    public static final boolean VERBOSE = true;
    
    public static void main(String[] args)
    {
        //TODO: enormous speedups would be possible by exploiting model rotation!
        
        if (args.length != 1)
        {
            System.err.println("Usage: java NecessaryClauseStatistics [cnf file list]");
            System.exit(1);
        }
        List<String> fileList = new LinkedList<String>();
        Scanner fileListScanner;
        try
        {
            fileListScanner = new Scanner(new File(args[0]));
            while (fileListScanner.hasNextLine())
            {
                fileList.add(fileListScanner.nextLine());
            }
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: file list \"" + args[0] + "\" not found!");
            System.exit(1);
        }
        for (String fileName : fileList)
        {
            CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(fileName);
            int origSize = instance.getSize();
            int numNecessaryClauses = 0;
            
            MUCStatistics stat = new MUCStatistics();
            stat.instanceName = fileName;
            
            MiniSATFiles files = new MiniSATFiles();
            files.sourceFile = new File(fileName);
            files.createTargetFile(files.sourceFile.getParent() + "/trg-" + files.sourceFile.getName());
            files.createExtendedFile(files.sourceFile.getParent() + "/ext-" + files.sourceFile.getName());
            files.createTempFiles(files.sourceFile.getParent() + "/ext-" + files.sourceFile.getName());
            
            MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);
            
            System.err.print(fileName + ": (" + origSize + "," + instance.getHighestVar() + ") -> ");
            
            boolean[] statusKnown = new boolean[instance.getSize()];
            
            for (int i = 1; i <= instance.getSize(); i++)
            {
                if (!statusKnown[i-1])
                {
                    List<Integer> prunedClauses = getPrunedClauses(i,instance,files);
                    if (prunedClauses == null)
                    {
                        if (VERBOSE) System.err.println("  clause " + i + " is critical!");
                        numNecessaryClauses++;
                        statusKnown[i-1] = true;
                    }
                    else
                    {
                        if (VERBOSE) System.err.println("  clause " + i + " unnecessary");
                        for (int j = i; j <= instance.getSize(); j++)
                        {
                            if (!statusKnown[j-1] && !prunedClauses.contains(j))
                            {
                                if (VERBOSE) System.err.println("    fall-away clause " + j);
                                statusKnown[j-1] = true;
                            }
                        }
                    }
                }
            }        
            System.err.println(numNecessaryClauses);  
        }
    }
    
    public static List<Integer> getPrunedClauses(int candidate, CnfSatInstance instance, MiniSATFiles files)
    {
        files.createTempFiles(files.sourceFile.getName() + candidate);
        //set the freeze variables (TODO: avoid generating the different lists first)
        TreeSet<Integer> muc_cands = new TreeSet<Integer>();
        //List<Integer> muc = new ArrayList<Integer>();
        for (int i = 0; i <= instance.getSize(); i++)
        {
            muc_cands.add(i);
        }  
        //wrap in Integer object in order to remove the element candidate, not at the index candidate
        muc_cands.remove(new Integer(candidate));
        int[] freezeVariables = new int[instance.getSize()];
        Arrays.fill(freezeVariables, -1);
        for (int i = 1; i <= instance.getSize(); i++)
        {
            if (instance.isDontCareClause(i) || muc_cands.contains(i))
            {
                freezeVariables[i-1] = 1;
            }
        }
        //System.err.println("freezeVars: " + Arrays.toString(freezeVariables));
        File freezeFile = new File(files.sourceFile.getParent() + "/ext-" + files.sourceFile.getName() + ".red" + candidate + ".freeze");
        MiniSAT.createFreezeFile(freezeVariables, freezeFile, instance.getHighestVar() + 1);
        try
        {
            MiniSAT.solve(files.tmpFile, files.tmpProofFile, files.tmpResultFile, freezeFile);
            Boolean wasUnsatisfiable = MiniSAT.wasUnsatisfiable();
            if (wasUnsatisfiable != null)
            {
                List<Integer> assumptions = MiniSAT.getRelevantAssumptions(instance.getHighestVar() + 1, files.tmpProofFile);
                //delete temporary files
                files.deleteTempFiles();
                freezeFile.delete();
                if (wasUnsatisfiable)
                {
                    return assumptions;
                }
                else
                {
                    return null;
                }
                //System.err.println("reducedCore: " + reducedCore);
                /*if (modelRotation)
                {
                    model = MiniSAT.getCompleteModel(files.tmpResultFile);
                }*/
            }
            else
            {
                return null;
            }
        }
        catch (InterruptedException e)
        {
            System.err.println("ERROR: InterruptedException while executing UC reduction task!");
            return null;
        }
        catch (TimeoutException e)
        {
            System.err.println("ERROR: TimeoutException while executing UC reduction task!");
            return null;
        }
    }
}
