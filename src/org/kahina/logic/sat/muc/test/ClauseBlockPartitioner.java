package org.kahina.logic.sat.muc.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.io.minisat.ResultNotRetrievableException;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.data.PartitionBlockHandler;
import org.kahina.logic.sat.muc.io.MUCExtension;

public class ClauseBlockPartitioner
{
    static final boolean VERBOSE = false;
    
    public static void main(String[] args)
    {
        if (!MiniSAT.minisatFoundOnPath())
        {
            System.err.println("ERROR: No version of minisat found on path!");
            System.err.println("       Your path must include the minisat-extended directory.");
            System.exit(0);
        }
        
        if (args.length < 1)
        {
            System.err.println("Usage: (< python2.7 marco.py -v [same CNF file] |) java -jar partitionTester.jar [CNF file] ");
            System.exit(1);
        }
        CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(args[0]);

        MiniSATFiles files = new MiniSATFiles();
        files.sourceFile = new File(args[0]);
        files.createExtendedFile(args[0]);
        MUCStatistics stat = new MUCStatistics();
        stat.instanceName = files.sourceFile.getAbsolutePath();
        MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);
        
        PartitionBlockHandler blockHandler = new PartitionBlockHandler(instance);
        int reductionID = 0;
        TreeSet<Integer> clausesToCheck = new TreeSet<Integer>();
        for (int i = 1; i <= instance.getSize(); i++)
        {
            clausesToCheck.add(i);
        }
        blockHandler.ensureRepresentability(clausesToCheck);
        while (!clausesToCheck.isEmpty())
        {
            int candID = clausesToCheck.pollFirst();
            if (VERBOSE) System.err.print(candID + ": ");
            files.createTempFiles(files.sourceFile.getName() + reductionID);
            int[] freezeVariables = new int[stat.numClausesOrGroups];
            Arrays.fill(freezeVariables, 1);
            freezeVariables[candID - 1] = -1;
            MiniSAT.createFreezeFile(freezeVariables, files.tmpFreezeFile, stat.highestID + 1);
            List<Integer> reducedCore = null;
            try
            {
                reducedCore = MiniSAT.findUnsatisfiableCore(stat, files);
            }
            catch (InterruptedException e)
            {
                System.err.println("ERROR: InterruptedException while executing UC reduction task!");
            }
            catch (TimeoutException e)
            {
                System.err.println("ERROR: TimeoutException while executing UC reduction task!");
            }
            catch (ResultNotRetrievableException e) 
            {
                System.err.println(e);
            }
            if (reducedCore.size() == 0)
            {
                if (VERBOSE) System.err.println("critical");
            }
            else
            {
                TreeSet<Integer> block = new TreeSet<Integer>();
                block.addAll(reducedCore);
                if (VERBOSE) System.err.println(block);
                blockHandler.ensureRepresentability(block);
                for (int i = candID + 1; i <= instance.getSize(); i++)
                {
                    if (!block.contains(i))
                    {
                        clausesToCheck.remove(i);
                    }
                }
            }
            reductionID++;
            files.deleteTempFiles();
        }
        System.out.println("Block structure inferred:");
        for (TreeSet<Integer> block : blockHandler.retrieveBlocks())
        {
            for (int clauseID : block)
            {
                System.out.print(clauseID + " ");
            }
            System.out.println();
        }
        System.out.println("Reading in clause IDs lists and testing them for representability ... Ctrl+D to quit.");
        InputStreamReader converter = new InputStreamReader(System.in);
        BufferedReader in = new BufferedReader(converter);
        try
        {
            String inputLine = in.readLine();
            int representableCount = 0;
            while (inputLine != null)
            {
                //discard the MSS lines from MARCO's output
                if (inputLine.startsWith("S")) continue;
                //take an U at the start as a sign that we are dealing with MARCO's output
                if (inputLine.startsWith("U"))
                {
                    inputLine = inputLine.substring(2);
                }
                //just assume we have a space-separated list of clause IDS
                TreeSet<Integer> testedMUS = new TreeSet<Integer>();
                for (String idString : inputLine.split(" "))
                {
                    testedMUS.add(Integer.parseInt(idString));
                }
                if (!blockHandler.checkRepresentability(testedMUS))
                {
                    System.err.println("(" + representableCount + ") representable MUSes found");
                    representableCount = 0;
                    System.out.print("Unrepresentable MUS found: ");
                    for (int clauseID : testedMUS)
                    {
                        System.out.print(clauseID + " ");
                    }
                    System.out.println();
                }
                else
                {
                    representableCount++;
                }
                inputLine = in.readLine();
            }
            System.err.println("(" + representableCount + ") representable MUSes found");
            System.err.println("Finished.");
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
