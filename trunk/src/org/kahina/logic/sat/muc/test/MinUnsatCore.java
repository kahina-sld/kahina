package org.kahina.logic.sat.muc.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.cnf.GroupCnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.MiniSATFiles;
import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.bridge.MUCBridge;
import org.kahina.logic.sat.muc.bridge.MUCInstruction;
import org.kahina.logic.sat.muc.data.MUCStatistics;
import org.kahina.logic.sat.muc.io.MUCExtension;

public class MinUnsatCore
{
    public static boolean kahina = true;
    
    public static void computeMUC(String sourceFileName, String targetFileName, boolean group, long timeout)
    {
        long startTime = System.currentTimeMillis();
        MiniSATFiles files = new MiniSATFiles();
        files.sourceFile = new File(sourceFileName);
        files.createTargetFile(targetFileName);
        files.createTempFiles(targetFileName);
        MUCStatistics stat = new MUCStatistics();
        stat.instanceName = files.sourceFile.getName();
        if (!group)
        {
            MUCExtension.extendCNFBySelVars(files.sourceFile, files.tmpFile, stat);
        }
        else
        {
            MUCExtension.extendGroupCNFBySelVars(files.sourceFile, files.tmpFile, stat);
        }
        computeMUC(files, stat, startTime, timeout, group);
        stat.runtime = System.currentTimeMillis() - startTime;
        stat.printNicely(files.targetFile);
        files.deleteTempFiles();
        System.err.println("SAT Solver Calls: " + stat.numSATCalls);
        System.err.println("Initial number of relevant assumptions: " + stat.initNumRelAsm);
        System.err.println("Decrease in number of relevant assumptions: " + (stat.initNumRelAsm - stat.mucSize - stat.mucCandSize));
    }

    private static void computeMUC(MiniSATFiles files, MUCStatistics stat, long startTime, long timeout, boolean group)
    {
        MUCBridge bridge = null;
        if (kahina)
        {
            if (group)
            {
                GroupCnfSatInstance satInstance = GroupCnfSatInstance.parseDimacsGroupCnfFile(files.sourceFile.getAbsolutePath());
                System.err.println("Starting Kahina for MinUnsatCore on group SAT instance at " + files.sourceFile.getAbsolutePath());
                System.err.println("  Instance Size: (" + satInstance.getNumClauses() + "," + satInstance.getNumVariables() + "," + satInstance.getNumGroups() + ")");
                MUCInstance kahinaInstance = new MUCInstance(satInstance, stat, files);
                bridge = kahinaInstance.startNewSession();
            }
            else
            {
                CnfSatInstance satInstance = DimacsCnfParser.parseDimacsCnfFile(files.sourceFile.getAbsolutePath());
                System.err.println("Starting Kahina for MinUnsatCore on SAT instance at " + files.sourceFile.getAbsolutePath());
                System.err.println("  Instance Size: (" + satInstance.getNumClauses() + "," + satInstance.getNumVariables() + ")");
                MUCInstance kahinaInstance = new MUCInstance(satInstance, stat, files);
                bridge = kahinaInstance.startNewSession();
            }
        }
        Integer howmuchsmaller = 0, alreadyused = -10, rel_asm_last = 0;
        List<Integer> muc_cands = new ArrayList<Integer>();
        List<Integer> muc = new ArrayList<Integer>();
        boolean[] freezeVariables = new boolean[stat.numVarsExtended - stat.highestID];
        Arrays.fill(freezeVariables, Boolean.TRUE);
        try
        {
            MiniSAT.createFreezeFile(freezeVariables, files.tmpFreezeFile, stat.highestID + 1);
            MiniSAT.solve(files.tmpFile, files.tmpProofFile, files.tmpResultFile, files.tmpFreezeFile);
        }
        catch (Exception e)
        {
            System.out.println("Timeout");
            files.deleteTempFiles();
            files.targetFile.delete();
            System.err.println("Timeout: " + files.sourceFile.getAbsolutePath());
            if (!kahina) System.exit(0);
        }
        System.out.println("Solver finished");
        // if unsatisfiable
        if (MiniSAT.wasUnsatisfiable())
        {
            List<Integer> relevantAssumptions = MiniSAT.getRelevantAssumptions(freezeVariables, stat.highestID + 1);
            stat.initNumRelAsm = relevantAssumptions.size();
            rel_asm_last = relevantAssumptions.size();
            for (Integer a : relevantAssumptions)
            {
                muc_cands.add(a);
            }
            relevantAssumptions.clear();
            if (kahina)
            {
                bridge.registerMUC(muc_cands.toArray(new Integer[0]), muc.toArray(new Integer[0]));
            }
            //loop as long as Kahina is open; internal code sets kahina to false if it is closed
            while (kahina || muc_cands.size() > 0)
            {
                if ((System.currentTimeMillis() - startTime) > timeout)
                {
                    //do not use any timeout when using kahina!
                    if (!kahina) return;
                }
                System.out.println("");

                Integer k = 0;
                if (kahina)
                {
                    MUCInstruction instr = getNextMUCInstruction(bridge);
                    muc_cands.clear();
                    muc.clear();
                    for (int i : instr.step.getUc())
                    {
                        if (instr.step.getIcStatus(i) == 2)
                        {
                            muc.add(i);
                        }
                        else
                        {
                            muc_cands.add(i);
                        }
                    }
                    k = instr.selCandidate;
                }
                else
                {
                    k = muc_cands.get(muc_cands.size() - 1);
                }
                System.out.println("Tested Clause: " + k);
                if (alreadyused == k)
                {
                    System.out.println("should not happen");
                    System.out.println(howmuchsmaller);
                    System.exit(0);
                }
                alreadyused = k;
                muc_cands.remove(k);
                changeFreezeVariables(freezeVariables, muc_cands, muc);
                long time2 = System.currentTimeMillis();
                try
                {
                    MiniSAT.createFreezeFile(freezeVariables, files.tmpFreezeFile, stat.highestID + 1);
                    MiniSAT.solve(files.tmpFile, files.tmpProofFile, files.tmpResultFile, files.tmpFreezeFile);
                }
                catch (Exception e)
                {
                    System.err.println("Timeout");
                    files.deleteTempFiles();
                    files.targetFile.delete();
                    System.err.println("Timeout: " + files.sourceFile.getAbsolutePath());
                    System.exit(0);
                }
                System.err.println("DauerSolver: " + (System.currentTimeMillis() - time2));
                System.err.flush();
                stat.numSATCalls++;
                // if satisfiable
                if (!MiniSAT.wasUnsatisfiable())
                {
                    muc.add(k);
                    if (kahina)
                    {
                        bridge.registerSatisfiable();
                    }
                    stat.numSAT++;
                }
                else
                {
                    howmuchsmaller++;
                    stat.numUNSAT++;
                    relevantAssumptions = MiniSAT.getRelevantAssumptions(freezeVariables, stat.highestID + 1);
                    stat.registerNumRemovedClauses(rel_asm_last - relevantAssumptions.size());
                    rel_asm_last = relevantAssumptions.size();
                    muc_cands = new ArrayList<Integer>();
                    for (Integer a : relevantAssumptions)
                    {
                        if (!muc.contains(a))
                        {
                            muc_cands.add(a);
                        }
                    }
                    if (kahina)
                    {
                        bridge.registerMUC(muc_cands.toArray(new Integer[0]), muc.toArray(new Integer[0]));
                    }
                }
                relevantAssumptions.clear();
            }
        }
        else
        {
            System.err.println("Problem is satisfiable. No minimal unsatisfiable core available!");
        }
        stat.mucSize = muc.size();
        stat.mucCandSize = muc_cands.size();
        System.err.println("Minimization complete! howmuchsmaller = " + howmuchsmaller);
        while (kahina)
        {
            getNextMUCInstruction(bridge);
        }
    }

    private static void changeFreezeVariables(boolean[] freezeVariables, List<Integer> muc_cands, List<Integer> muc)
    {
        Arrays.fill(freezeVariables, Boolean.FALSE);
        for (Integer a : muc_cands)
        {
            freezeVariables[a] = true;
        }
        for (Integer a : muc)
        {
            freezeVariables[a] = true;
        }

    }


    
    //wrapper class for the file objects that need to be handed around
    private static class Files
    {
        File sourceFile;
        File targetFile;
        File tmpFile;
        File tmpResultFile;
        File tmpProofFile;
        File tmpFreezeFile;
        
        private void createTargetFile(String targetFileName)
        {
            targetFile = new File(targetFileName);
            try
            {
                targetFile.createNewFile();
            }
            catch (IOException e)
            {
                e.printStackTrace();
                System.err.println("IO error: failed to create target file");
                System.exit(0);
            }
        }
        
        public void createTempFiles(String targetFileName)
        {
            tmpFile = new File(targetFileName.concat("tmp"));
            tmpResultFile = new File(targetFileName.concat("erg"));
            tmpProofFile = new File(targetFileName.concat("bw"));
            tmpFreezeFile = new File(targetFileName.concat("fr"));
            try
            {
                tmpFile.createNewFile();
                tmpResultFile.createNewFile();
                tmpProofFile.createNewFile();
                tmpFreezeFile.createNewFile();
            }
            catch (IOException e)
            {
                e.printStackTrace();
                System.err.println("IO error: failed to create temporary files");
                System.exit(0);
            }
        }
        
        public void deleteTempFiles()
        {
            tmpFile.delete();
            tmpResultFile.delete();
            tmpProofFile.delete();
            tmpFreezeFile.delete();
        }
    }
    
    private static MUCInstruction getNextMUCInstruction(MUCBridge bridge)
    {
        System.err.println("MinUnsatCore.getNextMUCInstruction()");
        MUCInstruction instr = null;
        while (instr == null)
        {
            try
            {
                Thread.sleep(100);
            }
            catch (InterruptedException e)
            {
                
            }
            instr = bridge.getNextInstruction();
        }
        return instr;
    }

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        if (args.length < 3)
        {
            System.out.println("Benoetigte Argumente:  Quelldatei, Zieldatei, Gruppen?(0=Nein, 1=Ja)");
        }
        else
        {
            if (args[2].equals("0"))
            {
                MinUnsatCore.computeMUC(args[0], args[1], false, 14400000);
            }
            else
            {
                MinUnsatCore.computeMUC(args[0], args[1], true, 14400000);
            }
        }
        System.exit(0);
    }
}
