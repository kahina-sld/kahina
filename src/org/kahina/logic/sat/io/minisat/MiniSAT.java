package org.kahina.logic.sat.io.minisat;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.model.CompleteAssignment;
import org.kahina.logic.sat.data.model.PartialAssignment;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.muc.data.MUCStatistics;

public class MiniSAT
{
	public static final int UNFREEZE = 1;
	public static final int FREEZE = -1;
	
    private static long timeout = 600000;
    private static File lastResultFile;
    private static File lastProofFile;
    
    private static boolean VERBOSE = false;
    
    public static boolean minisatFoundOnPath()
    {
        try
        {
            Process p = Runtime.getRuntime().exec("minisat");
            p.getInputStream().close();
            p.getErrorStream().close();
            p.getOutputStream().close();
            p.destroy();
            return true;
        }
        catch (IOException e)
        {
            return false;
        }
    }
    
    public static boolean isSatisfiable(File cnfFile, File tmpResultFile) throws TimeoutException, InterruptedException, IOException
    {
        Process p = Runtime.getRuntime().exec("minisat " + cnfFile.getAbsolutePath() + " -c -r " + tmpResultFile.getAbsolutePath());
        BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
        // Set a timer to interrupt the process if it does not return within
        // the timeout period
        Timer timer = new Timer();
        timer.schedule((new MiniSAT()).new InterruptScheduler(Thread.currentThread()), timeout);
        try
        {
            p.waitFor();
        }
        catch (InterruptedException e)
        {
            // Stop the process from running
            p.getInputStream().close();
            p.getErrorStream().close();
            p.getOutputStream().close();
            p.destroy();
            throw new TimeoutException("did not return after " + timeout + " milliseconds");
        }
        finally
        {
            // Stop the timer
            timer.cancel();
        }
        String line;
        while ((line = input.readLine()) != null)
        {
            if (VERBOSE) System.err.println(line);
        }
        input.close();
        BufferedReader input2 = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        String line2;
        while ((line2 = input2.readLine()) != null)
        {
            if (VERBOSE) System.err.println(line2);
        }
        input2.close();
        p.getInputStream().close();
        p.getErrorStream().close();
        p.getOutputStream().close();
        p.destroy();
        return !wasUnsatisfiable(tmpResultFile);
    }
    
    /**
     * Solves a SAT instance and writes the learned unit clauses out into a file.
     * Requires a custom variant of MiniSAT to be installed!
     * @param cnfFile the SAT instance file (in DIMACS CNF format)
     * @param tmpResultFile the temporary file in which the result is to be stored
     * @param unitsFile the file where to store the units
     * @return whether the SAT instance was satisfiable or not; units are written to unitsFile
     */
    public static synchronized boolean solveAndDeriveUnits(File cnfFile, File tmpResultFile, File unitsFile) throws TimeoutException, InterruptedException, IOException
    {
        Process p = Runtime.getRuntime().exec("minisat " + cnfFile.getAbsolutePath() + " -c -r " + tmpResultFile.getAbsolutePath() + " -u " + unitsFile.getAbsolutePath());
        BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
        // Set a timer to interrupt the process if it does not return within
        // the timeout period
        Timer timer = new Timer();
        timer.schedule((new MiniSAT()).new InterruptScheduler(Thread.currentThread()), timeout);
        try
        {
            p.waitFor();
        }
        catch (InterruptedException e)
        {
            // Stop the process from running
            p.getInputStream().close();
            p.getErrorStream().close();
            p.getOutputStream().close();
            p.destroy();
            throw new TimeoutException("did not return after " + timeout + " milliseconds");
        }
        finally
        {
            // Stop the timer
            timer.cancel();
        }
        String line;
        while ((line = input.readLine()) != null)
        {
            if (VERBOSE) System.err.println(line);
        }
        input.close();
        BufferedReader input2 = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        String line2;
        while ((line2 = input2.readLine()) != null)
        {
            if (VERBOSE) System.err.println(line2);
        }
        input2.close();
        p.getInputStream().close();
        p.getErrorStream().close();
        p.getOutputStream().close();
        p.destroy();
        return !wasUnsatisfiable(tmpResultFile);
    }
    
    public static synchronized List<Integer> getImpliedUnits(CnfSatInstance instance, List<Integer> setVars)
    {
        File tempResultsFile = new File("unit-temp-res.cnf");
        File unitsFile = new File("units-temp.txt");
        File instancePlusUnits = new File("unit-temp.cnf");
        DimacsCnfOutput.writeDimacsCnfFileAndUnits("unit-temp.cnf", instance, setVars);
        List<Integer> units;
        try
        {
            solveAndDeriveUnits(instancePlusUnits, tempResultsFile, unitsFile);
        }
        catch (TimeoutException e)
        {
            e.printStackTrace();
            units = new LinkedList<Integer>();
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();
            units = new LinkedList<Integer>();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            units = new LinkedList<Integer>();
        }
        units = getLearnedUnitClauses(unitsFile);
        //instancePlusUnits.delete();
        //tempResultsFile.delete();
        //unitsFile.delete();
        return units;
    }
    
    public static synchronized boolean solveWithRefutationVariables(CnfSatInstance instance, File proofFile, File resultFile) throws TimeoutException, InterruptedException, IOException
    {
        File instanceFile = new File("instance-temp.cnf");
        DimacsCnfOutput.writeDimacsCnfFile("instance-temp.cnf", instance);
        Process p = Runtime.getRuntime().exec("minisat " + instanceFile.getAbsolutePath() + " -c -e -p " + proofFile.getAbsolutePath() + " -r " + resultFile.getAbsolutePath());
        BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
        Timer timer = new Timer();
        timer.schedule((new MiniSAT()).new InterruptScheduler(Thread.currentThread()), timeout);
        try
        {
            p.waitFor();
        }
        catch (InterruptedException e)
        {
            // Stop the process from running
            p.getInputStream().close();
            p.getErrorStream().close();
            p.getOutputStream().close();
            p.destroy();
            throw new TimeoutException("did not return after " + timeout + " milliseconds");
        }
        finally
        {
            // Stop the timer
            timer.cancel();
        }
        String line;
        while ((line = input.readLine()) != null)
        {
            if (VERBOSE) System.err.println(line);
        }
        input.close();
        BufferedReader input2 = new BufferedReader(new InputStreamReader(p.getErrorStream()));
        String line2;
        while ((line2 = input2.readLine()) != null)
        {
            if (VERBOSE) System.err.println(line2);
        }
        input2.close();
        p.getInputStream().close();
        p.getErrorStream().close();
        p.getOutputStream().close();
        p.destroy();
        return !wasUnsatisfiable(resultFile);
    }

    
    public static List<Integer> findUnsatisfiableCore(MUCStatistics stat, MiniSATFiles files) throws TimeoutException, InterruptedException
    {
        try
        {
            Process p = Runtime.getRuntime().exec("minisat " + files.tmpFile.getAbsolutePath() + " -p " + files.tmpProofFile.getAbsolutePath() + " -c -r " + files.tmpResultFile.getAbsolutePath() + " -f " + files.tmpFreezeFile);
            BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            // Set a timer to interrupt the process if it does not return within
            // the timeout period
            Timer timer = new Timer();
            timer.schedule((new MiniSAT()).new InterruptScheduler(Thread.currentThread()), timeout);
            try
            {
                p.waitFor();
            }
            catch (InterruptedException e)
            {
                // Stop the process from running
                p.getInputStream().close();
                p.getErrorStream().close();
                p.getOutputStream().close();
                p.destroy();
                throw new TimeoutException("did not return after " + timeout + " milliseconds");
            }
            finally
            {
                // Stop the timer
                timer.cancel();
            }
            String line;
            while ((line = input.readLine()) != null)
            {
                if (VERBOSE) System.err.println(line);
            }
            input.close();
            BufferedReader input2 = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            String line2;
            while ((line2 = input2.readLine()) != null)
            {
                if (VERBOSE) System.err.println(line2);
            }
            input2.close();
            p.getInputStream().close();
            p.getErrorStream().close();
            p.getOutputStream().close();
            p.destroy();
            if (wasUnsatisfiable(files.tmpResultFile))
            {
                //offsetID := stat.highestID
                return getRelevantAssumptions(stat.highestID + 1, files.tmpProofFile);
            }
            else
            {
                return new LinkedList<Integer>();
            }
        }
        catch (IOException e)
        {
            System.err.println("IO Error during SAT solving");
            e.printStackTrace();
            System.exit(0);
        }
        return null;
    }

    public static void solve(File inputFile, File proofFile, File resultFile, File freezeFile) throws TimeoutException, InterruptedException
    {
        try
        {
            Process p = Runtime.getRuntime().exec("minisat " + inputFile.getAbsolutePath() + " -p " + proofFile.getAbsolutePath() + " -c -r " + resultFile.getAbsolutePath() + " -f " + freezeFile);
            BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            // Set a timer to interrupt the process if it does not return within
            // the timeout period
            Timer timer = new Timer();
            timer.schedule((new MiniSAT()).new InterruptScheduler(Thread.currentThread()), timeout);

            try
            {
                p.waitFor();
            }
            catch (InterruptedException e)
            {
                // Stop the process from running
                p.getInputStream().close();
                p.getErrorStream().close();
                p.getOutputStream().close();
                p.destroy();
                throw new TimeoutException("did not return after " + timeout + " milliseconds");
            }
            finally
            {
                // Stop the timer
                timer.cancel();
            }
            String line;
            while ((line = input.readLine()) != null)
            {
                if (VERBOSE) System.out.println(line);
            }
            input.close();
            BufferedReader input2 = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            String line2;
            while ((line2 = input2.readLine()) != null)
            {
                if (VERBOSE) System.err.println(line2);
            }
            input2.close();
            p.getInputStream().close();
            p.getErrorStream().close();
            p.getOutputStream().close();
            p.destroy();
            lastResultFile = resultFile;
            lastProofFile = proofFile;
        }
        catch (IOException e)
        {
            System.err.println("IO-Fehler while SATSolving");
            e.printStackTrace();
            System.exit(0);
        }

    }

    private class InterruptScheduler extends TimerTask
    {
        Thread target = null;

        public InterruptScheduler(Thread target)
        {
            this.target = target;
        }

        @Override
        public void run()
        {
            target.interrupt();
        }
    }

    //checks result file to see whether the last SAT problem was unsatisfiable
    public static Boolean wasUnsatisfiable()
    {
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(lastResultFile));
            String line;
            while ((line = input.readLine()) != null)
            {
                if (line.equals("UNSAT"))
                {
                    // System.out.println("UNSAT");
                    return true;
                }
                else if (line.equals("SAT"))
                {
                    // System.out.println("SAT");
                    return false;
                }
            }
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.err.println("IOfailed testunsatisfiable");
            System.exit(0);
        }
        System.err.println("ERROR: Result file says neither SAT nor UNSAT! Returning null!");
        return null;
    }
    
    //gets the model from a result file in case of SAT, null if UNSAT
    public static CompleteAssignment getCompleteModel()
    {
        CompleteAssignment model = null;
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(lastResultFile));
            String line;
            while ((line = input.readLine()) != null)
            {
                if (line.equals("UNSAT"))
                {
                    break;
                }
                else if (line.equals("SAT"))
                {
                    model = new CompleteAssignment();
                    continue;
                }
                else
                {
                    for (String litString : line.split(" "))
                    {
                        int lit = Integer.parseInt(litString);
                        if (lit < 0)
                        {
                            model.setFalse(-lit);
                        }
                        else if (lit > 0)
                        {
                            model.setTrue(lit);
                        }
                    }
                }
            }
        }
        catch (IOException e)
        {
            System.err.println("ERROR: failed to read the MiniSAT result file! Null model returned!");
            e.printStackTrace();      
        }
        return model;
    }
    
  //gets the model from a result file in case of SAT, null if UNSAT
    public static CompleteAssignment getCompleteModel(File resultFile)
    {
        CompleteAssignment model = null;
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(resultFile));
            String line;
            while ((line = input.readLine()) != null)
            {
                if (line.equals("UNSAT"))
                {
                    break;
                }
                else if (line.equals("SAT"))
                {
                    model = new CompleteAssignment();
                    continue;
                }
                else
                {
                    for (String litString : line.split(" "))
                    {
                        int lit = Integer.parseInt(litString);
                        if (lit < 0)
                        {
                            model.setFalse(-lit);
                        }
                        else if (lit > 0)
                        {
                            model.setTrue(lit);
                        }
                    }
                }
            }
        }
        catch (IOException e)
        {
            System.err.println("ERROR: failed to read the MiniSAT result file " + resultFile.getName() + "! Null model returned!");
            e.printStackTrace();      
        }
        return model;
    }
    
    public static PartialAssignment getPartialModel(File resultFile)
    {
        PartialAssignment model = null;
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(resultFile));
            String line;
            while ((line = input.readLine()) != null)
            {
                if (line.equals("UNSAT"))
                {
                    break;
                }
                else if (line.equals("SAT"))
                {
                    model = new PartialAssignment();
                    continue;
                }
                else
                {
                    for (String litString : line.split(" "))
                    {
                        if (litString.length() > 0)
                        {
                            int lit = Integer.parseInt(litString);
                            if (lit < 0)
                            {
                                model.assign(-lit, false);
                            }
                            else if (lit > 0)
                            {
                                model.assign(lit, true);
                            }
                        }
                    }
                }
            }
        }
        catch (IOException e)
        {
            System.err.println("ERROR: failed to read MiniSAT result file " + resultFile.getName() + "! Null model returned!");
            e.printStackTrace();      
        }
        return model;
    }
    
    // checks result file to see whether a SAT problem was unsatisfiable
    public static boolean wasUnsatisfiable(File resultFile)
    {
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(resultFile));
            String line;
            while ((line = input.readLine()) != null)
            {
                if (line.equals("UNSAT"))
                {
                    //System.err.println("MiniSat says: " + resultFile.getName() + " is UNSAT");
                    return true;
                }
                else if (line.equals("SAT"))
                {
                    //System.err.println("MiniSat says: " + resultFile.getName() + " is SAT");
                    return false;
                }
            }
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            System.err.println("ERROR: Could not read result file " + resultFile.getName() + "! Assuming SAT!");
        }
        //System.out.println("SAT");
        System.err.println("ERROR: MiniSat result file " + resultFile.getName() + " says neither SAT nor UNSAT! Assuming SAT!");
        return false;
    }
    
    public static Set<Integer> getResolutionVariables(File proofFile)
    {
        Set<Integer> resolutionVariables = new TreeSet<Integer>();
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(proofFile));
            String line, line2;
            while ((line2 = input.readLine()) != null)
            {
                line = line2;
                StringTokenizer st = new StringTokenizer(line2);
                int numberOfZeros = 0;
                while (numberOfZeros < 2 && st.hasMoreTokens())
                {
                    line = st.nextToken();
                    if (line.equals("0"))
                    {
                      numberOfZeros++;
                    }
                }
                while (st.hasMoreTokens())
                {
                    line = st.nextToken();
                    if (!line.equals("0"))
                    {
                        resolutionVariables.add(Integer.parseInt(line));
                    }
                    else
                    {
                        break;
                    }
                }
            }
            input.close();
            return resolutionVariables;
        }
        catch (IOException e)
        {
            System.err.println("ERROR: failed to extract resolution variables from proof! Null set returned!");
            e.printStackTrace();
        }
        return null;
    }
    
    public static List<Integer> getVarRelevanceOrdering(File proofFile)
    {
        List<Integer> ordering = new ArrayList<Integer>();
        Map<Integer,Integer> varCounts = new HashMap<Integer,Integer>();
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(proofFile));
            String line, line2;
            while ((line2 = input.readLine()) != null)
            {
                line = line2;
                String[] tokens = line.split(" ");
                for (int i = 1; i < tokens.length; i++)
                {
                    int var = Integer.parseInt(tokens[i]);
                    if (var != 0)
                    {
                        if (var < 0) var = -var;
                        Integer previousCount = varCounts.get(var);
                        if (previousCount == null)
                        {
                            varCounts.put(var, 1);
                            ordering.add(var);
                        }
                        else
                        {
                            varCounts.put(var, previousCount + 1);
                        }
                    }
                }
            }
            input.close();
            Collections.sort(ordering, new VarRelevanceComparator(varCounts));
            return ordering;
        }
        catch (IOException e)
        {
            System.err.println("ERROR: failed to read proof file \"" + proofFile + "\"! Null set returned!");
            e.printStackTrace();
        }
        return null;
    }

    // extract the relevant assumptions from the last proof file
    public static List<Integer> getRelevantAssumptions(int[] freezeVariables, int offsetID)
    {
        List<Integer> relevantAssumptions = new ArrayList<Integer>();
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(lastProofFile));
            String line, line2 = "";
            while ((line = input.readLine()) != null)
            {
                line2 = line;
            }
            Arrays.fill(freezeVariables, 1);
            StringTokenizer st = new StringTokenizer(line2);
            int i = 0;
            while (st.hasMoreTokens())
            {
                line = st.nextToken();
                if (i == 0)
                {
                    i++;
                }
                else
                {
                    if (!line.equals("0"))
                    {
                        //System.err.println(line + "-> freezeVariables[" + (((-1) * Integer.parseInt(line)) - offsetID) + "] = true");
                        freezeVariables[((-1) * Integer.parseInt(line)) - offsetID] = -1;
                        relevantAssumptions.add(((-1) * Integer.parseInt(line)) - offsetID);
                    }
                    else
                    {
                        break;
                    }
                }
            }
            input.close();
            return relevantAssumptions;
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IOfailed rel_asmberechnen");
        }
        return null;
    }
    
    public static List<Integer> getLearnedUnitClauses(File unitFile)
    {
        List<Integer> units = new ArrayList<Integer>();
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(unitFile));
            String line = input.readLine();
            if (line != null)
            {
                StringTokenizer st = new StringTokenizer(line);
                int i = 0;
                while (st.hasMoreTokens())
                {
                    line = st.nextToken();
                    if (i == 0)
                    {
                        i++;
                    }
                    else
                    {
                        if (!line.equals("0"))
                        {
                            units.add((Integer.parseInt(line)));
                        }
                        else
                        {
                            break;
                        }
                    }
                }
                input.close();
            }
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("ERROR: Could not read units file, returning empty unit list!");
        }
        return units;
    }
    
    //extract the relevant assumptions from proof file
    public static List<Integer> getRelevantAssumptions(int offsetID, File proofFile)
    {
        List<Integer> relevantAssumptions = new ArrayList<Integer>();
        BufferedReader input;
        try
        {
            input = new BufferedReader(new FileReader(proofFile));
            String line, line2 = "";
            while ((line = input.readLine()) != null)
            {
                line2 = line;
            }
            StringTokenizer st = new StringTokenizer(line2);
            int i = 0;
            while (st.hasMoreTokens())
            {
                line = st.nextToken();
                if (i == 0)
                {
                    i++;
                }
                else
                {
                    if (!line.equals("0"))
                    {
                        relevantAssumptions.add(((-1) * Integer.parseInt(line)) - offsetID);
                    }
                    else
                    {
                        break;
                    }
                }
            }
            input.close();
            return relevantAssumptions;
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IOfailed rel_asmberechnen");
        }
        return null;
    }
    
    /* marks freezed variables with 1
     *  1 means the variable can be used
     * -1 means the variable is not used 
     */
    public static void createFreezeFile(int[] freezeVariables, File freezeFile, int offsetID)
    {
        StringBuffer freezeBuffer = new StringBuffer("");
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
    
    private static class VarRelevanceComparator implements Comparator<Integer>
    {
        Map<Integer,Integer> varCounts;
        
        public VarRelevanceComparator(Map<Integer,Integer> varCounts)
        {
            this.varCounts = varCounts;
        }
        
        @Override
        public int compare(Integer arg0, Integer arg1)
        {
            if (varCounts.get(arg0) == varCounts.get(arg1)) return 0;
            else if (varCounts.get(arg0) < varCounts.get(arg1)) return 1;
            else if (varCounts.get(arg0) > varCounts.get(arg1)) return -1;
            return 0;
        }    
    }
}
