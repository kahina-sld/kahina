package org.kahina.logic.sat.muc.data;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

//wrapper for statistics of a MUC computation
public class MUCStatistics
{
    public String instanceName;
    public int maxNumRemovedClauses;
    public int highestID = 0;
    public int numVarsExtended = 0;
    public int numClausesOrGroups = 0;
    public int numSATCalls = 0;
    public int numSAT = 0;
    public int numUNSAT = 0;
    public int initNumRelAsm = 0;
    public int mucSize = 0;
    public int mucCandSize = 0;
    public long runtime = 0;
    
    public void registerNumRemovedClauses(int num)
    {
        if (num > maxNumRemovedClauses)
        {
            maxNumRemovedClauses = num;
        }
    }
    
    public void printNicely(File statFile)
    {
        StringBuffer statistics = new StringBuffer();

        statistics.append("Name of SAT instance: " + this.instanceName + "\n");
        statistics.append("Number of MiniSAT calls: " + this.numSATCalls + "\n");
        statistics.append(" of which UNSAT: " + this.numUNSAT + "\n");
        statistics.append(" of which SAT: " + this.numSAT + "\n");
        
        double temp;
        if (this.numSATCalls != 0)
        {
            temp = this.numUNSAT * 100 / this.numSATCalls;
        }
        else
        {
            temp = 0;
        }
        statistics.append(" => percentage UNSAT: " + temp + "\n");

        statistics.append("Initial Size of UNSAT Core: " + this.initNumRelAsm + "\n");
        statistics.append("Size of MUC: " + (this.mucSize + this.mucCandSize) + "\n");
        statistics.append("Reduction of UNSAT Core Size: " + (this.initNumRelAsm - this.mucSize - this.mucCandSize) + "\n");

        statistics.append("Average UC Reduction in case of UNSAT: ");
        if (this.numUNSAT > 0)
        {
            statistics.append(((this.initNumRelAsm - this.mucSize) / this.numUNSAT) + "\n");
        }
        else
        {
            statistics.append(0 + "\n");

        }
        statistics.append("Maximum UC Reduction in case of UNSAT: " + this.maxNumRemovedClauses + "\n");
        statistics.append("Runtime: " + runtime + " ms\n");
        statistics.append("Number of Clauses/Groups: " + this.numClausesOrGroups + "\n");
        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(statFile.getAbsolutePath()));
            out.write(statistics.toString());
            out.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to print statistics");
            System.exit(0);
        }
    }
    
    public void printOneLine(File statFile)
    {
        StringBuffer statisticLineOne = new StringBuffer(), statisticLineTwo = new StringBuffer();

        statisticLineOne.append("Name der SAT-Instanz & ");
        statisticLineTwo.append(this.instanceName + " & ");

        statisticLineOne.append("Anzahl Solver-Aufrufe & ");
        statisticLineTwo.append(this.numSATCalls + " & ");

        statisticLineOne.append("Anzahl UNSAT & ");
        statisticLineTwo.append(this.numUNSAT + " & ");

        statisticLineOne.append("Anzahl SAT & ");
        statisticLineTwo.append(this.numSAT + " & ");
        double temp;
        if (this.numSATCalls != 0)
        {
            temp = this.numUNSAT * 100 / this.numSATCalls;
        }
        else
        {
            temp = 0;
        }
        statisticLineOne.append("Prozentualer Anteil UNSAT & ");
        statisticLineTwo.append(temp + " & ");

        statisticLineOne.append("Groeße UNSAT-Core Anfang & ");
        statisticLineTwo.append(this.initNumRelAsm + " & ");

        statisticLineOne.append("Groeße MUC & ");
        statisticLineTwo.append((this.mucSize + this.mucCandSize) + " & ");

        statisticLineOne.append("Verkleinerung UNSAT-Core & ");
        statisticLineTwo.append((this.initNumRelAsm - this.mucSize - this.mucCandSize) + " & ");

        statisticLineOne.append("Durchschnittliche Verkleinerung UC bei UNSAT & ");
        if (this.numUNSAT > 0)
        {
            statisticLineTwo.append(((this.initNumRelAsm - this.mucSize) / this.numUNSAT) + " & ");
        }
        else
        {
            statisticLineTwo.append(0 + " & ");

        }
        statisticLineOne.append("Maximale Verkleinerung UC bei UNSAT & ");
        statisticLineTwo.append(this.maxNumRemovedClauses + " & ");

        statisticLineOne.append("Laufzeit in ms & ");
        statisticLineTwo.append(runtime + " & ");

        statisticLineOne.append("Klauseln/Gruppen Problem & ");
        statisticLineTwo.append(this.numClausesOrGroups + " & ");

        try
        {
            BufferedWriter out = new BufferedWriter(new FileWriter(statFile.getAbsolutePath()));
            out.write(statisticLineOne.toString());
            out.newLine();
            out.write(statisticLineTwo.toString());
            out.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to print statistics");
            System.exit(0);
        }
    }
    
    public String toString()
    {
        StringBuffer string = new StringBuffer();
        string.append("MUCStatistics:\n");
        string.append("  instanceName = " + instanceName + "\n");
        string.append("  maxNumRemovedClauses = " + maxNumRemovedClauses + "\n");
        string.append("  highestID = "  + highestID + "\n");
        string.append("  numVarsExtended = " + numVarsExtended + "\n");
        string.append("  numClausesOrGroups = " + numClausesOrGroups + "\n");
        string.append("  numSATCalls = " + numSATCalls + "\n");
        string.append("  numSAT = " + numSAT + "\n");
        string.append("  numUNSAT = " + numUNSAT + "\n");
        string.append("  initNumRelAsm = " + initNumRelAsm + "\n");
        string.append("  mucSize = " + mucSize + "\n");
        string.append("  mucCandSize = " + mucCandSize + "\n");
        string.append("  runtime = " + runtime + "\n");
        return string.toString();
    }
}
