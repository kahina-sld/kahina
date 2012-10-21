package org.kahina.logic.sat.io.cnf;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class DimacsCnfOutput
{
    public static void writeDimacsCnfFile(String fileName, CnfSatInstance cnf)
    {
        try
        {
            FileWriter out = new FileWriter(new File(fileName));
            out.write("p cnf " + cnf.getNumVars() + " " + cnf.getNumClauses() + "\n");
            for (List<Integer> clause : cnf.getClauses())
            {
                for (Integer lit : clause)
                {
                    out.write(lit + " ");
                }
                out.write("0\n");
            }
            out.close();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: Could not write to DIMACS CNF file: " + fileName);
            e.printStackTrace();
        }
    }
    
    public static void writeDimacsCnfFileAndUnits(String fileName, CnfSatInstance cnf, List<Integer> units)
    {
        try
        {
            FileWriter out = new FileWriter(new File(fileName));
            out.write("p cnf " + cnf.getNumVars() + " " + cnf.getNumClauses() + "\n");
            for (List<Integer> clause : cnf.getClauses())
            {
                for (Integer lit : clause)
                {
                    out.write(lit + " ");
                }
                out.write("0\n");
            }
            for (int unit : units)
            {
                out.write(unit + " 0\n");
            }
            out.close();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: Could not write to DIMACS CNF file: " + fileName);
            e.printStackTrace();
        }
    }
    
    public static void writeSymbolDimacsCnfFile(String fileName, CnfSatInstance cnf)
    {
        try
        {
            FileWriter out = new FileWriter(new File(fileName));
            out.write("p cnf " + cnf.getNumVars() + " " + cnf.getNumClauses() + "\n");
            for (List<Integer> clause : cnf.getClauses())
            {
                for (Integer lit : clause)
                {
                    out.write(cnf.getSymbolForLiteral(lit) + " ");
                }
                out.write("0\n");
            }
            out.close();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: Could not write to DIMACS CNF file: " + fileName);
            e.printStackTrace();
        }
    }
    
    public static void writeVariableOccurrences(String fileName, CnfSatInstance cnf)
    {
        try
        {
            FileWriter out = new FileWriter(new File(fileName));
            int[] occurrences = new int[cnf.getNumVars() + 1];
            for (List<Integer> clause : cnf.getClauses())
            {
                for (int var : clause)
                {
                    if (var < 0) var = -var;
                    occurrences[var]++;
                }
            }
            for (int i = 0; i < occurrences.length; i++)
            {
                if (occurrences[i] > 0)
                {
                    out.write(cnf.getSymbolForLiteral(i) + " " + occurrences[i] + "\n");
                }
            }
            out.close();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: Could not write variable occurrence statistics: " + fileName);
            e.printStackTrace();
        }
    }
}
