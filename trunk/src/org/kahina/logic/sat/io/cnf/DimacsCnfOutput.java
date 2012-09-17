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
}
