package org.kahina.logic.sat.test;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class ResolutionVariableExtractor
{
    public static void main(String[] args)
    {
        if (args.length != 1)
        {
            System.err.println("Usage: java ResolutionVariableExtractor [cnf file]");
            System.exit(1);
        }
        CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(args[0]);
        try
        {
            Set<Integer> resolutionVariables = MiniSAT.findRefutationVariables(instance);
            System.err.println(resolutionVariables);
        }
        catch (TimeoutException e)
        {
            System.err.println("ERROR: MiniSAT timed out!");
            e.printStackTrace();
            System.exit(1);
        }
        catch (InterruptedException e)
        {
            System.err.println("ERROR: MiniSAT was interrupted!");
            e.printStackTrace();
            System.exit(1);
        }
    }
}
