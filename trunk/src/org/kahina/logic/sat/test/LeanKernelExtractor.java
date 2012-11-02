package org.kahina.logic.sat.test;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;

public class LeanKernelExtractor
{
    public static void main(String[] args)
    {
        if (args.length != 2)
        {
            System.err.println("Usage: java LeanKernelExtractor [cnf file] [result file]");
            System.exit(1);
        }
        System.err.print("Loading CNF file ... ");
        CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(args[0]);
        System.err.println("done, instance has " + instance.getNumClauses() + " clauses and " + instance.getNumVariables() + " variables");
        instance.reduceToLeanKernel();
        DimacsCnfOutput.writeDimacsCnfFile(args[1], instance);
    }
}
