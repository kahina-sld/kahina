package org.kahina.logic.sat.test;

import java.io.File;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.data.proof.ResolutionProofTree;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;
import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.proof.ResolutionProofParser;

public class ResolutionProofRelevanceExtractor
{
    public static void main(String[] args)
    {
        if (args.length < 2)
        {
            System.err.println("Usage: java ResolutionProofTreeViewer [proof file] [CNF file]");
            System.exit(1);
        }
        CnfSatInstance satInstance = DimacsCnfParser.parseDimacsCnfFile(args[1]);
        
        ResolutionProofTree proof = ResolutionProofParser.createResolutionProofTree(args[0], satInstance);
        
        System.err.println("by number of occurrences in resolution proof tree:");
        
        for (int var : proof.getVarRelevanceOrdering())
        {
            System.err.print(var + ",");
        }
        System.err.println("\nby number of occurrences in proof file:");
        for (int var : MiniSAT.getVarRelevanceOrdering(new File(args[0])))
        {
            System.err.print(var + ",");
        }
    }
}
