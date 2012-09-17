package org.kahina.logic.sat.io.cnf;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class DimacsSymbolConverter
{
    public static void main(String[] args)
    {
        if (args.length < 1)
        {
            System.err.println("Usage: DimacsSymbolConverter [File]");
        }
        CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(args[0]);
        DimacsCnfOutput.writeSymbolDimacsCnfFile(args[0]+"-symbolic.txt", instance);
    }
}
