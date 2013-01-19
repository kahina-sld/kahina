package org.kahina.logic.sat.test;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;
import org.kahina.logic.sat.io.cnf.DimacsCnfParser;

public class InstanceSizeStatistics
{
    public static void main(String[] args)
    {
        if (args.length != 1)
        {
            System.err.println("Usage: java InstanceSizeStatistics [cnf file list]");
            System.exit(1);
        }
        List<String> fileList = new LinkedList<String>();
        Scanner fileListScanner;
        try
        {
            fileListScanner = new Scanner(new File(args[0]));
            while (fileListScanner.hasNextLine())
            {
                fileList.add(fileListScanner.nextLine());
            }
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: file list \"" + args[0] + "\" not found!");
            System.exit(1);
        }
        for (String fileName : fileList)
        {
            CnfSatInstance instance = DimacsCnfParser.parseDimacsCnfFile(fileName);
            int numClauses = instance.getSize();
            int numVars = instance.getHighestVar();
            double avgClauseSize = 0.0;
            for (int i = 0; i < numClauses; i++)
            {
                avgClauseSize += instance.getClause(i).size();
            }
            avgClauseSize /= numClauses;
            System.err.println(fileName + ": (" + numClauses + "," + numVars + "," + avgClauseSize + ")");  
        }
    }
}
