package org.kahina.logic.sat.test;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.io.minisat.MiniSAT;
import org.kahina.logic.sat.io.minisat.ResultNotRetrievableException;

public class LearnedUnitsExtractor
{
    public static void main(String[] args)
    {
        if (args.length != 1)
        {
            System.err.println("Usage: java LearnedUnitsExtractor [cnf file]");
            System.exit(1);
        }
        File cnfFile = new File(args[0]);
        File tmpResultFile = new File(cnfFile.getAbsolutePath() + "-result");
        File unitFile = new File(cnfFile.getAbsolutePath() + "-units");
        try
        {
            MiniSAT.solveAndDeriveUnits(cnfFile, tmpResultFile, unitFile);
            System.err.println(MiniSAT.getLearnedUnitClauses(unitFile));
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
        catch (IOException e)
        {
            System.err.println("ERROR: some file could not be created or read!");
            e.printStackTrace();
            System.exit(1);
        } 
        catch (ResultNotRetrievableException e) 
        {
			System.err.println(e);
            System.exit(1);
		}
    }
}
