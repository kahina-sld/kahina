package org.kahina.logic.sat.io.free;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.data.free.RandomFormulaGenerator;
import org.kahina.logic.sat.data.free.VarName;
import org.kahina.logic.sat.io.cnf.DimacsCnfOutput;
import org.kahina.logic.sat.io.free.BooleanFormulaOutput;
import org.kahina.logic.sat.io.free.TseitinTransformationVisitor;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class UnsatInstanceGenerator
{
    public static BooleanFormula generateUnsatInstance(int numVars, int maxHeight, int maxFan, boolean complete)
    {
        VarName.resetNames();
        BooleanFormula f = RandomFormulaGenerator.randomFormula(numVars, maxHeight, maxFan, complete);
        TseitinTransformationVisitor visitor = new TseitinTransformationVisitor();
        int topVar = f.accept(visitor);
        DimacsCnfOutput.writeDimacsCnfFile("tseitin.cnf", visitor.getCNF(topVar));
        File cnfFile = new File("tseitin.cnf");
        while (!provablyUnsatisfiable(cnfFile))
        {
            System.err.println("FAILURE: generated instance of size (" + f.getSize() + "," + visitor.getCNF(topVar).getNumClauses() + ") is satisfiable!");
            VarName.resetNames();
            f = RandomFormulaGenerator.randomFormula(numVars, maxHeight, maxFan, complete);
            visitor = new TseitinTransformationVisitor();
            topVar = f.accept(visitor);
            DimacsCnfOutput.writeDimacsCnfFile("tseitin.cnf", visitor.getCNF(topVar));
        }
        System.err.println("SUCCESS: generated instance of size (" + f.getSize() + "," + visitor.getCNF(topVar).getNumClauses() + ") is unsatisfiable!");
        cnfFile.delete();
        return f;
    }
    
    public static BooleanFormula generateUnsatInstance(int numVars, int maxHeight, double[] fanProb, boolean complete)
    {
        VarName.resetNames();
        BooleanFormula f = RandomFormulaGenerator.randomFormula(numVars, maxHeight, fanProb, complete);
        TseitinTransformationVisitor visitor = new TseitinTransformationVisitor();
        int topVar = f.accept(visitor);
        DimacsCnfOutput.writeDimacsCnfFile("tseitin.cnf", visitor.getCNF(topVar));
        File cnfFile = new File("tseitin.cnf");
        while (!provablyUnsatisfiable(cnfFile))
        {
            VarName.resetNames();
            f = RandomFormulaGenerator.randomFormula(numVars, maxHeight, fanProb, complete);
            visitor = new TseitinTransformationVisitor();
            topVar = f.accept(visitor);
            DimacsCnfOutput.writeDimacsCnfFile("tseitin.cnf", visitor.getCNF(topVar));
        }
        cnfFile.delete();
        return f;
    }
    
    private static boolean provablyUnsatisfiable(File cnfFile)
    {
        File tmpResultFile = new File("minisat-result.txt");
        try
        {
            boolean result = MiniSAT.isSatisfiable(cnfFile, tmpResultFile);
            tmpResultFile.delete();
            return !result;
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        catch (TimeoutException e)
        {
            e.printStackTrace();
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();
        }
        return false;
    }
    
    public static void main(String[] args)
    {
        if (args.length < 2)
        {
            System.err.println("USAGE: UnsatInstanceGenerator [path] [number]");
        }
        String path = args[0];
        int number = Integer.parseInt(args[1]);
        for (int i = 1; i <= number; i++)
        {
            BooleanFormula instance = generateUnsatInstance(4, 10, 3, false);
            BooleanFormulaOutput.writeDaimlerFile(path + "/unsat-instance" + i, instance);
        }
    }
}
