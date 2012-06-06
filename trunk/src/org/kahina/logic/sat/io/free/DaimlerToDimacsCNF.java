package org.kahina.logic.sat.io.free;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.kahina.logic.sat.data.free.BooleanFormula;

public class DaimlerToDimacsCNF
{
    public static void main(String[] args)
    {
        if (args.length < 2)
        {
            System.err.println("USAGE: DaimlerToDimacsCNF [daimlerFile] [targetCnfFile]");
            System.exit(1);
        }
        try
        {
            BooleanFormula f = BooleanFormulaParser.parseFile(args[0]);
            BooleanFormulaOutput.writeDimacsCnfFile(args[1], f);
        }
        catch (FileNotFoundException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
