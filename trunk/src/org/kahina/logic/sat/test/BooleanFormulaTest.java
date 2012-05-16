package org.kahina.logic.sat.test;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.io.free.BooleanFormulaParser;

public class BooleanFormulaTest
{

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        try
        {
            BooleanFormula f = BooleanFormulaParser.parseFile("/stud/dellert/formula_test.simplified.abc");
            System.out.println(f.toStringWithMinimumBracing());
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
