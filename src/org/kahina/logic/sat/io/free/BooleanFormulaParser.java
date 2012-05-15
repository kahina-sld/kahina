package org.kahina.logic.sat.io.free;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import org.kahina.logic.sat.data.free.BooleanConstant;
import org.kahina.logic.sat.data.free.BooleanFormula;
import org.kahina.logic.sat.data.free.BooleanVariable;
import org.kahina.logic.sat.data.free.VarName;

public class BooleanFormulaParser
{
    private static final int START = 0;
    private static final int EXPECT_FORMULA = 1;
    private static final int READING_NAME = 2;
    
    public static BooleanFormula parseFile(String fileName) throws FileNotFoundException, IOException
    {
        FileReader in = new FileReader(new File(fileName));
 
        int state = EXPECT_FORMULA;
        List<BooleanFormula> cellar = new LinkedList<BooleanFormula>();
        StringBuilder nameBuilder = null;
        char c = 0;
        while (c != -1)
        {
            c = (char) in.read();
            if (c == '(')
            {
                
            }
            else if (c == ')')
            {
                if (state == READING_NAME)
                {
                    cellar.add(0,new BooleanVariable(new VarName(Integer.parseInt(nameBuilder.toString()))));
                    nameBuilder = null;
                }
                state = EXPECT_FORMULA;
            }
            else if (c == '-')
            {
                
            }
            else if (c == '+')
            {
                if (state == READING_NAME)
                {
                    cellar.add(0,new BooleanVariable(new VarName(Integer.parseInt(nameBuilder.toString()))));
                    nameBuilder = null;
                }
            }
            else if (c == '/')
            {
                if (state == READING_NAME)
                {
                    cellar.add(0,new BooleanVariable(new VarName(Integer.parseInt(nameBuilder.toString()))));
                    nameBuilder = null;
                }
            }
            else if (c == 'T')
            {
                cellar.add(0,new BooleanConstant(true));
            }
            else if (c == 'F')
            {
                cellar.add(0,new BooleanConstant(false));
            }
            else if (c == 'L')
            {
                if (state == EXPECT_FORMULA)
                {
                    state = READING_NAME;
                    nameBuilder = new StringBuilder();
                }
                else
                {
                    System.err.println("ERROR: found name identifier 'L' outside formula context, skipping it.");
                }
            }
            else if (c >= '0' && c <= '9')
            {
                if (state == READING_NAME)
                {
                    nameBuilder.append(c);
                }
                else
                {
                    System.err.println("ERROR: found name symbol '" + c + "' outside name context, ignoring it.");
                }
            }
            else
            {
                System.err.println("ERROR: could not interpret symbol '" + c + "', ignoring it.");
            }
        }
        
        return new BooleanFormula();
    }
}
