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
import org.kahina.logic.sat.data.free.Conjunction;
import org.kahina.logic.sat.data.free.Disjunction;
import org.kahina.logic.sat.data.free.Negation;
import org.kahina.logic.sat.data.free.VarName;

public class BooleanFormulaParser
{
    private static final int EXPECT_FORMULA = 0;
    private static final int READING_NAME = 1;
    
    public static final boolean verbose = false;
    
    public static BooleanFormula parseFile(String fileName) throws FileNotFoundException, IOException
    {
        FileReader in = new FileReader(new File(fileName));
 
        int state = EXPECT_FORMULA;
        List<BooleanFormula> subformulaStack = new LinkedList<BooleanFormula>();
        List<Character> symbolStack = new LinkedList<Character>();
        StringBuilder nameBuilder = null;
        int ch = 0;
        char c;
        int i = 0;
        while (ch != -1)
        {
            ch = in.read();
            i++;
            c = (char) ch;
            if (c == '(')
            {
                if (state == EXPECT_FORMULA)
                {
                    symbolStack.add(0,'(');
                }
                else if (state == READING_NAME)
                {
                    
                }
            }
            else if (c == ')')
            {
                if (state == READING_NAME)
                {
                    subformulaStack.add(0,new BooleanVariable(new VarName(Integer.parseInt(nameBuilder.toString()))));
                    nameBuilder = null;
                    state = EXPECT_FORMULA;
                }
                if (state == EXPECT_FORMULA)
                {
                    //negation for atoms does not need brackets
                    if (symbolStack.get(0) == '-')
                    {
                        subformulaStack.add(0, new Negation(subformulaStack.remove(0)));
                        symbolStack.remove(0);
                    }
                    //+ takes precedence over /, so construct conjunction for all unbracketed +es on stack
                    List<BooleanFormula> conjuncts = new LinkedList<BooleanFormula>();
                    conjuncts.add(subformulaStack.remove(0));
                    while (symbolStack.get(0) == '+')
                    {
                        conjuncts.add(0,subformulaStack.remove(0));
                        symbolStack.remove(0);
                    }
                    if (conjuncts.size() > 1)
                    {
                        subformulaStack.add(0,new Conjunction(conjuncts));
                    }
                    else
                    {
                        subformulaStack.add(0, conjuncts.get(0));
                    }
                    //do the same for disjunctions
                    List<BooleanFormula> disjuncts = new LinkedList<BooleanFormula>();
                    disjuncts.add(subformulaStack.remove(0));
                    while (symbolStack.get(0) == '/')
                    {
                        disjuncts.add(0,subformulaStack.remove(0));
                        symbolStack.remove(0);
                    }
                    if (disjuncts.size() > 1)
                    {
                        subformulaStack.add(0,new Disjunction(disjuncts));
                    }
                    else
                    {
                        subformulaStack.add(0, disjuncts.get(0));
                    }
                    //cancel out the ( for this )
                    if (symbolStack.get(0) == '(')
                    {
                        symbolStack.remove(0);
                    }
                    else
                    {
                        System.err.println("ERROR:no matching '(' for ')' at position " + i + ".");
                    }
                    //process the negation if there was one
                    if (symbolStack.size() > 0 && symbolStack.get(0) == '-')
                    {
                        subformulaStack.add(0, new Negation(subformulaStack.remove(0)));
                        symbolStack.remove(0);
                    }
                }
            }
            else if (c == '-')
            {
                if (state == EXPECT_FORMULA)
                {
                    symbolStack.add(0,'-');
                }
                else if (state == READING_NAME)
                {
                    System.err.println("ERROR: encountered '-' at position " + i + " while reading a name, ignoring it.");
                }
            }
            else if (c == '+')
            {
                if (state == READING_NAME)
                {
                    subformulaStack.add(0,new BooleanVariable(new VarName(Integer.parseInt(nameBuilder.toString()))));
                    nameBuilder = null;
                    //negation for atoms does not need brackets
                    if (symbolStack.get(0) == '-')
                    {
                        subformulaStack.add(0, new Negation(subformulaStack.remove(0)));
                        symbolStack.remove(0);
                    }
                    state = EXPECT_FORMULA;
                }
                if (state == EXPECT_FORMULA)
                {
                    symbolStack.add(0,'+');
                }
            }
            else if (c == '/')
            {
                if (state == READING_NAME)
                {
                    subformulaStack.add(0,new BooleanVariable(new VarName(Integer.parseInt(nameBuilder.toString()))));
                    nameBuilder = null;
                    state = EXPECT_FORMULA;
                }
                if (state == EXPECT_FORMULA)
                {
                    //negation for atoms does not need brackets
                    if (symbolStack.get(0) == '-')
                    {
                        subformulaStack.add(0, new Negation(subformulaStack.remove(0)));
                        symbolStack.remove(0);
                    }
                    //+ takes precedence over /, so construct conjunction for all unbracketed +es on stack
                    List<BooleanFormula> conjuncts = new LinkedList<BooleanFormula>();
                    conjuncts.add(subformulaStack.remove(0));
                    while (symbolStack.get(0) == '+')
                    {
                        conjuncts.add(0,subformulaStack.remove(0));
                        symbolStack.remove(0);
                    }
                    if (conjuncts.size() > 1)
                    {
                        subformulaStack.add(0,new Conjunction(conjuncts));
                    }
                    else
                    {
                        subformulaStack.add(0,conjuncts.get(0));
                    }
                    symbolStack.add(0,'/');
                }
            }
            else if (c == 'T')
            {
                if (state == EXPECT_FORMULA)
                {
                    subformulaStack.add(0,new BooleanConstant(true));
                }
                else
                {
                    System.err.println("ERROR: encountered 'T' outside formula context, ignoring it.");
                }    
            }
            else if (c == 'F')
            {
                if (state == EXPECT_FORMULA)
                {
                    subformulaStack.add(0,new BooleanConstant(false));
                }
                else
                {
                    System.err.println("ERROR: encountered 'F' outside formula context, ignoring it.");
                }
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
            else if (c == '\n')
            {
                //do nothing, newlines are simply ignored
            }
            else if (ch == -1)
            {
                //end of file, process the outermost bracket if necessary
                if (subformulaStack.size() > 1 && symbolStack.size() > 0)
                {
                  //negation for atoms does not need brackets
                    if (symbolStack.get(0) == '-')
                    {
                        subformulaStack.add(0, new Negation(subformulaStack.remove(0)));
                        symbolStack.remove(0);
                    }
                    //+ takes precedence over /, so construct conjunction for all unbracketed +es on stack
                    List<BooleanFormula> conjuncts = new LinkedList<BooleanFormula>();
                    conjuncts.add(subformulaStack.remove(0));
                    while (symbolStack.size() > 0 && symbolStack.get(0) == '+')
                    {
                        conjuncts.add(0,subformulaStack.remove(0));
                        symbolStack.remove(0);
                    }
                    if (conjuncts.size() > 1)
                    {
                        subformulaStack.add(0,new Conjunction(conjuncts));
                    }
                    else
                    {
                        subformulaStack.add(0, conjuncts.get(0));
                    }
                    //do the same for disjunctions
                    List<BooleanFormula> disjuncts = new LinkedList<BooleanFormula>();
                    disjuncts.add(subformulaStack.remove(0));
                    while (symbolStack.size() > 0 && symbolStack.get(0) == '/')
                    {
                        disjuncts.add(0,subformulaStack.remove(0));
                        symbolStack.remove(0);
                    }
                    if (disjuncts.size() > 1)
                    {
                        subformulaStack.add(0,new Disjunction(disjuncts));
                    }
                    else
                    {
                        subformulaStack.add(0, disjuncts.get(0));
                    }
                }
            }
            else
            {
                System.err.println("ERROR: could not interpret symbol '" + c + "', ignoring it.");
            }
            if (verbose)
            {
                System.err.println("reading char: " + c);
                System.err.println("  subformula stack: " + subformulaStack);
                System.err.println("  symbol stack:     " + symbolStack);
            }
        }
        in.close();
        if (subformulaStack.size() == 1)
        {
            return subformulaStack.get(0);
        }
        else
        {
            System.err.println("ERROR: parsed file did not contain exactly one formula, returning F.");
            return new BooleanConstant(false);
        }
    }
}
