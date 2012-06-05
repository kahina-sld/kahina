package org.kahina.logic.sat.io.cnf;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Pattern;

import org.kahina.logic.sat.data.cnf.CnfSatInstance;

public class DimacsCnfParser
{
    public static CnfSatInstance parseDimacsCnfFile(String fileName)
    {
        CnfSatInstance sat = new CnfSatInstance();
        try
        {
            Scanner in = new Scanner(new File(fileName));
            //ignore comment lines
            while (in.hasNext(Pattern.compile("c (.*)\n")))
            {
                in.nextLine();
            }
            //process the problem line
            String problemLine = in.nextLine();
            String[] params = problemLine.split(" ");
            if (!params[0].equals("p"))
            {
                System.err.println("ERROR: Dimacs CNF file appears to miss the problem line!");
                System.err.println("       Returning empty SAT instance!");
                return sat;
            }
            if (!params[1].equals("cnf"))
            {
                System.err.println("ERROR: Parsing a non-CNF Dimacs file with the Dimacs CNF parser!");
                System.err.println("       Returning empty SAT instance!");
            }
            sat.setNumVars(Integer.parseInt(params[2]));
            sat.setNumClauses(Integer.parseInt(params[3]));
            //read in clauses
            List<Integer> currentClause = new LinkedList<Integer>();
            while (in.hasNext())
            {
                Integer literal = Integer.parseInt(in.next());
                if (literal == 0)
                {
                    sat.getClauses().add(currentClause);
                    currentClause = new LinkedList<Integer>();
                }
                else
                {
                    currentClause.add(literal);
                }
            }
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: Dimacs CNF file not found: " + fileName);
            System.err.println("       Returning empty SAT instance!");
        }
        return sat;
    }
}
