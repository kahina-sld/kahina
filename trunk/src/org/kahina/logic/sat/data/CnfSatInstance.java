package org.kahina.logic.sat.data;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Pattern;

import org.kahina.core.data.graph.AdjacListsGraph;
import org.kahina.core.data.graph.KahinaGraph;

public class CnfSatInstance extends KahinaSatInstance
{
    private int numClauses;
    private int numVars;
    private List<List<Integer>> clauses;
    
    public CnfSatInstance()
    {
        numClauses = 0;
        numVars = 0;
        clauses = new ArrayList<List<Integer>>();
    }
    
    public int getNumClauses()
    {
        return numClauses;
    }
    
    public int getNumVariables()
    {
        return numVars;
    }
    
    public List<List<Integer>> getClauses()
    {
        return clauses;
    }
    
    public KahinaGraph generateClauseGraph()
    {
        //generate var -> clause map for lookup
        Map<Integer,List<Integer>> varClauseMap = new HashMap<Integer,List<Integer>>();
        for (int i = 0; i < clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i);
            for (int literal : clause)
            {
                int var = Math.abs(literal);
                List<Integer> clausesForVar = varClauseMap.get(var);
                if (clausesForVar == null)
                {
                    clausesForVar = new LinkedList<Integer>();
                    varClauseMap.put(var, clausesForVar);
                }
                clausesForVar.add(i);
            }
        }
        //generate graph linking clause nodes via variable edges
        KahinaGraph graph = new AdjacListsGraph();
        for (int i = 0; i < clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i);
            graph.addVertex(i, i + "");
            for (int literal : clause)
            {
                int var = Math.abs(literal);
                for (int clauseSharingVar : varClauseMap.get(var))
                {
                    graph.addUndirectedEdge(i, clauseSharingVar, var + "");
                }
            }
        }
        return graph;
    }
    
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
            sat.numVars = Integer.parseInt(params[2]);
            sat.numClauses = Integer.parseInt(params[3]);
            //read in clauses
            List<Integer> currentClause = new LinkedList<Integer>();
            while (in.hasNext())
            {
                Integer literal = Integer.parseInt(in.next());
                if (literal == 0)
                {
                    sat.clauses.add(currentClause);
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
