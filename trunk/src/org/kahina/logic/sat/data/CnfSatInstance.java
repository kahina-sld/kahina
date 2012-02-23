package org.kahina.logic.sat.data;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
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
    
    @SuppressWarnings("unchecked")
    public KahinaGraph generateClauseGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        //generate var -> clause map for lookup; generate clause vertices at the same time
        List<Integer>[] varClauseMap = (List<Integer>[]) new List[numVars];
        for (int i = 0; i < numVars; i++)
        {
            varClauseMap[i] = new LinkedList<Integer>();
        }
        for (int i = 1; i <= clauses.size(); i++)
        {
            graph.addVertex(i, i + "");
            List<Integer> clause = clauses.get(i-1);
            for (int literal : clause)
            {
                int var = Math.abs(literal);
                varClauseMap[var-1].add(i);
            }
        }
        System.err.println("Generating clause graph of " + numClauses + " clauses:");
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i-1);
            for (int literal : clause)
            {
                int var = Math.abs(literal);
                for (int j : varClauseMap[var-1])
                {
                    //do not add undirected nodes twice!
                    if (j > i)
                    {
                        graph.addUndirectedEdge(i, j, var + "");
                        numEdges++;
                    }
                }
            }
            if (i % 100 == 0)
            {
                System.err.println("  " + i + " clauses processed.");
            }
        }
        System.err.println("  Ready! Total number of edges: " + numEdges);
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
