package org.kahina.logic.sat.data.cnf;

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
import org.kahina.logic.sat.data.KahinaSatInstance;

public class CnfSatInstance extends KahinaSatInstance
{
    private int numClauses;
    private int numVars;
    protected List<List<Integer>> clauses;
    
    //literals -> clauses; important for efficient computation of views
    //  entries [0,...,numVars-1] for positive literals
    //  entries [numVars,...,2*numVars-1] for negative literals 
    protected List<Integer>[] occurrenceMap = null;
    
    public CnfSatInstance()
    {
        setNumClauses(0);
        setNumVars(0);
        clauses = new ArrayList<List<Integer>>();
        occurrenceMap = null;
    }
    
    //generate lit -> clause map for lookup
    //caching this makes the computation of different views a lot faster
    @SuppressWarnings("unchecked")
    public void computeOccurrenceMap()
    {
        System.err.print("Generating occurrence map for " + (getNumVars() * 2) + " literals ... ");
        occurrenceMap = (List<Integer>[]) new List[getNumVars() * 2];
        for (int i = 0; i < getNumVars() * 2; i++)
        {
            occurrenceMap[i] = new LinkedList<Integer>();
        }
        for (int i = 1; i <= clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i-1);
            for (int literal : clause)
            {
                int pos = literal;
                if (literal < 0) pos = getNumVars() + Math.abs(literal);
                occurrenceMap[pos-1].add(i);
            }
        }
        System.err.println("Ready!");
    }
    
    //to free up memory; next visualization computation will take a lot longer
    public void discardOccurrenceMap()
    {
        occurrenceMap = null;
    }
    
    private void makeSureOccurrenceMapExists()
    {
        if (occurrenceMap == null)
        {
            computeOccurrenceMap();
        }
    }
    
    public int getNumClauses()
    {
        return numClauses;
    }
    
    public int getNumVariables()
    {
        return getNumVars();
    }
    
    public List<List<Integer>> getClauses()
    {
        return clauses;
    }
    
    public KahinaGraph generateClaByVarGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureOccurrenceMapExists();
        System.err.println("Generating claByVar graph of " + getNumClauses() + " clauses:");
        //generate clause vertices
        for (int i = 1; i <= clauses.size(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i-1);
            for (int literal : clause)
            {
                int var = Math.abs(literal);
                //positive occurrences of var
                for (int j : occurrenceMap[var-1])
                {
                    //do not add undirected nodes twice!
                    if (j > i)
                    {
                        graph.addUndirectedEdge(i, j, var + "");
                        numEdges++;
                    }
                }
                //negative occurrences of var
                for (int j : occurrenceMap[getNumVars() + var-1])
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
    
    public KahinaGraph generateClaByLitGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureOccurrenceMapExists();
        System.err.println("Generating claByLit graph of " + getNumClauses() + " clauses:");
        //generate clause vertices
        for (int i = 1; i <= clauses.size(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i-1);
            for (int literal : clause)
            {
                int pos = literal;
                if (literal < 0) pos = getNumVars() + Math.abs(literal);
                for (int j : occurrenceMap[pos-1])
                {
                    //do not add undirected nodes twice!
                    if (j > i)
                    {
                        graph.addUndirectedEdge(i, j, literal + "");
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
    
    public KahinaGraph generateClaByCompLitGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureOccurrenceMapExists();
        System.err.println("Generating claByCompLit graph of " + getNumClauses() + " clauses:");
        //generate clause vertices
        for (int i = 1; i <= clauses.size(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i-1);
            for (int literal : clause)
            {
                //switch around the literal to look up
                int pos = literal;
                if (literal < 0) pos = Math.abs(literal);
                else
                {
                    pos += getNumVars();
                }
                for (int j : occurrenceMap[pos-1])
                {
                    //do not add undirected nodes twice!
                    if (j > i)
                    {
                        graph.addUndirectedEdge(i, j, literal + "");
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
    
    public KahinaGraph generateVarByClaGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureOccurrenceMapExists();
        System.err.println("Generating varByCla graph of " + getNumVars() + " variables:");
        //generate variable vertices
        for (int i = 1; i <= getNumVars(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link variable vertices via clause edges
        int numEdges = 0;
        for (int var1 = 1; var1 <= getNumVars(); var1++)
        {
            Set<Integer> clausesWithVar1 = new HashSet<Integer>();
            clausesWithVar1.addAll(occurrenceMap[var1 - 1]);
            clausesWithVar1.addAll(occurrenceMap[getNumVars() + var1 - 1]);
            for (int var2 = var1 + 1; var2 <= getNumVars(); var2++)
            {
                int found = 0;
                for (int clause : occurrenceMap[var2 - 1])
                {
                    if (clausesWithVar1.contains(clause))
                    {
                        found = clause;
                        break;
                    }
                }
                if (found == 0)
                {
                    for (int clause : occurrenceMap[getNumVars() + var2 - 1])
                    {
                        if (clausesWithVar1.contains(clause))
                        {
                            found = clause;
                            break;
                        }
                    }
                }
                if (found != 0)
                {
                    graph.addUndirectedEdge(var1, var2, found + "");
                    numEdges++;
                }
            }
            if (var1 % 100 == 0)
            {
                System.err.println("  " + var1 + " variables processed.");
            }
        }
        System.err.println("  Ready! Total number of edges: " + numEdges);
        return graph;
    }
    
    public KahinaGraph generateLitByClaGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureOccurrenceMapExists();
        System.err.println("Generating litByCla graph of " + getNumVars() * 2 + " literals:");
        //generate literal vertices
        for (int i = 1; i <= getNumVars(); i++)
        {
            graph.addVertex(i, i + "");
        }
        for (int i = 1; i <= getNumVars(); i++)
        {
            graph.addVertex(getNumVars() + i, "-" + i);
        }
        //link literal vertices via clause edges
        int numEdges = 0;
        for (int lit1 = 1; lit1 <= getNumVars() * 2; lit1++)
        {
            Set<Integer> clausesWithLit1 = new HashSet<Integer>();
            clausesWithLit1.addAll(occurrenceMap[lit1 - 1]);
            for (int lit2 = lit1 + 1; lit2 <= getNumVars() * 2; lit2++)
            {
                int found = 0;
                for (int clause : occurrenceMap[lit2 - 1])
                {
                    if (clausesWithLit1.contains(clause))
                    {
                        found = clause;
                        break;
                    }
                }
                if (found != 0)
                {
                    graph.addUndirectedEdge(lit1, lit2, found + "");
                    numEdges++;
                }
            }
            if (lit1 % 100 == 0)
            {
                System.err.println("  " + lit1 + " literals processed.");
            }
        }
        System.err.println("  Ready! Total number of edges: " + numEdges);
        return graph;
    }

    public void setNumVars(int numVars)
    {
        this.numVars = numVars;
    }

    public int getNumVars()
    {
        return numVars;
    }

    public void setNumClauses(int numClauses)
    {
        this.numClauses = numClauses;
    }
}
