package org.kahina.logic.sat.data.cnf;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Pattern;

import org.kahina.core.data.graph.AdjacListsGraph;
import org.kahina.core.data.graph.KahinaGraph;

public class GroupCnfSatInstance extends CnfSatInstance
{
    private int numGroups;
    private int[] clauseToGroup;
    private List<Integer>[] groupToClauses;
    
    //literals -> groups; important for efficient computation of views
    //  entries [0,...,numVars-1] for positive literals
    //  entries [numVars,...,2*numVars-1] for negative literals 
    private List<Integer>[] groupOccurrenceMap = null;
    
    @SuppressWarnings("unchecked")
    public GroupCnfSatInstance()
    {
        super();
        numGroups = 0;
        clauseToGroup = new int[0];
        groupToClauses = (List<Integer>[]) new List[0];
    }
    
    //generate lit -> group map for lookup
    //caching this makes the computation of different views a lot faster
    @SuppressWarnings("unchecked")
    public void computeGroupOccurrenceMap()
    {
        System.err.print("Generating group occurrence map for " + (getNumVars() * 2) + " literals ... ");
        occurrenceMap = (List<Integer>[]) new List[getNumVars() * 2];
        for (int i = 0; i < getNumVars() * 2; i++)
        {
            occurrenceMap[i] = new LinkedList<Integer>();
        }
        for (int g = 1; g <= numGroups; g++)
        {
            List<Integer> clauseIds = groupToClauses[g-1];
            for (int i : clauseIds)
            {
                List<Integer> clause = clauses.get(i-1);
                for (int literal : clause)
                {
                    int pos = literal;
                    if (literal < 0) pos = getNumVars() + Math.abs(literal);
                    occurrenceMap[pos-1].add(g);
                }
            }
        }
        System.err.println("Ready!");
    }
    
    //to free up memory; next visualization computation will take a lot longer
    public void discardGroupOccurrenceMap()
    {
        groupOccurrenceMap = null;
    }
    
    private void makeSureGroupOccurrenceMapExists()
    {
        if (groupOccurrenceMap == null)
        {
            computeGroupOccurrenceMap();
        }
    }
    
    public int getNumGroups()
    {
        return numGroups;
    }
    
    public List<Integer> getClauseGroup(int groupID)
    {
        List<Integer> group = groupToClauses[groupID];
        if (group == null)
        {
            group = new LinkedList<Integer>();
        }
        return group;
    }
    
    public int getGroupForClause(int clauseID)
    {
        Integer groupID = clauseToGroup[clauseID];
        if (groupID == null)
        {
            groupID = -1;
        }
        return groupID;
    }
    
    public KahinaGraph generateClaGroupByVarGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureGroupOccurrenceMapExists();
        System.err.println("Generating claGroupByVar graph of " + numGroups + " groups:");
        //generate group vertices
        for (int i = 1; i <= numGroups; i++)
        {
            graph.addVertex(i, "g" + i);
        }
        //link group vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= numGroups; i++)
        {
            List<Integer> clauseIDs = groupToClauses[i-1];
            for (int c : clauseIDs)
            {
                List<Integer> clause = clauses.get(c-1);
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
            }
            if (i % 10 == 0)
            {
                System.err.println("  " + i + " groups processed.");
            }
        }
        System.err.println("  Ready! Total number of edges: " + numEdges);
        return graph;
    }
    
    public KahinaGraph generateClaGroupByLitGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureGroupOccurrenceMapExists();
        System.err.println("Generating claGroupByLit graph of " + numGroups + " groups:");
        //generate group vertices
        for (int i = 1; i <= numGroups; i++)
        {
            graph.addVertex(i, "g" + i);
        }
        //link group vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= numGroups; i++)
        {
            List<Integer> clauseIDs = groupToClauses[i-1];
            for (int c : clauseIDs)
            {
                List<Integer> clause = clauses.get(c-1);
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
            }
            if (i % 10 == 0)
            {
                System.err.println("  " + i + " groups processed.");
            }
        }
        System.err.println("  Ready! Total number of edges: " + numEdges);
        return graph;
    }
    
    public KahinaGraph generateClaGroupByCompLitGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureGroupOccurrenceMapExists();
        System.err.println("Generating claGroupByCompLit graph of " + numGroups + " groups:");
        //generate group vertices
        for (int i = 1; i <= numGroups; i++)
        {
            graph.addVertex(i, "g" + i);
        }
        //link group vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= numGroups; i++)
        {
            List<Integer> clauseIDs = groupToClauses[i-1];
            for (int c : clauseIDs)
            {
                List<Integer> clause = clauses.get(c-1);
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
            }
            if (i % 10 == 0)
            {
                System.err.println("  " + i + " groups processed.");
            }
        }
        System.err.println("  Ready! Total number of edges: " + numEdges);
        return graph;
    }
    
    public KahinaGraph generateVarByClaGroupGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureGroupOccurrenceMapExists();
        System.err.println("Generating varByClaGroup graph of " + getNumVars() + " variables:");
        //generate variable vertices
        for (int i = 1; i <= getNumVars(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link variable vertices via clause edges
        int numEdges = 0;
        for (int var1 = 1; var1 <= getNumVars(); var1++)
        {
            Set<Integer> groupsWithVar1 = new HashSet<Integer>();
            groupsWithVar1.addAll(groupOccurrenceMap[var1 - 1]);
            groupsWithVar1.addAll(groupOccurrenceMap[getNumVars() + var1 - 1]);
            for (int var2 = var1 + 1; var2 <= getNumVars(); var2++)
            {
                int found = 0;
                for (int group : groupOccurrenceMap[var2 - 1])
                {
                    if (groupsWithVar1.contains(group))
                    {
                        found = group;
                        break;
                    }
                }
                if (found == 0)
                {
                    for (int group : groupOccurrenceMap[getNumVars() + var2 - 1])
                    {
                        if (groupsWithVar1.contains(group))
                        {
                            found = group;
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
    
    public KahinaGraph generateLitByClaGroupGraph()
    {
        KahinaGraph graph = new AdjacListsGraph();
        makeSureGroupOccurrenceMapExists();
        System.err.println("Generating litByClaGroup graph of " + getNumVars() * 2 + " literals:");
        //generate literal vertices
        for (int i = 1; i <= getNumVars(); i++)
        {
            graph.addVertex(i, i + "");
        }
        for (int i = 1; i <= getNumVars(); i++)
        {
            graph.addVertex(getNumVars() + i, "-" + i);
        }
        //link literal vertices via clause group edges
        int numEdges = 0;
        for (int lit1 = 1; lit1 <= getNumVars() * 2; lit1++)
        {
            Set<Integer> groupsWithLit1 = new HashSet<Integer>();
            groupsWithLit1.addAll(groupOccurrenceMap[lit1 - 1]);
            for (int lit2 = lit1 + 1; lit2 <= getNumVars() * 2; lit2++)
            {
                int found = 0;
                for (int group : groupOccurrenceMap[lit2 - 1])
                {
                    if (groupsWithLit1.contains(group))
                    {
                        found = group;
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
    
    @SuppressWarnings("unchecked")
    public static GroupCnfSatInstance parseDimacsGroupCnfFile(String fileName)
    {
        GroupCnfSatInstance sat = new GroupCnfSatInstance();
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
                System.err.println("ERROR: Dimacs group CNF file appears to miss the problem line!");
                System.err.println("       Returning empty group SAT instance!");
                return sat;
            }
            if (!params[1].equals("gcnf"))
            {
                System.err.println("ERROR: Parsing a non-group CNF Dimacs file with the Dimacs group CNF parser!");
                System.err.println("       Returning empty group SAT instance!");
            }
            sat.setNumVars(Integer.parseInt(params[2]));
            sat.setNumClauses(Integer.parseInt(params[3]));
            sat.numGroups = Integer.parseInt(params[4]);
            sat.clauseToGroup = new int[sat.getNumClauses()];
            sat.groupToClauses = (List<Integer>[]) new List[sat.numGroups];
            //read in clauses
            List<Integer> currentClause = new LinkedList<Integer>();
            int clauseID = 1;
            int groupID = 0;
            boolean expectGroupID = true;
            while (in.hasNext())
            {
                if (expectGroupID)
                {
                    String groupIDInBraces = in.next();
                    groupID = Integer.parseInt(groupIDInBraces.substring(1,groupIDInBraces.length() - 1));
                    expectGroupID = false;
                }
                else
                {
                    Integer literal = Integer.parseInt(in.next());
                    if (literal == 0)
                    {
                        sat.clauses.add(currentClause);
                        sat.clauseToGroup[clauseID-1] = 0;
                        if (groupID != 0)
                        {
                            List<Integer> groupClauses = sat.groupToClauses[groupID-1];
                            if (groupClauses == null)
                            {
                                groupClauses = new LinkedList<Integer>();
                                sat.groupToClauses[groupID-1] = groupClauses;
                            }
                            groupClauses.add(clauseID);
                        }
                        currentClause = new LinkedList<Integer>();
                        expectGroupID = true;
                        clauseID++;
                    }
                    else
                    {
                        currentClause.add(literal);
                    }
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
