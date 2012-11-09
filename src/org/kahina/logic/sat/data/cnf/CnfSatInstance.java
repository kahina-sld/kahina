package org.kahina.logic.sat.data.cnf;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.TimeoutException;
import java.util.regex.Pattern;

import org.kahina.core.data.graph.AdjacListsGraph;
import org.kahina.core.data.graph.KahinaGraph;
import org.kahina.logic.sat.data.KahinaSatInstance;
import org.kahina.logic.sat.data.model.PartialAssignment;
import org.kahina.logic.sat.io.minisat.MiniSAT;

public class CnfSatInstance extends KahinaSatInstance
{
    protected int numClauses;
    protected int numVars;
    protected List<List<Integer>> clauses;
    
    //literals -> clauses; important for efficient computations
    //  entries [0,...,numVars-1] for positive literals
    //  entries [numVars,...,2*numVars-1] for negative literals 
    protected Map<Integer,List<Integer>> occurrenceMap = null;
    
    protected boolean needsUpdate = true;
    
    public CnfSatInstance()
    {
        setNumClauses(0);
        setNumVars(0);
        clauses = new ArrayList<List<Integer>>();
        occurrenceMap = null;
    }
    
    public CnfSatInstance copy()
    {
        CnfSatInstance copy = new CnfSatInstance();
        copy.setNumClauses(numClauses);
        copy.setNumVars(numVars);
        for (List<Integer> clause : clauses)
        {
            List<Integer> clauseCopy = new LinkedList<Integer>();
            clauseCopy.addAll(clause);
            copy.clauses.add(clauseCopy);
        }
        copy.needsUpdate = needsUpdate;
        return copy;
    }
    
    //generate lit -> clause map for lookup
    //caching this makes the computation of different views a lot faster
    @SuppressWarnings("unchecked")
    public void computeOccurrenceMap()
    {
        //System.err.print("Generating occurrence map for " + (getNumVars() * 2) + " literals ... ");
        long startTime = System.currentTimeMillis();
        occurrenceMap = new TreeMap<Integer,List<Integer>>();
        for (int i = 1; i <= getNumVars(); i++)
        {
            occurrenceMap.put(i, new LinkedList<Integer>());
            occurrenceMap.put(-i, new LinkedList<Integer>());
        }
        for (int i = 0; i < clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i);
            for (int literal : clause)
            {
                occurrenceMap.get(literal).add(i);
            }
        }
        System.err.println("  " + (System.currentTimeMillis() - startTime) + " ms for rebuilding the occurrence map");
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
    
    /**
     * If you modify the clause set call .announceChange() afterwards!
     * @return
     */
    public List<List<Integer>> getClauses()
    {
        return clauses;
    }
    
    public List<Integer> getClause(int clauseID)
    {
        return clauses.get(clauseID);
    }
    
    public CnfSatInstance selectClauses(Collection<Integer> clauseIDs)
    {
        CnfSatInstance subInstance = new CnfSatInstance();
        for (int clauseID: clauseIDs)
        {
            //TODO: think about risks of structure reuse!
            subInstance.getClauses().add(clauses.get(clauseID));
            subInstance.symbolTable = symbolTable;
        }
        subInstance.setNumClauses(subInstance.getClauses().size());
        subInstance.setNumVars(subInstance.searchHighestVariable());
        return subInstance;
    }
    
    public List<Integer> deriveUnitsByPropagation(List<Integer> toPropagate)
    {
        boolean VERBOSE = false;
        if (VERBOSE) System.err.println("metaInstance.deriveUnitsByPropation:\n  Input: " + toPropagate);
        Set<Integer> partialModel = new TreeSet<Integer>();
        for (int literal : toPropagate)
        {
            partialModel.add(literal);
        }
        List<Integer> derivedUnits = new LinkedList<Integer>();
        //we need the occurrence map; TODO: update this map dynamically
        computeOccurrenceMap();
        long startTime = System.currentTimeMillis();
        //propagation: initialize counters for remaining clause size
        int[] clauseSize = new int[clauses.size()];
        for (int i = 0; i < clauses.size(); i++)
        {
            List<Integer> clause = clauses.get(i);
            clauseSize[i] = clause.size();
            if (clause.size() == 1)
            {
                toPropagate.add(clause.get(0));
                partialModel.add(clause.get(0));
                derivedUnits.add(clause.get(0));
            }
        }
        //propagation: initialize list of fulfilled clauses
        Set<Integer> fulfilledClauses = new HashSet<Integer>();
        while (toPropagate.size() > 0)
        {
            int propLit = toPropagate.remove(0);
            if (VERBOSE) System.err.println("  Propagating literal: " + propLit);
            //any clause with the propagated literal is fulfilled, can be ignored
            if (VERBOSE) System.err.println("    literal occurring in: " + getOccurrences(propLit));
            for (int clauseID : getOccurrences(propLit))
            {
                if (!fulfilledClauses.contains(clauseID))
                {
                    if (VERBOSE) System.err.println("      fulfilled clause: " + clauseID);
                    fulfilledClauses.add(clauseID);
                    //OPTIONAL EXTENSION: detect and process pure literals 
                    //  DISADVANTAGE: no clean propagation any more!
                    //for each literal in the clause, was this the last instance?
                    for (int literal : clauses.get(clauseID))
                    {
                        boolean clauseForLitRemains = false;
                        for (int litClauseID : getOccurrences(literal))
                        {
                            if (!fulfilledClauses.contains(litClauseID))
                            {
                                clauseForLitRemains = true;
                                break;
                            }
                        }
                        //if so, any clause with the complementary literal can be ignored
                        //TODO: this should be done recursively, up to fixpoint!
                        if (!clauseForLitRemains)
                        {
                            for (int cplLitClauseID : getOccurrences(-literal))
                            {
                                fulfilledClauses.add(cplLitClauseID);
                            }
                        }
                    }
                }
            }
            //clauses with complementary literals are reduced
            if (VERBOSE) System.err.println("    complement occurring in: " + getOccurrences(-propLit));
            for (int clauseID : getOccurrences(-propLit))
            {
                clauseSize[clauseID]--;
                //the clause was reduced to a unit clause!
                if (clauseSize[clauseID] == 1)
                {
                    //determine which unit is left
                    int newUnit = 0;
                    for (int literal : clauses.get(clauseID))
                    {
                        if (!partialModel.contains(-literal))
                        {
                            newUnit = literal;
                            break;
                        }
                    }
                    //learn and propagate the new unit
                    toPropagate.add(newUnit);
                    partialModel.add(newUnit);
                    derivedUnits.add(newUnit);
                    if (VERBOSE) System.err.println("      added unit: " + newUnit);
                }
            }
            if (VERBOSE) System.err.println("    clause sizes: " + clauseSize);
            if (VERBOSE) System.err.println("    #toPropagate: " + toPropagate.size());
            if (VERBOSE) System.err.println("    #fulfilled:   " + fulfilledClauses.size());
        }
        if (VERBOSE) System.err.println("  Output: " + derivedUnits);
        System.err.println("  " + (System.currentTimeMillis() - startTime) + " ms" + " for propagation");
        return derivedUnits;
    }
    
    private List<Integer> getOccurrences(int literal)
    {
        List<Integer> occurrences = occurrenceMap.get(literal);
        if (occurrences == null)
        {
            occurrences = new LinkedList<Integer>();
        }
        return occurrences;
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
                for (int j : occurrenceMap.get(var))
                {
                    //do not add undirected nodes twice!
                    if (j > i)
                    {
                        graph.addUndirectedEdge(i, j, var + "");
                        numEdges++;
                    }
                }
                //negative occurrences of var
                for (int j : occurrenceMap.get(-var))
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
                for (int j : occurrenceMap.get(literal))
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
                for (int j : occurrenceMap.get(literal))
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
            clausesWithVar1.addAll(occurrenceMap.get(var1));
            clausesWithVar1.addAll(occurrenceMap.get(-var1));
            for (int var2 = var1 + 1; var2 <= getNumVars(); var2++)
            {
                int found = 0;
                for (int clause : occurrenceMap.get(var2))
                {
                    if (clausesWithVar1.contains(clause))
                    {
                        found = clause;
                        break;
                    }
                }
                if (found == 0)
                {
                    for (int clause : occurrenceMap.get(-var2))
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
        for (int lit1 = -getNumVars(); lit1 <= getNumVars(); lit1++)
        {
            if (lit1 == 0) continue;
            Set<Integer> clausesWithLit1 = new HashSet<Integer>();
            clausesWithLit1.addAll(occurrenceMap.get(lit1));
            for (int lit2 = lit1 + 1; lit2 <= getNumVars(); lit2++)
            {
                if (lit2 == 0) continue;
                int found = 0;
                for (int clause : occurrenceMap.get(lit2))
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
    
    public int searchHighestVariable()
    {
        int highestVar = 0;
        for (List<Integer> clause : clauses)
        {
            for (int var : clause)
            {
                if (var < 0) var = - var;
                if (var > highestVar) highestVar = var;
            }
        }
        return highestVar;
    }
    
    public void deleteVariablesDestructively(Set<Integer> removedVars)
    {
        System.err.print("deleting " + removedVars.size() + " variables destructively ... ");
        for (int i = 0; i < numClauses; i++)
        {
            List<Integer> clause = clauses.get(i);
            for (int j = 0; j < clause.size(); j++)
            {
                int var = clause.get(j);
                if (var < 0) var = -var;
                if (removedVars.contains(var))
                {
                    clause.remove(j);
                    j--;
                }
            }
            if (clause.size() == 0)
            {
                clauses.remove(i);
                numClauses--;
                i--;
            }
        }
        System.err.println("done, clauses left: " + numClauses);
        needsUpdate = true;
    }
    
    public void applyAssignmentDestructively(PartialAssignment assignment)
    {
        System.err.print("applyAssignmentDestructively() ... ");
        for (int i = 0; i < numClauses; i++)
        {
            List<Integer> clause = clauses.get(i);
            for (int j = 0; j < clause.size(); j++)
            {
                int var = clause.get(j);
                if (var < 0) var = -var;
                Boolean varValue = assignment.getValue(var);
                if (varValue == null) continue;
                if (varValue)
                {
                    if (clause.get(j) < 0)
                    {
                        clause.remove(j);
                        j--;
                    }
                    else
                    {
                        clause.clear();
                        j = 0;
                    }
                }
                else
                {
                    if (clause.get(j) < 0)
                    {
                        clause.clear();
                        j = 0;
                    }
                    else
                    {
                        clause.remove(j);
                        j--;
                    }
                }
            }
            if (clause.size() == 0)
            {
                clauses.remove(i);
                numClauses--;
                i--;
            }
        }
        System.err.println("done");
        needsUpdate = true;
    }
    
    public void reduceToLeanKernel()
    {
        CnfSatInstance copy = this.copy();
        PartialAssignment maxAutarky = copy.extractMaxAutarkyDestructively();
        applyAssignmentDestructively(maxAutarky);
        needsUpdate = true;
    }
    
    public PartialAssignment extractMaxAutarkyDestructively()
    {   
        try
        {
            boolean sat = false;
            Set<Integer> removedVars = new HashSet<Integer>();
            while (sat == false)
            {
                File proofFile = new File("temp.proof");
                File resFile = new File("temp.res");
                sat = MiniSAT.solveWithRefutationVariables(this, proofFile, resFile);
                if (sat)
                {
                    PartialAssignment maxAutarky = MiniSAT.getPartialModel(resFile);
                    System.err.println("Found partial model:       size " + maxAutarky.size() + ".");
                    maxAutarky.unassignVars(removedVars);
                    System.err.println("Resulting maximal autarky: size " + maxAutarky.size() + ".");  
                    proofFile.delete();
                    resFile.delete();
                    return maxAutarky;
                }
                else
                {
                    Set<Integer> resolutionVariables = MiniSAT.getResolutionVariables(proofFile);
                    if (resolutionVariables.size() == 0)
                    {
                        System.err.println("ERROR: no resolution variables in an UNSAT problem! Returning null autarky!");
                        return null;
                    }
                    this.deleteVariablesDestructively(resolutionVariables);
                    if (this.clauses.size() == 0)
                    {
                        System.err.println("Found the trivial autarky, so the input clause set was lean!");
                        return new PartialAssignment();
                    }
                    removedVars.addAll(resolutionVariables);
                }
                proofFile.delete();
                resFile.delete();
            }
        }
        catch (TimeoutException e)
        {
            System.err.println("ERROR: MiniSAT timed out! Returning null autarky!");
            e.printStackTrace();
        }
        catch (InterruptedException e)
        {
            System.err.println("ERROR: MiniSAT was interrupted! Returning null autarky!");
            e.printStackTrace();
        }
        catch (IOException e)
        {
            System.err.println("ERROR: some file could not be created or read! Returning null autarky!");
            e.printStackTrace();
        }
        return null;
    }
    
    public void announceChange()
    {
        needsUpdate = true;
    }
    
    public boolean needsUpdate()
    {
        if (needsUpdate)
        {
            needsUpdate = false;
            return true;
        }
        else 
        {
            return false;
        }
    }
}
