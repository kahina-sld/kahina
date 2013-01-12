package org.kahina.logic.sat.data.cnf;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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
    int maxClauseID;
    protected int maxVarID;
    
    //ID conversion table for purposes of indirection; determines the size
    protected List<Integer> clauseIDs;
    //reverse ID conversion table
    protected Map<Integer,Integer> reverseConversionTable;
    
    //makes the clause contents accessible by their converted IDs
    protected Map<Integer, List<Integer>> clauseStore;
    
    private HashSet<Integer> dontCareClauses;
    
    //literals -> clauses; important for efficient computations
    protected Map<Integer,List<Integer>> occurrenceMap = null;
    
    private boolean subsumptionCheck = false;
    
    //this tells KahinaSatInstanceListView whether it suffices to just append new clauses
    protected boolean needsRebuild = false;
    
    public CnfSatInstance()
    {
        maxClauseID = -1;
        maxVarID = 0;
        clauseIDs = new LinkedList<Integer>();
        reverseConversionTable = new HashMap<Integer,Integer>();
        clauseStore = new TreeMap<Integer, List<Integer>>();
        occurrenceMap = new HashMap<Integer,List<Integer>>();
        dontCareClauses = new HashSet<Integer>();
    }
    
    public CnfSatInstance copy()
    {
        CnfSatInstance copy = new CnfSatInstance();
        copy.maxClauseID = maxClauseID;
        copy.maxVarID = maxVarID;
        copy.clauseIDs.addAll(clauseIDs);
        for (int clauseID : clauseStore.keySet())
        {
            List<Integer> clauseCopy = new LinkedList<Integer>();
            clauseCopy.addAll(clauseStore.get(clauseID));
            copy.clauseStore.put(clauseID,clauseCopy);
        }
        copy.needsUpdate = needsUpdate;
        copy.dontCareClauses.addAll(dontCareClauses);
        copy.computeOccurrenceMap();
        return copy;
    }
    
    public void setSubsumptionCheck(boolean check)
    {
        if (check)
        {
            subsumptionCheck = true;
            //TODO: removeSubsumedClauses();
        }
        else
        {
            subsumptionCheck = false;
        }
    }
    
    public int addClause(List<Integer> clause)
    {
        if (occurrenceMap != null)
        {
            if (subsumptionCheck)
            {
                List<Integer> subsumedClausesIdcs = getSubsumedClauseIndices(clause);
                if (subsumedClausesIdcs != null)
                {
                    clauseStore.put(maxClauseID + 1, clause); 
                    maxClauseID++;
                    clauseIDs.add(maxClauseID);
                    reverseConversionTable.put(maxClauseID, clauseIDs.size() - 1);
                    int offset = 0;
                    for (int subsumedClauseIdx : subsumedClausesIdcs)
                    {
                        removeClauseIndex(subsumedClauseIdx - offset);
                        offset++;
                    }
                    for (int literal : clause)
                    {
                        List<Integer> occurrences = occurrenceMap.get(clause);
                        if (occurrences == null)
                        {
                            occurrences = new LinkedList<Integer>();
                            occurrenceMap.put(literal, occurrences);
                        }
                        occurrences.add(maxClauseID);
                        if (literal < 0) literal = -literal;
                        if (literal > maxVarID) maxVarID = literal;
                    }
                    return maxClauseID;
                }
                else
                {
                    //clause is subsumed by an existing clause, we do not add it
                    return -1;
                }
            }
            else
            {
                clauseStore.put(maxClauseID + 1, clause); 
                maxClauseID++;
                clauseIDs.add(maxClauseID);
                reverseConversionTable.put(maxClauseID, clauseIDs.size() - 1);
                for (int literal : clause)
                {
                    List<Integer> occurrences = occurrenceMap.get(literal);
                    if (occurrences == null)
                    {
                        occurrences = new LinkedList<Integer>();
                        occurrenceMap.put(literal, occurrences);
                    }
                    occurrences.add(maxClauseID);
                    if (literal < 0) literal = -literal;
                    if (literal > maxVarID) maxVarID = literal;
                }
                return maxClauseID;
            }
        }
        //TODO: implement this for the case without an occurrence map
        return -1;
    }
    
    private int idToIdx(int clauseID)
    {
        return reverseConversionTable.get(clauseID);
    }
    
    protected int idxToId(int clauseIndex)
    {
        return clauseIDs.get(clauseIndex);
    }
    
    /**
     * Removes the clause with the given internal ID.
     * @param clauseID the ID of the clause to be removed.
     */
    public void removeClauseID(int clauseID)
    {
        int clauseIndex = idToIdx(clauseID);
        removeClauseIndex(clauseIndex);
    }
    
    /**
     * Removes the clause at the given index (not an internal ID!).
     * @param clauseIndex the index of the clause to be removed.
     */
    public void removeClauseIndex(int clauseIndex)
    {
        //System.err.println("removeClauseIndex(" + clauseIndex + ")");
        //delete the information and table entries about the removed clause
        int removedClauseID = clauseIDs.remove(clauseIndex);
        List<Integer> removedClause = clauseStore.remove(removedClauseID);
        reverseConversionTable.remove(removedClauseID);
        if (occurrenceMap != null)
        {
            for (int lit : removedClause)
            {
                occurrenceMap.get(lit).remove(new Integer(removedClauseID));
            }
        }
        //adapt the other entries in the index <-> ID table
        for (int i = clauseIndex; i < clauseIDs.size(); i++)
        {
            //System.err.println("reverseConversionTable.put(idxToId(" + i + "), " + (i-1) + ")");
            reverseConversionTable.put(idxToId(i), i);
        }
        /*System.err.println("reverseConversionTable.remove(idxToId(" + (clauseIDs.size() - 1) + "))");
        System.err.println("  clauseIDs: " + clauseIDs);
        System.err.println("  reverseConversionTable: ");
        for (int id : reverseConversionTable.keySet())
        {
            System.err.println("    " + id + " -> " + reverseConversionTable.get(id));
        }*/
    }
    
    /**
     * Finds the clauses subsumed by some clause.
     * @param clause the clause to check for subsumption against the clauses in this CNF
     * @return null if the new clause is subsumed, a list of indices of subsumed clauses otherwise
     */
    public List<Integer> getSubsumedClauseIndices(List<Integer> clause)
    {
        int[] overlapSize = new int[clauseIDs.size()];
        for (int literal : clause)
        {
            List<Integer> occurrences = occurrenceMap.get(literal);
            if (occurrences != null)
            {
                for (int clauseID : occurrences)
                {
                    overlapSize[idToIdx(clauseID)]++;
                }
            }
        }
        List<Integer> subsumedClauses = new LinkedList<Integer>();
        for (int i = 0; i < clauseIDs.size(); i++)
        {
            if (overlapSize[i] == clause.size()) return null;
            else if (overlapSize[i] == clauseStore.get(idxToId(i)).size())
            {
                subsumedClauses.add(i);
            }
        }
        return subsumedClauses;
    }
    
    public int getSize()
    {
        return clauseIDs.size();
    }
    
    //generate lit -> clause map for lookup
    //caching this makes the computation of different views a lot faster
    public void computeOccurrenceMap()
    {
        //System.err.print("Generating occurrence map for " + (getNumVars() * 2) + " literals ... ");
        occurrenceMap = new TreeMap<Integer,List<Integer>>();
        for (int i = 1; i <= maxVarID; i++)
        {
            occurrenceMap.put(i, new LinkedList<Integer>());
            occurrenceMap.put(-i, new LinkedList<Integer>());
        }
        for (int i = 0; i < getSize(); i++)
        {
            int clauseID = idxToId(i);
            List<Integer> clause = clauseStore.get(clauseID);
            for (int literal : clause)
            {
                occurrenceMap.get(literal).add(clauseID);
            }
        }
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
    
    public boolean isDontCareClause(int clauseIndex)
    {
        return dontCareClauses.contains(clauseIndex);
    }
    
    public void applyDontCareFilter(ClauseFilter filter)
    {
        dontCareClauses.clear();
        for (int clauseIdx = 0; clauseIdx < getSize(); clauseIdx++)
        {
            if (filter.acceptsClause(clauseIdx))
            {
                dontCareClauses.add(clauseIdx);
            }
        }
        System.err.println("don't-care filter selects " + dontCareClauses.size() + "/" + getSize() + " clauses");
    }
    
    public List<Integer> getClause(int clauseIndex)
    {
        return clauseStore.get(idxToId(clauseIndex));
    }
    
    /**
     * Extracts a subinstance of the this instance defined by clause indices.
     * The clauses are not copied, but pointers to clauses are shared, 
     * i.e. modifying clauses in the subinstance will corrupt this instance!
     * For a safely modifiable version, use copy() on the result.
     * @param clauseIndices a list of 1-based clause indices in this instance
     * @return a new sat instance containing the clauses at the selected indices
     */
    public CnfSatInstance selectClauses(Collection<Integer> clauseIndices)
    {
        CnfSatInstance subInstance = new CnfSatInstance();
        for (int clauseIdx: clauseIndices)
        {
            //TODO: think about risks of structure reuse!
            subInstance.addClause(clauseStore.get(idxToId(clauseIdx - 1)));
            subInstance.symbolTable = symbolTable;
        }
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
        //propagation: initialize counters for remaining clause size
        int[] clauseSize = new int[getSize()];
        for (int i = 0; i < clauseSize.length; i++)
        {
            List<Integer> clause = clauseStore.get(idxToId(i));
            clauseSize[i] = clause.size();
            if (clause.size() == 1)
            {
                toPropagate.add(clause.get(0));
                partialModel.add(clause.get(0));
                derivedUnits.add(clause.get(0));
            }
        }
        //propagation: initialize set of fulfilledClauses
        Set<Integer> fulfilledClauseIDs = new HashSet<Integer>();
        while (toPropagate.size() > 0)
        {
            int propLit = toPropagate.remove(0);
            if (VERBOSE) System.err.println("  Propagating literal: " + propLit);
            //any clause with the propagated literal is fulfilled, can be ignored
            if (VERBOSE) System.err.println("    literal occurring in: " + getOccurrenceIDs(propLit));
            for (int clauseID : getOccurrenceIDs(propLit))
            {
                if (!fulfilledClauseIDs.contains(clauseID))
                {
                    if (VERBOSE) System.err.println("      fulfilled clause: " + clauseID);
                    fulfilledClauseIDs.add(clauseID);
                    //OPTIONAL EXTENSION: detect and process pure literals 
                    //  DISADVANTAGE: no clean propagation any more!
                    //for each literal in the clause, was this the last instance?
                    for (int literal : clauseStore.get(clauseID))
                    {
                        boolean clauseForLitRemains = false;
                        for (int litClauseID : getOccurrenceIDs(literal))
                        {
                            if (!fulfilledClauseIDs.contains(litClauseID))
                            {
                                clauseForLitRemains = true;
                                break;
                            }
                        }
                        //if so, any clause with the complementary literal can be ignored
                        //TODO: this would need to be done recursively up to fixpoint!
                        if (!clauseForLitRemains)
                        {
                            for (int cplLitClauseID : getOccurrenceIDs(-literal))
                            {
                                fulfilledClauseIDs.add(cplLitClauseID);
                            }
                        }
                    }
                }
            }
            //clauses with complementary literals are reduced
            if (VERBOSE) System.err.println("    complement occurring in: " + getOccurrenceIDs(-propLit));
            for (int clauseID : getOccurrenceIDs(-propLit))
            {
                clauseSize[idToIdx(clauseID)]--;
                //the clause was reduced to a unit clause!
                if (clauseSize[idToIdx(clauseID)] == 1)
                {
                    //determine which unit is left
                    int newUnit = 0;
                    for (int literal : clauseStore.get(clauseID))
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
            if (VERBOSE) System.err.println("    #fulfilled:   " + fulfilledClauseIDs.size());
        }
        if (VERBOSE) System.err.println("  Output: " + derivedUnits);
        return derivedUnits;
    }
    
    private List<Integer> getOccurrenceIDs(int literal)
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
        System.err.println("Generating claByVar graph of " + getSize() + " clauses:");
        //generate clause vertices
        for (int i = 1; i <= getSize(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= getSize(); i++)
        {
            List<Integer> clause = clauseStore.get(idxToId(i-1));
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
        System.err.println("Generating claByLit graph of " + getSize() + " clauses:");
        //generate clause vertices
        for (int i = 1; i <= getSize(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= getSize(); i++)
        {
            List<Integer> clause = clauseStore.get(idxToId(i-1));
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
        System.err.println("Generating claByCompLit graph of " + getSize() + " clauses:");
        //generate clause vertices
        for (int i = 1; i <= getSize(); i++)
        {
            graph.addVertex(i, i + "");
        }
        //link clause vertices via variable edges
        int numEdges = 0;
        for (int i = 1; i <= getSize(); i++)
        {
            List<Integer> clause = clauseStore.get(idxToId(i-1));
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
        System.err.println("Generating varByCla graph of " + maxVarID + " variables:");
        //generate variable vertices
        for (int i = 1; i <= maxVarID; i++)
        {
            graph.addVertex(i, i + "");
        }
        //link variable vertices via clause edges
        int numEdges = 0;
        for (int var1 = 1; var1 <= maxVarID; var1++)
        {
            Set<Integer> clausesWithVar1 = new HashSet<Integer>();
            clausesWithVar1.addAll(occurrenceMap.get(var1));
            clausesWithVar1.addAll(occurrenceMap.get(-var1));
            for (int var2 = var1 + 1; var2 <= maxVarID; var2++)
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
        System.err.println("Generating litByCla graph of " + maxVarID * 2 + " literals:");
        //generate literal vertices
        for (int i = 1; i <= maxVarID; i++)
        {
            graph.addVertex(i, i + "");
        }
        for (int i = 1; i <= maxVarID; i++)
        {
            graph.addVertex(maxVarID + i, "-" + i);
        }
        //link literal vertices via clause edges
        int numEdges = 0;
        for (int lit1 = -maxVarID; lit1 <= maxVarID; lit1++)
        {
            if (lit1 == 0) continue;
            Set<Integer> clausesWithLit1 = new HashSet<Integer>();
            clausesWithLit1.addAll(occurrenceMap.get(lit1));
            for (int lit2 = lit1 + 1; lit2 <= maxVarID; lit2++)
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

    public int getHighestVar()
    {
        return maxVarID;
    }
    
    public void deleteVariablesDestructively(Set<Integer> removedVars)
    {
        for (int i = 0; i < getSize(); i++)
        {
            List<Integer> clause = clauseStore.get(idxToId(i));
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
                removeClauseIndex(i);
                i--;
            }
        }
        needsUpdate = true;
    }
    
    public void applyAssignmentDestructively(PartialAssignment assignment)
    {
        System.err.print("applyAssignmentDestructively() ... ");
        for (int i = 0; i < getSize(); i++)
        {
            List<Integer> clause = clauseStore.get(idxToId(i));
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
                removeClauseIndex(i);
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
                    System.err.print("deleting " + removedVars.size() + " resolution variables destructively ... ");
                    this.deleteVariablesDestructively(resolutionVariables);
                    System.err.println("done, clauses left: " + getSize());
                    if (getSize() == 0)
                    {
                        System.err.println("Found the trivial autarky, so the input clause set was lean!");
                        return new PartialAssignment();
                    }
                    removedVars.addAll(resolutionVariables);
                    Set<Integer> unitClashVars = determineUnitClashVars();
                    while (unitClashVars.size() > 0)
                    {
                        System.err.print("deleting " + unitClashVars.size() + " clashing units destructively ... ");
                        this.deleteVariablesDestructively(unitClashVars);
                        System.err.println("done, clauses left: " + getSize());
                        removedVars.addAll(unitClashVars);
                        unitClashVars = determineUnitClashVars();
                    }
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
    
    private Set<Integer> determineUnitClashVars()
    {
        Set<Integer> units = new HashSet<Integer>();
        Set<Integer> clashVars = new HashSet<Integer>();
        for (List<Integer> clause : clauseStore.values())
        {
            if (clause.size() == 1)
            {
                int unit = clause.get(0);
                if (units.contains(-unit))
                {
                    if (unit < 0)
                    {
                        clashVars.add(-unit);
                    }
                    else
                    {
                        clashVars.add(unit);
                    }
                }
                else
                {
                    units.add(unit);
                }
            }
        }
        return clashVars;
    }
    
    public Map<String,Integer> generateClauseToIndexMap()
    {
        Map<String,Integer> clauseToIDMap = new HashMap<String,Integer>();
        StringBuilder clauseRepresentation;
        for (int i = 0; i < getSize(); i++)
        {
            clauseRepresentation = new StringBuilder();
            for (int lit : clauseStore.get(idxToId(i)))
            {
                clauseRepresentation.append(lit + ".");
            }
            clauseToIDMap.put(clauseRepresentation.toString(), i);
        }
        return clauseToIDMap;
    }
    
    public void announceChangedClauses()
    {
        needsUpdate = true;
        needsRebuild = true;
    }
    
    public void announceAddedClauses()
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
    
    public boolean needsRebuild()
    {
        if (needsRebuild)
        {
            needsRebuild = false;
            return true;
        }
        else 
        {
            return false;
        }
    }
}
