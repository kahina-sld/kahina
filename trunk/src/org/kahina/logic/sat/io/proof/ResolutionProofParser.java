package org.kahina.logic.sat.io.proof;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.kahina.core.data.dag.DAGtoTreeConversion;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.logic.sat.data.KahinaSatInstance;
import org.kahina.logic.sat.data.proof.ResolutionProofDAG;
import org.kahina.logic.sat.data.proof.ResolutionProofDAGtoTreeConversion;
import org.kahina.logic.sat.data.proof.ResolutionProofTree;

public class ResolutionProofParser
{
    public static ResolutionProofDAG createResolutionProofDAG(String fileName, KahinaSatInstance sat)
    {
        ResolutionProofDAG proof = new ResolutionProofDAG(sat);
        try
        {
            Scanner in = new Scanner(new File(fileName));
            
            String currentLine;
            String[] tokens;
            while (in.hasNext())
            {
                currentLine = in.nextLine();
                tokens = currentLine.split(" ");
                
                int clauseID = Integer.parseInt(tokens[0]);
                int i = 1;
                List<Integer> clause = new LinkedList<Integer>();
                String clauseString = "";
                while (i < tokens.length)
                {
                    int literal = Integer.parseInt(tokens[i]);
                    i++;
                    if (literal == 0) break;
                    clause.add(literal);
                    clauseString += literal + " ";
                }
                proof.addNode(clauseID, clauseString, 0);
                proof.setNodeClause(clauseID, clause);
                while (i < tokens.length)
                {
                    int parentID = Integer.parseInt(tokens[i]);
                    i++;
                    if (parentID != 0) proof.addEdge(parentID, clauseID, "");              
                }
            }
        }
        catch (FileNotFoundException e)
        {
            System.err.println("ERROR: Resolution proof file not found: " + fileName);
            System.err.println("       Returning empty resolution proof!");
        }
        return proof;
    }
    
    public static ResolutionProofTree createResolutionProofTree(String fileName, KahinaSatInstance sat)
    {
        ResolutionProofDAG proof = createResolutionProofDAG(fileName, sat);
        //this method does not work because the proof output includes irrelevant learned clauses!
        /*//find refutation node (we can find it by descending from any root via any path)
        int arbitraryRootID = proof.getRoots().iterator().next();
        int startID = arbitraryRootID;
        System.err.print("finding refutation node, startID: " + startID);
        List<Integer> outgoingEdges = proof.getOutgoingEdges(startID);
        System.err.println(", outgoing edges: " + outgoingEdges);
        while (outgoingEdges.size() > 0)
        {
            startID = proof.getEndNode(outgoingEdges.get(0));
            System.err.print("  startID: " + startID + " via edge " + outgoingEdges.get(0));
            outgoingEdges = proof.getOutgoingEdges(startID);
            System.err.println(", outgoing edges: " + outgoingEdges);
        }*/
        ResolutionProofTree tree = new ResolutionProofTree(sat);
        ResolutionProofDAGtoTreeConversion.fillTreeWithBackwardExpansionFromNode(proof, proof.getRefutationNode(), tree);
        return tree;
    }
}
