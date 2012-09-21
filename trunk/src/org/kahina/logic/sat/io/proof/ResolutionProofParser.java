package org.kahina.logic.sat.io.proof;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.kahina.logic.sat.data.proof.ResolutionProof;

public class ResolutionProofParser
{
    public static ResolutionProof parseResolutionProof(String fileName)
    {
        ResolutionProof proof = new ResolutionProof();
        try
        {
            Scanner in = new Scanner(new File(fileName));
            
            String currentLine;
            String[] tokens;
            //List<Integer> currentClause = new LinkedList<Integer>();
            //read in clauses and comment lines which encode symbol definitions
            while (in.hasNext())
            {
                currentLine = in.nextLine();
                tokens = currentLine.split(" ");
                
                int clauseID = Integer.parseInt(tokens[0]);
                int i = 1;
                String clauseString = "";
                while (i < tokens.length)
                {
                    int literal = Integer.parseInt(tokens[i]);
                    i++;
                    if (literal == 0) break;
                    clauseString += literal + " ";
                }
                proof.addNode(clauseID, clauseString, 0);
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
}
