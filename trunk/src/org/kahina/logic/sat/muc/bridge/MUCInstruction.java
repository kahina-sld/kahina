package org.kahina.logic.sat.muc.bridge;

import org.kahina.logic.sat.muc.MUCStep;

/**
 * Contains everything the MinUnsatCore algorithm needs to re-establish a certain state, and which muc_candidate is selected
 * 
 * @author dellert
 *
 */
public class MUCInstruction
{
    public MUCStep step;  
    public int selCandidate;  
}
