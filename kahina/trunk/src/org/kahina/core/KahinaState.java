package org.kahina.core;

import org.kahina.data.KahinaObject;
import org.kahina.data.tree.KahinaLayeredTree;
import org.kahina.data.tree.KahinaTree;

/**
 * The current state of a Kahina instance.
 * 
 * Implicitly contains all the information on the current run of Kahina.
 * 
 * Can be saved to and loaded from a database file, allowing to interrupt and continue runs.
 * 
 *  @author jdellert
 */

public class KahinaState
{   

    
    //the data structures that a kahina state always contains
    KahinaLayeredTree stepTree;
    KahinaTree secondaryStepTree;
    

}
