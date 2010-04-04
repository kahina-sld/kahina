package org.kahina.core;

import org.kahina.data.KahinaDataHandlingMethod;
import org.kahina.data.tree.KahinaDbTree;
import org.kahina.data.tree.KahinaMemTree;
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
    KahinaTree stepTree;
    KahinaTree secondaryStepTree;
    
    public KahinaState(KahinaInstance kahina, int dataHandlingMethod)
    {
        switch (dataHandlingMethod)
        {
            case KahinaDataHandlingMethod.DATABASE:
            {
                stepTree = new KahinaDbTree(kahina.getDatabaseHandler());
                secondaryStepTree = new KahinaDbTree(kahina.getDatabaseHandler());;
                break;
            }
            case KahinaDataHandlingMethod.MEMORY:
            {
                stepTree = new KahinaMemTree();
                secondaryStepTree = new KahinaMemTree();
                break;
            }
        }
    }
    
    public KahinaTree getStepTree()
    {
        return stepTree;
    }
    
    public KahinaTree getSecondaryStepTree()
    {
        return secondaryStepTree;
    }
}
