package org.kahina.tulipa;

import org.kahina.core.KahinaRunner;
import org.kahina.core.KahinaState;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.data.dag.KahinaMemDAG;

public class TulipaState extends KahinaState
{
    KahinaDAG dag;
    
    public TulipaState(TulipaInstance kahina, int dataHandlingMethod)
    {
        super(kahina, dataHandlingMethod);
        if (dataHandlingMethod == KahinaDataHandlingMethod.DATABASE)
        {
            //dag = new KahinaDbDAG();
        }
        else
        {
            dag = new KahinaMemDAG();
        }
    }
    
    public KahinaDAG getDAG()
    {
        return dag;
    } 
}
