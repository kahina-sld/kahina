package org.kahina.tulipa;

import org.kahina.core.KahinaState;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.dag.KahinaDAG;
import org.kahina.core.data.dag.KahinaMemDAG;
import org.kahina.tulipa.data.grammar.TulipaGrammar;

public class TulipaState extends KahinaState
{
    KahinaDAG dag;
    TulipaGrammar grammar;
    
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
        grammar = new TulipaGrammar();
    }
    
    public KahinaDAG getDAG()
    {
        return dag;
    } 
    
    public TulipaGrammar getGrammar()
    {
    	return grammar;
    }
}
