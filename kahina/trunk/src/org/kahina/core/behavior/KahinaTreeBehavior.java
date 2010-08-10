package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.event.KahinaEvent;

public class KahinaTreeBehavior extends KahinaBehavior<KahinaTree> implements KahinaListener
{   
    public KahinaTreeBehavior(KahinaTree tree, KahinaInstance kahina)
    {
        super(tree, kahina);
    }
    
    public void processEvent(KahinaEvent e)
    {
        
    }
    
}
