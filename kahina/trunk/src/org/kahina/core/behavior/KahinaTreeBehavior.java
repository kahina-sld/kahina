package org.kahina.core.behavior;

import org.kahina.core.KahinaInstance;
import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaListener;
import org.kahina.core.control.event.KahinaEvent;
import org.kahina.core.data.tree.KahinaTree;

public class KahinaTreeBehavior extends KahinaBehavior<KahinaTree> implements KahinaListener
{   
    public KahinaTreeBehavior(KahinaTree tree, KahinaController control, KahinaInstance kahina)
    {
        super(tree, control, kahina);
        control.registerListener("tree", this);
    }
    
    public void processEvent(KahinaEvent e)
    {
        
    }
    
}
