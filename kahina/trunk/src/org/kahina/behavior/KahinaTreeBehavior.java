package org.kahina.behavior;

import org.kahina.control.KahinaController;
import org.kahina.control.KahinaListener;
import org.kahina.control.event.KahinaEvent;
import org.kahina.control.event.KahinaTreeEvent;
import org.kahina.control.event.LogicProgrammingBridgeEvent;
import org.kahina.core.KahinaInstance;
import org.kahina.data.DataManager;
import org.kahina.data.tree.KahinaTree;

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
