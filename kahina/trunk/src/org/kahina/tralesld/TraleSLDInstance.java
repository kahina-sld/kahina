package org.kahina.tralesld;

import org.kahina.core.KahinaInstance;
import org.kahina.core.KahinaRunner;
import org.kahina.core.data.DataManager;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.MemDataManager;
import org.kahina.lp.gui.LogicProgrammingGUI;
import org.kahina.tralesld.bridge.TraleSLDBridge;

public class TraleSLDInstance extends KahinaInstance
{
    TraleSLDState state;
    TraleSLDBridge bridge;
    
    public TraleSLDInstance()
    {
        if (KahinaRunner.getDatabaseHandler() != null)
        {
            state = new TraleSLDState(this, KahinaDataHandlingMethod.DATABASE);
        }
        else
        {
            state = new TraleSLDState(this, KahinaDataHandlingMethod.MEMORY);
        }
        gui = new LogicProgrammingGUI(TraleSLDStep.class, this, controller);
        bridge = new TraleSLDBridge(this, gui, controller);
    }
    
    public TraleSLDState getState()
    {
        return state;
    }
    
    public TraleSLDBridge getBridge()
    {
        return bridge;
    }
}
