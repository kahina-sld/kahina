package org.tralesld.core;

import org.kahina.core.KahinaInstance;
import org.kahina.data.DataManager;
import org.kahina.data.DbDataManager;
import org.kahina.data.KahinaDataHandlingMethod;
import org.kahina.data.MemDataManager;
import org.kahina.gui.LogicProgrammingGUI;
import org.tralesld.bridge.TraleSLDBridge;

public class TraleSLDInstance extends KahinaInstance
{
    TraleSLDState state;
    TraleSLDBridge bridge;
    
    public TraleSLDInstance(DbDataManager dataManager)
    {
        this.dataManager = dataManager;
        state = new TraleSLDState(this, KahinaDataHandlingMethod.DATABASE);
        gui = new LogicProgrammingGUI(TraleSLDStep.class, this, controller);
        bridge = new TraleSLDBridge(this, gui, controller);
    }
    
    public TraleSLDInstance(MemDataManager dataManager)
    {
        this.dataManager = dataManager;
        state = new TraleSLDState(this, KahinaDataHandlingMethod.MEMORY);
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
