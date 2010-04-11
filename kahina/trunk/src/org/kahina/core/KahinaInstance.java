package org.kahina.core;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.control.KahinaController;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.visual.source.KahinaSourceCodeView;
import org.kahina.core.visual.tree.KahinaTreeView;

public class KahinaInstance
{
    protected KahinaState state;
    protected KahinaController controller;
    protected KahinaGUI gui;
    protected KahinaBridge bridge;
    
    private int nextStepID;
    
    public KahinaInstance()
    {
        if (KahinaRunner.getDatabaseHandler() != null)
        {
            state = new KahinaState(this, KahinaDataHandlingMethod.DATABASE);
        }
        else
        {
            state = new KahinaState(this, KahinaDataHandlingMethod.MEMORY);
        }
        controller = new KahinaController();
        gui = new KahinaGUI(KahinaStep.class, this, controller);
        bridge = new KahinaBridge(this, gui, controller);
        nextStepID = 0;
        
        fillViewRegistry();
    }

    public KahinaController getController()
    {
        return controller;
    }

    public KahinaGUI getGUI()
    {
        return gui;
    }
    
    public KahinaBridge getBridge()
    {
        return bridge;
    }
    
    public KahinaState getState()
    {
        return state;
    }
    
    public int getNewStepID()
    {
        return nextStepID++;
    }
    
    /**
     * overwrite this to register views for user-defined datatypes
     * MUST register views for all data types
     * use super.fillViewRegistry() in implementations to register most basic views
     */
    protected void fillViewRegistry()
    {
        KahinaViewRegistry.registerMapping(KahinaTree.class, KahinaTreeView.class);
        KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, KahinaSourceCodeView.class);
    }
}
