package org.kahina.core;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.source.KahinaSourceCodeLocation;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.gui.KahinaGUI;
import org.kahina.core.gui.KahinaViewRegistry;
import org.kahina.core.visual.KahinaDefaultView;
import org.kahina.core.visual.source.KahinaSourceCodeView;
import org.kahina.core.visual.tree.KahinaTreeView;

public abstract class KahinaInstance<S extends KahinaState, G extends KahinaGUI, B extends KahinaBridge>
{
    protected S state;
    protected G gui;
    protected B bridge;
    
    private int nextStepID;
    
    public KahinaInstance()
    {   
        fillViewRegistry();
    	state = createState();
    	gui = createGUI();
    	bridge = createBridge();
        nextStepID = 0;     
    }
    
    protected abstract S createState();
    
    protected abstract G createGUI();
    
    protected abstract B createBridge();

    public G getGUI()
    {
        return gui;
    }
    
    public B getBridge()
    {
        return bridge;
    }
    
    public S getState()
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
        KahinaViewRegistry.registerMapping(KahinaObject.class, KahinaDefaultView.class);
        KahinaViewRegistry.registerMapping(KahinaTree.class, KahinaTreeView.class);
        KahinaViewRegistry.registerMapping(KahinaSourceCodeLocation.class, KahinaSourceCodeView.class);
    }
}
