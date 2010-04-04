package org.kahina.core;

import org.kahina.bridge.KahinaBridge;
import org.kahina.control.KahinaController;
import org.kahina.data.DataManager;
import org.kahina.data.DbDataManager;
import org.kahina.data.KahinaDataHandlingMethod;
import org.kahina.data.MemDataManager;
import org.kahina.gui.KahinaGUI;
import org.kahina.io.database.DatabaseHandler;

public class KahinaInstance
{
    protected DataManager dataManager;
    protected KahinaState state;
    protected KahinaController controller;
    protected KahinaGUI gui;
    protected KahinaBridge bridge;
    
    private int nextStepID;
    
    public KahinaInstance()
    {
        controller = new KahinaController();
        nextStepID = 0;
    }
    
    public KahinaInstance(DbDataManager dataManager)
    {
    	this.dataManager = dataManager;
        state = new KahinaState(this, KahinaDataHandlingMethod.DATABASE);
        controller = new KahinaController();
        gui = new KahinaGUI(this, controller);
        bridge = new KahinaBridge(this, gui, controller);
        nextStepID = 0;
    }
    
    public KahinaInstance(MemDataManager dataManager)
    {
        this.dataManager = dataManager;
        state = new KahinaState(this, KahinaDataHandlingMethod.MEMORY);
        controller = new KahinaController();
        gui = new KahinaGUI(this, controller);
        bridge = new KahinaBridge(this, gui, controller);
        nextStepID = 0;
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
    
    public DatabaseHandler getDatabaseHandler()
    {
        if (dataManager instanceof DbDataManager)
        {
            return ((DbDataManager) dataManager).getDatabaseHandler();
        }
        else
        {
            return null;
        }
    }
}
