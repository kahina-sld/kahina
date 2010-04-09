package org.tralesld.core;

import org.kahina.core.KahinaRunner;
import org.kahina.data.DbDataManager;
import org.kahina.io.database.DatabaseHandler;
import org.tralesld.bridge.TraleSLDBridge;

public class TraleSLDRunner extends KahinaRunner
{
    public static void main(String[] args)
    {
        TraleSLDInstance kahina = new TraleSLDInstance(new DbDataManager(new DatabaseHandler()));
        kahina.getGUI().buildAndShow();
        // TODO database handler should be closed
    }
    
    public static TraleSLDBridge runAndGetBridge()
    {
        System.err.println("Starting TraleSLD instance...");
        TraleSLDInstance kahina = new TraleSLDInstance(new DbDataManager(new DatabaseHandler()));
        kahina.getGUI().buildAndShow();
        return kahina.getBridge();
    }
}
