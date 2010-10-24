package org.kahina.tulipa;

import org.kahina.core.KahinaRunner;
import org.kahina.tulipa.bridge.TulipaBridge;

public class TulipaRunner extends KahinaRunner
{
    public static void initialize()
    {
        KahinaRunner.initialize();
    }
    
    public static TulipaBridge runAndGetBridge()
    {
        System.err.println("Starting Kahina.TuLiPa instance...");
        initialize();
        TulipaInstance kahina = new TulipaInstance();
        kahina.getGUI().prepare(getControl());
        kahina.getGUI().show();
        return kahina.getBridge();
    }
}
