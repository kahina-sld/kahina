package org.kahina.tralesld;

import org.kahina.core.KahinaRunner;
import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.tralesld.bridge.TraleSLDBridge;

public class TraleSLDRunner extends KahinaRunner
{
    public static void main(String[] args)
    {
        KahinaRunner.initialize(KahinaDataHandlingMethod.DATABASE);
        TraleSLDInstance kahina = new TraleSLDInstance();
        kahina.getGUI().buildAndShow();
        // TODO database handler should be closed
    }
    
    public static TraleSLDBridge runAndGetBridge()
    {
        System.err.println("Starting TraleSLD instance...");
        KahinaRunner.initialize(KahinaDataHandlingMethod.DATABASE);
        TraleSLDInstance kahina = new TraleSLDInstance();
        kahina.getGUI().buildAndShow();
        return kahina.getBridge();
    }
}
