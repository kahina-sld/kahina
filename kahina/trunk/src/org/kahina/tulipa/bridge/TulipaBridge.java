package org.kahina.tulipa.bridge;

import java.util.ArrayList;
import java.util.HashMap;

import org.kahina.core.bridge.KahinaBridge;
import org.kahina.tralesld.TraleSLDState;
import org.kahina.tulipa.TulipaState;

public class TulipaBridge extends KahinaBridge
{
    public static final boolean verbose = false;
    
    TulipaState state;
    
    public TulipaBridge(TulipaState state)
    {
        super();
        this.state = state;
    }
}
