package org.kahina.core.gui;

import java.awt.BorderLayout;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaAbortEvent;
import org.kahina.core.visual.KahinaView;

public class KahinaWindow extends JFrame
{
	private static final long serialVersionUID = 6613805267152521669L;
    
    private static final boolean verbose = false;
    
    public KahinaWindow()
    {                 
        setLayout(new BorderLayout());
    }
}
