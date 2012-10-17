package org.kahina.logic.sat.freemuc.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaInstance;
import org.kahina.logic.sat.freemuc.FreeMUCInstance;

public class FreeMUCFileMenu extends JMenu
{
    public FreeMUCFileMenu(KahinaInstance<?, ?, ?, ?> kahina)
    {
        super("File");
        FreeMUCInstance instance = (FreeMUCInstance) kahina;
        add(new JMenuItem(instance.LOAD_FILE_ACTION));
        addSeparator();
        //TODO: make this more intuitively accessible
        add(new JMenuItem(instance.SAT_CHECK_ACTION));
    }
}