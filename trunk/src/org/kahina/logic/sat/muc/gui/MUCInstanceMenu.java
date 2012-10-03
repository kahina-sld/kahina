package org.kahina.logic.sat.muc.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.logic.sat.muc.MUCInstance;

public class MUCInstanceMenu extends JMenu
{
    public MUCInstanceMenu(MUCInstance kahina)
    {
        super("Instance");
        add(new JMenuItem(kahina.US_DIMACS_EXPORT_ACTION));
        add(new JMenuItem(kahina.US_SYMBOLIC_DIMACS_EXPORT_ACTION));
        add(new JMenuItem(kahina.US_EXPORT_VAR_OCCURRENCES));
    }
}
