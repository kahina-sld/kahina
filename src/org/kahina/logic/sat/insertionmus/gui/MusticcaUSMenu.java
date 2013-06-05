package org.kahina.logic.sat.insertionmus.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.logic.sat.insertionmus.MUCInstance;

public class MusticcaUSMenu extends JMenu
{
    public MusticcaUSMenu(MUCInstance kahina)
    {
        super("US");
        add(new JMenuItem(kahina.US_DIMACS_EXPORT_ACTION));
        add(new JMenuItem(kahina.US_SYMBOLIC_DIMACS_EXPORT_ACTION));
        add(new JMenuItem(kahina.US_EXPORT_VAR_OCCURRENCES));
    }
}
