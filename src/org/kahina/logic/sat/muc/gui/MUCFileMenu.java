package org.kahina.logic.sat.muc.gui;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.logic.sat.muc.MUCInstance;

public class MUCFileMenu extends JMenu
{
    public MUCFileMenu(MUCInstance kahina)
    {
        super("File");
        add(new JMenuItem(kahina.LOAD_FILE_ACTION));
        add(new JMenuItem(kahina.LOAD_PATH_ACTION));
        addSeparator();
        add(new JMenuItem(kahina.QUIT_ACTION));
    }
}
