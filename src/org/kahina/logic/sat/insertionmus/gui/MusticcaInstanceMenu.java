package org.kahina.logic.sat.insertionmus.gui;

import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;


import org.kahina.logic.sat.insertionmus.MUCInstance;

public class MusticcaInstanceMenu extends JMenu
{
    public JMenuItem loadInstanceItem;
    
    public MusticcaInstanceMenu(MUCInstance kahina)
    {
        super("Instance");
        
        //default action listener
        MusticcaInstanceMenuListener defaultListener = new MusticcaInstanceMenuListener(kahina, this);
        
        loadInstanceItem = new JMenuItem("Load Instance ...");
        loadInstanceItem.setActionCommand("loadInstance");
        loadInstanceItem.addActionListener(defaultListener);
        add(loadInstanceItem);
        
        this.addSeparator();
        
        JMenuItem quitItem = new JMenuItem("Quit");
        quitItem.setActionCommand("quit");
        quitItem.addActionListener(defaultListener);
        this.add(quitItem);
    }
    
    /**
     * Sets a specialized menu listener for this menu.
     * An application should call this to define functionality for the menu.
     * The argument must inherit from MusticcaInstanceMenuListener.
     * @param listener the listener to process the menu's ActionEvents
     */
    public void setActionListener(MusticcaInstanceMenuListener listener)
    {      
        for (ActionListener oldListener : loadInstanceItem.getActionListeners())
        {
            loadInstanceItem.removeActionListener(oldListener);
        }
        loadInstanceItem.addActionListener(listener);
    }
}
