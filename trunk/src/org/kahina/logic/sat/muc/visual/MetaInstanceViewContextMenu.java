package org.kahina.logic.sat.muc.visual;

import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.kahina.logic.sat.muc.MUCInstance;

public class MetaInstanceViewContextMenu extends JPopupMenu
{
    MetaInstanceViewPanel view;
    MUCInstance kahina;
    
    public MetaInstanceViewContextMenu(ActionListener l, MetaInstanceViewPanel view, MUCInstance kahina, int listIndex)
    {
        this.view = view;
        this.kahina = kahina;
        
        JMenuItem strengthenWedgeItem = new JMenuItem("Strengthen this wedge");
        strengthenWedgeItem.setActionCommand("strengthenWedge");
        strengthenWedgeItem.addActionListener(l);
        add(strengthenWedgeItem);
        
        JMenuItem mergeWedgesItem = new JMenuItem("Attempt to merge selected wedges");
        mergeWedgesItem.setActionCommand("mergeWedges");
        mergeWedgesItem.addActionListener(l);
        add(mergeWedgesItem);
        
        JMenuItem expandWedgesItem = new JMenuItem("Globally expand unsat wedges (slow!)");
        expandWedgesItem.setActionCommand("expandWedges");
        expandWedgesItem.addActionListener(l);
        add(expandWedgesItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, MetaInstanceViewPanel view, MUCInstance kahina, int listIndex)
    {
        return new MetaInstanceViewContextMenu(l, view, kahina, listIndex);
    }
}
