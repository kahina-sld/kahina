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
        
        JMenuItem solveAndTestItem = new JMenuItem("Generate new US candidate");
        solveAndTestItem.setActionCommand("generateUSCandidate");
        solveAndTestItem.addActionListener(l);
        add(solveAndTestItem);
        
        addSeparator();
        
        JMenuItem importMetaConstraintsItem = new JMenuItem("Import additional meta constraints");
        importMetaConstraintsItem.setActionCommand("importMetaConstraints");
        importMetaConstraintsItem.addActionListener(l);
        add(importMetaConstraintsItem);
        
        JMenuItem exportMetaConstraintsItem = new JMenuItem("Export current meta constraints");
        exportMetaConstraintsItem.setActionCommand("exportMetaConstraints");
        exportMetaConstraintsItem.addActionListener(l);
        add(exportMetaConstraintsItem);
        
        JMenuItem printStatisticsItem = new JMenuItem("Print size statistics to the console");
        printStatisticsItem.setActionCommand("printStatistics");
        printStatisticsItem.addActionListener(l);
        add(printStatisticsItem);
        
        JMenuItem refreshItem = new JMenuItem("Refresh View");
        refreshItem.setActionCommand("refresh");
        refreshItem.addActionListener(l);
        add(refreshItem);
        
        /*JMenuItem strengthenWedgeItem = new JMenuItem("Strengthen this wedge");
        strengthenWedgeItem.setActionCommand("strengthenWedge" + listIndex);
        strengthenWedgeItem.addActionListener(l);
        add(strengthenWedgeItem);
        
        JMenuItem mergeWedgesItem = new JMenuItem("Attempt to merge selected wedges");
        mergeWedgesItem.setActionCommand("mergeWedges");
        mergeWedgesItem.addActionListener(l);
        add(mergeWedgesItem);
        
        JMenuItem expandWedgesItem = new JMenuItem("Globally expand unsat wedges (slow!)");
        expandWedgesItem.setActionCommand("expandWedges");
        expandWedgesItem.addActionListener(l);
        add(expandWedgesItem);*/
    }
    
    public static JPopupMenu getMenu(ActionListener l, MetaInstanceViewPanel view, MUCInstance kahina, int listIndex)
    {
        return new MetaInstanceViewContextMenu(l, view, kahina, listIndex);
    }
}
