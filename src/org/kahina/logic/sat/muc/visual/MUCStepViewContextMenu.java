package org.kahina.logic.sat.muc.visual;

import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.kahina.logic.sat.muc.MUCInstance;

public class MUCStepViewContextMenu extends JPopupMenu
{
    MUCStepView view;
    MUCInstance kahina;
    
    public MUCStepViewContextMenu(ActionListener l, MUCStepView view, MUCInstance kahina)
    {
        this.view = view;
        this.kahina = kahina;
        
        JMenuItem selectAllItem = new JMenuItem("Select all");
        selectAllItem.setActionCommand("selectAll");
        selectAllItem.addActionListener(l);
        add(selectAllItem);
        
        JMenuItem selectDialogItem = new JMenuItem("Select ...");
        selectDialogItem.setEnabled(false);
        selectDialogItem.setActionCommand("selectDialog");
        selectDialogItem.addActionListener(l);
        add(selectDialogItem);
        
        addSeparator();
        
        JMenuItem reduceItem = new JMenuItem("Reduce by this clause (= double-click)");
        reduceItem.setActionCommand("reduce");
        reduceItem.addActionListener(l);
        add(reduceItem);
        
        JMenuItem reduceMRItem = new JMenuItem("Reduce by this clause + model rotation");
        selectDialogItem.setEnabled(false);
        reduceMRItem.setActionCommand("reduceMR");
        reduceMRItem.addActionListener(l);
        add(reduceMRItem);
        
        JMenuItem reduceSelectedOnceItem = new JMenuItem("Reduce by selected clauses at once");
        reduceSelectedOnceItem.setActionCommand("reduceSelOnce");
        reduceSelectedOnceItem.addActionListener(l);
        add(reduceSelectedOnceItem);
        
        JMenuItem reduceSelectedIndividuallyItem = new JMenuItem("Reduce by selected clauses individually");
        reduceSelectedIndividuallyItem.setActionCommand("reduceSelIndiv");
        reduceSelectedIndividuallyItem.addActionListener(l);
        add(reduceSelectedIndividuallyItem);
        
        addSeparator();
        
        JMenuItem findAutarkiesItem = new JMenuItem("Find autarkies");
        findAutarkiesItem.setEnabled(false);
        findAutarkiesItem.setActionCommand("findAutarkies");
        findAutarkiesItem.addActionListener(l);
        add(findAutarkiesItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, MUCStepView view, MUCInstance kahina)
    {
        return new MUCStepViewContextMenu(l, view, kahina);
    }
}
