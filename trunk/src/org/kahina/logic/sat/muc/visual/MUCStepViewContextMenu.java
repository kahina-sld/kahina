package org.kahina.logic.sat.muc.visual;

import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.kahina.logic.sat.muc.MUCInstance;

public class MUCStepViewContextMenu extends JPopupMenu
{
    MUCStepViewPanel view;
    MUCInstance kahina;
    
    public MUCStepViewContextMenu(ActionListener l, MUCStepViewPanel view, MUCInstance kahina, int listIndex)
    {
        this.view = view;
        this.kahina = kahina;
        
        JMenuItem selectAllItem = new JMenuItem("Select all");
        if (view.view.currentStep == null)
        {
            selectAllItem.setEnabled(false);
        }
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
        reduceItem.setActionCommand("reduce" + listIndex);
        reduceItem.addActionListener(l);
        add(reduceItem);
        
        JMenuItem reduceMRItem = new JMenuItem("Reduce by this clause + model rotation");
        selectDialogItem.setEnabled(false);
        reduceMRItem.setActionCommand("reduceMR" + listIndex);
        reduceMRItem.addActionListener(l);
        add(reduceMRItem);
        
        JMenuItem reduceSelectedOnceItem = new JMenuItem("Reduce by selected clauses at once");
        if (view.getList().getSelectedIndices().length == 0)
        {
            reduceSelectedOnceItem.setEnabled(false);
        }
        reduceSelectedOnceItem.setActionCommand("redSelOnce");
        reduceSelectedOnceItem.addActionListener(l);
        add(reduceSelectedOnceItem);
        
        JMenuItem reduceSelectedIndividuallyItem = new JMenuItem("Reduce by selected clauses individually");
        if (view.getList().getSelectedIndices().length == 0)
        {
            reduceSelectedIndividuallyItem.setEnabled(false);
        }
        reduceSelectedIndividuallyItem.setActionCommand("redSelIndiv");
        reduceSelectedIndividuallyItem.addActionListener(l);
        add(reduceSelectedIndividuallyItem);
        
        addSeparator();
        
        JMenuItem findAutarkiesItem = new JMenuItem("Find autarkies");
        findAutarkiesItem.setEnabled(false);
        findAutarkiesItem.setActionCommand("findAutarkies");
        findAutarkiesItem.addActionListener(l);
        add(findAutarkiesItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, MUCStepViewPanel view, MUCInstance kahina, int listIndex)
    {
        return new MUCStepViewContextMenu(l, view, kahina, listIndex);
    }
}
