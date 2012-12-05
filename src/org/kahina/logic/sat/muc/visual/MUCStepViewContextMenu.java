package org.kahina.logic.sat.muc.visual;

import java.awt.event.ActionListener;

import javax.swing.JMenu;
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
        
        JMenu subselectionMenu = new JMenu("Subselection");
        if (view.getList().getSelectedIndices().length == 0)
        {
            subselectionMenu.setEnabled(false);
        }
        
        JMenu byStatusMenu = new JMenu("by status");
        
        JMenuItem unknownStatusItem = new JMenuItem("clauses of unkown status");
        unknownStatusItem.setActionCommand("unknownStatusSubselection");
        unknownStatusItem.addActionListener(l);
        byStatusMenu.add(unknownStatusItem);
        
        JMenuItem fallAwayStatusItem = new JMenuItem("fall-away clauses");
        fallAwayStatusItem.setActionCommand("fallAwayStatusSubselection");
        fallAwayStatusItem.addActionListener(l);
        byStatusMenu.add(fallAwayStatusItem);
        
        JMenuItem reducedStatusItem = new JMenuItem("explicitly reduced clauses");
        reducedStatusItem.setActionCommand("reducedStatusSubselection");
        reducedStatusItem.addActionListener(l);
        byStatusMenu.add(reducedStatusItem);
        
        JMenuItem criticalStatusItem = new JMenuItem("critical clauses");
        criticalStatusItem.setActionCommand("criticalStatusSubselection");
        criticalStatusItem.addActionListener(l);
        byStatusMenu.add(criticalStatusItem);
        
        subselectionMenu.add(byStatusMenu);
        
        JMenu bySizeMenu = new JMenu("by size");
        subselectionMenu.add(bySizeMenu);
        
        JMenu firstMenu = new JMenu("first");
        subselectionMenu.add(firstMenu);
        
        JMenu lastMenu = new JMenu("last");
        subselectionMenu.add(lastMenu);
        
        JMenu randomMenu = new JMenu("random");
        subselectionMenu.add(randomMenu);
        
        JMenuItem literalSelectionItem = new JMenu("containing literal ...");
        subselectionMenu.add(literalSelectionItem);
        
        add(subselectionMenu);
        
        addSeparator();
        
        JMenuItem reduceItem = new JMenuItem("Reduce by this clause (= double-click)");
        reduceItem.setActionCommand("reduce" + listIndex);
        reduceItem.addActionListener(l);
        add(reduceItem);
        
        JMenuItem reduceMRItem = new JMenuItem("Reduce by this clause + model rotation");
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
        
        JMenuItem findAutarkiesItem = new JMenuItem("Reduce to Lean Kernel");
        if (view.view.currentStep == null)
        {
            findAutarkiesItem.setEnabled(false);
        }
        findAutarkiesItem.setActionCommand("leanKernel");
        findAutarkiesItem.addActionListener(l);
        add(findAutarkiesItem);
    }
    
    public static JPopupMenu getMenu(ActionListener l, MUCStepViewPanel view, MUCInstance kahina, int listIndex)
    {
        return new MUCStepViewContextMenu(l, view, kahina, listIndex);
    }
}
