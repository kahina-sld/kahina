package org.kahina.logic.sat.insertionmus.visual;

import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.kahina.logic.sat.insertionmus.MUCInstance;

public class MUCStepViewContextMenu extends JPopupMenu
{
    MUCStepViewPanel view;
    MUCInstance kahina;
    
    public MUCStepViewContextMenu(ActionListener l, MUCStepViewPanel view, MUCInstance kahina, int listIndex)
    {
        this.view = view;
        this.kahina = kahina;
        
        int selectionSize = view.getList().getSelectedIndices().length;
        
        JMenuItem selectAllItem = new JMenuItem("Select all");
        if (view.view.currentStep == null)
        {
            selectAllItem.setEnabled(false);
        }
        selectAllItem.setActionCommand("selectAll");
        selectAllItem.addActionListener(l);
        add(selectAllItem);
        
        JMenu subselectionMenu = new JMenu("Subselection");
        if (selectionSize == 0)
        {
            subselectionMenu.setEnabled(false);
        }
        
        JMenu byStatusMenu = new JMenu("By status");
        
        JMenuItem unknownStatusItem = new JMenuItem("Clauses of unknown status");
        unknownStatusItem.setActionCommand("subselectStatusUnknown");
        unknownStatusItem.addActionListener(l);
        byStatusMenu.add(unknownStatusItem);
        
        JMenuItem fallAwayStatusItem = new JMenuItem("Fall-away clauses");
        fallAwayStatusItem.setActionCommand("subselectStatusFallAway");
        fallAwayStatusItem.addActionListener(l);
        byStatusMenu.add(fallAwayStatusItem);
        
        JMenuItem reducedStatusItem = new JMenuItem("Explicitly reduced clauses");
        reducedStatusItem.setActionCommand("subselectStatusReduced");
        reducedStatusItem.addActionListener(l);
        byStatusMenu.add(reducedStatusItem);
        
        JMenuItem criticalStatusItem = new JMenuItem("Critical clauses");
        criticalStatusItem.setActionCommand("subselectStatusCritical");
        criticalStatusItem.addActionListener(l);
        byStatusMenu.add(criticalStatusItem);
        
        subselectionMenu.add(byStatusMenu);
        
        JMenu bySizeMenu = new JMenu("By size");
        JMenuItem unitItem = new JMenuItem("unit clauses");
        unitItem.setActionCommand("subselectSize1");
        unitItem.addActionListener(l);
        bySizeMenu.add(unitItem);
        for (int i = 2; i <= 5; i++)
        {
            JMenuItem sizeItem = new JMenuItem(i + " literals");
            sizeItem.setActionCommand("subselectSize" + i);
            sizeItem.addActionListener(l);
            bySizeMenu.add(sizeItem);
        }
        JMenuItem largeSizeItem = new JMenuItem("more than 5 literals");
        largeSizeItem.setActionCommand("subselectSizeLarge");
        largeSizeItem.addActionListener(l);
        bySizeMenu.add(largeSizeItem);
        subselectionMenu.add(bySizeMenu);
        
        JMenu firstMenu = new JMenu("First");
        JMenuItem first1Item = new JMenuItem("clause");
        first1Item.setActionCommand("subselectFirst1");
        if (selectionSize < 1)
        {
            first1Item.setEnabled(false);
        }
        first1Item.addActionListener(l);
        firstMenu.add(first1Item);
        JMenuItem first2Item = new JMenuItem("2 clauses");
        first2Item.setActionCommand("subselectFirst2");
        first2Item.addActionListener(l);
        if (selectionSize < 2)
        {
            first2Item.setEnabled(false);
        }
        firstMenu.add(first2Item);
        JMenuItem first5Item = new JMenuItem("5 clauses");
        first5Item.setActionCommand("subselectFirst5");
        first5Item.addActionListener(l);
        if (selectionSize < 5)
        {
            first5Item.setEnabled(false);
        }
        firstMenu.add(first5Item);
        JMenuItem first10Item = new JMenuItem("10 clauses");
        first10Item.setActionCommand("subselectFirst10");
        first10Item.addActionListener(l);
        if (selectionSize < 10)
        {
            first10Item.setEnabled(false);
        }
        firstMenu.add(first10Item);
        JMenuItem first20Item = new JMenuItem("20 clauses");
        first20Item.setActionCommand("subselectFirst20");
        first20Item.addActionListener(l);
        if (selectionSize < 20)
        {
            first20Item.setEnabled(false);
        }
        firstMenu.add(first20Item);
        JMenuItem first50Item = new JMenuItem("50 clauses");
        first50Item.setActionCommand("subselectFirst50");
        first50Item.addActionListener(l);
        if (selectionSize < 50)
        {
            first50Item.setEnabled(false);
        }
        firstMenu.add(first50Item);
        JMenuItem first100Item = new JMenuItem("100 clauses");
        first100Item.setActionCommand("subselectFirst100");
        first100Item.addActionListener(l);
        if (selectionSize < 100)
        {
            first100Item.setEnabled(false);
        }
        firstMenu.add(first100Item);
        subselectionMenu.add(firstMenu);
        
        JMenu lastMenu = new JMenu("Last");
        JMenuItem last1Item = new JMenuItem("clause");
        last1Item.setActionCommand("subselectLast1");
        last1Item.addActionListener(l);
        if (selectionSize < 1)
        {
            last1Item.setEnabled(false);
        }
        lastMenu.add(last1Item);
        JMenuItem last2Item = new JMenuItem("2 clauses");
        last2Item.setActionCommand("subselectLast2");
        last2Item.addActionListener(l);
        if (selectionSize < 2)
        {
            last2Item.setEnabled(false);
        }
        lastMenu.add(last2Item);
        JMenuItem last5Item = new JMenuItem("5 clauses");
        last5Item.setActionCommand("subselectLast5");
        last5Item.addActionListener(l);
        if (selectionSize < 5)
        {
            last5Item.setEnabled(false);
        }
        lastMenu.add(last5Item);
        JMenuItem last10Item = new JMenuItem("10 clauses");
        last10Item.setActionCommand("subselectLast10");
        last10Item.addActionListener(l);
        if (selectionSize < 10)
        {
            last10Item.setEnabled(false);
        }
        lastMenu.add(last10Item);
        JMenuItem last20Item = new JMenuItem("20 clauses");
        last20Item.setActionCommand("subselectLast20");
        last20Item.addActionListener(l);
        if (selectionSize < 20)
        {
            last20Item.setEnabled(false);
        }
        lastMenu.add(last20Item);
        JMenuItem last50Item = new JMenuItem("50 clauses");
        last50Item.setActionCommand("subselectLast50");
        last50Item.addActionListener(l);
        if (selectionSize < 50)
        {
            last50Item.setEnabled(false);
        }
        lastMenu.add(last50Item);
        JMenuItem last100Item = new JMenuItem("100 clauses");
        last100Item.setActionCommand("subselectLast100");
        last100Item.addActionListener(l);
        if (selectionSize < 100)
        {
            last100Item.setEnabled(false);
        }
        lastMenu.add(last100Item);
        subselectionMenu.add(lastMenu);
        
        JMenu randomMenu = new JMenu("Random");
        JMenuItem random1Item = new JMenuItem("clause");
        random1Item.setActionCommand("subselectRandom1");
        random1Item.addActionListener(l);
        if (selectionSize < 1)
        {
            random1Item.setEnabled(false);
        }
        randomMenu.add(random1Item);
        JMenuItem random2Item = new JMenuItem("2 clauses");
        random2Item.setActionCommand("subselectRandom2");
        random2Item.addActionListener(l);
        if (selectionSize < 2)
        {
            random2Item.setEnabled(false);
        }
        randomMenu.add(random2Item);
        JMenuItem random5Item = new JMenuItem("5 clauses");
        random5Item.setActionCommand("subselectRandom5");
        random5Item.addActionListener(l);
        if (selectionSize < 5)
        {
            random5Item.setEnabled(false);
        }
        randomMenu.add(random5Item);
        JMenuItem random10Item = new JMenuItem("10 clauses");
        random10Item.setActionCommand("subselectRandom10");
        random10Item.addActionListener(l);
        if (selectionSize < 10)
        {
            random10Item.setEnabled(false);
        }
        randomMenu.add(random10Item);
        JMenuItem random20Item = new JMenuItem("20 clauses");
        random20Item.setActionCommand("subselectRandom20");
        random20Item.addActionListener(l);
        if (selectionSize < 20)
        {
            random20Item.setEnabled(false);
        }
        randomMenu.add(random20Item);
        JMenuItem random50Item = new JMenuItem("50 clauses");
        random50Item.setActionCommand("subselectRandom50");
        random50Item.addActionListener(l);
        if (selectionSize < 50)
        {
            random50Item.setEnabled(false);
        }
        randomMenu.add(random50Item);
        JMenuItem random100Item = new JMenuItem("100 clauses");
        random100Item.setActionCommand("subselectRandom100");
        random100Item.addActionListener(l);
        if (selectionSize < 100)
        {
            random100Item.setEnabled(false);
        }
        randomMenu.add(random100Item);
        subselectionMenu.add(randomMenu);
        
        JMenuItem literalSelectionItem = new JMenuItem("containing literal ...");
        literalSelectionItem.setActionCommand("subselectLiteral");
        literalSelectionItem.addActionListener(l);
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
