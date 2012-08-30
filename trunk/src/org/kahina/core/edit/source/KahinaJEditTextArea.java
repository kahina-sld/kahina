package org.kahina.core.edit.source;

import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.gjt.sp.jedit.IPropertyManager;
import org.gjt.sp.jedit.textarea.StandaloneTextArea;

public class KahinaJEditTextArea extends StandaloneTextArea
{
    private ActionListener actionListener;
    
    public KahinaJEditTextArea(IPropertyManager propMng, ActionListener actionListener)
    {
        super(propMng);
        this.actionListener = actionListener;
    }
    
    public void createPopupMenu(MouseEvent evt)
    {
        super.createPopupMenu(evt);
        JPopupMenu menu = getRightClickPopup();
        
        menu.addSeparator();
        
        JMenuItem addLineBreakPointItem = new JMenuItem("Add Break Point");
        addLineBreakPointItem.setActionCommand("addLineBreakPoint");
        addLineBreakPointItem.addActionListener(actionListener);
        menu.add(addLineBreakPointItem);
        
        JMenuItem addLineCreepPointItem = new JMenuItem("Add Creep Point");
        addLineCreepPointItem.setActionCommand("addLineCreepPoint");
        addLineCreepPointItem.addActionListener(actionListener);
        menu.add(addLineCreepPointItem);
        
        JMenuItem addLineCompletePointItem = new JMenuItem("Add Complete Point");
        addLineCompletePointItem.setActionCommand("addLineCompletePoint");
        addLineCompletePointItem.addActionListener(actionListener);
        menu.add(addLineCompletePointItem);

        JMenuItem addLineSkipPointItem = new JMenuItem("Add Skip Point");
        addLineSkipPointItem.setActionCommand("addLineSkipPoint");
        addLineSkipPointItem.addActionListener(actionListener);
        menu.add(addLineSkipPointItem);
        
        JMenuItem addLineFailPointItem = new JMenuItem("Add Fail Point");
        addLineFailPointItem.setActionCommand("addLineFailPoint");
        addLineFailPointItem.addActionListener(actionListener);
        menu.add(addLineFailPointItem);
        
    }
}
