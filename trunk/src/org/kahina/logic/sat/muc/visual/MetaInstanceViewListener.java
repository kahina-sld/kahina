package org.kahina.logic.sat.muc.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JList;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.gui.ClauseSelectionEvent;

public class MetaInstanceViewListener  extends MouseAdapter implements ActionListener
{
    private final MUCInstance kahina;
    private final MetaInstanceViewPanel view;
    
    long lastClick = 0;
    public final static long DBL_CLICK_INTERVAL = 200;
    
    public MetaInstanceViewListener(MUCInstance kahina, MetaInstanceViewPanel view)
    {
        this.kahina = kahina;
        this.view = view;
    }
    
    @Override
    public void mouseClicked(MouseEvent e)
    {
        if (e.getSource() instanceof JList)
        {
            int listIndex = ((JList) e.getSource()).locationToIndex(new Point(e.getX(), e.getY()));
            long time = System.currentTimeMillis();
            //System.err.println("label: " + label + " interval: " + (time - lastClick) + " = " + time + " - " + lastClick);
            if (time - lastClick > DBL_CLICK_INTERVAL)
            {

            }
            else
            {

            }
        }
    }
    
    public void mousePressed(MouseEvent e)
    {
        maybeShowPopup(e);
    }
    
    public void mouseReleased(MouseEvent e)
    {
        maybeShowPopup(e);
    }
    
    protected void maybeShowPopup(MouseEvent e) 
    {
        if (e.isPopupTrigger()) 
        {
            int listIndex = ((JList) e.getSource()).locationToIndex(new Point(e.getX(), e.getY()));
            MetaInstanceViewContextMenu.getMenu(this, view, kahina, listIndex).show(e.getComponent(),e.getX(), e.getY());
        }
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
    }
}
