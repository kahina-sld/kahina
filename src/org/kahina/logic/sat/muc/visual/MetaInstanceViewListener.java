package org.kahina.logic.sat.muc.visual;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import javax.swing.JList;

import org.kahina.logic.sat.muc.MUCInstance;
import org.kahina.logic.sat.muc.MUCStep;
import org.kahina.logic.sat.muc.data.MUCMetaInstance;
import org.kahina.logic.sat.muc.data.PartitionBlockHandler;
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
        if (s.startsWith("strengthenWedge"))
        {
            int clauseIndex = Integer.parseInt(s.substring(15));
            List<Integer> clause = new LinkedList<Integer>();
            clause.addAll(kahina.getState().getMetaInstance().getClause(clauseIndex));
            //TODO: start an expansion agent (needs to be implemented)
        }
        else if (s.equals("printStatistics"))
        {
            int numOrigClauses = kahina.getSatInstance().getSize();
            PartitionBlockHandler blocks = kahina.getState().getPartitionBlocks();
            MUCMetaInstance metaInstance = kahina.getState().getMetaInstance();
            int uncompressedNumClauses = 0;
            int uncompressedSumClauseLength = 0;
            int compressedNumClauses = metaInstance.getSize();
            int compressedSumClauseLength = 0;
            for (int idx = 0; idx < compressedNumClauses; idx++)
            {
                compressedSumClauseLength += metaInstance.getClause(idx).size();
                boolean isBlockDefinition = false;
                int uncompressedClauseLength = 0;
                for (int lit : metaInstance.getClause(idx))
                {
                    if (lit < -numOrigClauses) isBlockDefinition = true;
                    if (lit > numOrigClauses)
                    {
                        uncompressedClauseLength += blocks.getBlock(lit - numOrigClauses).size();
                    }
                    else
                    {
                        uncompressedClauseLength++;
                    }
                }
                if (!isBlockDefinition)
                {
                    uncompressedNumClauses++;
                    uncompressedSumClauseLength += uncompressedClauseLength;
                }
            }
            System.err.println("uncompressedNumClauses:\t" + uncompressedNumClauses);
            System.err.println("uncompressedSumClauseLength:\t" + uncompressedSumClauseLength);
            System.err.println("compressedNumClauses:\t" + compressedNumClauses);
            System.err.println("compressedSumClauseLength:\t" + compressedSumClauseLength);
        }
        else if (s.equals("refresh"))
        {
            view.view.getModel().announceChangedClauses();
            view.view.requireRedraw();
            view.view.recalculate();
            view.updateDisplay();
        }
    }
}
