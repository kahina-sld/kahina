package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaDialogEvent;

public class KahinaBreakpointMenu  extends JMenu implements ActionListener
{
    private static final long serialVersionUID = -1290849167486564257L;

    public KahinaBreakpointMenu()
    {
        super("Breakpoints");       
        
        JMenuItem primaryBreakpointsItem = new JMenuItem("Breakpoints (search tree)");
        primaryBreakpointsItem.setActionCommand("editPrimaryBreakpoints");
        primaryBreakpointsItem.addActionListener(this);
        this.add(primaryBreakpointsItem);
        
        JMenuItem secondaryBreakpointsItem = new JMenuItem("Breakpoints (call tree)");
        secondaryBreakpointsItem.setActionCommand("editSecondaryBreakpoints");
        secondaryBreakpointsItem.addActionListener(this);
        this.add(secondaryBreakpointsItem);
        
        this.addSeparator();       
        
        JMenuItem primaryWarnPointsItem = new JMenuItem("Warn points (search tree)");
        primaryWarnPointsItem.setActionCommand("editPrimaryWarnPoints");
        primaryWarnPointsItem.addActionListener(this);
        this.add(primaryWarnPointsItem);
        
        JMenuItem secondaryWarnPointsItem = new JMenuItem("Warn points (call tree)");
        secondaryWarnPointsItem.setActionCommand("editSecondaryWarnPpoints");
        secondaryWarnPointsItem.addActionListener(this);
        this.add(secondaryWarnPointsItem);
        
        this.addSeparator();             
        
        JMenuItem creepPointsItem = new JMenuItem("Creep points (call tree)");
        creepPointsItem.setActionCommand("editCreepPoints");
        creepPointsItem.addActionListener(this);
        this.add(creepPointsItem);
        
        JMenuItem failPointsItem = new JMenuItem("Fail points (call tree)");
        failPointsItem.setActionCommand("editFailPoints");
        failPointsItem.addActionListener(this);
        this.add(failPointsItem);
        
        JMenuItem skipPointsItem = new JMenuItem("Skip points (call tree)");
        skipPointsItem.setActionCommand("editSkipPoints");
        skipPointsItem.addActionListener(this);
        this.add(skipPointsItem);
    }
    
    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("editPrimaryBreakpoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PRIMARY_BREAKPOINTS));
        }
        else if (s.equals("editSecondaryBreakpoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.SECONDARY_BREAKPOINTS));
        }
        if (s.equals("editPrimaryWarnPoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.PRIMARY_WARN_POINTS));
        }
        else if (s.equals("editSecondaryWarnPoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.SECONDARY_WARN_POINTS));
        }
        else if (s.equals("editSkipPoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.SKIP_POINTS));
        }
        else if (s.equals("editCreepPoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.CREEP_POINTS));
        }
        else if (s.equals("editFailPoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.FAIL_POINTS));
        }
    }
}
