package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import org.kahina.core.KahinaRunner;
import org.kahina.core.event.KahinaDialogEvent;
import org.kahina.core.event.KahinaSystemEvent;

public class KahinaBreakpointMenu  extends JMenu implements ActionListener
{
    private static final long serialVersionUID = -1290849167486564257L;

    public KahinaBreakpointMenu()
    {
        super("Breakpoints");       
        
        JMenuItem primaryBreakpointsItem = new JMenuItem("Primary Breakpoints");
        primaryBreakpointsItem.setActionCommand("editPrimaryBreakpoints");
        primaryBreakpointsItem.addActionListener(this);
        this.add(primaryBreakpointsItem);
        
        JMenuItem secondaryBreakpointsItem = new JMenuItem("Secondary Breakpoints");
        secondaryBreakpointsItem.setActionCommand("editSecondaryBreakpoints");
        secondaryBreakpointsItem.addActionListener(this);
        this.add(secondaryBreakpointsItem);
        
        this.addSeparator();
             
        JMenuItem parseOptionsItem = new JMenuItem("Skip Points");
        parseOptionsItem.setActionCommand("editSkipPoints");
        parseOptionsItem.addActionListener(this);
        this.add(parseOptionsItem);
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
        else if (s.equals("editSkipPoints"))
        {
            KahinaRunner.processEvent(new KahinaDialogEvent(KahinaDialogEvent.SKIP_POINTS));
        }
    }
}
