package org.kahina.core.visual.breakpoint;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.kahina.core.data.breakpoint.KahinaBreakpoint;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.patterns.TreeAutomaton;
import org.kahina.core.edit.breakpoint.BreakpointEditorEvent;

public class KahinaControlPointProfileListener implements ActionListener, ListSelectionListener
{
    KahinaControlPointProfileViewPanel profilePanel;
    
    public KahinaControlPointProfileListener(KahinaControlPointProfileViewPanel profilePanel)
    {
        this.profilePanel = profilePanel;
    }

    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newControlPoint"))
        {
            //TODO: adapt type argument, 0 is not an ideal default value!
            KahinaControlPoint newControlPoint = new KahinaControlPoint(0);
            profilePanel.view.getModel().addControlPoint(newControlPoint);
            profilePanel.pointList.setListData(profilePanel.view.getModel().getControlPoints());
            profilePanel.pointList.setSelectedIndex(profilePanel.view.getModel().getSize() - 1);
        } 
        //TODO: reactivate these parts!
        /*else if (s.equals("activateBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.ACTIVATE_BREAKPOINT, curID));
        } 
        else if (s.equals("deactivateBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.DEACTIVATE_BREAKPOINT, curID));
        } 
        else if (s.equals("removeBreakpoint"))
        {
            control.processEvent(new BreakpointEditorEvent(BreakpointEditorEvent.REMOVE_BREAKPOINT, curID));
        }*/
        //TODO: new profile, import profile, save profile etc.
        //TODO: deal with the activation status!
        //adaptActivationStatus();
    }

    public void valueChanged(ListSelectionEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }  
}
