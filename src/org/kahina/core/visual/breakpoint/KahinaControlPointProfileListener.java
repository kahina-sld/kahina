package org.kahina.core.visual.breakpoint;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class KahinaControlPointProfileListener implements ActionListener, ListSelectionListener
{
    KahinaControlPointProfileView profileView;
    
    public KahinaControlPointProfileListener(KahinaControlPointProfileViewPanel profileView)
    {
        this.profileView = profileView.view;
    }

    public void actionPerformed(ActionEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }

    public void valueChanged(ListSelectionEvent arg0)
    {
        // TODO Auto-generated method stub
        
    }  
}
