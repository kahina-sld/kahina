package org.kahina.core.visual.breakpoint;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import javax.swing.JFileChooser;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.kahina.core.control.KahinaControlActuator;
import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.data.breakpoint.LogicProgrammingControlPoint;
import org.kahina.lp.data.breakpoint.LogicProgrammingControlPointProfile;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaControlPointProfileListener implements ActionListener, ListSelectionListener
{
    KahinaControlPointProfileViewPanel profilePanel;
    LogicProgrammingState lpState;
    
    public KahinaControlPointProfileListener(KahinaControlPointProfileViewPanel profilePanel, LogicProgrammingState lpState)
    {
        this.profilePanel = profilePanel;
        this.lpState = lpState;
    }

    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newControlPoint"))
        {
            LogicProgrammingControlPoint newControlPoint = new LogicProgrammingControlPoint(profilePanel.getKahina().getControl(), lpState);
            profilePanel.view.getModel().addControlPoint(newControlPoint);
            profilePanel.pointList.setListData(profilePanel.view.getModel().getControlPoints());
            profilePanel.pointList.setSelectedIndex(profilePanel.view.getModel().getSize() - 1);
        } 
        if (s.equals("loadControlPoint"))
        {
            //TODO: adapt type argument, 0 is just the value for breakpoints!
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.showOpenDialog(profilePanel);
            File inputFile = chooser.getSelectedFile();
            try
            {
                Document dom = XMLUtil.parseXMLStream(new FileInputStream(inputFile), false);
                LogicProgrammingControlPoint newControlPoint = LogicProgrammingControlPoint.importXML(dom.getDocumentElement(), profilePanel.getKahina().getControl(), lpState);
                profilePanel.view.getModel().addControlPoint(newControlPoint);
                profilePanel.pointList.setListData(profilePanel.view.getModel().getControlPoints());
                profilePanel.pointList.setSelectedIndex(profilePanel.view.getModel().getSize() - 1);
            }
            catch (FileNotFoundException ex)
            {
                System.err.println("Input file not found!");
            }           
        } 
        else if (s.equals("removeControlPoint"))
        {
            System.err.println("removeControlPoint");
            profilePanel.removeCurrentControlPoint();
            profilePanel.pointList.setListData(profilePanel.view.getModel().getControlPoints());
        }
        else if (s.equals("newProfile"))
        {
            profilePanel.view.display(new KahinaControlPointProfile(profilePanel.view.getModel().getActuator()));
            profilePanel.updateDisplay();
        }
        else if (s.equals("saveProfile"))
        {      
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.showSaveDialog(profilePanel);
            File outputFile = chooser.getSelectedFile();
            
            Document dom = XMLUtil.newEmptyDocument();
            Element profileElement = profilePanel.view.getModel().exportXML(dom);
            XMLUtil.writeXML(profileElement, outputFile.getAbsolutePath());
        }
        else if (s.equals("loadProfile"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.showOpenDialog(profilePanel);
            File inputFile = chooser.getSelectedFile();
            try
            {
                Document dom = XMLUtil.parseXMLStream(new FileInputStream(inputFile), false);
                KahinaControlActuator actuator = profilePanel.view.getModel().getActuator();
                profilePanel.view.display(LogicProgrammingControlPointProfile.importXML(dom.getDocumentElement(), actuator));
                profilePanel.updateDisplay();
            }
            catch (FileNotFoundException ex)
            {
                System.err.println("Input file not found!");
            }
        }
        //TODO: deal with the activation status!
        //adaptActivationStatus();
    }

    public void valueChanged(ListSelectionEvent arg0)
    {
        int curID = profilePanel.pointList.getSelectedIndex();
        if (curID == -1)
        {
            profilePanel.pointPanel.view.display(null);
        } 
        else
        {
            profilePanel.pointPanel.view.display(profilePanel.view.getModel().getControlPoint(curID));
        }
        profilePanel.pointPanel.updateDisplay();
        profilePanel.pointPanel.revalidate();
        //TODO: deal with the activation status!
        //adaptActivationStatus();
    }  
}
