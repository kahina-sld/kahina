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

import org.kahina.core.data.breakpoint.KahinaControlPoint;
import org.kahina.core.data.breakpoint.KahinaControlPointProfile;
import org.kahina.core.io.util.XMLUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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
            //TODO: adapt type argument, 0 is just the value for breakpoints!
            KahinaControlPoint newControlPoint = new KahinaControlPoint(0);
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
                KahinaControlPoint newControlPoint = KahinaControlPoint.importXML(dom.getDocumentElement());
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
            profilePanel.view.display(new KahinaControlPointProfile());
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
                profilePanel.view.display(KahinaControlPointProfile.importXML(dom.getDocumentElement()));
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
