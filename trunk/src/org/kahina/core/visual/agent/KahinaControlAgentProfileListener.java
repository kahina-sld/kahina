package org.kahina.core.visual.agent;

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
import org.kahina.core.data.agent.KahinaControlAgent;
import org.kahina.core.data.agent.KahinaControlAgentProfile;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.io.util.XMLUtil;
import org.kahina.lp.LogicProgrammingState;
import org.kahina.lp.data.breakpoint.LogicProgrammingControlAgent;
import org.kahina.lp.data.breakpoint.LogicProgrammingControlAgentProfile;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class KahinaControlAgentProfileListener implements ActionListener, ListSelectionListener
{
    KahinaControlAgentProfileViewPanel profilePanel;
    KahinaTree stepTree;
    
    public KahinaControlAgentProfileListener(KahinaControlAgentProfileViewPanel profilePanel, KahinaTree stepTree)
    {
        this.profilePanel = profilePanel;
        this.stepTree = stepTree;
    }

    public void actionPerformed(ActionEvent e)
    {
        String s = e.getActionCommand();
        if (s.equals("newControlPoint"))
        {
            LogicProgrammingControlAgent newControlAgent = new LogicProgrammingControlAgent(profilePanel.getKahina().getControl(), stepTree);
            profilePanel.view.getModel().addControlAgent(newControlAgent);
            profilePanel.pointList.setListData(profilePanel.view.getModel().getControlPoints());
            profilePanel.pointList.setSelectedIndex(profilePanel.view.getModel().getSize() - 1);
        } 
        if (s.equals("loadControlPoint"))
        {
            JFileChooser chooser = new JFileChooser(new File("."));
            chooser.showOpenDialog(profilePanel);
            File inputFile = chooser.getSelectedFile();
            try
            {
                Document dom = XMLUtil.parseXMLStream(new FileInputStream(inputFile), false);
                LogicProgrammingControlAgent newControlAgent = LogicProgrammingControlAgent.importXML(dom.getDocumentElement(), profilePanel.getKahina().getControl(), stepTree);
                profilePanel.view.getModel().addControlAgent(newControlAgent);
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
            profilePanel.removeCurrentControlAgent();
            profilePanel.pointList.setListData(profilePanel.view.getModel().getControlPoints());
        }
        else if (s.equals("newProfile"))
        {
            profilePanel.view.display(new KahinaControlAgentProfile(profilePanel.view.getModel().getActuator()));
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
                profilePanel.view.display(LogicProgrammingControlAgentProfile.importXML(dom.getDocumentElement(), actuator));
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
        System.err.println("curID = " + curID);
        if (curID == -1)
        {
            profilePanel.pointPanel.view.display(null);
        } 
        else
        {
            profilePanel.pointPanel.view.display(profilePanel.view.getModel().getControlAgent(curID));
        }
        profilePanel.pointPanel.updateDisplay();
        profilePanel.pointPanel.revalidate();
        //TODO: deal with the activation status!
        //adaptActivationStatus();
    }  
}
