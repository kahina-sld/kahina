package org.kahina.core.test;

import java.awt.Color;
import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.kahina.core.control.KahinaController;
import org.kahina.core.control.KahinaEventTypes;
import org.kahina.core.data.tree.KahinaMemTree;
import org.kahina.core.data.tree.KahinaTree;
import org.kahina.core.data.tree.LayerDecider;
import org.kahina.core.visual.tree.KahinaTreeView;
import org.kahina.core.visual.tree.KahinaTreeViewMarker;
import org.kahina.core.visual.tree.KahinaTreeViewOptions;
import org.kahina.core.visual.tree.KahinaTreeViewPanel;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class KahinaTreeTest
{	
    public static void main(String[] args)
    {
        try
        {
        	LayerDecider decider = new TestLayerDecider();
        	
            File file = new File("src/org/kahina/core/test/trale-tree.xml");
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document dom = db.parse(file);
            //TestLayeredTree m1 = TestLayeredTree.importXML(dom);
            KahinaTree m1 = KahinaMemTree.importXML(dom, decider);
            
            file = new File("src/org/kahina/core/test/trale-tree2.xml");
            dbf = DocumentBuilderFactory.newInstance();
            db = dbf.newDocumentBuilder();
            dom = db.parse(file);
            TestLayeredTree m2 = TestLayeredTree.importXML(dom);
            //KahinaTree m2 = KahinaDbTree.importXML(dom, decider, data, m1);
            
            KahinaController control = new KahinaController();
            
            KahinaTreeView v0 = new KahinaTreeView(control);
            v0.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
            v0.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
            v0.getConfig().setSecondaryLineShapePolicy(KahinaTreeViewOptions.INVISIBLE_LINES);
            v0.getConfig().setVerticalDistance(3);
            v0.getConfig().setHorizontalDistance(18);
            v0.display(m1,0,17);
            v0.displaySecondaryTree(m2);
            v0.getConfig().toggleSecondDimensionDisplay();
            
            v0.setStatusColorEncoding(0,new Color(0,255,0));
            v0.setStatusColorEncoding(1,new Color(255,0,0));
            v0.setStatusColorEncoding(2,new Color(0,255,255));
            v0.setStatusColorEncoding(3,new Color(255,255,255)); 
            
            control.registerListener(KahinaEventTypes.SELECTION, v0);
            control.registerListener(KahinaEventTypes.UPDATE, v0);
            
            KahinaTreeView v1 = new KahinaTreeView(control);
            v1.getConfig().setLineShapePolicy(KahinaTreeViewOptions.STRAIGHT_LINES);
            v1.getConfig().setNodePositionPolicy(KahinaTreeViewOptions.CENTERED_NODES);
            v1.getConfig().setSecondaryLineShapePolicy(KahinaTreeViewOptions.INVISIBLE_LINES);
            v1.getConfig().setVerticalDistance(3);
            v1.getConfig().setHorizontalDistance(18);
            v1.display(m1,1,17);
            v1.displaySecondaryTree(m2);
            v1.getConfig().toggleSecondDimensionDisplay();
            
            v1.setStatusColorEncoding(0,new Color(0,255,0));
            v1.setStatusColorEncoding(1,new Color(255,0,0));
            v1.setStatusColorEncoding(2,new Color(0,255,255));
            v1.setStatusColorEncoding(3,new Color(255,255,255));  
            
            control.registerListener(KahinaEventTypes.SELECTION, v1);
            control.registerListener(KahinaEventTypes.UPDATE, v1);
            
            KahinaTreeView v2 = new KahinaTreeView(control);
            v2.getConfig().setHorizontalDistance(15);
            v2.display(m1,2,17);
            v2.displaySecondaryTree(m2);
            
            v2.setStatusColorEncoding(0,new Color(0,255,0));
            v2.setStatusColorEncoding(1,new Color(255,0,0));
            v2.setStatusColorEncoding(2,new Color(0,255,255));
            v2.setStatusColorEncoding(3,new Color(255,255,255));  
            
            control.registerListener(KahinaEventTypes.SELECTION, v2);
            control.registerListener(KahinaEventTypes.UPDATE, v2);

            KahinaTreeViewMarker treeMarker = new KahinaTreeViewMarker(m1,m2);
            KahinaTreeViewPanel vp0 = new KahinaTreeViewPanel(treeMarker, control);
            JScrollPane vp0pane = new JScrollPane(vp0);
            vp0pane.setBounds(0, 30, 500, 200);
            JLabel vp0label = new JLabel("Layer 0 (Rule applications)");
            vp0label.setBounds(0, 10, 500, 15);
            KahinaTreeViewPanel vp1 = new KahinaTreeViewPanel(treeMarker, control);
            JScrollPane vp1pane = new JScrollPane(vp1);
            vp1pane.setBounds(0, 260, 500, 200);
            JLabel vp1label = new JLabel("Layer 1 (Goal calls)");
            vp1label.setBounds(0, 240, 500, 15);
            KahinaTreeViewPanel vp2 = new KahinaTreeViewPanel(treeMarker, control);
            JScrollPane vp2pane = new JScrollPane(vp2);
            vp2pane.setBounds(0, 490, 500, 200);
            JLabel vp2label = new JLabel("Layer 2 (Detail View)");
            vp2label.setBounds(0, 470, 500, 15);
            
            control.registerListener("redraw", vp0);
            control.registerListener("redraw", vp1);
            control.registerListener("redraw", vp2);
            
            JFrame w = new JFrame("Kahina TreeView Demo");
            w.setSize(510, 720);
            w.setLayout(null);
            w.add(vp0label);
            w.add(vp0pane);
            w.add(vp1label);
            w.add(vp1pane);
            w.add(vp2label);
            w.add(vp2pane);
            w.setVisible(true);
            w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            w.setResizable(false);
            vp0.setView(v0); 
            vp1.setView(v1); 
            vp2.setView(v2); 
        }
        catch (ParserConfigurationException e)
        {
            System.err.println(e.getMessage());
        }
        catch (SAXException e)
        {
            System.err.println(e.getMessage());
        }
        catch (IOException e)
        {
            System.err.println(e.getMessage());
        }
    }
}
