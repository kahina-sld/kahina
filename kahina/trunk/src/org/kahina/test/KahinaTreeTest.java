package org.kahina.test;

import java.awt.Color;
import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.kahina.data.KahinaTypeException;
import org.kahina.data.tree.KahinaDbTree;
import org.kahina.data.tree.KahinaTree;
import org.kahina.data.tree.LayerDecider;
import org.kahina.io.database.DatabaseHandler;
import org.kahina.visual.tree.KahinaTreeView;
import org.kahina.visual.tree.KahinaTreeViewMarker;
import org.kahina.visual.tree.KahinaTreeViewPanel;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class KahinaTreeTest
{	
    public static void main(String[] args)
    {
        try
        {
        	LayerDecider decider = new TestLayerDecider();
        	DatabaseHandler data = new DatabaseHandler();
        	
            File file = new File("src/org/kahina/test/trale-tree.xml");
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document dom = db.parse(file);
            //TestLayeredTree m1 = TestLayeredTree.importXML(dom);
            KahinaTree m1 = KahinaDbTree.importXML(dom, decider, data, null);
            
            file = new File("src/org/kahina/test/trale-tree2.xml");
            dbf = DocumentBuilderFactory.newInstance();
            db = dbf.newDocumentBuilder();
            dom = db.parse(file);
            //TestLayeredTree m2 = TestLayeredTree.importXML(dom);
            KahinaTree m2 = KahinaDbTree.importXML(dom, decider, data, m1);
            
            KahinaTreeView v0 = new KahinaTreeView();
            v0.setLineShapePolicy(KahinaTreeView.STRAIGHT_LINES);
            v0.setNodePositionPolicy(KahinaTreeView.CENTERED_NODES);
            v0.setSecondaryLineShapePolicy(KahinaTreeView.INVISIBLE_LINES);
            v0.setVerticalDistance(3);
            v0.setHorizontalDistance(18);
            v0.display(m1,0,17);
            v0.displaySecondaryTree(m2);
            v0.toggleSecondDimensionDisplay();
            
            v0.setStatusColorEncoding(0,new Color(0,255,0));
            v0.setStatusColorEncoding(1,new Color(255,0,0));
            v0.setStatusColorEncoding(2,new Color(0,255,255));
            v0.setStatusColorEncoding(3,new Color(255,255,255));  
            
            KahinaTreeView v1 = new KahinaTreeView();
            v1.setLineShapePolicy(KahinaTreeView.STRAIGHT_LINES);
            v1.setNodePositionPolicy(KahinaTreeView.CENTERED_NODES);
            v1.setSecondaryLineShapePolicy(KahinaTreeView.INVISIBLE_LINES);
            v1.setVerticalDistance(3);
            v1.setHorizontalDistance(18);
            v1.display(m1,1,17);
            v1.displaySecondaryTree(m2);
            v1.toggleSecondDimensionDisplay();
            
            v1.setStatusColorEncoding(0,new Color(0,255,0));
            v1.setStatusColorEncoding(1,new Color(255,0,0));
            v1.setStatusColorEncoding(2,new Color(0,255,255));
            v1.setStatusColorEncoding(3,new Color(255,255,255));  
            
            KahinaTreeView v2 = new KahinaTreeView();
            v2.setHorizontalDistance(15);
            v2.display(m1,2,17);
            v2.displaySecondaryTree(m2);
            
            v2.setStatusColorEncoding(0,new Color(0,255,0));
            v2.setStatusColorEncoding(1,new Color(255,0,0));
            v2.setStatusColorEncoding(2,new Color(0,255,255));
            v2.setStatusColorEncoding(3,new Color(255,255,255));  

            KahinaTreeViewMarker treeMarker = new KahinaTreeViewMarker(m1,m2);
            KahinaTreeViewPanel vp0 = new KahinaTreeViewPanel(treeMarker);
            JScrollPane vp0pane = new JScrollPane(vp0);
            vp0pane.setBounds(0, 30, 500, 200);
            JLabel vp0label = new JLabel("Layer 0 (Rule applications)");
            vp0label.setBounds(0, 10, 500, 15);
            KahinaTreeViewPanel vp1 = new KahinaTreeViewPanel(treeMarker);
            JScrollPane vp1pane = new JScrollPane(vp1);
            vp1pane.setBounds(0, 260, 500, 200);
            JLabel vp1label = new JLabel("Layer 1 (Goal calls)");
            vp1label.setBounds(0, 240, 500, 15);
            KahinaTreeViewPanel vp2 = new KahinaTreeViewPanel(treeMarker);
            JScrollPane vp2pane = new JScrollPane(vp2);
            vp2pane.setBounds(0, 490, 500, 200);
            JLabel vp2label = new JLabel("Layer 2 (Detail View)");
            vp2label.setBounds(0, 470, 500, 15);
            
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
        catch (KahinaTypeException e)
        {
            System.err.println(e.getMessage());
        }
    }
}
