package org.kahina.test;

import java.awt.Color;
import java.awt.Font;
import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import org.kahina.data.KahinaDataHandlingMethod;
import org.kahina.data.KahinaTypeException;
import org.kahina.data.chart.*;
import org.kahina.io.chart.KahinaChartIO;
import org.kahina.visual.chart.KahinaChartView;
import org.kahina.visual.chart.KahinaChartViewMarker;
import org.kahina.visual.chart.KahinaChartViewPanel;

public class KahinaChartTest
{
    public static void main(String[] args)
    {
        try
        {
            File file = new File("org/kahina/test/test-chart.xml");
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document dom = db.parse(file);
            KahinaChart m = KahinaChartIO.importXML(dom, KahinaDataHandlingMethod.MEMORY);
            KahinaChartView v = new KahinaChartView();
            v.display(m);       

            v.setStatusColorEncoding(0,new Color(100,255,100));
            v.setStatusColorEncoding(1,new Color(255,100,100));
            v.setStatusColorEncoding(2,new Color(0,255,0));
            v.setStatusColorEncoding(3,new Color(255,0,0));  
            
            v.setStatusFontEncoding(2, new Font(Font.SANS_SERIF,Font.BOLD, 10));
            v.setStatusFontEncoding(3, new Font(Font.SANS_SERIF,Font.BOLD, 10));

            KahinaChartViewMarker chartMarker = new KahinaChartViewMarker(m);
            KahinaChartViewPanel vp = new KahinaChartViewPanel(chartMarker);
            JScrollPane vpp = new JScrollPane(vp);
            vpp.setBounds(0, 0, 500, 300);
            JFrame w = new JFrame("Kahina ChartView Demo");
            w.setSize(510, 330);
            w.setLayout(null);
            w.add(vpp);
            w.setVisible(true);
            w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            w.setResizable(false);
            vp.setView(v);        
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
