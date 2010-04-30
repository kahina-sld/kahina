package org.kahina.core.test;

import java.awt.Color;
import java.awt.Font;
import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.kahina.core.data.KahinaDataHandlingMethod;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.io.chart.KahinaChartIO;
import org.kahina.core.io.database.DatabaseHandler;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.core.visual.chart.KahinaChartViewPanel;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class KahinaChartTest
{
    public static void main(String[] args)
    {
        try
        {
            File file = new File("src/org/kahina/test/test-chart.xml");
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document dom = db.parse(file);
            DatabaseHandler data = new DatabaseHandler(new File("otoka.dat"));
            KahinaChart m = KahinaChartIO.importXML(dom, KahinaDataHandlingMethod.DATABASE, data);
            //KahinaChart m = KahinaChartIO.importXML(dom, KahinaDataHandlingMethod.MEMORY, null);
            KahinaChartView v = new KahinaChartView();
            v.display(m);       

            v.setStatusColorEncoding(0,new Color(100,255,100));
            v.setStatusColorEncoding(1,new Color(255,100,100));
            v.setStatusColorEncoding(2,new Color(0,255,0));
            v.setStatusColorEncoding(3,new Color(255,0,0));  
            
            v.setStatusFontEncoding(2, new Font(Font.SANS_SERIF,Font.BOLD, 10));
            v.setStatusFontEncoding(3, new Font(Font.SANS_SERIF,Font.BOLD, 10));

            KahinaChartViewPanel vp = new KahinaChartViewPanel();
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
    }
}
