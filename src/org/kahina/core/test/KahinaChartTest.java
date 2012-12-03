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

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.KahinaInstance;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.visual.chart.DisplayAllChartEdgesDecider;
import org.kahina.core.visual.chart.KahinaChartView;
import org.kahina.core.visual.chart.KahinaChartViewPanel;
import org.kahina.tralesld.TraleSLDInstance;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class KahinaChartTest
{
    public static void main(String[] args)
    {
        String fileName = "/home/johannes/pro/kahina/src/org/kahina/core/test/test-chart.xml";
        if (args.length > 0)
        {
            fileName = args[0];
        }
    	KahinaInstance<?, ?, ?, ?> kahina = new KahinaDefaultInstance();
        //DatabaseHandler data = new DatabaseHandler(new File("otoka.dat"));
        //KahinaChart m = KahinaChart.importXML(dom, KahinaDataHandlingMethod.DATABASE, data);
        KahinaChart m = KahinaChart.importXML(fileName);
        KahinaChartView v = new KahinaChartView(kahina);
        v.setDisplayDecider(new DisplayAllChartEdgesDecider());
        v.display(m);       

        v.setStatusColorEncoding(0,new Color(100,255,100)); //successful edge
        v.setStatusColorEncoding(1,new Color(255,100,100)); //unsuccessful edge
        v.setStatusColorEncoding(2,new Color(0,255,0)); //highlighted successful edge
        v.setStatusColorEncoding(3,new Color(255,0,0)); //highlighted unsuccessful edge
        
        //highlighted edges also have captions in bold font
        v.setStatusFontEncoding(2, new Font(Font.SANS_SERIF,Font.BOLD, 10));
        v.setStatusFontEncoding(3, new Font(Font.SANS_SERIF,Font.BOLD, 10));

        KahinaChartViewPanel vp = new KahinaChartViewPanel(kahina);
        JScrollPane vpp = new JScrollPane(vp);
        vpp.setBounds(0, 0, 500, 300);
        JFrame w = new JFrame("Kahina ChartView Demo");
        w.setSize(510, 330);
        w.setLayout(null);
        w.add(vpp);
        w.setVisible(true);
        w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        vp.setView(v);        
    }
}
