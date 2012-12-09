package org.kahina.core.test;

import java.awt.Color;
import java.awt.Font;

import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.kahina.core.KahinaDefaultInstance;
import org.kahina.core.KahinaInstance;
import org.kahina.core.data.chart.KahinaChart;
import org.kahina.core.visual.chart.DisplayAllChartEdgesDecider;
import org.kahina.core.visual.chart.KahinaRecursiveChartView;
import org.kahina.core.visual.chart.KahinaRecursiveChartViewPanel;

public class KahinaRecursiveChartTest
{
    public static void main(String[] args)
    {
        String fileName = "src/org/kahina/qtype/test/qtype-test-chart.xml";
        if (args.length > 0)
        {
            fileName = args[0];
        }
        KahinaInstance<?, ?, ?, ?> kahina = new KahinaDefaultInstance();
        //DatabaseHandler data = new DatabaseHandler(new File("otoka.dat"));
        //KahinaChart m = KahinaChart.importXML(dom, KahinaDataHandlingMethod.DATABASE, data);
        KahinaChart m = KahinaChart.importXML(fileName);
        KahinaRecursiveChartView v = new KahinaRecursiveChartView(kahina);
        v.setDisplayDecider(new DisplayAllChartEdgesDecider());
        v.display(m);       

        v.setStatusColorEncoding(0,new Color(100,180,100)); //successful edge
        v.setStatusColorEncoding(1,new Color(180,100,100)); //unsuccessful edge
        v.setStatusColorEncoding(2,new Color(250,250,150)); //active edge
        v.setStatusColorEncoding(3,new Color(0,255,0)); //highlighted successful edge
        v.setStatusColorEncoding(4,new Color(255,0,0)); //highlighted unsuccessful edge
        
        //highlighted edges also have captions in bold font
        v.setStatusFontEncoding(3, new Font(Font.SANS_SERIF,Font.BOLD, 10));
        v.setStatusFontEncoding(4, new Font(Font.SANS_SERIF,Font.BOLD, 10));

        KahinaRecursiveChartViewPanel vp = new KahinaRecursiveChartViewPanel(kahina);
        JScrollPane vpp = new JScrollPane(vp);
        //vpp.setBounds(0, 0, 500, 300);
        JFrame w = new JFrame("Kahina RecursiveChartView Demo");
        w.setSize(510, 330);
        w.setLayout(new BoxLayout(w.getContentPane(), BoxLayout.LINE_AXIS));
        w.add(vpp);
        w.setVisible(true);
        w.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        vp.setView(v);        
    }
}
