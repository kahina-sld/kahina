package org.kahina.core.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JProgressBar;


public class KahinaProgressBar extends JPanel implements ActionListener
{
    private static final long serialVersionUID = 5212055650892082890L;
    
    JProgressBar progressBar;
    JButton cancelButton;
    
    boolean canceled = false;
    
    public KahinaProgressBar()
    {
        progressBar = new JProgressBar(0,100);
        progressBar.setStringPainted(true);
        this.add(progressBar);
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);
        this.add(cancelButton);
    }

    public void tellTaskProgress(int percentage, String statusMessage)
    {
        if (percentage < 0)
        {
            System.err.println("WARNING: KahinaProgressBar was told to display " + percentage + " %, displaying 0 % instead.");
            progressBar.setValue(0);
        }
        else if (percentage > 100)
        {
            System.err.println("WARNING: KahinaProgressBar was told to display " + percentage + " %, displaying 100 % instead.");
            progressBar.setValue(100);
        }
        else
        {
            progressBar.setValue(percentage);
        }
        progressBar.setString(statusMessage);
        revalidate();
    }
    
    public boolean cancelButtonClicked()
    {
        return canceled;
    }

    @Override
    public void actionPerformed(ActionEvent e)
    {
        if (e.getActionCommand().equals("Cancel"))
        {
            canceled = true;
        }
    }
    
    public static KahinaProgressBar addToPanel(JPanel panel)
    {
        KahinaProgressBar progressBar = new KahinaProgressBar();
        panel.add(progressBar);
        return progressBar;
    }
    
    public static void removeProgressBar(JPanel panel, KahinaProgressBar progressBar)
    {
        panel.remove(progressBar);
    }
}
