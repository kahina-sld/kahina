package org.kahina.logic.sat.muc.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.StringTokenizer;

import org.kahina.logic.sat.muc.data.MUCStatistics;

public class MUCExtension
{
    // erzeugt aus cnf erweiterte CNF-Datei mit Selektorvariablen
    public static void extendCNFBySelVars(File orig, File extended, MUCStatistics stat)
    {
        BufferedReader in;
        BufferedWriter out;
        int numVarsOrig = 0;
        int numClauses = 0;
        // read in the original file
        try
        {
            out = new BufferedWriter(new FileWriter(extended));
            in = new BufferedReader(new FileReader(orig));
            String zeile = null;
            while ((zeile = in.readLine()) != null)
            {
                int i = 0;
                String string;
                StringBuffer buffer = new StringBuffer("");
                StringTokenizer st = new StringTokenizer(zeile);
                while (st.hasMoreTokens())
                {
                    string = st.nextToken();
                    if (string.equals("c"))
                    {
                        i++;
                        break;
                    }
                    if (string.equals("p") && i == 0)
                    {
                        i++;
                    }
                    else
                    {
                        if (!string.equals("0") && i == 0)
                        {
                            buffer.append(string + " ");
                            if (Integer.parseInt(string) > stat.highestID && Integer.parseInt(string) > numVarsOrig)
                            {
                                stat.highestID = Integer.parseInt(string);
                            }
                            if (Integer.parseInt(string) < -stat.highestID && Integer.parseInt(string) < -numVarsOrig)
                            {
                                stat.highestID = -Integer.parseInt(string);
                            }

                        }
                        else if (i == 1)
                        {
                            if (!string.equals("cnf"))
                            {
                                numVarsOrig = Integer.parseInt(string);
                                stat.highestID = Integer.parseInt(string);
                                i++;
                            }
                        }
                        else if (i == 2)
                        {
                            i++;
                            break;
                        }
                    }
                }
            }
            out.close();

            stat.numVarsExtended = stat.highestID;
            stat.numClausesOrGroups = stat.numVarsExtended - numVarsOrig;
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to generate extended CNF");
            System.exit(0);
        }
        // output extended version to target file
        try
        {
            out = new BufferedWriter(new FileWriter(extended));
            in = new BufferedReader(new FileReader(orig));
            String zeile = null;
            while ((zeile = in.readLine()) != null)
            {
                int i = 0;
                String string;
                StringBuffer buffer = new StringBuffer("");
                StringTokenizer st = new StringTokenizer(zeile);
                while (st.hasMoreTokens())
                {
                    string = st.nextToken();
                    if (string.equals("c"))
                    {
                        i++;
                        break;
                    }
                    if (string.equals("p") && i == 0)
                    {
                        i++;
                    }
                    else
                    {
                        if (!string.equals("0") && i == 0)
                        {
                            buffer.append(string + " ");
                        }
                        else if (i == 1)
                        {
                            if (!string.equals("cnf"))
                            {
                                i++;
                            }
                        }
                        else if (i == 2)
                        {
                            numClauses = Integer.parseInt(string);
                            buffer.append("p cnf " + (numVarsOrig + numClauses) + " " + numClauses);
                            out.write("" + buffer);
                            out.newLine();
                            i++;
                            break;
                        }
                    }
                }
                if (i == 0 && !zeile.equals(""))
                {
                    out.write("" + buffer + (-(stat.numVarsExtended + 1)) + " 0");
                    out.newLine();
                    stat.numVarsExtended++;
                }
            }
            out.close();
            stat.numClausesOrGroups = stat.numVarsExtended - stat.highestID;
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to generate extended CNF");
            System.exit(0);
        }
    }

    public static void extendGroupCNFBySelVars(File orig, File extended, MUCStatistics stat)
    {
        BufferedReader in;
        BufferedWriter out;
        int numVarsOrig = 0;
        int numClauses = 0;
        try
        {
            out = new BufferedWriter(new FileWriter(extended));
            in = new BufferedReader(new FileReader(orig));
            String zeile = null;
            int a = 0;
            while ((zeile = in.readLine()) != null)
            {
                int i = 0;
                String string;
                StringBuffer buffer = new StringBuffer("");
                StringTokenizer st = new StringTokenizer(zeile);
                while (st.hasMoreTokens())
                {
                    string = st.nextToken();
                    if (string.equals("c"))
                    {
                        i++;
                        break;
                    }
                    if (string.equals("p") && i == 0)
                    {
                        i++;
                    }
                    else
                    {
                        if (!string.equals("0") && i == 0)
                        {
                            if (!string.startsWith("{"))
                            {
                                buffer.append(string + " ");
                            }
                            else
                            {
                                String temp = string.substring(1, string.length() - 1);
                                a = Integer.parseInt(temp);
                            }
                        }
                        else if (i == 1)
                        {
                            if (!string.equals("gcnf"))
                            {
                                numVarsOrig = Integer.parseInt(string);
                                stat.highestID = Integer.parseInt(string);
                                stat.numVarsExtended = Integer.parseInt(string);
                                i++;
                            }
                        }
                        else if (i == 2)
                        {
                            numClauses = Integer.parseInt(string);
                            i++;
                        }
                        else if (i == 3)
                        {
                            stat.numVarsExtended = stat.numVarsExtended + 1 + Integer.parseInt(string);
                            buffer.append("p cnf " + (stat.numVarsExtended) + " " + numClauses);
                            out.write("" + buffer);
                            out.newLine();
                            i++;
                            break;
                        }
                    }
                }
                if (i == 0 && !zeile.equals(""))
                {
                    out.write("" + buffer + (-(a + 1 + numVarsOrig)) + " 0");
                    out.newLine();
                }
            }
            stat.numClausesOrGroups = stat.numVarsExtended - stat.highestID;
            out.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            System.err.println("IO error: failed to generate extended group CNF");
            System.exit(0);
        }

    }
    
    public static void main(String[] args)
    {
        if (args.length != 2)
        {
            System.err.println("Usage: java MUCExtension [cnf file] [extended file]");
        }
        File satFile = new File(args[0]);
        File extendedFile = new File(args[1]);
        MUCStatistics stat = new MUCStatistics();
        stat.instanceName = satFile.getAbsolutePath();
        MUCExtension.extendCNFBySelVars(satFile, extendedFile, stat);
        System.err.println(stat.toString());
    }
}
