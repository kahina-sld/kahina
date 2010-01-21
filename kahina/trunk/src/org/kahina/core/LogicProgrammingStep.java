package org.kahina.core;

public class LogicProgrammingStep extends KahinaStep
{
    //logic programming steps possess one of five types
    int type;
    
    public static final int CALL = 0;
    public static final int EXIT = 1;
    public static final int DET_EXIT = 2;
    public static final int FAIL = 3;
    public static final int REDO = 4;
}
