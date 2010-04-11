package org.kahina.core.data;

public class KahinaTypeException extends Exception
{
    String badType;
    String requestedType;
    
   public KahinaTypeException(String badType, String requestedType)
   {
       this.badType = badType;
       this.requestedType = requestedType;
   }
   
   public String getMessage()
   {
       return "Incompatible type: " + badType + ". Should be: " + requestedType;
   }
}
