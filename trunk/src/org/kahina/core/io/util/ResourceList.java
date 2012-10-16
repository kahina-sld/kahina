package org.kahina.core.io.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

/**
 * list resources available from the classpath @ *
 */
public class ResourceList
{

    /**
     * for all elements of java.class.path get a Collection of resources Pattern
     * pattern = Pattern.compile(".*"); gets all resources
     * 
     * @param pattern
     *            the pattern to match
     * @return the resources in the order they are found
     */
    public static Collection<String> getResources(
        final Pattern pattern){
        final ArrayList<String> retval = new ArrayList<String>();
        final String classPath = System.getProperty("java.class.path", ".");
        final String[] classPathElements = classPath.split(":");
        for(final String element : classPathElements){
            retval.addAll(getResources(element, pattern));
        }
        return retval;
    }

    public static Collection<String> getResources(
        final String element,
        final Pattern pattern)
        {
        final ArrayList<String> retval = new ArrayList<String>();
        final File file = new File(element);
        if(file.isDirectory()){
            retval.addAll(getResourcesFromDirectory(file, pattern));
        } else{
            retval.addAll(getResourcesFromJarFile(file, pattern));
        }
        return retval;
    }

    private static Collection<String> getResourcesFromJarFile(
        final File file,
        final Pattern pattern){
        final ArrayList<String> retval = new ArrayList<String>();
        ZipFile zf;
        try{
            zf = new ZipFile(file);
        } catch(final ZipException e){
            throw new Error(e);
        } catch(final IOException e){
            throw new Error(e);
        }
        final Enumeration e = zf.entries();
        while(e.hasMoreElements()){
            final ZipEntry ze = (ZipEntry) e.nextElement();
            final String fileName = ze.getName();
            final boolean accept = pattern.matcher(fileName).matches();
            if(accept){
                retval.add(fileName);
            }
        }
        try{
            zf.close();
        } catch(final IOException e1){
            throw new Error(e1);
        }
        return retval;
    }

    private static Collection<String> getResourcesFromDirectory(
        final File directory,
        final Pattern pattern){
        final ArrayList<String> retval = new ArrayList<String>();
        final File[] fileList = directory.listFiles();
        for(final File file : fileList){
            if(file.isDirectory()){
                retval.addAll(getResourcesFromDirectory(file, pattern));
            } else{
                try{
                    final String fileName = file.getCanonicalPath();
                    final boolean accept = pattern.matcher(fileName).matches();
                    if(accept){
                        retval.add(fileName);
                    }
                } catch(final IOException e){
                    throw new Error(e);
                }
            }
        }
        return retval;
    }
    
    public static String getProgramDirectory(Object object)
    {
        String PROGRAM_DIRECTORY = "";
        try {
            //Attempt to get the path of the actual JAR file, because the working directory is frequently not where the file is.
            //Example: file:/D:/all/Java/TitanWaterworks/TitanWaterworks-en.jar!/TitanWaterworks.class
            //Another example: /D:/all/Java/TitanWaterworks/TitanWaterworks.class
            PROGRAM_DIRECTORY = object.getClass().getClassLoader().getResource("").getPath(); // Gets the path of the class or jar.

            //Find the last ! and cut it off at that location. If this isn't being run from a jar, there is no !, so it'll cause an exception, which is fine.
            try {
                PROGRAM_DIRECTORY = PROGRAM_DIRECTORY.substring(0, PROGRAM_DIRECTORY.lastIndexOf('!'));
            } catch (Exception e) { }

            //Find the last / and cut it off at that location.
            PROGRAM_DIRECTORY = PROGRAM_DIRECTORY.substring(0, PROGRAM_DIRECTORY.lastIndexOf('/') + 1);
            //If it starts with /, cut it off.
            if (PROGRAM_DIRECTORY.startsWith("/")) PROGRAM_DIRECTORY = PROGRAM_DIRECTORY.substring(1, PROGRAM_DIRECTORY.length());
            //If it starts with file:/, cut that off, too.
            if (PROGRAM_DIRECTORY.startsWith("file:/")) PROGRAM_DIRECTORY = PROGRAM_DIRECTORY.substring(6, PROGRAM_DIRECTORY.length());
        } catch (Exception e) {
            //Current working directory instead.
        }
        return PROGRAM_DIRECTORY;
    }

    /**
     * list the resources that match args[0]
     * 
     * @param args
     *            args[0] is the pattern to match, or list all resources if
     *            there are no args
     */
    public static void main(final String[] args){
        Pattern pattern;
        if(args.length < 1){
            pattern = Pattern.compile(".*");
        } else{
            pattern = Pattern.compile(args[0]);
        }
        final Collection<String> list = ResourceList.getResources(pattern);
        for(final String name : list){
            System.out.println(name);
        }
    }
}  
