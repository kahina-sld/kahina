package org.kahina.core.data;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.List;

import org.kahina.io.database.DatabaseHandler;

public class KahinaObjectDbDataStore extends DbDataStore
{
	private enum FieldType
	{
		OBJECT, OBJECT_LIST, STRING, STRING_LIST, INT, INT_LIST;
	}

	private static final String TABLE_NAME_PREFIX = "KahinaObjectDbDataStore_";

	private static final String OBJECT_TABLE_NAME = TABLE_NAME_PREFIX + "object_values";

	private static final String STRING_TABLE_NAME = TABLE_NAME_PREFIX + "string_values";

	private static final String INT_TABLE_NAME = TABLE_NAME_PREFIX + "int_values";

	private PreparedStatement deleteObjectStatement;

	private PreparedStatement deleteStringStatement;

	private PreparedStatement deleteIntStatement;

	private PreparedStatement insertObjectStatement;

	private PreparedStatement insertStringStatement;

	private PreparedStatement insertIntStatement;

	private PreparedStatement selectObjectStatement;

	private PreparedStatement selectStringStatement;

	private PreparedStatement selectIntStatement;

	private List<String> fieldNames;

	private List<FieldType> fieldTypes;

	public KahinaObjectDbDataStore(Class<? extends KahinaObject> clazz, DbDataManager manager, DatabaseHandler db)
	{
		super(manager, db);
		createTablesIfNecessary();
		prepareStatements(clazz);
		examineClass(clazz);
	}

	private void createTablesIfNecessary()
	{
		String clientID = KahinaObjectDbDataStore.class.getName();
		if (db.isRegistered(clientID))
		{
			return;
		}
		db.execute("CREATE TABLE " + OBJECT_TABLE_NAME + " (" + "class_id INT, " + "object_id INT, " + "field_id INT, " + "value_class_id INT, " + "value_object_id INT, "
				+ "INDEX id (class_id, object_id, field_id)" + "); " + "CREATE TABLE KahinaObjectDbDataStore_string_values " + "(" + "class_id INT, " + "object_id INT, " + "field_id INT, "
				+ "value BLOB, " + "INDEX id (class_id, object_id, field_id)" + "); " + "CREATE TABLE KahinaObjectDbDataStore_int_values " + "(" + "class_id INT, " + "object_id INT, "
				+ "field_id INT, " + "value INT, " + "INDEX id (class_id, object_id, field_id)" + "); ");
		db.register(clientID);
	}

	private void prepareStatements(Class<? extends KahinaObject> clazz)
	{
		int classID = manager.getClassID(clazz);
		deleteObjectStatement = db.prepareStatement("DELETE FROM " + OBJECT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ?");
		insertObjectStatement = db.prepareStatement("INSERT INTO " + OBJECT_TABLE_NAME + " (class_id, object_id, field_id, value_class_id, value_object_id) VALUES (" + classID + ", ?, ?, ?, ?");
		selectObjectStatement = db.prepareStatement("SELECT value_class_id, value_object_id FROM " + OBJECT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ? AND field_id = ?");
		deleteStringStatement = db.prepareStatement("DELETE FROM " + STRING_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ?");
		insertStringStatement = db.prepareStatement("INSERT INTO " + STRING_TABLE_NAME + " (class_id, object_id, field_id, value) VALUES (" + classID + ", ?, ?, ?");
		selectStringStatement = db.prepareStatement("SELECT value FROM " + STRING_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ? AND field_id = ?");
		deleteIntStatement = db.prepareStatement("DELETE FROM " + INT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ?");
		insertIntStatement = db.prepareStatement("INSERT INTO " + INT_TABLE_NAME + " (class_id, object_id, field_id, value) VALUES (" + classID + ", ?, ?, ?");
		selectIntStatement = db.prepareStatement("SELECT value FROM " + INT_TABLE_NAME + " WHERE class_id = " + classID + " AND object_id = ? AND field_id = ?");
	}

	private void examineClass(Class<? extends KahinaObject> clazz)
	{
		fieldNames = new ArrayList<String>();
		fieldTypes = new ArrayList<FieldType>();
		for (Field field : clazz.getFields())
		{
			if (Modifier.isPublic(field.getModifiers()))
			{
				Class<?> type = field.getType();
				if (KahinaObject.class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.OBJECT);
				} else if (String.class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.STRING);
				} else if (int.class.isAssignableFrom(type))
				{
					fieldTypes.add(FieldType.INT);
					// TODO also allow Integer?
				} else
				{
					continue;
				}

				fieldNames.add(field.getName());
			}
		}
	}
	
	// TODO Still need a plan for updating the values of individual fields.

	@Override
	public KahinaObject retrieve(int id)
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void store(KahinaObject object, int id)
	{
		// TODO Auto-generated method stub

	}

	@Override
	public int store(KahinaObject object)
	{
		// TODO Auto-generated method stub
		return 0;
	}
}
