package org.kahina.core.data.lightweight;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.kahina.core.KahinaException;
import org.kahina.core.data.DbDataManager;
import org.kahina.core.data.DbDataStore;
import org.kahina.core.data.KahinaObject;
import org.kahina.core.data.LightweightKahinaObject;
import org.kahina.core.io.database.DatabaseHandler;

public class LightweightDbStore extends DbDataStore
{
	private static final String CLIENT_ID = LightweightDbStore.class.getName();

	private static final String TABLE_NAME_PREFIX = LightweightDbStore.class
			.getSimpleName();

	private static final String FIELD_VALUES_INT_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_field_values_int";

	private static final String FIELD_VALUES_LONG_VARCHAR_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_field_values_long_varchar";

	private static final String COLLECTION_ELEMENTS_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_collection_elements";

	private static final String MAP_ENTRIES_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_map_entries";

	private static final String STRING_VALUES_TABLE_NAME = TABLE_NAME_PREFIX
			+ "_string_values";

	private Constructor<?> constructor;

	private Field[] fields;

	private LVT[] lvts;

	private PreparedStatement selectFieldValueIntStatement;

	private PreparedStatement updateFieldValueIntStatement;

	private PreparedStatement insertFieldValueIntStatement;

	private PreparedStatement selectFieldValueLongVarcharStatement;

	private PreparedStatement updateFieldValueLongVarcharStatement;

	private PreparedStatement insertFieldValueLongVarcharStatement;

	public LightweightDbStore(Class<LightweightKahinaObject> datatype)
	{
		try
		{
			constructor = datatype.getConstructor();
		} catch (NoSuchMethodException e)
		{
			throw new KahinaException("Lightweight datatype " + datatype
					+ " fails to provide zero-arg constructor.", e);
		}
		examineType(datatype);
	}

	private void examineType(Class<LightweightKahinaObject> datatype)
	{
		List<Field> fields = new ArrayList<Field>();
		List<LVT> lvts = new ArrayList<LVT>();
		Field[] allFields = datatype.getFields();
		Arrays.sort(allFields, new Comparator<Field>()
		{

			@Override
			public int compare(Field o1, Field o2)
			{
				return o1.getName().compareTo(o2.getName());
			}

		});
		for (Field field : datatype.getFields())
		{
			if (Modifier.isPublic(field.getModifiers()))
			{
				LVT lvt = LVT.createLVT((Class<?>) field.getGenericType());
				if (lvt != null)
				{
					fields.add(field);
					lvts.add(lvt);
				}
			}
		}
		this.fields = fields.toArray(new Field[fields.size()]);
		this.lvts = lvts.toArray(new LVT[lvts.size()]);
	}

	@Override
	public void initialize(DbDataManager manager, DatabaseHandler db)
	{
		super.initialize(manager, db);
		createTablesIfNecessary();
		prepareStatements();
	}

	private void createTablesIfNecessary()
	{
		if (db.isRegistered(CLIENT_ID))
		{
			return;
		}
		db.createTable(FIELD_VALUES_INT_TABLE_NAME, "object_id INT",
				"field_id INT", "value INT",
				"PRIMARY KEY (object_id, field_id)");
		db.createTable(FIELD_VALUES_LONG_VARCHAR_TABLE_NAME, "object_id INT",
				"field_id INT", "value LONG VARCHAR",
				"PRIMARY KEY (object_id, field_id)");
		db.createTable(COLLECTION_ELEMENTS_TABLE_NAME, "collection_id INT",
				"element_id INT");
		db.createIndex(COLLECTION_ELEMENTS_TABLE_NAME, "_collection_id",
				"collection_id");
		db.createTable(MAP_ENTRIES_TABLE_NAME, "map_id INT", "key_id INT",
				"value_id INT", "PRIMARY KEY (map_id, key_id)");
		db.createTable(STRING_VALUES_TABLE_NAME, "value_id INT",
				"value LONG VARCHAR", "PRIMARY KEY value_id");
		db.register(CLIENT_ID);
	}

	private void prepareStatements()
	{
		selectFieldValueIntStatement = db.prepareStatement("SELECT value FROM "
				+ FIELD_VALUES_INT_TABLE_NAME
				+ " WHERE object_id = ? AND field_id = ?");
		updateFieldValueIntStatement = db.prepareStatement("UPDATE "
				+ FIELD_VALUES_INT_TABLE_NAME
				+ " SET value = ? WHERE object_id = ? AND field_id = ?");
		insertFieldValueIntStatement = db.prepareStatement("INSERT INTO "
				+ FIELD_VALUES_INT_TABLE_NAME
				+ " (object_id, field_id, value) VALUES (?, ?, ?)");
		selectFieldValueLongVarcharStatement = db
				.prepareStatement("SELECT value FROM "
						+ FIELD_VALUES_LONG_VARCHAR_TABLE_NAME
						+ " WHERE object_id = ? AND field_id = ?");
		updateFieldValueLongVarcharStatement = db.prepareStatement("UPDATE "
				+ FIELD_VALUES_LONG_VARCHAR_TABLE_NAME
				+ " SET value = ? WHERE object_id = ? AND field_id = ?");
		insertFieldValueLongVarcharStatement = db
				.prepareStatement("INSERT INTO "
						+ FIELD_VALUES_LONG_VARCHAR_TABLE_NAME
						+ " (object_id, field_id, value) VALUES (?, ?, ?)");
	}

	@Override
	public KahinaObject retrieve(int id)
	{
		try
		{
			KahinaObject result = (KahinaObject) constructor.newInstance();
			result.setID(id);
			for (int fieldID = 0; fieldID < fields.length; fieldID++)
			{
				lvts[fieldID].retrieveFieldValue(id, fieldID, fields[fieldID],
						result, this);
			}
			return result;
		} catch (IllegalArgumentException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (InstantiationException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		} catch (InvocationTargetException e)
		{
			throw new KahinaException("Could not retrieve object.", e);
		}
	}

	@Override
	public void store(KahinaObject object)
	{
		try
		{
			for (int fieldID = 0; fieldID < fields.length; fieldID++)
			{
				lvts[fieldID].storeFieldValue(object.getID(), fieldID,
						fields[fieldID], object, this);
			}
		} catch (IllegalAccessException e)
		{
			throw new KahinaException("Could not store object.", e);
		}
	}

	int retrieveInt(int id, int fieldID)
	{
		try
		{
			selectFieldValueIntStatement.setInt(1, id);
			selectFieldValueIntStatement.setInt(2, fieldID);
			return db.queryInteger(selectFieldValueIntStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	void storeInt(int objectID, int fieldID, int value)
	{
		try
		{
			updateFieldValueIntStatement.setInt(1, value);
			updateFieldValueIntStatement.setInt(2, objectID);
			updateFieldValueIntStatement.setInt(3, fieldID);

			if (updateFieldValueIntStatement.executeUpdate() == 0)
			{
				insertFieldValueIntStatement.setInt(1, objectID);
				insertFieldValueIntStatement.setInt(2, fieldID);
				insertFieldValueIntStatement.setInt(3, value);
				insertFieldValueIntStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	String retrieveLongVarchar(int id, int fieldID)
	{
		try
		{
			selectFieldValueLongVarcharStatement.setInt(1, id);
			selectFieldValueLongVarcharStatement.setInt(2, fieldID);
			return db.queryString(selectFieldValueLongVarcharStatement);
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

	void storeLongVarchar(int objectID, int fieldID, String value)
	{
		try
		{
			updateFieldValueLongVarcharStatement.setString(1, value);
			updateFieldValueLongVarcharStatement.setInt(2, objectID);
			updateFieldValueLongVarcharStatement.setInt(3, fieldID);

			if (updateFieldValueLongVarcharStatement.executeUpdate() == 0)
			{
				insertFieldValueLongVarcharStatement.setInt(1, objectID);
				insertFieldValueLongVarcharStatement.setInt(2, fieldID);
				insertFieldValueLongVarcharStatement.setString(3, value);
				insertFieldValueLongVarcharStatement.execute();
			}
		} catch (SQLException e)
		{
			throw new KahinaException("SQL error.", e);
		}
	}

}
