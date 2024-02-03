DROP TABLE IF EXISTS ingredients;

CREATE TABLE ingredients (
  ingredient_id INTEGER PRIMARY KEY
  ,ingredient TEXT NOT NULL
);

DROP TABLE IF EXISTS units;

CREATE TABLE units (
  unit_id INTEGER PRIMARY KEY
  ,unit TEXT NOT NULL
);

DROP TABLE IF EXISTS categories;

CREATE TABLE categories (
  category_id INTEGER PRIMARY KEY
  ,category TEXT NOT NULL
);

DROP TABLE IF EXISTS recipe_metadata;

CREATE TABLE recipe_metadata (
  recipe_id INTEGER PRIMARY KEY
  ,recipe_name TEXT NOT NULL UNIQUE
  ,source TEXT
  ,category_id INTEGER
  ,servings INTEGER
  ,duration TEXT
  ,created_on  DATETIME DEFAULT CURRENT_TIMESTAMP
  ,FOREIGN KEY (category_id)
    REFERENCES categories (category_id)
);

DROP TABLE IF EXISTS ingredients_in_recipes;

CREATE TABLE ingredients_in_recipes (
  ing_in_rec_id INTEGER PRIMARY KEY
  ,recipe_id INTEGER NOT NULL
  ,ingredient_id INTEGER NOT NULL
  ,amount REAL NOT NULL
  ,unit_id INTEGER NOT NULL
  ,specification TEXT
  ,preparation TEXT
  ,FOREIGN KEY (recipe_id)
    REFERENCES recipes_meta (recipie_id) ON DELETE CASCADE
  ,FOREIGN KEY (ingredient_id)
    REFERENCES ingredients (ingredient_id)
  ,FOREIGN KEY (unit_id)
    REFERENCES units (unit_id)
);

DROP TABLE IF EXISTS steps_in_recipes;

CREATE TABLE steps_in_recipes (
  step_in_rec_id INTEGER PRIMARY KEY
  ,recipe_id INTEGER NOT NULL
  ,step INTEGER NOT NULL
  ,description INTEGER NOT NULL
  ,FOREIGN KEY (recipe_id)
    REFERENCES recipes_meta (recipie_id) ON DELETE CASCADE
);
