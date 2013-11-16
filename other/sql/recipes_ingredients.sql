-- Table: cookbookingredients

-- DROP TABLE cookbookingredients;

CREATE TABLE cookbookingredients
(
  cookbookingredients_id integer NOT NULL,
  titleru text,
  titleen text,
  gramm integer,
  proteins numeric,
  fats numeric,
  carbohydrates numeric,
  kcal numeric,
  water numeric,
  CONSTRAINT cookbookingredients_pkey PRIMARY KEY (cookbookingredients_id )
)
WITH (
  OIDS=FALSE
);
ALTER TABLE cookbookingredients
  OWNER TO postgres;
