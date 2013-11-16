-- Table: cookbookingredientstypelist

-- DROP TABLE cookbookingredientstypelist;

CREATE TABLE cookbookingredientstypelist
(
  cookbookingredientstypelist_id integer NOT NULL,
  titleru text,
  titleen text,
  CONSTRAINT cookbookingredientstypelist_pkey PRIMARY KEY (cookbookingredientstypelist_id )
)
WITH (
  OIDS=FALSE
);
ALTER TABLE cookbookingredientstypelist
  OWNER TO postgres;
