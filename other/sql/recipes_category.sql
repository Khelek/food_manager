-- Table: cookbookcategory

-- DROP TABLE cookbookcategory;

CREATE TABLE cookbookcategory
(
  cookbookcategory_id integer NOT NULL,
  title text,
  rank integer,
  parentcategory_id integer,
  language_id integer,
  CONSTRAINT cookbookcategory_pkey PRIMARY KEY (cookbookcategory_id )
)
WITH (
  OIDS=FALSE
);
ALTER TABLE cookbookcategory
  OWNER TO postgres;
