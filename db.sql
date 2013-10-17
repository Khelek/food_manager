-- Table: products

DROP TABLE products;

CREATE TABLE products
(
  name text,
  fats real,
  proteins real,
  carbohydrates real,
  -- minerals
  ca real,
  pho real,
  na real,
  ka real,
  hl real,
  mg real,
  fe real,
  zi real,
  se real,
  ft real,
  jo real,
  -- vitamins
  a real,
  e real,
  d real,
  k real,
  c real,
  b1 real,
  b2 real,
  b5 real,
  b6 real,
  bc real,
  b12 real,
  pp real,
  h real,
  --
  calories real,
  types text,
  price real
)
WITH (
  OIDS=FALSE
);
ALTER TABLE products
  OWNER TO postgres;
