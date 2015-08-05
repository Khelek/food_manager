SELECT DISTINCT cookbook.cookbook_id, cookbook.title, im.text
  FROM cookbook 
  INNER JOIN cooksteps as im ON im.cookbook_id = cookbook.cookbook_id
  WHERE im.text ILIKE '%молоко%';
