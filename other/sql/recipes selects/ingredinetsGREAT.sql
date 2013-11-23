SELECT 
  cooksteps.id,
  searsh_result.cookbook_id,
  cooksteps.src, 
  cooksteps.text
FROM 
  public.cookbook, 
  public.cooksteps,
  (SELECT DISTINCT cookbook.cookbook_id, cookbook.title, cookbook.timecooking, cookbook.servingsnumber,
	cookbook.isstepphoto, cookbook.description
  FROM cookbook 
  INNER JOIN cookingredients as ingrs ON ingrs.cookbook_id = cookbook.cookbook_id
  INNER JOIN cookbookingredients as ingrname ON ingrname.cookbookingredients_id = ingrs.cookbookingredients_id
  INNER JOIN cooksteps as steps ON steps.cookbook_id = cookbook.cookbook_id
  WHERE 
	ingrname.title_ru ILIKE '%молоко%' 
	AND 
	(steps.text ILIKE '%молоко%' OR cookbook.description ILIKE '%ччмолоко%')
	AND 
	cookbook.title ILIKE '%%'
	AND 
	cookbook.cookbookcategory_id = 6
	AND
	(cookbook.timecooking >= 0 AND cookbook.timecooking < 30)
  ORDER BY cookbook.cookbook_id
  LIMIT 16) as searsh_result
WHERE 
  cookbook.cookbook_id = cooksteps.cookbook_id AND cookbook.cookbook_id = searsh_result.cookbook_id;

