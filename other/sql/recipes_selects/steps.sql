SELECT 
  cooksteps.src, 
  cooksteps.text
FROM 
  public.cookbook, 
  public.cooksteps
WHERE 
  cookbook.cookbook_id = cooksteps.cookbook_id AND cookbook.cookbook_id = 1683;
