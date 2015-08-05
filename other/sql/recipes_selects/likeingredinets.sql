SELECT 
	cookingredients.cookbook_id, 
	ingr.title_ru, 
	cookingredients.count,
	types.title_ru, 
	cookingredients.comment
FROM cookingredients 
INNER JOIN cookbookingredients as ingr ON  cookingredients.cookbookingredients_id = ingr.cookbookingredients_id
INNER JOIN cookbookingredientstype as types ON cookingredients.cookbookingredientstype_id = types.cookbookingredientstype_id
INNER JOIN cookbook ON cookingredients.cookbook_id = 1673
GROUP BY cookingredients.cookbook_id, 
	ingr.title_ru, 
	cookingredients.count,
	types.title_ru, 
	cookingredients.comment;
