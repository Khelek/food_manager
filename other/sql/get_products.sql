SELECT prod.name, nutrients.name, info.value
  FROM (SELECT products._id, products.name FROM products 
	INNER JOIN products_category ON products_category._id = products.category  
	WHERE products_category.name = 'Молочные продукты'	
	AND NOT ARRAY[products.name] && ARRAY['Сыр камамбер, 24%']
	ORDER BY random() LIMIT 10) as prod
  INNER JOIN info ON info.product = prod._id
  INNER JOIN nutrients ON info.nutrient = nutrients._id
  AND ARRAY[nutrients.name] && ARRAY['Калорийность', 'Стоимость', 'Белки', 'Углеводы', 'Жиры']
  ORDER BY prod.name;