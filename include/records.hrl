-record(person, {activity = 0, 
                 sex = <<>>, 
                 weight = 0, 
                 height = 0, 
                 age = 0, 
                 budget = 0, 
                 exclude_types = [], 
                 exclude_products = [],
                 family = []}). % family = [#person{}]
-record(auth, {login = <<>>, password = <<>>, registration = false}).
-record(answer, {success = true, response}).
-record(product, {name = <<>>, ccal = 0, price = 0, mass = 0}).
-record(table, {products = [#product{}], family = []}). %% family - продукты для членов семьи
-record(request, {login = <<>>, action = <<>>, body}).
-record(find_recipes, {in_all = <<>>, 
                       in_name = <<>>, 
                       in_text = <<>>, 
                       in_ingredients = <<>>, 
                       time_from = 0, 
                       time_to = 0,
                       catalog_id = 0,
                       complexity = 0,
                       amount = 0}).
-record(recipe, {name = <<>>, time = <<>>,
                 number_portion = 0,
                 complexity = 0,
                 ingredients = [],
                 steps_exists = true,
                 how_to_cook = <<>>,
                 steps = []}).
-record(recipes, {recipes = [#recipe{}]}).
-record(step, {photo_url = <<>>, comment = <<>>}).
