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

