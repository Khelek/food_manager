-record(person, {activity = 0, 
               sex = <<>>, 
               weight = 0, 
               height = 0, 
               age = 0, 
               budget = 0, 
               exclude_types = [], 
               exclude_products = [],
               family = []}).
-record(auth, {login = <<>>, password = <<>>, registration = false}).
-record(answer, {success = true, response = <<>>}).
-record(table, {products = []}).
-record(product, {name = <<>>, ccal = 0, price = 0}).



