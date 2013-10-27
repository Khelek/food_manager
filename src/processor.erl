-module(processor).

-export([calculate_products_list/1]).

-include("include/records.hrl").

calculate_products_list(Person) when is_record(Person, person) ->
    ExcludeItems = {Person#person.exclude_types, Person#person.exclude_products},
    Products = db:get_products(ExcludeItems), 
    PersonalData = {Person#person.sex, 
                    Person#person.weight, 
                    Person#person.height,
                    Person#person.age},
    Substances = calculate_substances(PersonalData), 
    calculate_grammsAtMonth(Products, Substances, Person#person.budget). %%еще чтото для семьи 

calculate_grammsAtMonth(Products, {Calories, FPC, Minerals, Vitamins}, Budget) ->
    DropSeconds = fun(L) -> lists:map(fun({_First, Second}) -> Second end, L) end,
    MineralsCoeff = DropSeconds(Minerals),
    VitaminsCoeff = DropSeconds(Vitamins),
    Coefficients = lists:flatten([Calories, Budget, FPC, MineralsCoeff, VitaminsCoeff]), 
    matlab:solve_equations(Products, Coefficients).

calculate_substances({PersonalData, Activity}) ->
    CPD = calories_per_day(PersonalData),
    {RealCPD, Coefficients} = 
        case Activity of 
            1 ->
                RealCPD = CPD + CPD / 6,
                {RealCPD, {0.12, 0.30, 0.58}};
            2 ->
                RealCPD = CPD + CPD / 3,
                {RealCPD, {0.12, 0.30, 0.58}};
            3 ->
                RealCPD = CPD + CPD / 2,
                {RealCPD, {0.11, 0.30, 0.59}};
            4 ->
                RealCPD = CPD + CPD * 2 / 3,
                {RealCPD, {0.11, 0.30, 0.59}};
            5 ->
                RealCPD = CPD +  CPD,
                {RealCPD, {0.11, 0.33, 0.56}}
                    
        end,
    Calories = RealCPD * 30,
    FPC = [Fats, Proteins, Carbohidrates] = fats_proteins_carbohydrates(Calories, Coefficients),
    Vitamins = vitamins(PersonalData),
    Minerals = minerals(PersonalData),
    {Calories, FPC, Minerals, Vitamins}. 

calories_per_day({Sex, Weight, Height, Age}) ->
    case Sex of
        male ->
            66 + (13.7 * Weight) + (5 * Height) - (6.8 * Age);
        female ->
            655 + (9.6 * Weight) + (1.8 * Height) - (4.5 * Age)
    end.

fats_proteins_carbohydrates(Calories, {CoefFat, CoefProt, CoefCarbo}) ->
    [fats(Calories, CoefFat), 
     proteins(Calories, CoefProt), 
     carbohydrates(Calories, CoefCarbo)].

fats(Calories, Coeff) ->
    (Calories * Coeff) / 4.

proteins(Calories, Coeff) ->
    (Calories * Coeff) / 9.

carbohydrates(Calories, Coeff) ->
    (Calories * Coeff) / 4.

vitamins({Sex, Weight, Height, Age}) when Age =< 0.5 ->
    [{a, 0.4}, {e, 3}, {d, 0.01}, {k, 0.005}, {c, 30}, {b1, 0.3}, {b2, 0.4},
     {b5, 2}, {b6, 0.3}, {bc, 0.025}, {b12, 0.0003}, {pp, 5}, {h, 0.01}];
vitamins({Sex, Weight, Height, Age}) when Age > 0.5, Age =< 1 ->
    [{a, 0.4}, {e, 4}, {d, 0.01}, {k, 0.01}, {c, 35}, {b1, 0.4}, {b2, 0.5},
     {b5, 3}, {b6, 0.6}, {bc, 0.035}, {b12, 0.0005}, {pp, 6}, {h, 0.015}];
vitamins({Sex, Weight, Height, Age}) when Age > 1, Age =< 3 ->
    [{a, 0.45}, {e, 6}, {d, 0.01}, {k, 0.015}, {c, 40}, {b1, 0.7}, {b2, 0.8},
     {b5, 3}, {b6, 1}, {bc, 0.05}, {b12, 0.0007}, {pp, 9}, {h, 0.02}];
vitamins({Sex, Weight, Height, Age}) when Age > 3, Age =< 6 ->
    [{a, 0.5}, {e, 7}, {d, 0.0025}, {k, 0.02}, {c, 45}, {b1, 0.9}, {b2, 1.1},
     {b5, 4}, {b6, 1.1}, {bc, 0.075}, {b12, 0.001}, {pp, 12}, {h, 0.025}];
vitamins({Sex, Weight, Height, Age}) when Age > 6, Age =< 10 ->
    [{a, 0.7}, {e, 7}, {d, 0.0025}, {k, 0.03}, {c, 45}, {b1, 1}, {b2, 1.2},
     {b5, 5}, {b6, 1.4}, {bc, 0.1}, {b12, 0.0014}, {pp, 7}, {h, 0.03}];

vitamins({Sex, Weight, Height, Age}) when Age > 10, Age =< 14, Sex == male ->
    [{a, 1}, {e, 10}, {d, 0.0025}, {k, 0.045}, {c, 50}, {b1, 1.3}, {b2, 1.5},
     {b5, 5}, {b6, 1.7}, {bc, 0.15}, {b12, 0.002}, {pp, 17}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 14, Age =< 18, Sex == male ->
    [{a, 1}, {e, 10}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.5}, {b2, 1.8},
     {b5, 5}, {b6, 1}, {bc, 0.05}, {b12, 0.002}, {pp, 9}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 18, Age =< 24, Sex == male ->
    [{a, 1}, {e, 10}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.5}, {b2, 1.7},
     {b5, 5}, {b6, 1}, {bc, 0.05}, {b12, 0.002}, {pp, 9}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 24, Age =< 50, Sex == male ->
    [{a, 1}, {e, 10}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.5}, {b2, 1.7},
     {b5, 5}, {b6, 1}, {bc, 0.05}, {b12, 0.002}, {pp, 9}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 50, Sex == male ->
    [{a, 1}, {e, 10}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.2}, {b2, 1.4},
     {b5, 5}, {b6, 1}, {bc, 0.05}, {b12, 0.002}, {pp, 9}, {h, 0.06}];

vitamins({Sex, Weight, Height, Age}) when Age > 10, Age =< 14, Sex == female ->
    [{a, 0.8}, {e, 8}, {d, 0.0025}, {k, 0.045}, {c, 50}, {b1, 1.1}, {b2, 1.3},
     {b5, 5}, {b6, 1.4}, {bc, 0.15}, {b12, 0.002}, {pp, 15}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 14, Age =< 18, Sex == female ->
    [{a, 0.8}, {e, 8}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.1}, {b2, 1.3},
     {b5, 5}, {b6, 1.5}, {bc, 0.18}, {b12, 0.002}, {pp, 15}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 18, Age =< 24, Sex == female ->
    [{a, 0.8}, {e, 8}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.1}, {b2, 1.3},
     {b5, 5}, {b6, 1.6}, {bc, 0.18}, {b12, 0.002}, {pp, 15}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 24, Age =< 50, Sex == female ->
    [{a, 0.8}, {e, 8}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1.1}, {b2, 1.3},
     {b5, 5}, {b6, 1.6}, {bc, 0.18}, {b12, 0.002}, {pp, 15}, {h, 0.06}];
vitamins({Sex, Weight, Height, Age}) when Age > 50, Sex == female ->
    [{a, 0.8}, {e, 8}, {d, 0.0025}, {k, 0.015}, {c, 60}, {b1, 1}, {b2, 1.2},
     {b5, 5}, {b6, 1.6}, {bc, 0.18}, {b12, 0.002}, {pp, 15}, {h, 0.06}].

minerals({Sex, Weight, Height, Age}) when Age =< 10 ->
    [{ca, 800}, {pho, 1000}, {na, 4000}, {ka, 2500}, {hl, 5000}, {mg, 300}, {fe, 15}, {zi, 10}, {se, 0.5}, {ft, 0.5}, {jo, 0.1}];
minerals({Sex, Weight, Height, Age}) when Age > 10, Age =< 25 ->
    [{ca, 900}, {pho, 1200}, {na, 5000}, {ka, 3500}, {hl, 6000}, {mg, 400}, {fe, 15}, {zi, 12}, {se, 0.5}, {ft, 0.5}, {jo, 0.15}];
minerals({Sex, Weight, Height, Age}) when Age > 25 ->
    [{ca, 1000}, {pho, 1500}, {na, 6000}, {ka, 5000}, {hl, 7000}, {mg, 500}, {fe, 15}, {zi, 15}, {se, 0.5}, {ft, 0.5}, {jo, 0.2}].
