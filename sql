select name, continent, 
case when continent = 'Europe' THEN 'Eurasia' else when continent =  'Asia' else
when continent like '%America' then 'America' else when continent = 'Caribbean' then 'America' else continent end from world where name like ('A%', 'B%')